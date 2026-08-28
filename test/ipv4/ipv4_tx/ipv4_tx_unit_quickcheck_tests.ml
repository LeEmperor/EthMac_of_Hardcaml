(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "ipv4_tx_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Ipv4_tx

   Typed examples and generated properties covering the 20-byte IPv4 header, its checksum,
   the payload pass-through, and the framing signals around both.

   Every case the superseded [ipv4_tx_legacy_assertion_test.ml] printed is here as an
   assertion - it printed PASS/FAIL lines but its own summary was the only thing that
   failed the build, and only for the eight scenarios it happened to list. The Quickcheck
   properties generalize those eight over payload length, protocol, endpoint configuration
   and backpressure schedule.

   Two independent oracles. [golden_datagram] rebuilds the whole datagram from
   [Hardcaml_verif.Ip_udp], which catches a wrong field as a byte mismatch; separately,
   [header_checksum_residue] sums the emitted header's own 16-bit words and requires
   0xFFFF, which is the receiver's test and does not reuse the builder at all. A checksum
   that is wrong in the same way in both the RTL and [Ip_udp] would pass the first and
   fail the second.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Ipv4_tx_testbench

let header_length = Ip_udp.Ipv4.header_length

(* The receiver's check: sum the header's 16-bit words - checksum field included this time
   - with end-around carry. A correct header sums to 0xFFFF. *)
let header_checksum_residue bytes =
  let header = List.take bytes header_length in
  let words =
    List.chunks_of header ~length:2
    |> List.map ~f:(function
      | [ high; low ] -> (high lsl 8) lor low
      | chunk ->
        raise_s [%message "header is not an even number of bytes" (chunk : int list)])
  in
  let sum = List.fold words ~init:0 ~f:( + ) in
  let rec fold sum =
    if sum > 0xFFFF then fold ((sum land 0xFFFF) + (sum lsr 16)) else sum
  in
  fold sum
;;

let check_datagram runner ~protocol ~payload (observation : Datagram_observation.t) =
  [%test_result: int list]
    ~message:runner.name
    observation.bytes
    ~expect:(runner.golden_datagram ~protocol ~payload);
  [%test_result: int]
    ~message:(runner.name ^ ": tlast lands on the final byte")
    observation.tlast_index
    ~expect:(header_length + List.length payload - 1);
  [%test_result: int]
    ~message:(runner.name ^ ": tx_start pulses")
    observation.tx_start_pulses
    ~expect:1;
  [%test_result: int]
    ~message:(runner.name ^ ": header checksum residue")
    (header_checksum_residue observation.bytes)
    ~expect:0xFFFF
;;

let check_run ?ready ?source_valid ?en runner ~protocol ~payload =
  check_datagram
    runner
    ~protocol
    ~payload
    (run_datagram ?ready ?source_valid ?en runner ~payload ~protocol)
;;

let%test_unit "a nominal UDP datagram is header ++ payload" =
  List.iter runners ~f:(fun runner ->
    check_run runner ~protocol:protocol_udp ~payload:(make_payload 26))
;;

let%test_unit "the protocol field is the runtime input, not a constant" =
  List.iter runners ~f:(fun runner ->
    check_run runner ~protocol:protocol_tcp ~payload:(make_payload 26);
    let udp = run_datagram runner ~payload:(make_payload 26) ~protocol:protocol_udp in
    let tcp = run_datagram runner ~payload:(make_payload 26) ~protocol:protocol_tcp in
    (* The protocol byte and the checksum that covers it are the only bytes that may
       differ between two datagrams identical in every other respect. How many of the two
       checksum bytes actually move depends on the protocol numbers - 17 against 6 changes
       the low byte and, with these endpoints, not the high one - so the claim is
       about *which* bytes may differ, not how many. *)
    let differing_indices =
      List.filter_mapi (List.zip_exn udp.bytes tcp.bytes) ~f:(fun index (left, right) ->
        Option.some_if (left <> right) index)
    in
    if not (List.mem differing_indices 9 ~equal:Int.equal)
    then
      raise_s
        [%message
          "the protocol byte did not follow its input"
            (runner.name : string)
            (differing_indices : int list)];
    [%test_result: int list]
      ~message:(runner.name ^ ": bytes outside protocol and checksum differed")
      (List.filter differing_indices ~f:(fun index -> index < 9 || index > 11))
      ~expect:[])
;;

let%test_unit "total_length tracks the payload length" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 1; 8; 26; 100 ] ~f:(fun length ->
      let observation =
        run_datagram runner ~payload:(make_payload length) ~protocol:protocol_udp
      in
      let total_length =
        (List.nth_exn observation.bytes 2 lsl 8) lor List.nth_exn observation.bytes 3
      in
      [%test_result: int]
        ~message:(sprintf "%s: total_length for %d payload bytes" runner.name length)
        total_length
        ~expect:(header_length + length)))
;;

let%test_unit "a one-byte payload frames on its only beat" =
  List.iter runners ~f:(fun runner ->
    check_run runner ~protocol:protocol_udp ~payload:[ 0x5A ])
;;

let%test_unit "a large payload is passed through unchanged" =
  check_run primary ~protocol:protocol_udp ~payload:(make_payload 100)
;;

let%test_unit "backpressure changes the cycles, not the datagram" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 26 in
    let free = run_datagram runner ~payload ~protocol:protocol_udp in
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      let stalled =
        run_datagram ~ready:(stall_every stride) runner ~payload ~protocol:protocol_udp
      in
      check_datagram runner ~protocol:protocol_udp ~payload stalled;
      [%test_result: int list]
        ~message:(sprintf "%s: stall_every %d bytes" runner.name stride)
        stalled.bytes
        ~expect:free.bytes;
      if stalled.cycles <= free.cycles
      then
        raise_s
          [%message
            "stalling did not cost cycles"
              (runner.name : string)
              (stride : int)
              (stalled.cycles : int)
              (free.cycles : int)]))
;;

(* mac_tready low every other cycle: the bubbles land on header bytes and on the tlast
   cycle, which is the pattern the legacy harness called "heavy stall". *)
let%test_unit "a two-byte payload survives an every-other-cycle stall" =
  List.iter runners ~f:(fun runner ->
    check_run ~ready:(stall_every 1) runner ~protocol:protocol_udp ~payload:[ 0xDE; 0xAD ])
;;

(* Net-new: the legacy harness held [l4_tvalid] high for the whole run, so a Payload state
   that ignored it - emitting the held byte again - would have gone unnoticed. *)
let%test_unit "source bubbles neither duplicate nor drop a payload byte" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      check_run
        ~source_valid:(stall_every stride)
        runner
        ~protocol:protocol_udp
        ~payload:(make_payload 12)))
;;

let%test_unit "backpressure and source bubbles together" =
  check_run
    ~ready:(stall_every 2)
    ~source_valid:(stall_every 3)
    primary
    ~protocol:protocol_udp
    ~payload:(make_payload 12)
;;

(* The FSM re-arms from Idle: tx_start must fire again and the header must be rebuilt for
   the new length and protocol, with no reset in between. *)
let%test_unit "back-to-back datagrams through one instance" =
  List.iter runners ~f:(fun runner ->
    let first = make_payload 12
    and second = make_payload 30 in
    match
      run_datagrams
        ~ready:(stall_every 2)
        runner
        ~datagrams:[ first, protocol_udp; second, protocol_tcp ]
    with
    | [ a; b ] ->
      check_datagram runner ~protocol:protocol_udp ~payload:first a;
      check_datagram runner ~protocol:protocol_tcp ~payload:second b
    | observations ->
      raise_s [%message "expected two datagrams" (List.length observations : int)])
;;

(* Findings RTL-7: a low [en] is backpressure in both directions and freezes the FSM. *)
let%test_unit "en pauses and resumes a datagram without losing or repeating a byte" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 12 in
    let en = stall_every 3 in
    let enabled = run_datagram runner ~payload ~protocol:protocol_udp in
    let paused = run_datagram ~en runner ~payload ~protocol:protocol_udp in
    check_datagram runner ~protocol:protocol_udp ~payload paused;
    [%test_result: int list]
      ~message:(runner.name ^ ": pause/resume changed the datagram")
      paused.bytes
      ~expect:enabled.bytes;
    if paused.cycles <= enabled.cycles
    then
      raise_s [%message "enable pauses did not delay the datagram" (runner.name : string)];
    List.iter paused.trace ~f:(fun observation ->
      if not observation.en
      then
        [%test_result: bool]
          ~message:(runner.name ^ ": a handshake or event escaped while en was low")
          (observation.output.m_tvalid
           || observation.output.m_tlast
           || observation.output.tx_start
           || observation.output.l4_tready)
          ~expect:false))
;;

let%test_unit "endpoints must be four bytes each" =
  List.iter runners ~f:(fun runner ->
    Or_error.ok_exn (elaborate ~src_ip:runner.src_ip ~dst_ip:runner.dst_ip));
  List.iter
    [ [ 10; 0; 0 ], [ 10; 0; 0; 1 ]
    ; [ 10; 0; 0; 1 ], [ 10; 0; 0 ]
    ; [ 10; 0; 0; 1; 5 ], [ 10; 0; 0; 1 ]
    ]
    ~f:(fun (src_ip, dst_ip) ->
      match elaborate ~src_ip ~dst_ip with
      | Ok () ->
        raise_s
          [%message
            "malformed endpoints elaborated" (src_ip : int list) (dst_ip : int list)]
      | Error _ -> ())
;;

let%test_unit "random payloads and protocols round-trip against the golden header" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "ipv4-tx-datagram")
    ~sexp_of:[%sexp_of: int list * int]
    ~shrinker:
      (Quickcheck.Shrinker.tuple2
         (List.quickcheck_shrinker Int.quickcheck_shrinker)
         Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:(fun (payload, protocol) ->
      List.iter runners ~f:(fun runner -> check_run runner ~protocol ~payload))
    (Quickcheck.Generator.both
       (Generators.byte_list ~min_length:1 ~max_length:20 ())
       Generators.byte)
;;

(* A ready/valid schedule drawn at random rather than a fixed stride, so an interaction
   between a bubble's phase and a field boundary is not designed out of the test.

   Liveness is not automatic here. A payload byte moves only when [mac_tready] and
   [l4_tvalid] are high on the *same* cycle, so two independently drawn schedules of the
   same period can be permanently disjoint - (f f t f f) against (f f f t t) never
   coincides, and the run deadlocks rather than failing an assertion. Fixing the two
   periods at 5 and 7, each forced to contain a [true], makes a coincidence certain: the
   periods are coprime, so by the remainder theorem some cycle hits a [true] in both. *)
let schedule_generator length =
  let open Quickcheck.Generator.Let_syntax in
  let%map pattern = List.gen_with_length length Bool.quickcheck_generator in
  if List.exists pattern ~f:Fn.id then pattern else true :: List.tl_exn pattern
;;

let schedule pattern cycle = List.nth_exn pattern (cycle % List.length pattern)

let%test_unit "random backpressure and source schedules leave the datagram unchanged" =
  Quickcheck.test
    ~trials:30
    ~seed:(`Deterministic "ipv4-tx-schedules")
    ~sexp_of:[%sexp_of: bool list * bool list]
    ~f:(fun (ready_pattern, valid_pattern) ->
      List.iter runners ~f:(fun runner ->
        check_run
          ~ready:(schedule ready_pattern)
          ~source_valid:(schedule valid_pattern)
          runner
          ~protocol:protocol_udp
          ~payload:(make_payload 10)))
    (Quickcheck.Generator.both (schedule_generator 5) (schedule_generator 7))
;;

let%test_unit "the header checksum is correct for random lengths and protocols" =
  Quickcheck.test
    ~trials:60
    ~seed:(`Deterministic "ipv4-tx-checksum")
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (payload_length, protocol) ->
      List.iter runners ~f:(fun runner ->
        let observation =
          run_datagram runner ~payload:(make_payload payload_length) ~protocol
        in
        [%test_result: int]
          ~message:(runner.name ^ ": header checksum residue")
          (header_checksum_residue observation.bytes)
          ~expect:0xFFFF))
    (Quickcheck.Generator.both
       (Generators.payload_length ~min_length:1 ~max_length:40 ())
       Generators.byte)
;;
