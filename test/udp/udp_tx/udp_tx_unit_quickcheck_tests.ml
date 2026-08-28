(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_tx_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Udp_tx

   Typed examples and generated properties covering the 8-byte UDP header, the payload
   pass-through, the framing around both, and the metadata the IPv4 layer consumes.

   The superseded [udp_tx_legacy_assertion_test.ml] was the best-developed harness in the
   repo - it already sampled the before-edge interface and already checked stream
   stability and ready propagation - so the translation is mostly a change of reporting:
   its eleven scenarios are the [let%test_unit] cases below, its per-scenario booleans are
   [%test_result] assertions, and the Quickcheck properties generalize the scenarios over
   payload length, port configuration and schedule.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Udp_tx_testbench

let check_datagram
  ?(check_stability = true)
  runner
  ~payload
  (observation : Datagram_observation.t)
  =
  let expected = runner.golden_datagram ~payload in
  [%test_result: int list] ~message:runner.name observation.bytes ~expect:expected;
  [%test_result: int list]
    ~message:(runner.name ^ ": exactly one tlast, on the final byte")
    observation.tlast_indices
    ~expect:[ List.length expected - 1 ];
  [%test_result: int]
    ~message:(runner.name ^ ": ip_start pulses")
    observation.ip_start_pulses
    ~expect:1;
  [%test_result: int option]
    ~message:(runner.name ^ ": l4_length at start")
    observation.l4_length_at_start
    ~expect:(Some (header_length + List.length payload));
  [%test_result: int list]
    ~message:(runner.name ^ ": protocol is the UDP constant throughout")
    observation.protocol_values
    ~expect:[ ip_protocol_udp ];
  [%test_result: bool]
    ~message:(runner.name ^ ": busy clears")
    observation.settled.busy
    ~expect:false;
  let ready_violations =
    payload_ready_violations observation ~payload_length:(List.length payload)
  in
  if not (List.is_empty ready_violations)
  then
    raise_s
      [%message
        "payload_tready did not follow l4_tready in Payload"
          (runner.name : string)
          (ready_violations : (int * bool * bool) list)];
  if check_stability
  then (
    let stability_violations = stalled_beat_violations observation in
    if not (List.is_empty stability_violations)
    then
      raise_s
        [%message
          "a stalled beat did not hold its data"
            (runner.name : string)
            (List.length stability_violations : int)])
;;

let check_run ?ready ?source_valid ?en ?payload_len_after_start runner ~payload =
  check_datagram
    ?check_stability:(Option.map source_valid ~f:(fun _ -> false))
    runner
    ~payload
    (run_datagram ?ready ?source_valid ?en ?payload_len_after_start runner ~payload)
;;

let%test_unit "a nominal datagram is header ++ payload" =
  List.iter runners ~f:(fun runner -> check_run runner ~payload:(make_payload 18))
;;

let%test_unit "a one-byte payload" =
  List.iter runners ~f:(fun runner -> check_run runner ~payload:[ 0x5A ])
;;

(* Unlike [Ipv4_tx], this block owns its framing: with no payload at all it asserts
   [m_tlast] on header byte 7 and returns to Idle. *)
let%test_unit "a zero-length datagram is framed by its own header" =
  List.iter runners ~f:(fun runner ->
    let observation = run_datagram runner ~payload:[] in
    check_datagram runner ~payload:[] observation;
    [%test_result: int]
      ~message:(runner.name ^ ": eight bytes, no payload")
      (List.length observation.bytes)
      ~expect:header_length)
;;

let%test_unit "a large payload is passed through unchanged" =
  check_run primary ~payload:(make_payload 300)
;;

let%test_unit "periodic downstream backpressure" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      let observation =
        run_datagram ~ready:(stall_every stride) runner ~payload:(make_payload 23)
      in
      check_datagram runner ~payload:(make_payload 23) observation;
      if observation.cycles <= header_length + 23
      then
        raise_s
          [%message
            "stalling did not cost cycles"
              (runner.name : string)
              (stride : int)
              (observation.cycles : int)]))
;;

let%test_unit "application-source valid bubbles" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      check_run ~source_valid:(stall_every stride) runner ~payload:(make_payload 17)))
;;

(* Both forms of final beat: the last payload byte for a non-empty datagram, and header
   byte 7 for an empty one. The RTL describes the final beat while stalled - it asserts
   [m_tlast] in Header on the [payload_rem = 0] path before the beat is accepted - and
   that is what these hold under backpressure. *)
let%test_unit "the final beat is held, unchanged, under backpressure" =
  List.iter runners ~f:(fun runner ->
    List.iter
      [ [], 2; make_payload 5, 3; make_payload 1, 4 ]
      ~f:(fun (payload, count) ->
        let total = header_length + List.length payload in
        let observation =
          run_datagram ~ready:(stall_final_beat ~count ~total) runner ~payload
        in
        check_datagram runner ~payload observation;
        if observation.cycles < total + count
        then
          raise_s
            [%message
              "the final beat was not actually stalled"
                (runner.name : string)
                (observation.cycles : int)
                (total : int)
                (count : int)]))
;;

(* The header's udp_length comes from [len_latch], so moving the input after the start
   pulse must not move the header. *)
let%test_unit "payload_len is latched at start" =
  List.iter runners ~f:(fun runner ->
    check_run ~payload_len_after_start:0x3456 runner ~payload:(make_payload 12);
    check_run ~payload_len_after_start:0 runner ~payload:(make_payload 12))
;;

(* Findings RTL-10. [l4_length] bypasses [len_latch] on the start cycle, so [Ipv4_tx] sees
   the new length at the edge that latches it, then comes from the latch for the rest of
   the datagram. *)
let%test_unit "l4_length holds the start-cycle length for the whole datagram" =
  let payload = make_payload 12 in
  let observation = run_datagram ~payload_len_after_start:0x30 primary ~payload in
  [%test_result: int option]
    ~message:"l4_length at start is the real length plus the header"
    observation.l4_length_at_start
    ~expect:(Some (header_length + List.length payload));
  let after_start =
    List.filter observation.trace ~f:(fun (item : Observation.t) ->
      not item.output.ip_start)
    |> List.map ~f:(fun (item : Observation.t) -> item.output.l4_length)
    |> List.dedup_and_sort ~compare:Int.compare
  in
  [%test_result: int list]
    ~message:"after start it holds the latched length"
    after_start
    ~expect:[ header_length + List.length payload ]
;;

let%test_unit "back-to-back datagrams through one instance" =
  List.iter runners ~f:(fun runner ->
    let first = make_payload 3
    and second = make_payload 31 in
    match run_datagrams ~ready:(stall_every 2) runner ~payloads:[ first; second ] with
    | [ a; b ] ->
      check_datagram runner ~payload:first a;
      check_datagram runner ~payload:second b
    | observations ->
      raise_s [%message "expected two datagrams" (List.length observations : int)])
;;

let%test_unit "a zero-length datagram between two normal ones" =
  match run_datagrams primary ~payloads:[ make_payload 4; []; make_payload 6 ] with
  | [ a; b; c ] ->
    check_datagram primary ~payload:(make_payload 4) a;
    check_datagram primary ~payload:[] b;
    check_datagram primary ~payload:(make_payload 6) c
  | observations ->
    raise_s [%message "expected three datagrams" (List.length observations : int)]
;;

(* The clear is synchronous: the cycle that applies reset still reports the old register
   contents, and the effect appears on the cycle after. Both halves are asserted so a
   change to the reset's timing cannot pass unnoticed. *)
let%test_unit "reset mid-datagram clears the block and a fresh datagram still works" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 9 in
    let observation = runner.run_reset_recovery ~payload in
    [%test_result: bool]
      ~message:(runner.name ^ ": busy while in flight")
      observation.in_flight.busy
      ~expect:true;
    [%test_result: bool]
      ~message:(runner.name ^ ": the reset cycle still reports the old busy")
      observation.during_reset.busy
      ~expect:true;
    [%test_result: bool]
      ~message:(runner.name ^ ": busy cleared after the reset")
      observation.after_reset.busy
      ~expect:false;
    [%test_result: bool]
      ~message:(runner.name ^ ": m_tvalid cleared after the reset")
      observation.after_reset.m_tvalid
      ~expect:false;
    [%test_result: bool]
      ~message:(runner.name ^ ": payload_tready cleared after the reset")
      observation.after_reset.payload_tready
      ~expect:false;
    check_datagram runner ~payload observation.datagram)
;;

(* Findings RTL-7: a low [en] is backpressure in both directions and freezes the FSM. *)
let%test_unit "en pauses and resumes a datagram without losing or repeating a byte" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 12 in
    let en ~cycle ~beat:_ ~beat_cycles:_ = cycle % 4 <> 3 in
    let enabled = run_datagram runner ~payload in
    let paused = run_datagram ~en runner ~payload in
    check_datagram runner ~payload paused;
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
          (observation.output.ip_start
           || observation.output.m_tvalid
           || observation.output.m_tlast
           || observation.output.payload_tready)
          ~expect:false))
;;

let%test_unit "random payloads round-trip against the golden datagram" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "udp-tx-datagram")
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:(List.quickcheck_shrinker Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:(fun payload -> List.iter runners ~f:(fun runner -> check_run runner ~payload))
    (Generators.byte_list ~min_length:0 ~max_length:24 ())
;;

let schedule_generator length =
  let open Quickcheck.Generator.Let_syntax in
  let%map pattern = List.gen_with_length length Bool.quickcheck_generator in
  if List.exists pattern ~f:Fn.id then pattern else true :: List.tl_exn pattern
;;

(* Periods 5 and 7 are coprime and each pattern is forced to contain a [true], so some
   cycle has the application presenting while the sink is ready and the run cannot
   deadlock. *)
let%test_unit "random backpressure and source schedules leave the datagram unchanged" =
  Quickcheck.test
    ~trials:30
    ~seed:(`Deterministic "udp-tx-schedules")
    ~sexp_of:[%sexp_of: bool list * bool list]
    ~f:(fun (ready_pattern, valid_pattern) ->
      List.iter runners ~f:(fun runner ->
        check_run
          ~ready:(pattern_schedule ready_pattern)
          ~source_valid:(pattern_schedule valid_pattern)
          runner
          ~payload:(make_payload 10)))
    (Quickcheck.Generator.both (schedule_generator 5) (schedule_generator 7))
;;

(* udp_length is the one header field that moves with the payload, and it is the field a
   receiver frames on, so it gets a property of its own across the 16-bit boundary the two
   header bytes straddle. *)
let%test_unit "udp_length is 8 plus the payload, most significant byte first" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "udp-tx-length")
    ~sexp_of:[%sexp_of: int]
    ~f:(fun payload_length ->
      List.iter runners ~f:(fun runner ->
        let observation = run_datagram runner ~payload:(make_payload payload_length) in
        let udp_length =
          (List.nth_exn observation.bytes 4 lsl 8) lor List.nth_exn observation.bytes 5
        in
        [%test_result: int]
          ~message:(sprintf "%s: udp_length for %d bytes" runner.name payload_length)
          udp_length
          ~expect:(header_length + payload_length)))
    (Generators.payload_length ~min_length:0 ~max_length:300 ())
;;
