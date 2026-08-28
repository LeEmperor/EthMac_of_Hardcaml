(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_rx_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Udp_rx

   Typed examples and generated properties covering the 8-byte header parse, the metadata
   record, the port filter, framing off the UDP length field, and the error channels.

   Every check the superseded [udp_rx_legacy_assertion_test.ml] printed is here as an
   assertion, and its eleven scenarios are generalized over payload length, ports and
   schedule. The oracle for the header is [udp_datagram], which is built from
   [Hardcaml_verif.Ip_udp]'s byte helpers - the same ones [Udp_tx]'s suite checks against,
   so the two directions agree on the wire format.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Udp_rx_testbench

let metadata_of ~src_port ~dst_port ~payload_length ~checksum =
  { Metadata.src_port
  ; dst_port
  ; udp_length = header_length + payload_length
  ; payload_length
  ; udp_checksum = checksum
  ; src_ip = ip32 src_ip
  ; dst_ip = ip32 dst_ip
  }
;;

let check_stripped
  ?(checksum = 0)
  ?(src_port = 0x1234)
  ?(dst_port = expected_dst_port)
  runner
  ~payload
  (observation : Datagram_observation.t)
  =
  [%test_result: int list]
    ~message:(runner.name ^ ": application payload")
    observation.payload
    ~expect:payload;
  [%test_result: int list]
    ~message:(runner.name ^ ": exactly one tlast, on the final byte")
    observation.tlast_indices
    ~expect:(if List.is_empty payload then [] else [ List.length payload - 1 ]);
  [%test_result: int list]
    ~message:(runner.name ^ ": exactly one tfirst, on the first byte")
    observation.tfirst_indices
    ~expect:(if List.is_empty payload then [] else [ 0 ]);
  [%test_result: int]
    ~message:(runner.name ^ ": app_start events")
    observation.app_start_events
    ~expect:1;
  [%test_result: Metadata.t option]
    ~message:(runner.name ^ ": metadata at app_start")
    observation.metadata
    ~expect:
      (Some
         (metadata_of ~src_port ~dst_port ~payload_length:(List.length payload) ~checksum));
  [%test_result: bool]
    ~message:(runner.name ^ ": busy clears")
    observation.settled.busy
    ~expect:false
;;

let datagram
  ?udp_length
  ?checksum
  ?(src_port = 0x1234)
  ?(dst_port = expected_dst_port)
  payload
  =
  udp_datagram ?udp_length ?checksum ~src_port ~dst_port ~payload ()
;;

let%test_unit "a nominal datagram is stripped to its payload, with metadata" =
  List.iter runners ~f:(fun runner ->
    let payload = [ 0xDE; 0xAD; 0xBE; 0xEF ] in
    let observation = run_datagram runner ~datagram:(datagram ~checksum:0xBEEF payload) in
    check_stripped ~checksum:0xBEEF runner ~payload observation;
    [%test_result: bool]
      ~message:(runner.name ^ ": port_match")
      observation.settled.port_match
      ~expect:true;
    [%test_result: bool]
      ~message:(runner.name ^ ": no crc_error")
      observation.settled.crc_error
      ~expect:false)
;;

let%test_unit "a one-byte payload carries both tfirst and tlast" =
  List.iter runners ~f:(fun runner ->
    let payload = [ 0x5A ] in
    let observation = run_datagram runner ~datagram:(datagram ~src_port:0xABCD payload) in
    check_stripped ~src_port:0xABCD runner ~payload observation)
;;

(* A UDP length of exactly 8 is a legal datagram with no application data: reported
   through [app_start] - a registered one-cycle pulse here - and emitting no payload byte.
   [Ipv4_rx] has no equivalent path; see findings RTL-9. *)
let%test_unit "a zero-length datagram is reported without a payload beat" =
  List.iter runners ~f:(fun runner ->
    let observation = run_datagram runner ~datagram:(datagram ~src_port:0x2222 []) in
    check_stripped ~src_port:0x2222 runner ~payload:[] observation)
;;

let%test_unit "a large payload is passed through unchanged" =
  List.iter runners ~f:(fun runner ->
    let payload = List.init 96 ~f:(fun index -> ((index * 37) + 11) land 0xFF) in
    let observation = run_datagram runner ~datagram:(datagram ~src_port:0x0001 payload) in
    check_stripped ~src_port:0x0001 runner ~payload observation)
;;

let%test_unit "the port filter is the only thing the policy changes" =
  let payload = [ 0x10; 0x20; 0x30; 0x40 ] in
  let wrong_port = datagram ~src_port:0x4321 ~dst_port:0x9999 payload in
  let accepted = run_datagram accept_all ~datagram:wrong_port in
  [%test_result: int list]
    ~message:"Accept_all forwards a mismatched port"
    accepted.payload
    ~expect:payload;
  [%test_result: bool]
    ~message:"Accept_all still reports the mismatch"
    accepted.settled.port_match
    ~expect:false;
  let dropped = run_datagram filter_port ~datagram:wrong_port in
  [%test_result: int list]
    ~message:"Filter_port drops a mismatched port"
    dropped.payload
    ~expect:[];
  [%test_result: int]
    ~message:"Filter_port suppresses app_start"
    dropped.app_start_events
    ~expect:0;
  [%test_result: bool]
    ~message:"Filter_port returns to idle"
    dropped.settled.busy
    ~expect:false;
  let matched = run_datagram filter_port ~datagram:(datagram ~src_port:0x4321 payload) in
  check_stripped ~src_port:0x4321 filter_port ~payload matched;
  [%test_result: bool]
    ~message:"Filter_port accepts the bound port"
    matched.settled.port_match
    ~expect:true
;;

let%test_unit "a non-UDP ip_protocol is flushed" =
  List.iter runners ~f:(fun runner ->
    let observation =
      run_datagram ~ip_protocol:ip_protocol_tcp runner ~datagram:(datagram [ 1; 2; 3 ])
    in
    [%test_result: int list] ~message:runner.name observation.payload ~expect:[];
    [%test_result: int]
      ~message:(runner.name ^ ": no app_start")
      observation.app_start_events
      ~expect:0;
    [%test_result: bool]
      ~message:(runner.name ^ ": flush clears busy")
      observation.settled.busy
      ~expect:false)
;;

let%test_unit "application backpressure holds the upstream ready low" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 23 in
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      let observation =
        run_datagram ~ready:(stall_every stride) runner ~datagram:(datagram payload)
      in
      check_stripped runner ~payload observation;
      [%test_result: bool]
        ~message:
          (sprintf "%s: upstream ready low on every app stall (%d)" runner.name stride)
        observation.upstream_ready_held_low
        ~expect:true))
;;

let%test_unit "source bubbles do not advance the header counter" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 12 in
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      let observation =
        run_datagram
          ~source_valid:(stall_every stride)
          runner
          ~datagram:(datagram payload)
      in
      check_stripped runner ~payload observation))
;;

let%test_unit "a datagram truncated inside the header aborts to idle" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 1; 3; 5; 7 ] ~f:(fun kept ->
      let observation =
        run_datagram runner ~datagram:(List.take (datagram [ 1; 2; 3 ]) kept)
      in
      [%test_result: int list]
        ~message:(sprintf "%s: %d header bytes" runner.name kept)
        observation.payload
        ~expect:[];
      [%test_result: int]
        ~message:(sprintf "%s: %d header bytes, no app_start" runner.name kept)
        observation.app_start_events
        ~expect:0;
      [%test_result: bool]
        ~message:(sprintf "%s: %d header bytes, returned to idle" runner.name kept)
        observation.settled.busy
        ~expect:false))
;;

let%test_unit "a payload shorter than udp_length raises crc_error" =
  List.iter runners ~f:(fun runner ->
    let delivered = [ 0xA1; 0xA2; 0xA3 ] in
    let observation = run_datagram runner ~datagram:(datagram ~udp_length:14 delivered) in
    [%test_result: int list]
      ~message:(runner.name ^ ": the bytes that did arrive are forwarded")
      observation.payload
      ~expect:delivered;
    [%test_result: int list]
      ~message:(runner.name ^ ": tlast never fires")
      observation.tlast_indices
      ~expect:[];
    [%test_result: bool]
      ~message:(runner.name ^ ": crc_error marks the truncation")
      observation.settled.crc_error
      ~expect:true;
    [%test_result: bool]
      ~message:(runner.name ^ ": returned to idle")
      observation.settled.busy
      ~expect:false)
;;

(* Net-new: the RTL frames off the UDP length field alone, so a datagram sitting inside a
   larger IP payload must have its surplus flushed rather than forwarded. *)
let%test_unit "a udp_length shorter than the bytes present flushes the surplus" =
  List.iter runners ~f:(fun runner ->
    let present = make_payload ~first:0x70 5 in
    let observation =
      run_datagram runner ~datagram:(datagram ~udp_length:(header_length + 2) present)
    in
    [%test_result: int list]
      ~message:(runner.name ^ ": only the promised bytes reach the application")
      observation.payload
      ~expect:(List.take present 2);
    [%test_result: int list]
      ~message:(runner.name ^ ": tlast on the promised final byte")
      observation.tlast_indices
      ~expect:[ 1 ];
    [%test_result: bool]
      ~message:(runner.name ^ ": returned to idle")
      observation.settled.busy
      ~expect:false)
;;

let%test_unit "the lower layer's error flag reaches crc_error" =
  List.iter runners ~f:(fun runner ->
    let payload = [ 0xC0; 0xFF; 0xEE ] in
    let observation = run_datagram ~fcs_bad:true runner ~datagram:(datagram payload) in
    check_stripped runner ~payload observation;
    [%test_result: bool]
      ~message:(runner.name ^ ": crc_error latched")
      observation.settled.crc_error
      ~expect:true)
;;

(* Net-new. [frame_done] and [frame_error] are wired straight from the IPv4 inputs with no
   state in between, which is what lets the FCS verdict arrive after the payload tlast. *)
let%test_unit "frame_done and frame_error are unconditional passthroughs" =
  List.iter runners ~f:(fun runner ->
    let observation =
      run_datagram
        ~frame_status:(fun ~cycle ~last_byte:_ -> cycle % 3 = 0, cycle % 2 = 0)
        runner
        ~datagram:(datagram (make_payload 8))
    in
    (* The drain cycles past the datagram drive the channel low, so the schedule only
       describes the cycles that presented a byte. *)
    List.iter observation.trace ~f:(fun (item : Observation.t) ->
      if Option.is_some item.rx_index
      then
        [%test_result: bool * bool]
          ~message:(sprintf "%s: cycle %d" runner.name item.cycle)
          (item.output.frame_done, item.output.frame_error)
          ~expect:(item.cycle % 3 = 0, item.cycle % 2 = 0)))
;;

(* Findings RTL-11. [checksum_ok] is a stub tied to [vdd] - the UDP checksum is latched
   and reported raw but never verified. Pinned so the day it becomes real, this test is
   what changes. *)
let%test_unit "checksum_ok is a stub tied high" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 0x0000; 0xBEEF; 0xFFFF ] ~f:(fun checksum ->
      let observation =
        run_datagram runner ~datagram:(datagram ~checksum (make_payload 6))
      in
      List.iter observation.trace ~f:(fun (item : Observation.t) ->
        [%test_result: bool]
          ~message:
            (sprintf "%s: checksum 0x%04x, cycle %d" runner.name checksum item.cycle)
          item.output.checksum_ok
          ~expect:true)))
;;

(* [m_tfirst] is a beat qualifier, not a pulse: while the application stalls the first
   payload byte it stays high, and [app_start] with it. A consumer has to qualify it with
   the handshake - which is what [app_start_events] does - and this pins the underlying
   shape so a change to it is visible. *)
let%test_unit "the SOF qualifier repeats while the first beat is stalled" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 4 in
    let observation =
      run_datagram ~ready:(fun cycle -> cycle > 12) runner ~datagram:(datagram payload)
    in
    check_stripped runner ~payload observation;
    let raw_app_start_cycles =
      List.count observation.trace ~f:(fun (item : Observation.t) ->
        item.output.app_start)
    in
    if raw_app_start_cycles <= 1
    then
      raise_s
        [%message
          "the SOF qualifier did not repeat under backpressure"
            (runner.name : string)
            (raw_app_start_cycles : int)];
    [%test_result: int]
      ~message:(runner.name ^ ": but only one of them is an event")
      observation.app_start_events
      ~expect:1)
;;

(* Findings RTL-7: a low [en] backpressures IPv4 and freezes the parser. *)
let%test_unit "en pauses and resumes a datagram without losing or repeating a byte" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 12 in
    let en = stall_every 3 in
    let enabled = run_datagram runner ~datagram:(datagram payload) in
    let paused = run_datagram ~en runner ~datagram:(datagram payload) in
    check_stripped runner ~payload paused;
    [%test_result: int list]
      ~message:(runner.name ^ ": pause/resume changed the payload")
      paused.payload
      ~expect:enabled.payload;
    if paused.cycles <= enabled.cycles
    then
      raise_s [%message "enable pauses did not delay the datagram" (runner.name : string)];
    List.iter paused.trace ~f:(fun observation ->
      if not observation.en
      then
        [%test_result: bool]
          ~message:(runner.name ^ ": a handshake or event escaped while en was low")
          (observation.output.m_axis_tready
           || observation.output.m_tvalid
           || observation.output.m_tlast
           || observation.output.m_tfirst
           || observation.output.app_start
           || observation.output.frame_done)
          ~expect:false))
;;

let%test_unit "the debug knob decides whether keep folds anything" =
  List.iter [ true; false ] ~f:(fun drop_on_port_mismatch ->
    [%test_result: bool]
      ~message:"debug on: keep folds the internal registers"
      (keep_is_constant ~drop_on_port_mismatch ~debug:true)
      ~expect:false;
    [%test_result: bool]
      ~message:"debug off: keep is tied off"
      (keep_is_constant ~drop_on_port_mismatch ~debug:false)
      ~expect:true)
;;

let%test_unit "random payloads and ports are stripped to themselves" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "udp-rx-strip")
    ~sexp_of:[%sexp_of: int list * int * int]
    ~f:(fun (payload, src_port, checksum) ->
      List.iter runners ~f:(fun runner ->
        let observation =
          run_datagram runner ~datagram:(datagram ~src_port ~checksum payload)
        in
        check_stripped ~src_port ~checksum runner ~payload observation))
    (Quickcheck.Generator.tuple3
       (Generators.byte_list ~min_length:0 ~max_length:20 ())
       Generators.port
       Generators.port)
;;

(* Only [Accept_all] forwards an arbitrary destination port, so the port sweep runs there;
   [Filter_port]'s behaviour on a mismatch is the dedicated test above. *)
let%test_unit "any destination port is reported, and port_match compares it" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "udp-rx-ports")
    ~sexp_of:[%sexp_of: int]
    ~f:(fun dst_port ->
      let payload = make_payload 6 in
      let observation = run_datagram accept_all ~datagram:(datagram ~dst_port payload) in
      check_stripped ~dst_port accept_all ~payload observation;
      [%test_result: bool]
        ~message:(sprintf "port_match for 0x%04x" dst_port)
        observation.settled.port_match
        ~expect:(dst_port = expected_dst_port))
    Generators.port
;;

let schedule_generator length =
  let open Quickcheck.Generator.Let_syntax in
  let%map pattern = List.gen_with_length length Bool.quickcheck_generator in
  if List.exists pattern ~f:Fn.id then pattern else true :: List.tl_exn pattern
;;

let schedule pattern cycle = List.nth_exn pattern (cycle % List.length pattern)

(* Periods 5 and 7 are coprime and each pattern is forced to contain a [true], so some
   cycle has the source presenting while the application is ready and the run cannot
   deadlock. *)
let%test_unit "random backpressure and source schedules leave the payload unchanged" =
  Quickcheck.test
    ~trials:30
    ~seed:(`Deterministic "udp-rx-schedules")
    ~sexp_of:[%sexp_of: bool list * bool list]
    ~f:(fun (ready_pattern, valid_pattern) ->
      let payload = make_payload 10 in
      List.iter runners ~f:(fun runner ->
        let observation =
          run_datagram
            ~ready:(schedule ready_pattern)
            ~source_valid:(schedule valid_pattern)
            runner
            ~datagram:(datagram payload)
        in
        check_stripped runner ~payload observation))
    (Quickcheck.Generator.both (schedule_generator 5) (schedule_generator 7))
;;
