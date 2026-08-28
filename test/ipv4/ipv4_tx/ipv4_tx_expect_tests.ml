(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "ipv4_tx_expect_tests.ml" *)

(* Expect Test Suite: Ipv4_tx

   Golden traces of the datagram the block puts on the wire and of the handshake around
   it. One row per cycle, sampled [before_edge], which is the cycle the byte is on the
   wire and the cycle the MAC would accept it on.

   The wire dumps are the readable form of the header itself: bytes 2..3 are total_length,
   byte 9 the protocol, bytes 10..11 the checksum, and 12..19 the two endpoints. Printing
   both configurations side by side is what makes the checksum visibly a function of the
   endpoints rather than a constant.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Ipv4_tx_testbench

let hex bytes = List.map bytes ~f:(sprintf "%02x") |> String.concat ~sep:" "

let%expect_test "a two-byte datagram, cycle by cycle" =
  let observation = run_datagram primary ~payload:[ 0xAA; 0xBB ] ~protocol:protocol_udp in
  let trace = List.map observation.trace ~f:primary.compact in
  print_s [%sexp (trace : Compact_observation.t list)];
  [%expect
    {|
    (((beat Idle) (m_tdata 0x45) (active_outputs (tx_start)))
     ((beat (Header 0)) (m_tdata 0x45) (active_outputs (m_tvalid busy)))
     ((beat (Header 1)) (m_tdata 0x00) (active_outputs (m_tvalid busy)))
     ((beat (Header 2)) (m_tdata 0x00) (active_outputs (m_tvalid busy)))
     ((beat (Header 3)) (m_tdata 0x16) (active_outputs (m_tvalid busy)))
     ((beat (Header 4)) (m_tdata 0x00) (active_outputs (m_tvalid busy)))
     ((beat (Header 5)) (m_tdata 0x00) (active_outputs (m_tvalid busy)))
     ((beat (Header 6)) (m_tdata 0x40) (active_outputs (m_tvalid busy)))
     ((beat (Header 7)) (m_tdata 0x00) (active_outputs (m_tvalid busy)))
     ((beat (Header 8)) (m_tdata 0x40) (active_outputs (m_tvalid busy)))
     ((beat (Header 9)) (m_tdata 0x11) (active_outputs (m_tvalid busy)))
     ((beat (Header 10)) (m_tdata 0xb7) (active_outputs (m_tvalid busy)))
     ((beat (Header 11)) (m_tdata 0x7b) (active_outputs (m_tvalid busy)))
     ((beat (Header 12)) (m_tdata 0xc0) (active_outputs (m_tvalid busy)))
     ((beat (Header 13)) (m_tdata 0xa8) (active_outputs (m_tvalid busy)))
     ((beat (Header 14)) (m_tdata 0x01) (active_outputs (m_tvalid busy)))
     ((beat (Header 15)) (m_tdata 0x0a) (active_outputs (m_tvalid busy)))
     ((beat (Header 16)) (m_tdata 0xc0) (active_outputs (m_tvalid busy)))
     ((beat (Header 17)) (m_tdata 0xa8) (active_outputs (m_tvalid busy)))
     ((beat (Header 18)) (m_tdata 0x01) (active_outputs (m_tvalid busy)))
     ((beat (Header 19)) (m_tdata 0x01) (active_outputs (m_tvalid busy)))
     ((beat (Payload 0)) (m_tdata 0xaa)
      (active_outputs (m_tvalid l4_tready busy)))
     ((beat (Payload 1)) (m_tdata 0xbb)
      (active_outputs (m_tvalid m_tlast l4_tready busy))))
    |}]
;;

let%expect_test "the header is a function of length, protocol and endpoints" =
  List.iter runners ~f:(fun runner ->
    List.iter
      [ 26, protocol_udp; 26, protocol_tcp; 100, protocol_udp ]
      ~f:(fun (length, protocol) ->
        let observation = run_datagram runner ~payload:(make_payload length) ~protocol in
        let header = List.take observation.bytes Ip_udp.Ipv4.header_length in
        print_s
          [%sexp
            { endpoints = (runner.name : string)
            ; payload_length = (length : int)
            ; protocol : int
            ; header = (hex header : string)
            }]));
  [%expect
    {|
    ((endpoints Primary) (payload_length 26) (protocol 17)
     (header "45 00 00 2e 00 00 40 00 40 11 b7 63 c0 a8 01 0a c0 a8 01 01"))
    ((endpoints Primary) (payload_length 26) (protocol 6)
     (header "45 00 00 2e 00 00 40 00 40 06 b7 6e c0 a8 01 0a c0 a8 01 01"))
    ((endpoints Primary) (payload_length 100) (protocol 17)
     (header "45 00 00 78 00 00 40 00 40 11 b7 19 c0 a8 01 0a c0 a8 01 01"))
    ((endpoints Broadcast_carry) (payload_length 26) (protocol 17)
     (header "45 00 00 2e 00 00 40 00 40 11 90 ad ac 10 fe 01 ff ff ff ff"))
    ((endpoints Broadcast_carry) (payload_length 26) (protocol 6)
     (header "45 00 00 2e 00 00 40 00 40 06 90 b8 ac 10 fe 01 ff ff ff ff"))
    ((endpoints Broadcast_carry) (payload_length 100) (protocol 17)
     (header "45 00 00 78 00 00 40 00 40 11 90 63 ac 10 fe 01 ff ff ff ff"))
    |}]
;;

let%expect_test "backpressure inserts idle beats without losing one" =
  let observation =
    run_datagram ~ready:(stall_every 1) primary ~payload:[ 0x5A ] ~protocol:protocol_udp
  in
  let beats = List.map observation.trace ~f:(fun item -> item.beat) in
  print_s
    [%sexp
      { cycles = (observation.cycles : int)
      ; bytes_accepted = (List.length observation.bytes : int)
      ; beats : Beat.t list
      }];
  [%expect
    {|
    ((cycles 43) (bytes_accepted 21)
     (beats
      (Idle Idle (Header 0) Idle (Header 1) Idle (Header 2) Idle (Header 3) Idle
       (Header 4) Idle (Header 5) Idle (Header 6) Idle (Header 7) Idle (Header 8)
       Idle (Header 9) Idle (Header 10) Idle (Header 11) Idle (Header 12) Idle
       (Header 13) Idle (Header 14) Idle (Header 15) Idle (Header 16) Idle
       (Header 17) Idle (Header 18) Idle (Header 19) Idle (Payload 0))))
    |}]
;;

let%expect_test "a source bubble is an idle beat, not a repeated byte" =
  let observation =
    run_datagram
      ~source_valid:(stall_every 2)
      primary
      ~payload:[ 0x11; 0x22; 0x33 ]
      ~protocol:protocol_udp
  in
  (* The payload tail only: the header phase ignores [l4_tvalid] entirely. *)
  let tail =
    List.filter observation.trace ~f:(fun item -> item.cycle >= Ip_udp.Ipv4.header_length)
    |> List.map ~f:primary.compact
  in
  print_s
    [%sexp
      { payload_out =
          (hex (List.drop observation.bytes Ip_udp.Ipv4.header_length) : string)
      ; tail : Compact_observation.t list
      }];
  [%expect
    {|
    ((payload_out "11 22 33")
     (tail
      (((beat (Header 19)) (m_tdata 0x01) (active_outputs (m_tvalid busy)))
       ((beat (Payload 0)) (m_tdata 0x11)
        (active_outputs (m_tvalid l4_tready busy)))
       ((beat (Payload 1)) (m_tdata 0x22)
        (active_outputs (m_tvalid l4_tready busy)))
       ((beat Idle) (m_tdata 0x33) (active_outputs (l4_tready busy)))
       ((beat (Payload 2)) (m_tdata 0x33)
        (active_outputs (m_tvalid m_tlast l4_tready busy))))))
    |}]
;;

let%expect_test "back-to-back datagrams re-arm the FSM" =
  let datagrams = [ make_payload 4, protocol_udp; make_payload 6, protocol_tcp ] in
  let observations = run_datagrams primary ~datagrams in
  List.iter observations ~f:(fun (observation : Datagram_observation.t) ->
    print_s
      [%sexp
        { bytes = (hex observation.bytes : string)
        ; tlast_index = (observation.tlast_index : int)
        ; tx_start_pulses = (observation.tx_start_pulses : int)
        ; cycles = (observation.cycles : int)
        }]);
  [%expect
    {|
    ((bytes
      "45 00 00 18 00 00 40 00 40 11 b7 79 c0 a8 01 0a c0 a8 01 01 40 41 42 43")
     (tlast_index 23) (tx_start_pulses 1) (cycles 25))
    ((bytes
      "45 00 00 1a 00 00 40 00 40 06 b7 82 c0 a8 01 0a c0 a8 01 01 40 41 42 43 44 45")
     (tlast_index 25) (tx_start_pulses 1) (cycles 27))
    |}]
;;
