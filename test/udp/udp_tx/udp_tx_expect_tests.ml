(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_tx_expect_tests.ml" *)

(* Expect Test Suite: Udp_tx

   Golden traces of the datagram the block hands down to IPv4 and of the handshake around
   it. One row per cycle, sampled [before_edge], which is the cycle the byte is on the
   wire and the cycle IPv4 would accept it on.

   Header bytes 0..3 are the two ports, 4..5 the udp_length, 6..7 the checksum field the
   block always emits as zero.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Udp_tx_testbench

let hex bytes = List.map bytes ~f:(sprintf "%02x") |> String.concat ~sep:" "

let print_summary (observation : Datagram_observation.t) =
  print_s
    [%sexp
      { bytes = (hex observation.bytes : string)
      ; tlast_indices = (observation.tlast_indices : int list)
      ; ip_start_pulses = (observation.ip_start_pulses : int)
      ; l4_length_at_start = (observation.l4_length_at_start : int option)
      ; cycles = (observation.cycles : int)
      }]
;;

let%expect_test "a three-byte datagram, cycle by cycle" =
  let observation = run_datagram primary ~payload:[ 0xAA; 0xBB; 0xCC ] in
  print_s
    [%sexp (List.map observation.trace ~f:primary.compact : Compact_observation.t list)];
  [%expect
    {|
    (((beat Idle) (m_tdata 0x12) (l4_tready true) (active_outputs (ip_start)))
     ((beat (Header 0)) (m_tdata 0x12) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 1)) (m_tdata 0x34) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 2)) (m_tdata 0x12) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 3)) (m_tdata 0x35) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 4)) (m_tdata 0x00) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 5)) (m_tdata 0x0b) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 6)) (m_tdata 0x00) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 7)) (m_tdata 0x00) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Payload 0)) (m_tdata 0xaa) (l4_tready true)
      (active_outputs (m_tvalid payload_tready busy)))
     ((beat (Payload 1)) (m_tdata 0xbb) (l4_tready true)
      (active_outputs (m_tvalid payload_tready busy)))
     ((beat (Payload 2)) (m_tdata 0xcc) (l4_tready true)
      (active_outputs (m_tvalid m_tlast payload_tready busy))))
    |}]
;;

let%expect_test "a zero-length datagram frames on header byte 7" =
  let observation = run_datagram primary ~payload:[] in
  print_s
    [%sexp (List.map observation.trace ~f:primary.compact : Compact_observation.t list)];
  [%expect
    {|
    (((beat Idle) (m_tdata 0x12) (l4_tready true) (active_outputs (ip_start)))
     ((beat (Header 0)) (m_tdata 0x12) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 1)) (m_tdata 0x34) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 2)) (m_tdata 0x12) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 3)) (m_tdata 0x35) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 4)) (m_tdata 0x00) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 5)) (m_tdata 0x08) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 6)) (m_tdata 0x00) (l4_tready true)
      (active_outputs (m_tvalid busy)))
     ((beat (Header 7)) (m_tdata 0x00) (l4_tready true)
      (active_outputs (m_tvalid m_tlast busy))))
    |}]
;;

let%expect_test "the header is a function of the ports and the payload length" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 0; 1; 18; 300 ] ~f:(fun length ->
      let observation = run_datagram runner ~payload:(make_payload length) in
      print_s
        [%sexp
          { ports = (runner.name : string)
          ; payload_length = (length : int)
          ; header = (hex (List.take observation.bytes header_length) : string)
          }]));
  [%expect
    {|
    ((ports Primary) (payload_length 0) (header "12 34 12 35 00 08 00 00"))
    ((ports Primary) (payload_length 1) (header "12 34 12 35 00 09 00 00"))
    ((ports Primary) (payload_length 18) (header "12 34 12 35 00 1a 00 00"))
    ((ports Primary) (payload_length 300) (header "12 34 12 35 01 34 00 00"))
    ((ports Wide) (payload_length 0) (header "00 a1 ff 07 00 08 00 00"))
    ((ports Wide) (payload_length 1) (header "00 a1 ff 07 00 09 00 00"))
    ((ports Wide) (payload_length 18) (header "00 a1 ff 07 00 1a 00 00"))
    ((ports Wide) (payload_length 300) (header "00 a1 ff 07 01 34 00 00"))
    |}]
;;

let%expect_test "the final beat is held while the sink stalls" =
  List.iter
    [ []; [ 0x5A; 0x5B ] ]
    ~f:(fun payload ->
      let total = header_length + List.length payload in
      let observation =
        run_datagram ~ready:(stall_final_beat ~count:2 ~total) primary ~payload
      in
      let tail =
        List.drop observation.trace (observation.cycles - 4)
        |> List.map ~f:primary.compact
      in
      print_s
        [%sexp
          { payload_length = (List.length payload : int)
          ; tail : Compact_observation.t list
          }]);
  [%expect
    {|
    ((payload_length 0)
     (tail
      (((beat (Header 6)) (m_tdata 0x00) (l4_tready true)
        (active_outputs (m_tvalid busy)))
       ((beat Idle) (m_tdata 0x00) (l4_tready false)
        (active_outputs (m_tvalid m_tlast busy)))
       ((beat Idle) (m_tdata 0x00) (l4_tready false)
        (active_outputs (m_tvalid m_tlast busy)))
       ((beat (Header 7)) (m_tdata 0x00) (l4_tready true)
        (active_outputs (m_tvalid m_tlast busy))))))
    ((payload_length 2)
     (tail
      (((beat (Payload 0)) (m_tdata 0x5a) (l4_tready true)
        (active_outputs (m_tvalid payload_tready busy)))
       ((beat Idle) (m_tdata 0x5b) (l4_tready false)
        (active_outputs (m_tvalid m_tlast busy)))
       ((beat Idle) (m_tdata 0x5b) (l4_tready false)
        (active_outputs (m_tvalid m_tlast busy)))
       ((beat (Payload 1)) (m_tdata 0x5b) (l4_tready true)
        (active_outputs (m_tvalid m_tlast payload_tready busy))))))
    |}]
;;

let%expect_test "reset mid-datagram, then a fresh one" =
  let observation = primary.run_reset_recovery ~payload:(make_payload 4) in
  print_s
    [%sexp
      { in_flight = (observation.in_flight : Output_snapshot.t)
      ; during_reset = (observation.during_reset : Output_snapshot.t)
      ; after_reset = (observation.after_reset : Output_snapshot.t)
      }];
  print_summary observation.datagram;
  [%expect
    {|
    ((in_flight
      ((ip_start false) (l4_length 28) (protocol 17) (m_tdata 18) (m_tvalid true)
       (m_tlast false) (payload_tready false) (busy true)))
     (during_reset
      ((ip_start false) (l4_length 8) (protocol 17) (m_tdata 53) (m_tvalid false)
       (m_tlast false) (payload_tready false) (busy true)))
     (after_reset
      ((ip_start false) (l4_length 8) (protocol 17) (m_tdata 18) (m_tvalid false)
       (m_tlast false) (payload_tready false) (busy false))))
    ((bytes "12 34 12 35 00 0c 00 00 40 65 8a af") (tlast_indices (11))
     (ip_start_pulses 1) (l4_length_at_start (12)) (cycles 13))
    |}]
;;

let%expect_test "back-to-back datagrams, including an empty one" =
  List.iter
    (run_datagrams primary ~payloads:[ make_payload 4; []; make_payload 6 ])
    ~f:print_summary;
  [%expect
    {|
    ((bytes "12 34 12 35 00 0c 00 00 40 65 8a af") (tlast_indices (11))
     (ip_start_pulses 1) (l4_length_at_start (12)) (cycles 13))
    ((bytes "12 34 12 35 00 08 00 00") (tlast_indices (7)) (ip_start_pulses 1)
     (l4_length_at_start (8)) (cycles 9))
    ((bytes "12 34 12 35 00 0e 00 00 40 65 8a af d4 f9") (tlast_indices (13))
     (ip_start_pulses 1) (l4_length_at_start (14)) (cycles 15))
    |}]
;;
