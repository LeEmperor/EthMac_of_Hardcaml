(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "ipv4_rx_expect_tests.ml" *)

(* Expect Test Suite: Ipv4_rx

   Golden traces of the header walk, the stripped payload, and the frame-level status. One
   row per cycle, sampled [before_edge], where the byte on the bus and the qualifiers
   describing it belong to the same cycle.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Ipv4_rx_testbench

let hex bytes = List.map bytes ~f:(sprintf "%02x") |> String.concat ~sep:" "

let print_summary (observation : Frame_observation.t) =
  print_s
    [%sexp
      { payload = (hex observation.payload : string)
      ; tfirst_index = (observation.tfirst_index : int)
      ; tlast_index = (observation.tlast_index : int)
      ; metadata = (observation.metadata : Metadata.t option)
      ; frame_done_pulses = (observation.frame_done_pulses : int)
      ; checksum_ok = (observation.settled.checksum_ok : bool)
      ; crc_error = (observation.settled.crc_error : bool)
      ; busy = (observation.settled.busy : bool)
      }]
;;

let%expect_test "a short frame, cycle by cycle" =
  let observation =
    run_frame
      strict
      ~frame:(ethernet_payload ~protocol:protocol_udp ~payload:[ 0xAA; 0xBB; 0xCC ] ())
  in
  print_s
    [%sexp (List.map observation.trace ~f:strict.compact : Compact_observation.t list)];
  [%expect
    {|
    (((beat Idle) (rx_byte 0x45) (active_outputs (m_axis_tready)))
     ((beat Idle) (rx_byte 0x00) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x17) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x40) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x40) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x11) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x6f) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x1d) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0xc0) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0xa8) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x01) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x0a) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x0a) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x07) (active_outputs (m_axis_tready busy)))
     ((beat (Payload 0)) (rx_byte 0xaa)
      (active_outputs (m_axis_tready m_tvalid m_tfirst checksum_ok busy)))
     ((beat (Payload 1)) (rx_byte 0xbb)
      (active_outputs (m_axis_tready m_tvalid checksum_ok busy)))
     ((beat (Payload 2)) (rx_byte 0xcc)
      (active_outputs
       (m_axis_tready m_tvalid m_tlast checksum_ok busy frame_done))))
    |}]
;;

let%expect_test "padding is dropped and the metadata reports the real length" =
  let observation =
    run_frame
      strict
      ~frame:
        (ethernet_payload
           ~pad_to:46
           ~protocol:protocol_udp
           ~payload:(make_payload ~first:0x80 10)
           ())
  in
  print_summary observation;
  [%expect
    {|
    ((payload "80 81 82 83 84 85 86 87 88 89") (tfirst_index 0) (tlast_index 9)
     (metadata
      (((protocol 17) (payload_length 10) (src_ip 3232235786) (dst_ip 167772167))))
     (frame_done_pulses 1) (checksum_ok true) (crc_error false) (busy false))
    |}]
;;

let%expect_test "a bad checksum, under each policy" =
  List.iter runners ~f:(fun runner ->
    let observation =
      run_frame
        runner
        ~frame:
          (ethernet_payload
             ~corrupt:true
             ~protocol:protocol_udp
             ~payload:(make_payload ~first:0x50 8)
             ())
    in
    print_s [%sexp (runner.name : string)];
    print_summary observation);
  [%expect
    {|
    Strict
    ((payload "") (tfirst_index -1) (tlast_index -1) (metadata ())
     (frame_done_pulses 1) (checksum_ok false) (crc_error false) (busy false))
    Permissive
    ((payload "50 51 52 53 54 55 56 57") (tfirst_index 0) (tlast_index 7)
     (metadata
      (((protocol 17) (payload_length 8) (src_ip 3232235786) (dst_ip 167772167))))
     (frame_done_pulses 1) (checksum_ok false) (crc_error false) (busy false))
    |}]
;;

let%expect_test "the frames that never reach layer 4" =
  let cases =
    [ "non-IPv4 ethertype", arp_ethertype, make_payload ~first:0xA0 30
    ; ( "IHL > 5"
      , ipv4_ethertype
      , ethernet_payload
          ~version_ihl:0x46
          ~protocol:protocol_udp
          ~payload:(make_payload 8)
          () )
    ; ( "truncated inside the header"
      , ipv4_ethertype
      , List.take
          (ethernet_payload ~protocol:protocol_udp ~payload:(make_payload 8) ())
          10 )
    ; ( "total_length = 20, no payload (RTL-9)"
      , ipv4_ethertype
      , ip_header ~protocol:protocol_udp ~payload_length:0 () )
    ]
  in
  List.iter cases ~f:(fun (label, eth_type, frame) ->
    print_s [%sexp (label : string)];
    print_summary (run_frame ~eth_type strict ~frame));
  [%expect
    {|
    "non-IPv4 ethertype"
    ((payload "") (tfirst_index -1) (tlast_index -1) (metadata ())
     (frame_done_pulses 1) (checksum_ok false) (crc_error false) (busy false))
    "IHL > 5"
    ((payload "") (tfirst_index -1) (tlast_index -1) (metadata ())
     (frame_done_pulses 1) (checksum_ok false) (crc_error false) (busy false))
    "truncated inside the header"
    ((payload "") (tfirst_index -1) (tlast_index -1) (metadata ())
     (frame_done_pulses 1) (checksum_ok false) (crc_error false) (busy false))
    "total_length = 20, no payload (RTL-9)"
    ((payload "") (tfirst_index -1) (tlast_index -1)
     (metadata
      (((protocol 17) (payload_length 0) (src_ip 3232235786) (dst_ip 167772167))))
     (frame_done_pulses 1) (checksum_ok true) (crc_error false) (busy false))
    |}]
;;

let%expect_test "a truncated payload is forwarded as far as it got" =
  let observation =
    run_frame
      strict
      ~frame:
        (ip_header ~protocol:protocol_udp ~payload_length:16 ()
         @ make_payload ~first:0x60 8)
  in
  print_summary observation;
  [%expect
    {|
    ((payload "60 61 62 63 64 65 66 67") (tfirst_index 0) (tlast_index -1)
     (metadata
      (((protocol 17) (payload_length 16) (src_ip 3232235786) (dst_ip 167772167))))
     (frame_done_pulses 1) (checksum_ok true) (crc_error true) (busy false))
    |}]
;;

let%expect_test "a bad FCS rides the frame_done channel, not the payload stream" =
  let observation =
    run_frame
      ~fcs_bad:true
      strict
      ~frame:(ethernet_payload ~protocol:protocol_udp ~payload:(make_payload 6) ())
  in
  let final_cycles =
    List.drop observation.trace (observation.cycles - 3) |> List.map ~f:strict.compact
  in
  print_summary observation;
  print_s [%sexp (final_cycles : Compact_observation.t list)];
  [%expect
    {|
    ((payload "40 41 42 43 44 45") (tfirst_index 0) (tlast_index 5)
     (metadata
      (((protocol 17) (payload_length 6) (src_ip 3232235786) (dst_ip 167772167))))
     (frame_done_pulses 1) (checksum_ok true) (crc_error true) (busy false))
    (((beat (Payload 3)) (rx_byte 0x43)
      (active_outputs (m_axis_tready m_tvalid checksum_ok busy)))
     ((beat (Payload 4)) (rx_byte 0x44)
      (active_outputs (m_axis_tready m_tvalid checksum_ok busy)))
     ((beat (Payload 5)) (rx_byte 0x45)
      (active_outputs
       (m_axis_tready m_tvalid m_tlast checksum_ok busy frame_done frame_error))))
    |}]
;;
