(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_rx_expect_tests.ml" *)

(* Expect Test Suite: Udp_rx

   Golden traces of the header walk, the stripped payload, and the metadata and status the
   application sees. One row per cycle, sampled [before_edge], where the byte on the bus
   and the qualifiers describing it belong to the same cycle.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Udp_rx_testbench

let hex bytes = List.map bytes ~f:(sprintf "%02x") |> String.concat ~sep:" "

let print_summary (observation : Datagram_observation.t) =
  print_s
    [%sexp
      { payload = (hex observation.payload : string)
      ; tfirst_indices = (observation.tfirst_indices : int list)
      ; tlast_indices = (observation.tlast_indices : int list)
      ; app_start_events = (observation.app_start_events : int)
      ; metadata = (observation.metadata : Metadata.t option)
      ; port_match = (observation.settled.port_match : bool)
      ; crc_error = (observation.settled.crc_error : bool)
      ; busy = (observation.settled.busy : bool)
      }]
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

let%expect_test "a four-byte datagram, cycle by cycle" =
  let observation =
    run_datagram
      accept_all
      ~datagram:(datagram ~checksum:0xBEEF [ 0xDE; 0xAD; 0xBE; 0xEF ])
  in
  print_s
    [%sexp
      (List.map observation.trace ~f:accept_all.compact : Compact_observation.t list)];
  [%expect
    {|
    (((beat Idle) (rx_byte 0x12) (app_tready true)
      (active_outputs (m_axis_tready)))
     ((beat Idle) (rx_byte 0x34) (app_tready true)
      (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x12) (app_tready true)
      (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x35) (app_tready true)
      (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (app_tready true)
      (active_outputs (m_axis_tready port_match busy)))
     ((beat Idle) (rx_byte 0x0c) (app_tready true)
      (active_outputs (m_axis_tready port_match busy)))
     ((beat Idle) (rx_byte 0xbe) (app_tready true)
      (active_outputs (m_axis_tready port_match busy)))
     ((beat Idle) (rx_byte 0xef) (app_tready true)
      (active_outputs (m_axis_tready port_match busy)))
     ((beat (Payload 0)) (rx_byte 0xde) (app_tready true)
      (active_outputs
       (m_axis_tready m_tvalid m_tfirst app_start port_match busy)))
     ((beat (Payload 1)) (rx_byte 0xad) (app_tready true)
      (active_outputs (m_axis_tready m_tvalid port_match busy)))
     ((beat (Payload 2)) (rx_byte 0xbe) (app_tready true)
      (active_outputs (m_axis_tready m_tvalid port_match busy)))
     ((beat (Payload 3)) (rx_byte 0xef) (app_tready true)
      (active_outputs
       (m_axis_tready m_tvalid m_tlast port_match busy frame_done)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match))))
    |}]
;;

let%expect_test "an empty datagram reports itself on app_start alone" =
  let observation = run_datagram accept_all ~datagram:(datagram ~src_port:0x2222 []) in
  print_s
    [%sexp
      (List.map observation.trace ~f:accept_all.compact : Compact_observation.t list)];
  print_summary observation;
  [%expect
    {|
    (((beat Idle) (rx_byte 0x22) (app_tready true)
      (active_outputs (m_axis_tready)))
     ((beat Idle) (rx_byte 0x22) (app_tready true)
      (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x12) (app_tready true)
      (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x35) (app_tready true)
      (active_outputs (m_axis_tready busy)))
     ((beat Idle) (rx_byte 0x00) (app_tready true)
      (active_outputs (m_axis_tready port_match busy)))
     ((beat Idle) (rx_byte 0x08) (app_tready true)
      (active_outputs (m_axis_tready port_match busy)))
     ((beat Idle) (rx_byte 0x00) (app_tready true)
      (active_outputs (m_axis_tready port_match busy)))
     ((beat Idle) (rx_byte 0x00) (app_tready true)
      (active_outputs (m_axis_tready port_match busy frame_done)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready app_start port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match))))
    ((payload "") (tfirst_indices ()) (tlast_indices ()) (app_start_events 1)
     (metadata
      (((src_port 8738) (dst_port 4661) (udp_length 8) (payload_length 0)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error false) (busy false))
    |}]
;;

let%expect_test "the port filter, under each policy" =
  List.iter runners ~f:(fun runner ->
    List.iter [ expected_dst_port; 0x9999 ] ~f:(fun dst_port ->
      print_s [%sexp { policy = (runner.name : string); dst_port : int }];
      print_summary
        (run_datagram runner ~datagram:(datagram ~dst_port [ 0x10; 0x20; 0x30; 0x40 ]))));
  [%expect
    {|
    ((policy Accept_all) (dst_port 4661))
    ((payload "10 20 30 40") (tfirst_indices (0)) (tlast_indices (3))
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 12) (payload_length 4)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error false) (busy false))
    ((policy Accept_all) (dst_port 39321))
    ((payload "10 20 30 40") (tfirst_indices (0)) (tlast_indices (3))
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 39321) (udp_length 12) (payload_length 4)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match false) (crc_error false) (busy false))
    ((policy Filter_port) (dst_port 4661))
    ((payload "10 20 30 40") (tfirst_indices (0)) (tlast_indices (3))
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 12) (payload_length 4)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error false) (busy false))
    ((policy Filter_port) (dst_port 39321))
    ((payload "") (tfirst_indices ()) (tlast_indices ()) (app_start_events 0)
     (metadata ()) (port_match false) (crc_error false) (busy false))
    |}]
;;

let%expect_test "framing comes from udp_length, not from the frame's own end" =
  let present = make_payload ~first:0x70 5 in
  List.iter
    [ "exactly as promised", header_length + 5
    ; "shorter than the bytes present", header_length + 2
    ; "longer than the bytes present", header_length + 9
    ]
    ~f:(fun (label, udp_length) ->
      print_s [%sexp (label : string)];
      print_summary (run_datagram accept_all ~datagram:(datagram ~udp_length present)));
  [%expect
    {|
    "exactly as promised"
    ((payload "70 71 72 73 74") (tfirst_indices (0)) (tlast_indices (4))
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 13) (payload_length 5)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error false) (busy false))
    "shorter than the bytes present"
    ((payload "70 71") (tfirst_indices (0)) (tlast_indices (1))
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 10) (payload_length 2)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error false) (busy false))
    "longer than the bytes present"
    ((payload "70 71 72 73 74") (tfirst_indices (0)) (tlast_indices ())
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 17) (payload_length 9)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error true) (busy false))
    |}]
;;

let%expect_test "the datagrams that never reach the application" =
  let cases =
    [ "non-UDP ip_protocol", ip_protocol_tcp, datagram [ 1; 2; 3 ]
    ; "truncated inside the header", ip_protocol_udp, List.take (datagram [ 1; 2; 3 ]) 5
    ]
  in
  List.iter cases ~f:(fun (label, ip_protocol, bytes) ->
    print_s [%sexp (label : string)];
    print_summary (run_datagram ~ip_protocol accept_all ~datagram:bytes));
  [%expect
    {|
    "non-UDP ip_protocol"
    ((payload "") (tfirst_indices ()) (tlast_indices ()) (app_start_events 0)
     (metadata ()) (port_match false) (crc_error false) (busy false))
    "truncated inside the header"
    ((payload "") (tfirst_indices ()) (tlast_indices ()) (app_start_events 0)
     (metadata ()) (port_match true) (crc_error false) (busy false))
    |}]
;;

let%expect_test "the SOF qualifier is held, not pulsed, while the first beat stalls" =
  let observation =
    run_datagram
      ~ready:(fun cycle -> cycle > 12)
      accept_all
      ~datagram:(datagram (make_payload 4))
  in
  let payload_phase =
    List.filter observation.trace ~f:(fun (item : Observation.t) ->
      item.cycle >= header_length)
    |> List.map ~f:accept_all.compact
  in
  print_s [%sexp (payload_phase : Compact_observation.t list)];
  print_summary observation;
  [%expect
    {|
    (((beat Idle) (rx_byte 0x40) (app_tready false)
      (active_outputs (m_tvalid m_tfirst app_start port_match busy)))
     ((beat Idle) (rx_byte 0x40) (app_tready false)
      (active_outputs (m_tvalid m_tfirst app_start port_match busy)))
     ((beat Idle) (rx_byte 0x40) (app_tready false)
      (active_outputs (m_tvalid m_tfirst app_start port_match busy)))
     ((beat Idle) (rx_byte 0x40) (app_tready false)
      (active_outputs (m_tvalid m_tfirst app_start port_match busy)))
     ((beat Idle) (rx_byte 0x40) (app_tready false)
      (active_outputs (m_tvalid m_tfirst app_start port_match busy)))
     ((beat (Payload 0)) (rx_byte 0x40) (app_tready true)
      (active_outputs
       (m_axis_tready m_tvalid m_tfirst app_start port_match busy)))
     ((beat (Payload 1)) (rx_byte 0x41) (app_tready true)
      (active_outputs (m_axis_tready m_tvalid port_match busy)))
     ((beat (Payload 2)) (rx_byte 0x42) (app_tready true)
      (active_outputs (m_axis_tready m_tvalid port_match busy)))
     ((beat (Payload 3)) (rx_byte 0x43) (app_tready true)
      (active_outputs
       (m_axis_tready m_tvalid m_tlast port_match busy frame_done)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match)))
     ((beat Idle) (rx_byte --) (app_tready true)
      (active_outputs (m_axis_tready port_match))))
    ((payload "40 41 42 43") (tfirst_indices (0)) (tlast_indices (3))
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 12) (payload_length 4)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error false) (busy false))
    |}]
;;

let%expect_test "a bad FCS is forwarded as crc_error" =
  let observation =
    run_datagram ~fcs_bad:true accept_all ~datagram:(datagram [ 0xC0; 0xFF; 0xEE ])
  in
  print_summary observation;
  [%expect
    {|
    ((payload "c0 ff ee") (tfirst_indices (0)) (tlast_indices (2))
     (app_start_events 1)
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 11) (payload_length 3)
        (udp_checksum 0) (src_ip 3232235777) (dst_ip 3232235786))))
     (port_match true) (crc_error true) (busy false))
    |}]
;;
