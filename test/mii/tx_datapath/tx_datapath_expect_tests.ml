(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_datapath_expect_tests.ml" *)

(* Expect Test Suite: Tx_datapath

   Golden tables of the emitted byte for each [byte_mux_sel] x [mac_byte_sel] combination.
   Read as the mux's truth table: one row per selector pair, which is what makes a
   misrouted leg obvious on sight rather than only as a failed assertion.

   The rows where [mac_byte_sel] does not select anything meaningful - every value on the
   Idle, Preamble, Sfd, Payload and Fcs legs, and 6 and 7 on the header legs - are folded
   out of the tables below and covered by the exhaustive sweep in the Quickcheck suite.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Tx_datapath_testbench

let payload_byte = 0x3C
let fcs_byte = 0x26

let selectors ?(mac_byte_sel = 0) ?(pad = false) byte_source =
  { Selectors.byte_source; mac_byte_sel; s_axis_tdata = payload_byte; fcs_byte; pad }
;;

let print_table observations =
  print_s
    [%sexp (List.map observations ~f:Testbench.compact : Compact_observation.t list)]
;;

let%expect_test "the constant and pass-through legs" =
  print_table
    (Testbench.run_selectors
       [ selectors Byte_source.Idle
       ; selectors Byte_source.Preamble
       ; selectors Byte_source.Sfd
       ; selectors Byte_source.Payload
       ; selectors ~pad:true Byte_source.Payload
       ; selectors Byte_source.Fcs
       ]);
  [%expect
    {|
    (((byte_source Idle) (mac_byte_sel 0) (pad false) (byte_out 0x00))
     ((byte_source Preamble) (mac_byte_sel 0) (pad false) (byte_out 0x55))
     ((byte_source Sfd) (mac_byte_sel 0) (pad false) (byte_out 0xd5))
     ((byte_source Payload) (mac_byte_sel 0) (pad false) (byte_out 0x3c))
     ((byte_source Payload) (mac_byte_sel 0) (pad true) (byte_out 0x00))
     ((byte_source Fcs) (mac_byte_sel 0) (pad false) (byte_out 0x26)))
    |}]
;;

let%expect_test "the header legs walk their bytes with mac_byte_sel" =
  let header_sweep byte_source =
    List.init 6 ~f:(fun mac_byte_sel -> selectors ~mac_byte_sel byte_source)
  in
  print_table
    (Testbench.run_selectors
       (header_sweep Byte_source.Dst_mac @ header_sweep Byte_source.Src_mac));
  [%expect
    {|
    (((byte_source Dst_mac) (mac_byte_sel 0) (pad false) (byte_out 0xff))
     ((byte_source Dst_mac) (mac_byte_sel 1) (pad false) (byte_out 0xff))
     ((byte_source Dst_mac) (mac_byte_sel 2) (pad false) (byte_out 0xff))
     ((byte_source Dst_mac) (mac_byte_sel 3) (pad false) (byte_out 0xff))
     ((byte_source Dst_mac) (mac_byte_sel 4) (pad false) (byte_out 0xff))
     ((byte_source Dst_mac) (mac_byte_sel 5) (pad false) (byte_out 0xff))
     ((byte_source Src_mac) (mac_byte_sel 0) (pad false) (byte_out 0x02))
     ((byte_source Src_mac) (mac_byte_sel 1) (pad false) (byte_out 0x00))
     ((byte_source Src_mac) (mac_byte_sel 2) (pad false) (byte_out 0x00))
     ((byte_source Src_mac) (mac_byte_sel 3) (pad false) (byte_out 0x00))
     ((byte_source Src_mac) (mac_byte_sel 4) (pad false) (byte_out 0x00))
     ((byte_source Src_mac) (mac_byte_sel 5) (pad false) (byte_out 0x01)))
    |}]
;;

let%expect_test "the ethertype leg, default and IPv4" =
  let ethertype_sweep =
    List.init 2 ~f:(fun mac_byte_sel -> selectors ~mac_byte_sel Byte_source.Eth_type)
  in
  print_endline "default (0x9999):";
  print_table (Testbench.run_selectors ethertype_sweep);
  print_endline "ipv4 (0x0800):";
  print_table (Ipv4_testbench.run_selectors ethertype_sweep);
  [%expect
    {|
    default (0x9999):
    (((byte_source Eth_type) (mac_byte_sel 0) (pad false) (byte_out 0x99))
     ((byte_source Eth_type) (mac_byte_sel 1) (pad false) (byte_out 0x99)))
    ipv4 (0x0800):
    (((byte_source Eth_type) (mac_byte_sel 0) (pad false) (byte_out 0x08))
     ((byte_source Eth_type) (mac_byte_sel 1) (pad false) (byte_out 0x00)))
    |}]
;;
