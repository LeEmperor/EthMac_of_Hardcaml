(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_ipv4_rx_unit_quickcheck_tests.ml" *)
(* Integration cases for the composed IPv4 and UDP receive parsers. *)

open! Core
open! Udp_ipv4_rx_testbench

let%test_unit "headers, padding, filtering, checksum policy, stalls, and round trip" =
  run_all ()
;;
