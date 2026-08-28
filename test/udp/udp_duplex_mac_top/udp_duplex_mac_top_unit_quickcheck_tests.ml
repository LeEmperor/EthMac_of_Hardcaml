(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_duplex_mac_top_unit_quickcheck_tests.ml" *)
(* RX-only, TX-only, and simultaneous full-duplex integration cases. *)

open! Core
open! Udp_duplex_mac_top_testbench

let%test_unit "RX, TX, and both directions concurrently" =
  ignore (run_all () : result list)
;;
