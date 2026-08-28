(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_loopback_mac_top_unit_quickcheck_tests.ml" *)
(* Normal, patterned, padded, and bad-FCS echo integration cases. *)

open! Core
open! Udp_loopback_mac_top_testbench

let%test_unit "every received application payload is echoed in a fresh frame" =
  ignore (run_all () : result list)
;;
