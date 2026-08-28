(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_mac_top_unit_quickcheck_tests.ml" *)
(* Frame-level integration cases for the UDP/IPv4/MII transmit composition. *)

open! Core
open! Udp_mac_top_testbench

let%test_unit "all legacy frame boundaries, bubbles, and no-reset sequences" =
  ignore (run_all () : observation list)
;;
