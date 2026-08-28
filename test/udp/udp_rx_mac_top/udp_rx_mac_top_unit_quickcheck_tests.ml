(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_rx_mac_top_unit_quickcheck_tests.ml" *)
(* Frame-level integration cases for the MII/IPv4/UDP receive composition. *)

open! Core
open! Udp_rx_mac_top_testbench

let%test_unit "nominal, padded, filtered, and corrupt-FCS frames" = run_all ()
