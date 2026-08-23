(* Author: Bohdan Purtell

   Testbench: "udp_alcotest_lib"

   Series of testables and library functions for alcotest integration for UDP MAC stack in
   hardcaml.

   Design Notes:

   for an expect-test based item should we compare the output of the frame against a
   golden? or have a sw model compoes each frame? probably a sw-model
*)

open! Core
open! Alcotest
open! Hardcaml
open! Mii_of_hardcaml
open! Udp_of_hardcaml

(* module Dut = struct *)
(**)
(* end *)

(* module Stuff = struct *)
let expected_base_eth_frame =
  [ (* preamble *)
    0x55
  ; 0x55
  ; 0x55
  ; 0x55
  ; 0x55
  ; 0x55
  ; 0xD5 (* sfd *)

         (* eth: dst_port *)
  ; 0x12
  ; 0x34
  ; 0x56
  ; 0x78
  ; 0x9A
  ; 0xBC (* eth: src_port *)
  ; 0xBC
  ; 0x9A
  ; 0x78
  ; 0x56
  ; 0x34
  ; 0x12 (* eth: eth_type *)
  ; 0x99
  ; 0x99
  ]
;;

(* let bruh : unit = *)
(*   Stdio.print_endline "bruh" *)
(* () *)
(* in *)
(* end *)

(* standard ocaml list compomsure: a -> (b -> (c -> (d)))) : hehe closure

   match thing with = | [] -> [] | x :: rest -> x @ [rest] -> rotate_left
*)

(* check : 'a testable -> string -> 'a -> 'a -> unit
*)

(* let test_eth_frame () = *)
(* Alcotest.(check (list int)) *)
(*   "base_eth_frame"          (* test name *) *)
(* expected_base_eth_frame (* expected *) *)
(* Mac_top. (* actual *) *)
(**)
