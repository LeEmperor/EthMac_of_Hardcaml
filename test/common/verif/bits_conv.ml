(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "bits_conv.ml" *)
(* Scalar <-> [Hardcaml.Bits.t] conversions shared by every testbench.

   These wrappers exist so a suite never has to spell out the truncating [Bits]
   constructors inline; every phase-1 testbench had already grown its own copy of [bit].

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml

(* [Step.input_hold] records want a [Bits.t] per field, so booleans are the common
   currency of every [inputs ~...] helper. *)
let bit value = if value then Bits.vdd else Bits.gnd
let to_bool bits = Bits.to_bool bits
let to_int bits = Bits.to_int_trunc bits
let of_int ~width value = Bits.of_int_trunc ~width value
