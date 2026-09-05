(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "shared_primitives_hierarchy_tests.ml" *)
(* Hierarchical emission check for the reusable masked CRC boundary. *)

open! Core
open! Hardcaml
open! Signal
open! Mac_10g_of_hardcaml
module Circuit = Circuit.With_interface (Mac_10g_crc32.I) (Mac_10g_crc32.O)

let%test_unit "masked CRC emits as an independent hierarchy" =
  let scope = Scope.create ~flatten_design:false () in
  let top = Circuit.create_exn ~name:"crc_parent" (Mac_10g_crc32.hierarchical scope) in
  let database = Scope.circuit_database scope in
  let rtl =
    Rtl.create ~database Verilog [ top ] |> Rtl.full_hierarchy |> Rope.to_string
  in
  assert (String.is_substring rtl ~substring:"module mac_10g_crc32");
  assert (String.is_substring rtl ~substring:"module crc_parent")
;;
