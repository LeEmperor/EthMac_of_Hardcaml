(* University of Florida *)
(* Author: Bohdan Purtell *)

open! Core
open! Hardcaml
open! Signal
open! Mac_10g_of_hardcaml
module Tx_circuit = Circuit.With_interface (Mac_10g_tx.I) (Mac_10g_tx.O)

let%test_unit "formatter emits as an independent hierarchy" =
  let scope = Scope.create ~flatten_design:false () in
  let top = Tx_circuit.create_exn ~name:"tx_parent" (Mac_10g_tx.hierarchical scope) in
  let rtl =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ top ]
    |> Rtl.full_hierarchy
    |> Rope.to_string
  in
  assert (String.is_substring rtl ~substring:"module mac_10g_tx");
  assert (String.is_substring rtl ~substring:"module tx_parent")
;;
