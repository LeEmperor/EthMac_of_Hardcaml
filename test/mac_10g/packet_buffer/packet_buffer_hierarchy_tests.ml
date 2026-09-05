(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "packet_buffer_hierarchy_tests.ml" *)
(* Hierarchy and inferred-memory shape checks for the banked packet buffer. *)

open! Core
open! Hardcaml
open! Packet_buffer_testbench
module Circuit = Circuit.With_interface (Dut.I) (Dut.O)

let%test_unit "packet buffer emits a reusable hierarchy with named byte banks" =
  let scope = Scope.create ~flatten_design:false () in
  let top = Circuit.create_exn ~name:"packet_buffer_parent" (Dut.hierarchical scope) in
  let database = Scope.circuit_database scope in
  let rtl =
    Rtl.create ~database Verilog [ top ] |> Rtl.full_hierarchy |> Rope.to_string
  in
  assert (String.is_substring rtl ~substring:"module mac_10g_packet_buffer_32_4");
  List.iter (List.range 0 8) ~f:(fun bank ->
    assert (String.is_substring rtl ~substring:(sprintf "byte_bank_%d" bank)));
  assert (String.is_substring rtl ~substring:"descriptor_length");
  assert (String.is_substring rtl ~substring:"descriptor_error")
;;
