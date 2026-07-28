(*
  Jane Street Capital
  Author: Bohdan Purtell
  
  Test: "test_tx_byte_disassembler"

  Desc:
    My second attempt at learning how to test, Jane Sreet style.
*)

open! Core
(* open! Hardcaml *)
(* open! Signal *)
open! Mii_of_hardcaml
open! Hardcaml_step_testbench

let () = Stdio.print_endline "=== Imported Test: test_tx_byte_disassembler ===";;

module Dut = Tx_byte_disassembler

(* what else do we want to observe? *)
module Observation = struct
  type t = 
    { 
      nibble_pair : (int * int) option
    } [@@deriving sexp, equal, compare]
end

let expected_observation byte : Observation.t =
  let lo : int  = byte land 0xF in
  let hi : int  = (byte lsr 4) land 0xF in
  {
    nibble_pair = Some (lo, hi) (* expect to see nibble tuple pair *)
  }
;;

module Generators = struct
  let byte = Quickcheck.Generator.weighted_union
  [
    (1.0, Quickcheck.Generator.of_list [ 0x00;] );
    (255.0, Int.gen_incl 0x01 0xFF)
  ]

  let byte_sequence : int list Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 32 in
    List.gen_with_length length byte
  ;;
end

module Testbench = struct

end

