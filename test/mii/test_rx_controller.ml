(*
  Jane Street Capital
  Author: Bohdan Purtell

  Test: "test_rx_controller"

  Unit tests for the rx-side controller.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench

let () = Stdio.print_endline "=== Imported Test: test_rx_controller ===";;

module Dut = Rx_controller

(* what do we plan to observe? *)
(* compose wire activity into actual TLM-items *)

module Observation = struct
  type t = 
    { o_byte_assembler_en : bool
    ; o_dst_mac_reg_en      : bool
    ; o_src_mac_reg_en      : bool
    ; o_eth_type_reg_en     : bool

    ; o_payload_sel : bool

    ; o_emit_payload  : bool
    ; o_fcs_present   : bool

    ; o_in_preamble : bool
    ; o_in_dst_mac  : bool
    ; o_in_payload  : bool
    } [@@deriving sexp, equal, compare]
end

(* statefulness must be maintined for the reference model here *)
let expected_observation byte : Observation.t =
  { o_byte_assembler_en   = false
    ; o_dst_mac_reg_en      = false
    ; o_src_mac_reg_en      = false
    ; o_eth_type_reg_en     = false

    ; o_payload_sel = false

    ; o_emit_payload  = false
    ; o_fcs_present   = false

    ; o_in_preamble = false
    ; o_in_dst_mac  = false
    ; o_in_payload  = false
  }

(* there needs to be a generator for the randomized payload sequence that is fed, but we need to reconstruct the actaul control signal map that comes back out *)
module Generators = struct
  (* a byte, represented as a "random" variable almost, we define it's constraints *)
  let byte : int Quickcheck.Generator.t = 
    Int.gen_incl 0x00 0xFF
  ;;

  let byte2 = 
    Quickcheck.Generator.weighted_union
    [ 
      (4.0, Quickcheck.Generator.of_list
        [ 0x00
        ; 0x01
        ; 0x0F
        ; 0x10
        ; 0x7F
        ; 0x80
        ; 0xFE
        ; 0xFF ])
    ; 6.0, Int.gen_incl 0x00 0xFF
    ]
  ;;

  (* define a random-len (constrained) sequence of these random variables *)
  (* this is so mathematical its very cool *)
  let byte_sequence : int list Quickcheck.Generator.t = 
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 16 in
    List.gen_with_length length byte
  ;;
end

