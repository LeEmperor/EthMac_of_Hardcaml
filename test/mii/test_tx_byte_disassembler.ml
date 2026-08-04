(*
  Jane Street Capital
  Author: Bohdan Purtell
  
  Test: "test_tx_byte_disassembler"

  Desc:
    My second attempt at learning how to test, Jane Sreet style.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench

let () = Stdio.print_endline "=== Imported Test: test_tx_byte_disassembler ===";;

(* fundamentally we're driving in a byte and shooting for 2 nibbles out *)
module Dut = Tx_byte_disassembler

(* what else do we want to observe? *)
module Observation = struct
  type t =
    { ready_during_lo : bool
    ; ready_during_hi : bool
    ; tx_en_during_lo : bool
    ; tx_en_during_hi : bool
    ; nibble_pair : (int * int) option
    }
  [@@deriving sexp, equal, compare]
end

let expected_observation byte : Observation.t =
  { ready_during_lo = true
  ; ready_during_hi = false
  ; tx_en_during_lo = true
  ; tx_en_during_hi = true
  ; nibble_pair = Some (byte land 0xF, (byte lsr 4) land 0xF)
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
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)

  let bit value = if value then Bits.vdd else Bits.gnd

  (* way to abstract some input mapper over the I shape? perhaps a functor? *)
  (* present a byte to the disassembler at a time *)
  let inputs ~reset ~en ~byte_in ~byte_in_valid =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; byte_in = Bits.of_int_trunc ~width:8 byte_in
    ; byte_in_valid = bit byte_in_valid
    }
  ;;

  let drive_byte (handler : Step.Handler.t @ local) byte =
    let lo_cycle =
      Step.cycle
        handler
        (inputs
           ~reset:false
           ~en:true
           ~byte_in:byte
           ~byte_in_valid:true)
    in
    let hi_cycle =
      Step.cycle
        handler
        (inputs
           ~reset:false
           ~en:true
           ~byte_in:0
           ~byte_in_valid:false)
    in
    Step.O_data.before_edge lo_cycle, Step.O_data.before_edge hi_cycle
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles:num_cycles
      handler
      (inputs ~reset:true ~en:false ~byte_in:0 ~byte_in_valid:false)
  ;;

  let observe_byte ~during_lo ~during_hi =
    let ready_during_lo = Bits.to_bool during_lo.Dut.O.ready in
    let ready_during_hi = Bits.to_bool during_hi.Dut.O.ready in
    let tx_en_during_lo = Bits.to_bool during_lo.Dut.O.tx_en in
    let tx_en_during_hi = Bits.to_bool during_hi.Dut.O.tx_en in

    let nibble_pair =
      if tx_en_during_lo && tx_en_during_hi
      then
        Some
          ( Bits.to_int_trunc during_lo.Dut.O.tx_d
          , Bits.to_int_trunc during_hi.Dut.O.tx_d
          )
      else None
    in
    { Observation.ready_during_lo
    ; ready_during_hi
    ; tx_en_during_lo
    ; tx_en_during_hi
    ; nibble_pair
    }
  ;;
end

