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
      ; tx_en_after_lo  : bool
      (* ; tx_en_after_lo  : bool *) (* should this be checking for after?*)
      ; tx_en_after_hi  : bool
      ; nibble_pair : (int * int) option
    } [@@deriving sexp, equal, compare]
end

let expected_observation byte : Observation.t =
  let lo : int  = byte land 0xF in
  let hi : int  = (byte lsr 4) land 0xF in

  { ready_during_lo = true
  ; ready_during_hi = false
  ; tx_en_after_lo = true
  ; tx_en_after_hi = true
  ; nibble_pair = Some(lo, hi)
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

  module Byte_transaction = struct
    type t = int [@@deriving compare, equal, sexp]

    let to_nibbles byte = 
      let low = byte land 0xF in
      let high = (byte lsr 4) land 0xF in
      (low, high)
    ;;
  end

    let bit value = 
    if value then Bits.vdd else Bits.gnd
  ;;

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

  (* going unneeded as we need to drive bytes as the granular TLM *)
  let drive_nibble (handler : Step.Handler.t @ local) nibble = 
    let step_result = 
      Step.cycle
        handler
        (inputs 
          ~reset:false 
          ~en:true 
          ~byte_in:nibble 
          ~byte_in_valid:true)
    in
    Step.O_data.after_edge step_result
  ;;

  let drive_byte (handler : Step.Handler.t @ local) byte =
    let lo_cycle = 
      Step.cycle
        handler
        (inputs
          ~reset:false
          ~en:true
          ~byte_in:byte
          ~byte_in_valid:true
  )
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
    Step.O_data.before_edge lo_cycle,
    Step.O_data.before_edge hi_cycle

;;
  let reset ?(num_cycles=1) (handler : Step.Handler.t @ local) = 
    Step.delay
      ~num_cycles:num_cycles
      handler
      (inputs ~reset:true ~en:false ~byte_in:0 ~byte_in_valid:false)
;;

  (* after_low_nibble = simulator state after low nibble edge *)
  (* could i instead refer to these as even/odd edges? *)
  let obesrve_byte ~ready ~tx_en ~during_lo ~during_hi =
    let ready_after_low_nibble = 
      Bits.to_bool during_lo.Dut.O.ready
    in

    let ready_after_high_nibble = 
      Bits.to_bool during_hi.Dut.O.ready
    in

    (* these are both option? *)
    let low_nibble : int option = 
      if (ready_after_low_nibble)
      then Some (Bits.to_int_trunc during_lo.tx_d)
      else None
    in

    let high_nibble : int option = 
      if (ready_after_high_nibble)
      then Some (Bits.to_int_trunc during_hi.tx_d)
      else None
    in

    (* where tf my stdlib go lmao *)
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
    (* let nibble_pair = Some (0, 0) in *)

    (* rebuild of this record*)
    { Observation.ready_during_lo = false
      ; ready_during_hi = false
      ; tx_en_after_lo = false
      ; tx_en_after_hi = false
      ; nibble_pair
    }
  ;;

  let drive_byte (handler : Step.Handler.t @ local) byte = 
    let lo, hi = Byte_transaction.to_nibbles byte in
    let after_lo_nibble = drive_nibble handler lo in
    let after_hi_nibble = drive_nibble handler hi in
    (* (drive_nibble handler lo, drive_nibble handler hi) *)
      (* execution order in tuples is NOT guaranteed; can we make it guaranteed? *)
    (after_lo_nibble, after_hi_nibble)
  ;;

end

