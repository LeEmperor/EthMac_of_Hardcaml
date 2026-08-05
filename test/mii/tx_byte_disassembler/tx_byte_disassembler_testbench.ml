(* 
   University of Florida 
   Author: Bohdan Purtell

   Testbench Support: "Tx_byte_disassembler"

   Reusable testbench components for tx byte disassembler module.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
module Dut = Tx_byte_disassembler

(* A raw view of the output pins at one sampling point. *)
module Output_snapshot = struct
  type t =
    { ready : bool
    ; tx_en : bool
    ; tx_d : int
    }
  [@@deriving sexp, equal, compare]
end

(* One byte-to-two-nibbles transaction. The two active beats are sampled immediately
   before their clock edges; [after_hi] is sampled immediately after the high-nibble edge
   and checks that the serializer returned idle. *)
module Observation = struct
  type t =
    { ready_during_lo : bool
    ; ready_during_hi : bool
    ; tx_en_during_lo : bool
    ; tx_en_during_hi : bool
    ; lo_nibble : int option
    ; hi_nibble : int option
    ; after_hi : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Reset_while_busy_observation = struct
  type t =
    { low_before_reset : Output_snapshot.t
    ; after_reset : Output_snapshot.t
    ; following_byte : Observation.t
    }
  [@@deriving sexp, equal, compare]
end

module Busy_valid_observation = struct
  type t =
    { accepted_low : Output_snapshot.t
    ; accepted_high_while_busy : Output_snapshot.t
    ; after_busy_pulse : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)

  (* should become a common library highkey - i keep saying that and then pushing it off: #techdebt lmao *)
  module Byte_transaction = struct
    type t = int [@@deriving compare, equal, sexp]

    let to_nibbles byte =
      let lo = byte land 0xF in
      let hi = (byte lsr 4) land 0xF in
      lo, hi
    ;;
  end

  let bit value = if value then Bits.vdd else Bits.gnd

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
      Step.cycle handler (inputs ~reset:false ~en:true ~byte_in:byte ~byte_in_valid:true)
    in
    let hi_cycle =
      Step.cycle handler (inputs ~reset:false ~en:true ~byte_in:0 ~byte_in_valid:false)
    in
    ( Step.O_data.before_edge lo_cycle
    , Step.O_data.before_edge hi_cycle (* i think commas with yoda punctuation is a crime against the Lord *)
    , Step.O_data.after_edge hi_cycle )
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs 
        ~reset:true 
        ~en:false 
        ~byte_in:0  
        ~byte_in_valid:false
      )
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { ready = Bits.to_bool output.ready
    ; tx_en = Bits.to_bool output.tx_en
    ; tx_d = Bits.to_int_trunc output.tx_d
    }
  ;;

  let observe_byte ~during_lo ~during_hi ~after_hi =
    let ready_during_lo = Bits.to_bool during_lo.Dut.O.ready in
    let ready_during_hi = Bits.to_bool during_hi.Dut.O.ready in
    let tx_en_during_lo = Bits.to_bool during_lo.Dut.O.tx_en in
    let tx_en_during_hi = Bits.to_bool during_hi.Dut.O.tx_en in
    { Observation.ready_during_lo
    ; ready_during_hi
    ; tx_en_during_lo
    ; tx_en_during_hi (* some_if is rlly cool lmao; very Rusty *)
    ; lo_nibble = Option.some_if tx_en_during_lo (Bits.to_int_trunc during_lo.Dut.O.tx_d)
    ; hi_nibble = Option.some_if tx_en_during_hi (Bits.to_int_trunc during_hi.Dut.O.tx_d)
    ; after_hi = snapshot after_hi
    }
  ;;

  let drive_and_observe_byte (handler : Step.Handler.t @ local) byte =
    let during_lo, during_hi, after_hi = drive_byte handler byte in
    observe_byte ~during_lo ~during_hi ~after_hi
  ;;

  let scenario ~bytes (handler : Step.Handler.t @ local) _initial_outputs =
    reset handler;
    let rec loop (handler : Step.Handler.t @ local) = function
      | [] -> []
      | byte :: remaining_bytes ->
        let observation = drive_and_observe_byte handler byte in
        observation :: loop handler remaining_bytes
    in
    loop handler bytes
  ;;

  let create_simulator () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    Sim.create (Dut.create scope)
  ;;

  let run_with_timeout ~timeout ~testbench =
    let simulator = create_simulator () in
    match Step.run_with_timeout ~timeout () ~simulator ~testbench with
    | Some result -> result
    | None -> failwith "Tx_byte_disassembler testbench timed out"
  ;;

  let run_bytes bytes =
    run_with_timeout ~timeout:(4 + (2 * List.length bytes)) ~testbench:(scenario ~bytes)
  ;;

  let run_reset_while_idle () =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      Step.cycle
        handler
        (inputs ~reset:true ~en:false ~byte_in:0 ~byte_in_valid:false)
      |> Step.O_data.after_edge
      |> snapshot
    in
    run_with_timeout ~timeout:4 ~testbench
  ;;

  (* this shit has GOT to be more composable on Jah[seh] *)
  let run_reset_while_busy ~interrupted_byte ~following_byte =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =

      (* rst *)
      reset handler;

      (* lo *)
      let low_cycle =
        Step.cycle
          handler
          (inputs
             ~reset:false
             ~en:true
             ~byte_in:interrupted_byte
             ~byte_in_valid:true)
      in

      (* hi *)
      let reset_cycle =
        Step.cycle
          handler
          (inputs ~reset:true ~en:false ~byte_in:0 ~byte_in_valid:false)
      in
      { Reset_while_busy_observation.low_before_reset =
          snapshot (Step.O_data.before_edge low_cycle)
      ; after_reset = snapshot (Step.O_data.after_edge reset_cycle)
      ; following_byte = drive_and_observe_byte handler following_byte
      }
    in
    run_with_timeout ~timeout:8 ~testbench
  ;;

  (* absolutely diabolique *)
  let run_valid_pulse_while_busy ~accepted_byte ~offered_while_busy =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =

      (* rst *)
      reset handler;

      (* low portion *)
      let low_cycle =
        Step.cycle
          handler
          (inputs
             ~reset:false
             ~en:true
             ~byte_in:accepted_byte
             ~byte_in_valid:true)
      in

      (* high portion *)
      (* do you think high and hi fight eachother because one is shorter than the other? *)
      let busy_cycle =
        Step.cycle
          handler
          (inputs
             ~reset:false
             ~en:true
             ~byte_in:offered_while_busy
             ~byte_in_valid:true)
      in

      let after_pulse_cycle =
        Step.cycle
          handler
          (inputs ~reset:false ~en:true ~byte_in:0 ~byte_in_valid:false)
      in

      { Busy_valid_observation.accepted_low =
          snapshot (Step.O_data.before_edge low_cycle)
      ; accepted_high_while_busy = snapshot (Step.O_data.before_edge busy_cycle)
      ; after_busy_pulse = snapshot (Step.O_data.before_edge after_pulse_cycle)
      }
    in

    run_with_timeout ~timeout:7 ~testbench
  ;;
end
