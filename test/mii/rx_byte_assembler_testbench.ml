(* Shared DUT fixture and simulation scenarios for both assertion and expect tests. *)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench

module Dut = Rx_byte_assembler

module Observation = struct
  type t =
    { valid_after_low_nibble : bool
    ; valid_after_high_nibble : bool
    ; completed_byte : int option
    }
  [@@deriving sexp, equal, compare]
end

module Output_snapshot = struct
  type t =
    { byte_out : int
    ; byte_valid : bool
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)

  module Byte_transaction = struct
    type t = int [@@deriving compare, equal, sexp]

    let to_nibbles byte = byte land 0xF, (byte lsr 4) land 0xF
  end

  let bit value = if value then Bits.vdd else Bits.gnd

  let inputs ~reset ~en ~rx_data =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; rx_data = Bits.of_int_trunc ~width:4 rx_data
    }
  ;;

  let drive_nibble (handler : Step.Handler.t @ local) nibble =
    Step.cycle handler (inputs ~reset:false ~en:true ~rx_data:nibble)
    |> Step.O_data.after_edge
  ;;

  let drive_byte (handler : Step.Handler.t @ local) byte =
    let low, high = Byte_transaction.to_nibbles byte in
    let after_low_nibble = drive_nibble handler low in
    let after_high_nibble = drive_nibble handler high in
    after_low_nibble, after_high_nibble
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs ~reset:true ~en:false ~rx_data:0)
  ;;

  let observe_byte ~after_low_nibble ~after_high_nibble =
    let valid_after_low_nibble =
      Bits.to_bool after_low_nibble.Dut.O.byte_valid
    in
    let valid_after_high_nibble =
      Bits.to_bool after_high_nibble.Dut.O.byte_valid
    in
    { Observation.valid_after_low_nibble
    ; valid_after_high_nibble
    ; completed_byte =
        (if valid_after_high_nibble
         then Some (Bits.to_int_trunc after_high_nibble.byte_out)
         else None)
    }
  ;;

  let drive_and_observe_byte (handler : Step.Handler.t @ local) byte =
    let after_low_nibble, after_high_nibble = drive_byte handler byte in
    observe_byte ~after_low_nibble ~after_high_nibble
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { byte_out = Bits.to_int_trunc output.byte_out
    ; byte_valid = Bits.to_bool output.byte_valid
    }
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
      Scope.create
        ~flatten_design:true
        ~auto_label_hierarchical_ports:true
        ()
    in
    Sim.create (Dut.create scope)
  ;;

  let run_with_timeout ~timeout ~testbench =
    let simulator = create_simulator () in
    match Step.run_with_timeout ~timeout () ~simulator ~testbench with
    | Some result -> result
    | None -> failwith "Rx_byte_assembler testbench timed out"
  ;;

  let run_bytes bytes =
    let timeout = 4 + (2 * List.length bytes) in
    run_with_timeout
      ~timeout
      ~testbench:(fun handler initial_outputs ->
        scenario ~bytes handler initial_outputs)
  ;;

  let run_with_disabled_cycles byte disabled_nibbles =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let low, high = Byte_transaction.to_nibbles byte in
      let after_low = drive_nibble handler low in
      let while_disabled =
        List.map disabled_nibbles ~f:(fun nibble ->
          Step.cycle
            handler
            (inputs ~reset:false ~en:false ~rx_data:nibble)
          |> Step.O_data.after_edge)
      in
      let after_high = drive_nibble handler high in
      List.map (after_low :: while_disabled @ [ after_high ]) ~f:snapshot
    in
    run_with_timeout
      ~timeout:(6 + List.length disabled_nibbles)
      ~testbench
  ;;

  let run_reset_mid_byte ~discarded_low ~byte =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let after_discarded_low = drive_nibble handler discarded_low in
      let after_reset =
        Step.cycle
          handler
          (inputs ~reset:true ~en:false ~rx_data:0)
        |> Step.O_data.after_edge
      in
      let low, high = Byte_transaction.to_nibbles byte in
      let after_low = drive_nibble handler low in
      let after_high = drive_nibble handler high in
      List.map
        [ after_discarded_low; after_reset; after_low; after_high ]
        ~f:snapshot
    in
    run_with_timeout ~timeout:8 ~testbench
  ;;
end
