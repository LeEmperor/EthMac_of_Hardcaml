(*
  University of Florida
  Author: Bohdan Purtell

  Testbench Support: Rx_byte_assembler

  Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit, Quickcheck, and expect test suites.

  Tags: { "ACTIVE"
          ; "TEST"
          ; "TESTBENCH"
          ; "PERSONAL_REFERENCE"
          ; "COMMON_ITEMS"
        }
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
(* open! Helper_circuits *)

module Dut = Rx_byte_assembler

(* think of as UVM monitor item *)
module Observation = struct
  type t =
    { valid_after_low_nibble : bool
    ; valid_after_high_nibble : bool
    ; completed_byte : int option
    }
  [@@deriving sexp, equal, compare]
end

(* partial observations *)
module Output_snapshot = struct
  type t =
    { byte_out : int
    ; byte_valid : bool
    }
  [@@deriving sexp, equal, compare]
end

(* main re-usables *)
module Testbench = struct
  (* eventsim to come eventually *)
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)

  (* highkey can probably lift this to Helper_circuits*)
  module Byte_transaction = struct
    type t = int
    [@@deriving compare, equal, sexp]

    let to_nibbles byte = 
      let lo = byte land 0xF in
      let hi = (byte lsr 4) land 0xF in
      (lo, hi) (* ik theres a structured binding for this but i like readability *)
  end

  (* very useful helper that codex thought up*)
  let bit value = if value then Bits.vdd else Bits.gnd

  (* fan an input record; thought is that theres a more generic function f that lets me take a module I and generate the "inputs" function for it with all the arguments labelled already and everything, and the implementation is left to me *)
  let inputs ~reset ~en ~rx_data =
    { Step.input_hold with
        reset = bit reset
      ; en = bit en
      ; rx_data = Bits.of_int_trunc ~width:4 rx_data
    }
  ;;

  (* the local step handler might require a personal oxcaml fork - i too lazy tho lmao*)
  let drive_nibble (handler : Step.Handler.t @ local) nibble =
    Step.cycle 
      handler (inputs 
            ~reset:false 
            ~en:true 
            ~rx_data:nibble)
    |> Step.O_data.after_edge
  ;;

  let drive_byte (handler : Step.Handler.t @ local) byte =
    let low, high = Byte_transaction.to_nibbles byte in
    let after_low_nibble = drive_nibble handler low in
    let after_high_nibble = drive_nibble handler high in

    (after_low_nibble, after_high_nibble)
  ;;

  (* perhaps this might be abstractable for any module that has a reset signal, and given set of inputs that need to be 0 during a reset condition? *)
  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs 
        ~reset:true 
        ~en:false 
        ~rx_data:0)(* is this a partial application or just a tuple lmao *)
  ;;

  (* one day we shall construct proper comparator models - good train activity highkey *)
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
        (
          if valid_after_high_nibble
          then Some (Bits.to_int_trunc after_high_nibble.byte_out)
          else None
        )
    }
  ;;

  let drive_and_observe_byte (handler : Step.Handler.t @ local) byte =
    let (after_low_nibble, after_high_nibble) = 
      drive_byte handler byte 
    in

    observe_byte ~after_low_nibble ~after_high_nibble
  ;;

  (* very curious implementation for "raw"er data that would otherwise be a subset of a whole Observation *)
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

  (* repetable boilerplate *)
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

  (* i think of this like a uvm sequence - probably unhealthy *)
  (* these really sohuld be composed elsewhere but i guess i don't have enough things to jutsify such a thing - also for TLM items its probably best to put them here so that whoeve rrefers to this module can have the items exist in an agnostic state regardless of the backing simulation (ie cycle vs event) *)
  let run_with_disabled_cycles byte disabled_nibbles =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =

      (*reset*)
      reset handler;

      (* setup inputs *)
      let low, high = Byte_transaction.to_nibbles byte in

      (* observe*)
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
    
    (* actual sequence execution -> the ownership model of the sequence owning it's own execution is very UVM-like and I've never truly understood it but alas *)
    run_with_timeout
      ~timeout:(6 + List.length disabled_nibbles)
      ~testbench
  ;;

  (* this one instead works with the remaining things after we halt at some random point; the quickcheck with one of those probabilistic unions might be interested to see applied here and whether or not we can see a relationship in between flow through behaviours and the proportions of items in the randomized union that would comprise the test casese or the reset distribution *)
  let run_reset_mid_byte ~discarded_low ~byte =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =

      (* reset *)
      reset handler;

      (* form a snapshot *)
      let after_discarded_low = drive_nibble handler discarded_low in
      let after_reset =
        Step.cycle
          handler
          (inputs ~reset:true ~en:false ~rx_data:0)
        |> Step.O_data.after_edge
      in

      let (low, high) = Byte_transaction.to_nibbles byte in (* eventually gotta stop doing this parenthesis thing but really helps me to reason about tuples - standard notation should be enforced in OCaml - I guess I could write it into formatter tooling myself lmao *)

    (* the thought is a structured binding can be used here, but the knowledge that execution order is not guaranteed for tuples in this language (thanks OCaml discord!) may be difficult -> haven't ever found this to be a problem but ill ask a dev about it eventually -> apparently the stdlib does these things as well to guarantee execution order; C++ has similar fallacies, which is why then() exists me thinks *)
      let after_low   = drive_nibble handler low in
      let after_high  = drive_nibble handler high in

      List.map
        [ after_discarded_low; after_reset; after_low; after_high ]
        ~f:snapshot
    in

    (* execute myself *)
    run_with_timeout ~timeout:8 ~testbench (* statistically the timeout is not guaranteed lmao - hopefully the regression rigs don't catch it *)
  ;;
end
