(* University of Florida *)
(* Author: Bohdan Purtell *)

(* Testbench Support: "Rx_controller"

   Repeatable testbench modules and test sequences for rx-sided controller on base-MAC.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
(* open! Helper_circuits *)

let () = Stdio.print_endline ""

module Dut = Rx_controller

(* operate on byte-level granularities, and compose from nibbles at a different level *)
(* should we compose on the before after sampling? or rather whether or not the enables
   were in their correct state relevant to each edge? *)

(* actually we should probably only serve on the output edges of things? *)
(* if we observe low, do we need to be able to chian things together in a differnent
   order? aka a lo must proced a high? *)

module Observation = struct
  type t =
    { byte_assembler_en_before : bool
    ; byte_assembler_en_after : bool
    ; dst_mac_reg_en_before : bool
    ; dst_mac_reg_en_after : bool
    ; src_mac_reg_en_before : bool
    ; src_mac_reg_en_after : bool
    ; eth_type_reg_en_before : bool
    ; eth_type_reg_en_after : bool
    ; payload_sel_before : bool
    ; payload_sel_after : bool
    ; emit_payload : bool
    }
  [@@deriving sexp, compare, enumerate]
end

module Snapshot = struct end

module Testbench = struct
  (* eventsim to come eventually *)
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)

  (* highkey can probably lift this to Helper_circuits *)
  module Byte_transaction = struct
    type t = int [@@deriving compare, equal, sexp]

    let to_nibbles byte =
      let lo = byte land 0xF in
      let hi = (byte lsr 4) land 0xF in
      lo, hi
    ;;
    (* ik theres a structured binding for this but i like readability *)
  end

  (* very useful helper that codex thought up *)
  let bit value = if value then Bits.vdd else Bits.gnd

  let inputs ~reset ~en ~rx_dv ~rx_er ~rx_data ~rx_data_valid =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; rx_dv = bit rx_dv
    ; rx_er = bit rx_er
    ; rx_data = Bits.of_int_trunc ~width:8 rx_data
    ; rx_data_valid = bit rx_data_valid
    }
  ;;

  (* the local step handler might require a personal oxcaml fork - i too lazy tho lmao *)
  let drive_nibble (handler : Step.Handler.t @ local) nibble =
    Step.cycle
      handler
      (inputs
         ~reset:false
         ~en:true
         ~rx_dv:true
         ~rx_er:false
         ~rx_data_valid:true
         ~rx_data:nibble)
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
      handler (* handler local passing lmao *)
      (inputs
         ~reset:true
         ~en:false
         ~rx_dv:false (* activation levels might need to be considered *)
         ~rx_er:false
         ~rx_data:0
         ~rx_data_valid:false)
  ;;

  (* shall we construct observaitons to be after edges or before? during perhaps? *)
  (* equivalent to implementing the driving motion of the driver *)
  (* how in the world do i do statement comparisons for something? *)
  (* an entire sequence for example should map to an entire set of things that get
     expected out at each cycle?

     perhaps we can gather up entire masks into some record, and then have those records
     compared against some larger model?
  *)

  (* Record labels are static in OCaml, so accept a typed accessor instead of a field
     name. For example: [~get:(fun o -> o.Dut.O.byte_assembler_en)]. *)
  let get_before ~get ~after_low_nibble = Bits.to_bool (get after_low_nibble)
  let get_after ~get ~after_high_nibble = Bits.to_bool (get after_high_nibble)

  (* let observe_control_mask ~after_low_nibble ~after_high_nibble = *)
  (**)
  (* (* anyway to automatically spec the observation after the before/after snapshots on
     the module map? *) *)
  (* let byte_assembler_en_before = *)
  (* Bits.to_bool after_low_nibble.Dut.O.byte_assembler_en *)
  (* in *)
  (**)
  (* let byte_assembler_en_after = *)
  (* Bits.to_bool after_high_nibble.Dut.O.byte_assembler_en *)
  (* in *)
  (**)
  (* () *)
  (* in *)

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
    | None -> failwith "Rx_byte_assembler testbench timed out"
  ;;
end

(* module I = struct *)
(*   type 'a t = { *)
(* (* spec *) *)
(* clock : 'a; *)
(* reset : 'a; *)
(* en : 'a; *)
(**)
(* (* control lines *) *)
(* rx_dv : 'a; *)
(* rx_er : 'a; *)
(**)
(* (* data line -> rxd presents Preamble/SFD *) *)
(* rx_data : 'a [@bits 8]; *)
(* rx_data_valid : 'a; *)
(*   } [@@deriving hardcaml] *)
(* end *)
(**)
(* module O = struct *)
(*   type 'a t = { *)
(* (* submodule ens *) *)
(* byte_assembler_en : 'a; *)
(**)
(* (* reg ens *) *)
(* dst_mac_reg_en : 'a; *)
(* src_mac_reg_en : 'a; *)
(* eth_type_reg_en : 'a; *)
(**)
(* (* sels *) *)
(* payload_sel : 'a; *)
(**)
(* (* misc *) *)
(* emit_payload : 'a; *)
(* fcs_present : 'a; *)
(**)
(* (* FSM state indicators — 1 when currently in that state *) *)
(* in_preamble : 'a; *)
(* in_dst_mac : 'a; *)
(* in_payload : 'a; *)
(**)
(* (* debug lines *) *)
(* keep : 'a; *)
(*   } [@@deriving hardcaml] *)
(* end *)
