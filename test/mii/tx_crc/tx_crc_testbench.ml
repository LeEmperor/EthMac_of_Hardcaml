(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_crc_testbench.ml" *)

(* Testbench Support: Tx_crc

   Shared DUT fixture, byte-level drivers, observations, and simulation scenarios used by
   the unit, Quickcheck, and expect test suites.

   The DUT holds the same reflected-polynomial accumulator as [Rx_crc], but publishes it
   inverted: [fcs_byte] is one byte of ~crc_reg selected by [byte_sel], in transmission
   order (0 = least significant). So the oracle for [crc_out] is
   [Hardcaml_verif.Crc32.bytes] - the raw accumulator - while the oracle for the four
   [fcs_byte] reads is [Crc32.fcs_bytes]. The two are not interchangeable.

   Timing note: [crc_out] and [fcs_byte] are both driven off the accumulator register, so
   a snapshot from [Step.O_data.after_edge] of a byte's cycle includes that byte. Reading
   a different [byte_sel] costs a cycle only because the fixture drives one input set per
   cycle; the mux itself is combinational, and the read cycles hold [data_valid] low so
   the accumulator does not move underneath them.

   Enable semantics: [en] low reloads the accumulator with 0xFFFFFFFF. It is the
   frame-boundary reset, not a stall - a stall is [en] high with [data_valid] low.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Tx_crc

(* [keep] is the module's synthesis anti-pruning OR-reduce and carries no verification
   meaning, so it is deliberately absent here. *)
module Output_snapshot = struct
  type t =
    { fcs_byte : int
    ; crc_out : int
    }
  [@@deriving sexp, equal, compare]
end

module Phase = struct
  type t =
    | Payload of int
    | Fcs_read of int (* the [byte_sel] value driven on this cycle *)
  [@@deriving sexp, equal, compare]
end

module Observation = struct
  type t =
    { phase : Phase.t
    ; byte : int
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

(* Hex, because the constants this block is judged against - 0xCBF43926 for the standard
   check vector, 0xFFFFFFFF at reload - are quoted in hex everywhere else. *)
module Compact_observation = struct
  type t =
    { phase : Phase.t
    ; byte : string
    ; crc_out : string
    ; fcs_byte : string
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Tx_crc"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs ~reset ~en ~data ~data_valid ~byte_sel =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; data = Bits.of_int_trunc ~width:8 data
    ; data_valid = bit data_valid
    ; byte_sel = Bits.of_int_trunc ~width:2 byte_sel
    }
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { fcs_byte = Bits.to_int_trunc output.fcs_byte
    ; crc_out = Bits.to_int_trunc output.crc_out
    }
  ;;

  let compact ({ phase; byte; output } : Observation.t) : Compact_observation.t =
    { phase
    ; byte = sprintf "0x%02x" byte
    ; crc_out = sprintf "0x%08x" output.crc_out
    ; fcs_byte = sprintf "0x%02x" output.fcs_byte
    }
  ;;

  let cycle (handler : Step.Handler.t @ local) ~reset ~en ~data ~data_valid ~byte_sel =
    Step.cycle handler (inputs ~reset ~en ~data ~data_valid ~byte_sel)
    |> Step.O_data.after_edge
  ;;

  let drive_byte ?(byte_sel = 0) (handler : Step.Handler.t @ local) byte =
    cycle handler ~reset:false ~en:true ~data:byte ~data_valid:true ~byte_sel
  ;;

  (* Accumulator holds: [en] stays high, only the accumulate strobe drops. *)
  let drive_read_cycle (handler : Step.Handler.t @ local) ~byte_sel =
    cycle handler ~reset:false ~en:true ~data:0 ~data_valid:false ~byte_sel
  ;;

  (* Accumulator reloads to 0xFFFFFFFF: this is the frame boundary. *)
  let drive_disabled_cycle (handler : Step.Handler.t @ local) =
    cycle handler ~reset:false ~en:false ~data:0 ~data_valid:false ~byte_sel:0
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs ~reset:true ~en:false ~data:0 ~data_valid:false ~byte_sel:0)
  ;;

  let observe_bytes (handler : Step.Handler.t @ local) bytes =
    let rec loop (handler : Step.Handler.t @ local) index = function
      | [] -> []
      | byte :: remaining_bytes ->
        let output = drive_byte handler byte |> snapshot in
        { Observation.phase = Phase.Payload index; byte; output }
        :: loop handler (index + 1) remaining_bytes
    in
    loop handler 0 bytes
  ;;

  (* Sweep [byte_sel] 0..3 with the accumulator parked, which is exactly what
     [tx_datapath] does while [tx_controller] sits in the Fcs state. *)
  let observe_fcs_reads (handler : Step.Handler.t @ local) =
    (* Written out rather than mapped over [0; 1; 2; 3]: a closure that both captures the
       local handler and allocates its result cannot be local, and OCaml does not fix the
       evaluation order of a list literal anyway. *)
    let read (handler : Step.Handler.t @ local) byte_sel =
      let output = drive_read_cycle handler ~byte_sel |> snapshot in
      { Observation.phase = Phase.Fcs_read byte_sel; byte = 0; output }
    in
    let byte_0 = read handler 0 in
    let byte_1 = read handler 1 in
    let byte_2 = read handler 2 in
    let byte_3 = read handler 3 in
    [ byte_0; byte_1; byte_2; byte_3 ]
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  let run_bytes bytes =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      observe_bytes handler bytes
    in
    run_with_timeout ~timeout:(4 + List.length bytes) ~testbench
  ;;

  (* Feed a payload, then read the four FCS bytes out through the mux. Returns the whole
     trace; [fcs_bytes_of] pulls the four emitted bytes back out of it. *)
  let run_bytes_then_read_fcs bytes =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let payload = observe_bytes handler bytes in
      payload @ observe_fcs_reads handler
    in
    run_with_timeout ~timeout:(8 + List.length bytes) ~testbench
  ;;

  let fcs_bytes_of (observations : Observation.t list) =
    List.filter_map observations ~f:(fun observation ->
      match observation.phase with
      | Phase.Fcs_read _ -> Some observation.output.fcs_byte
      | Phase.Payload _ -> None)
  ;;

  let final_snapshot observations =
    (List.last_exn (observations : Observation.t list)).output
  ;;

  (* The legacy harness's enable-drop case: garbage, an [en] low cycle, then the real
     payload. The FCS read afterwards must be the payload's alone. *)
  let run_enable_drop ~discarded ~bytes =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      List.iter discarded ~f:(fun byte ->
        ignore (drive_byte handler byte : Bits.t Dut.O.t));
      let after_disabled_cycle = drive_disabled_cycle handler |> snapshot in
      let payload = observe_bytes handler bytes in
      after_disabled_cycle, payload @ observe_fcs_reads handler
    in
    run_with_timeout ~timeout:(10 + List.length discarded + List.length bytes) ~testbench
  ;;
end
