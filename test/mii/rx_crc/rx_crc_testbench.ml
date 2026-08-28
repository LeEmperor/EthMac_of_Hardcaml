(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_crc_testbench.ml" *)

(* Testbench Support: Rx_crc

   Shared DUT fixture, byte-level drivers, observations, and simulation scenarios used by
   the unit, Quickcheck, and expect test suites. [rx_data] is the output of the byte
   assembler, so one CRC transaction is one complete byte.

   Timing note: [crc_out] is the raw accumulator register, so the snapshot taken from
   [Step.O_data.after_edge] of a byte's cycle is the accumulator *including* that byte.
   That is the same value [Hardcaml_verif.Crc32.bytes] returns - the raw accumulator, not
   the inverted FCS word. A receiver clocks the frame and its trailing FCS through the
   same accumulator and lands on [Crc32.residue] (0xDEBB20E3), which is exactly what
   [crc_valid] compares against.

   Enable semantics: the RTL reloads the accumulator with 0xFFFFFFFF whenever [reset] or
   [~en] holds, so dropping [en] mid-frame is the frame-boundary reset, not just a stall.
   [rx_data_valid] is the per-byte accumulate strobe; a cycle with [en] high and
   [rx_data_valid] low holds the accumulator.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Rx_crc

module Output_snapshot = struct
  type t =
    { crc_valid : bool
    ; crc_out : int
    }
  [@@deriving sexp, equal, compare]
end

(* Where in the driven byte stream a cycle sits. The FCS bytes are the four that a
   transmitter appended, so [Fcs 3] is the cycle on which [crc_valid] must rise. *)
module Phase = struct
  type t =
    | Payload of int
    | Fcs of int
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

(* Expect tests use this form: a 32-bit accumulator printed in decimal is unreadable
   against the hex constants the RTL and the standard both quote. *)
module Compact_observation = struct
  type t =
    { phase : Phase.t
    ; byte : string
    ; crc_out : string
    ; crc_valid : bool
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Rx_crc"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs ~reset ~en ~rx_data ~rx_data_valid =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; rx_data = Bits.of_int_trunc ~width:8 rx_data
    ; rx_data_valid = bit rx_data_valid
    }
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { crc_valid = Bits.to_bool output.crc_valid
    ; crc_out = Bits.to_int_trunc output.crc_out
    }
  ;;

  let compact ({ phase; byte; output } : Observation.t) : Compact_observation.t =
    { phase
    ; byte = sprintf "0x%02x" byte
    ; crc_out = sprintf "0x%08x" output.crc_out
    ; crc_valid = output.crc_valid
    }
  ;;

  let cycle (handler : Step.Handler.t @ local) ~reset ~en ~rx_data ~rx_data_valid =
    Step.cycle handler (inputs ~reset ~en ~rx_data ~rx_data_valid)
    |> Step.O_data.after_edge
  ;;

  let drive_byte (handler : Step.Handler.t @ local) byte =
    cycle handler ~reset:false ~en:true ~rx_data:byte ~rx_data_valid:true
  ;;

  (* [en] stays high, so the accumulator holds rather than reloading. *)
  let drive_hold_cycle (handler : Step.Handler.t @ local) =
    cycle handler ~reset:false ~en:true ~rx_data:0xEE ~rx_data_valid:false
  ;;

  (* [en] drops, which is the RTL's frame-boundary reload of 0xFFFFFFFF. *)
  let drive_disabled_cycle (handler : Step.Handler.t @ local) =
    cycle handler ~reset:false ~en:false ~rx_data:0 ~rx_data_valid:false
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs ~reset:true ~en:false ~rx_data:0 ~rx_data_valid:false)
  ;;

  let observe_bytes (handler : Step.Handler.t @ local) ~phase bytes =
    let rec loop (handler : Step.Handler.t @ local) index = function
      | [] -> []
      | byte :: remaining_bytes ->
        let output = drive_byte handler byte |> snapshot in
        { Observation.phase = phase index; byte; output }
        :: loop handler (index + 1) remaining_bytes
    in
    loop handler 0 bytes
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* Feed a byte span and report one observation per byte. No FCS is appended, so the
     final [crc_out] is [Crc32.bytes span].
  *)
  let run_bytes bytes =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      observe_bytes handler ~phase:(fun index -> Phase.Payload index) bytes
    in
    run_with_timeout ~timeout:(4 + List.length bytes) ~testbench
  ;;

  (* Feed a byte span followed by [fcs], the way a receiver sees the wire. The final
     [crc_out] is [Crc32.residue] when [fcs] is the span's real FCS.
  *)
  let run_bytes_with_fcs bytes ~fcs =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let payload =
        observe_bytes handler ~phase:(fun index -> Phase.Payload index) bytes
      in
      let fcs = observe_bytes handler ~phase:(fun index -> Phase.Fcs index) fcs in
      payload @ fcs
    in
    run_with_timeout ~timeout:(4 + List.length bytes + List.length fcs) ~testbench
  ;;

  let final_snapshot observations =
    (List.last_exn (observations : Observation.t list)).output
  ;;

  (* Cross-check of the RTL's [~en] reload: garbage is clocked in, [en] drops for one
     cycle, and the real frame then runs to a clean residue from the reloaded accumulator.
  *)
  let run_enable_drop ~discarded ~bytes ~fcs =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      List.iter discarded ~f:(fun byte ->
        ignore (drive_byte handler byte : Bits.t Dut.O.t));
      let after_disabled_cycle = drive_disabled_cycle handler |> snapshot in
      let payload =
        observe_bytes handler ~phase:(fun index -> Phase.Payload index) bytes
      in
      let fcs = observe_bytes handler ~phase:(fun index -> Phase.Fcs index) fcs in
      after_disabled_cycle, payload @ fcs
    in
    let timeout = 6 + List.length discarded + List.length bytes + List.length fcs in
    run_with_timeout ~timeout ~testbench
  ;;

  (* A cycle with [rx_data_valid] low must leave the accumulator untouched, so the two
     snapshots either side of the gap are equal.
  *)
  let run_valid_gap ~before ~after =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      List.iter before ~f:(fun byte -> ignore (drive_byte handler byte : Bits.t Dut.O.t));
      let before_gap = drive_hold_cycle handler |> snapshot in
      let during_gap = drive_hold_cycle handler |> snapshot in
      let after_gap =
        List.fold after ~init:before_gap ~f:(fun _ byte ->
          drive_byte handler byte |> snapshot)
      in
      before_gap, during_gap, after_gap
    in
    run_with_timeout ~timeout:(6 + List.length before + List.length after) ~testbench
  ;;
end
