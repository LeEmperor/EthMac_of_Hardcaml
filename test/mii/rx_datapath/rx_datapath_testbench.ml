(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_datapath_testbench.ml" *)

(* Testbench Support: Rx_datapath

   Shared DUT fixture, nibble-level drivers, observations, and simulation scenarios used
   by the unit, Quickcheck, and expect test suites.

   This DUT sits one level below [rx_controller]: it takes raw MII nibbles and the
   controller's register enables and selects, and it instantiates [rx_byte_assembler]
   internally. So the transaction here is a nibble pair, and the drivers are written in
   bytes with the low nibble first, exactly as the PHY presents them.

   Sampling point. The byte assembler registers [byte_valid] and [byte_out] together, so
   both are visible on [Step.O_data.after_edge] of a byte's *high* nibble cycle. That is
   the only cycle on which [raw_byte_out_valid] is high, and it is where [mac_top] gates
   its FIFO write ([payload_out_valid &: raw_byte_out_valid]) - so it is where this
   testbench samples too. Every observation below is that one cycle per byte.

   FCS-strip pipeline. [payload_out] is [raw_byte_out] delayed four *enabled* cycles,
   where the enable is [raw_byte_out_valid]. At the sampling point for byte k the pipeline
   has advanced k times, so it presents byte k-4: nothing emerges for the first four
   bytes, and when the driver drops [emit_payload] at the end of the frame the four bytes
   still in flight - the FCS - are never emitted. That is the whole mechanism, and it is
   why the collected list is the payload with the trailing four bytes gone.

   Header registers shift MSB-first on [<field>_reg_en &: raw_byte_out_valid], which is
   high one cycle after the byte completes. A capture therefore needs one trailing cycle
   past the last header byte before the register has settled.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Rx_datapath

(* [keep] is the module's synthesis anti-pruning OR-reduce and carries no verification
   meaning, so it is deliberately absent here. *)
module Output_snapshot = struct
  type t =
    { raw_byte_out : int
    ; raw_byte_out_valid : bool
    ; payload_out : int
    ; payload_out_valid : bool
    ; eth_type : int
    }
  [@@deriving sexp, equal, compare]
end

module Phase = struct
  type t =
    | Eth_type of int
    | Payload of int
    | Fcs of int
    | Drain of int
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

(* The compact form keeps only what a reader of the pipeline trace needs: which byte went
   in, which came out, and whether [mac_top] would have written it. *)
module Compact_observation = struct
  type t =
    { phase : Phase.t
    ; byte_in : string
    ; raw_byte_out : string
    ; payload_out : string
    ; emitted : bool
    }
  [@@deriving sexp, equal, compare]
end

module Strip_observation = struct
  type t =
    { collected : int list
    ; trace : Observation.t list
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Rx_datapath"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs
    ~reset
    ~en
    ~dst_mac_reg_en
    ~src_mac_reg_en
    ~byte_assembler_en
    ~eth_type_reg_en
    ~payload_sel
    ~emit_payload
    ~fcs_present
    ~rx_data
    =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; dst_mac_reg_en = bit dst_mac_reg_en
    ; src_mac_reg_en = bit src_mac_reg_en
    ; byte_assembler_en = bit byte_assembler_en
    ; eth_type_reg_en = bit eth_type_reg_en
    ; payload_sel = bit payload_sel
    ; emit_payload = bit emit_payload
    ; fcs_present = bit fcs_present
    ; rx_data = Bits.of_int_trunc ~width:4 rx_data
    }
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { raw_byte_out = Bits.to_int_trunc output.raw_byte_out
    ; raw_byte_out_valid = Bits.to_bool output.raw_byte_out_valid
    ; payload_out = Bits.to_int_trunc output.payload_out
    ; payload_out_valid = Bits.to_bool output.payload_out_valid
    ; eth_type = Bits.to_int_trunc output.eth_type
    }
  ;;

  (* [mac_top]'s FIFO write gate, reproduced verbatim. A byte counts as emitted only when
     both the payload valid and the assembled-byte strobe are high. *)
  let emitted (output : Output_snapshot.t) =
    output.payload_out_valid && output.raw_byte_out_valid
  ;;

  let compact ({ phase; byte; output } : Observation.t) : Compact_observation.t =
    { phase
    ; byte_in = sprintf "0x%02x" byte
    ; raw_byte_out = sprintf "0x%02x" output.raw_byte_out
    ; payload_out = sprintf "0x%02x" output.payload_out
    ; emitted = emitted output
    }
  ;;

  (* The register enables and selects a scenario holds steady across a span of bytes.
     Bundling them keeps the driver signature from growing a tail of booleans. *)
  module Control = struct
    type t =
      { dst_mac_reg_en : bool
      ; src_mac_reg_en : bool
      ; eth_type_reg_en : bool
      ; payload_sel : bool
      ; emit_payload : bool
      ; fcs_present : bool
      }

    let idle =
      { dst_mac_reg_en = false
      ; src_mac_reg_en = false
      ; eth_type_reg_en = false
      ; payload_sel = false
      ; emit_payload = false
      ; fcs_present = false
      }
    ;;

    let capturing_eth_type = { idle with eth_type_reg_en = true }

    (* What [rx_controller] holds through the payload and the FCS that follows it. *)
    let emitting_payload = { idle with payload_sel = true; emit_payload = true }
  end

  let cycle (handler : Step.Handler.t @ local) ~(control : Control.t) ~rx_data =
    Step.cycle
      handler
      (inputs
         ~reset:false
         ~en:true
         ~dst_mac_reg_en:control.dst_mac_reg_en
         ~src_mac_reg_en:control.src_mac_reg_en
         ~byte_assembler_en:true
         ~eth_type_reg_en:control.eth_type_reg_en
         ~payload_sel:control.payload_sel
         ~emit_payload:control.emit_payload
         ~fcs_present:control.fcs_present
         ~rx_data)
    |> Step.O_data.after_edge
  ;;

  (* Low nibble first, as the PHY presents it. The high nibble's cycle is the sampling
     point, so that is the snapshot the caller gets back. *)
  let drive_byte (handler : Step.Handler.t @ local) ~control byte =
    ignore (cycle handler ~control ~rx_data:(byte land 0xF) : Bits.t Dut.O.t);
    cycle handler ~control ~rx_data:((byte lsr 4) land 0xF)
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs
         ~reset:true
         ~en:false
         ~dst_mac_reg_en:false
         ~src_mac_reg_en:false
         ~byte_assembler_en:false
         ~eth_type_reg_en:false
         ~payload_sel:false
         ~emit_payload:false
         ~fcs_present:false
         ~rx_data:0)
  ;;

  (* top 10 functions ive ever written *)
  let observe_bytes (handler : Step.Handler.t @ local) ~control ~phase bytes =
    let rec loop (handler : Step.Handler.t @ local) index = function
      | [] -> []
      | byte :: remaining_bytes ->
        let output = drive_byte handler ~control byte |> snapshot in
        { Observation.phase = phase index; byte; output }
        :: loop handler (index + 1) remaining_bytes
    in
    loop handler 0 bytes
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* One nibble pair per byte, plus reset and pipeline slack. *)
  let byte_timeout count = 8 + (2 * count)

  (* Shift [bytes] into the ethertype register. The trailing byte is driven with the
     enable still high because the register only shifts on the cycle *after* a byte
     completes. *)
  let run_eth_type_capture bytes =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let control = Control.capturing_eth_type in
      let trace =
        observe_bytes handler ~control ~phase:(fun index -> Phase.Eth_type index) bytes
      in
      let settled = cycle handler ~control ~rx_data:0 |> snapshot in
      trace, settled.eth_type
    in
    run_with_timeout ~timeout:(byte_timeout (List.length bytes)) ~testbench
  ;;

  [@@@ocamlformat "enable"]

  (* Drive [payload] then [fcs] with the controller's payload selects held high, drop
     them, and drain the pipeline. [collected] is what [mac_top]'s FIFO would have
     written. *)
  let run_payload_strip ~payload ~fcs =
    let drain_bytes = 6 in
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let control = Control.emitting_payload in
      let payload_trace =
        observe_bytes handler ~control ~phase:(fun index -> Phase.Payload index) payload
      in
      let fcs_trace =
        observe_bytes handler ~control ~phase:(fun index -> Phase.Fcs index) fcs
      in
      let drain_trace =
        observe_bytes
          handler
          ~control:Control.idle
          ~phase:(fun index -> Phase.Drain index)
          (List.init drain_bytes ~f:(fun _ -> 0x00))
      in
      let trace = payload_trace @ fcs_trace @ drain_trace in
      let collected =
        List.filter_map trace ~f:(fun (observation : Observation.t) ->
          Option.some_if (emitted observation.output) observation.output.payload_out)
      in
      { Strip_observation.collected; trace }
    in
    let byte_count = List.length payload + List.length fcs + drain_bytes in
    run_with_timeout ~timeout:(byte_timeout byte_count) ~testbench
  ;;
end
