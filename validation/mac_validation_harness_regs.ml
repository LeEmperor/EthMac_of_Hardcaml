(*
 * Module: Mac_validation_harness_regs
 * AXI4-Lite register block for MAC validation.  ***STUB for now.***
 *
 * In a normal validation flow these registers are exposed to a processor
 * (Zynq PS, or a soft-core such as MicroBlaze) over AXI4-Lite so firmware can
 * poke control bits and read back status/counters live, without rebuilding
 * the bitstream. This module gives that block a real port shape now so the
 * harness can wire against it, while the body stays a stub: ready lines held
 * high, reads return zero, control outputs driven to safe defaults.
 *
 * Planned register map (word-addressed, 4 words):
 *   0x0  CONTROL  [0]=soft_reset  [1]=tx_start_req  [2]=enable
 *   0x4  STATUS   [0]=link_up     [1]=frame_done    [2]=crc_ok   [3]=in_payload
 *   0x8  RX_FRAMES  32-bit received-frame counter
 *   0xC  CRC_ERRS   32-bit CRC-error counter
 *)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; (* ── AXI4-Lite slave (from PS / soft-core master) ── *)
      s_axi_awaddr : 'a [@bits 4]
    ; s_axi_awvalid : 'a
    ; s_axi_wdata : 'a [@bits 32]
    ; s_axi_wstrb : 'a [@bits 4]
    ; s_axi_wvalid : 'a
    ; s_axi_bready : 'a
    ; s_axi_araddr : 'a [@bits 4]
    ; s_axi_arvalid : 'a
    ; s_axi_rready : 'a
    ; (* ── Observation taps from Mac_top (read-only status) ── *)
      frame_done : 'a
    ; frame_crc_ok : 'a
    ; in_payload : 'a
    ; tx_en : 'a
    ; rx_dv : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* ── AXI4-Lite slave responses ── *)
      s_axi_awready : 'a
    ; s_axi_wready : 'a
    ; s_axi_bresp : 'a [@bits 2]
    ; s_axi_bvalid : 'a
    ; s_axi_arready : 'a
    ; s_axi_rdata : 'a [@bits 32]
    ; s_axi_rresp : 'a [@bits 2]
    ; s_axi_rvalid : 'a
    ; (* ── Control lines back into the harness ── *)
      soft_reset : 'a
    ; tx_start_req : 'a
    ; enable : 'a
    ; keep : 'a
    }
  [@@deriving hardcaml]
end

(* STUB: acknowledge everything, read as zero, no control asserted. Replace with a real
   AXI4-Lite decode + status/counter registers once the processor side is wired up. Inputs
   are intentionally unused for now. *)
let create (_scope : Scope.t) (_i : _ I.t) : _ O.t =
  { O.s_axi_awready = vdd
  ; s_axi_wready = vdd
  ; s_axi_bresp = zero 2
  ; (* OKAY *)
    s_axi_bvalid = gnd
  ; s_axi_arready = vdd
  ; s_axi_rdata = zero 32
  ; s_axi_rresp = zero 2
  ; (* OKAY *)
    s_axi_rvalid = gnd
  ; soft_reset = gnd
  ; tx_start_req = gnd
  ; enable = vdd
  ; keep = gnd
  }
;;

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"mac_validation_harness_regs" create i
;;
