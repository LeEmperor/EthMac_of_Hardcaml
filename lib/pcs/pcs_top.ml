(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "pcs_top.ml" *)
(* Top-level composition boundary for a single-lane 10GBASE-R PCS

   The MAC-facing side is the internal 64-bit SDR XGMII described in
   [docs/xgmii_contract.md].

   The encoded side deliberately matches the semantic shape of an UltraScale+ 64b/66b
   gearbox: a 64-bit payload and a separate two-bit sync header.

   A board-specific GTY wrapper is responsible for adapting these signals to the exact
   transceiver primitive ports and reset sequence. Yay for PMA on SERDES.

   This file currently establishes the interfaces and safe reset/link-down behaviour.

   The Clause 49 encoder, scrambler, block synchronizer, descrambler, decoder, and BER
   monitor will be inserted at the marked boundaries below.
*)

open! Hardcaml
open! Signal
open! Xgmii_of_hardcaml (* shared contract *)

(* These aliases make the two reusable boundary types easy to discover from the top while
   allowing leaf encoder/decoder modules to depend on [Xgmii] and [Base_r_block] directly,
   without creating a dependency cycle through [Pcs_top].

   Gets awfully close to looking like smart_constructors lmao.
*)
module Xgmii_word = Xgmii.Word
module Encoded_block = Base_r_block

(* Headlands convention *)
module I = struct
  type 'a t =
    { (* MAC/RS -> PCS, TX clock domain. *)
      tx_clock_i : 'a
    ; tx_reset_i : 'a
    ; xgmii_txd_i : 'a [@bits 64]
    ; xgmii_txc_i : 'a [@bits 8]
    ; (* GT gearbox -> PCS, RX clock domain.

         Data and header valid are kept separate because the UltraScale+ gearbox exposes
         both indications.
      *)
      rx_clock_i : 'a
    ; rx_reset_i : 'a
    ; encoded_rx_data_i : 'a [@bits 64]
    ; encoded_rx_header_i : 'a [@bits 2]
    ; encoded_rx_data_valid_i : 'a
    ; encoded_rx_header_valid_i : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* PCS -> MAC/RS, RX clock domain. *)
      xgmii_rxd_o : 'a [@bits 64]
    ; xgmii_rxc_o : 'a [@bits 8]
    ; (* PCS -> GT gearbox, TX clock domain. [encoded_tx_block_valid_o] is an integration
         guard, not a native GTY pin. A transceiver adapter must not enable TX until it is
         asserted. *)
      encoded_tx_data_o : 'a [@bits 64]
    ; encoded_tx_header_o : 'a [@bits 2]
    ; encoded_tx_block_valid_o : 'a
    ; (* Block synchronizer -> GT RX gearbox. *)
      rx_gearbox_slip_o : 'a
    ; (* Clause 49 receive status, RX clock domain. *)
      rx_block_lock_o : 'a
    ; rx_high_ber_o : 'a
    ; rx_bad_block_o : 'a
    ; (* Encoder/contract status, TX clock domain. *)
      tx_bad_xgmii_o : 'a
    }
  [@@deriving hardcaml]
end

let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  let rx_spec = Reg_spec.create ~clock:i.rx_clock_i ~clear:i.rx_reset_i () in
  (* Until the RX pipeline exists, reset presents deterministic idle and the first
     non-reset RX edge changes to Local Fault. In particular, idle during reset must not
     be interpreted as an operational link. *)
  let xgmii_rxd =
    reg rx_spec ~clear_to:Xgmii.idle_word.data Xgmii.local_fault_word.data -- "xgmii_rxd"
  in
  let xgmii_rxc =
    reg rx_spec ~clear_to:Xgmii.idle_word.control Xgmii.local_fault_word.control
    -- "xgmii_rxc"
  in
  (* Deliberately emit an invalid placeholder and deassert valid. A constant, unscrambled
     idle control block is not a legal serial 10GBASE-R stream, so presenting one as live
     data here would make the scaffold dangerously convincing. *)
  { O.xgmii_rxd_o = xgmii_rxd
  ; xgmii_rxc_o = xgmii_rxc
  ; encoded_tx_data_o = zero 64
  ; encoded_tx_header_o = zero 2
  ; encoded_tx_block_valid_o = gnd
  ; rx_gearbox_slip_o = gnd
  ; rx_block_lock_o = gnd
  ; rx_high_ber_o = gnd
  ; rx_bad_block_o = gnd
  ; tx_bad_xgmii_o = gnd
  }
;;
