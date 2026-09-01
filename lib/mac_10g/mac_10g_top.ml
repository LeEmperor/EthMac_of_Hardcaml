(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_top.ml" *)
(* Frozen three-clock-domain interface for the 10G XGMII MAC.

   Phase 0 intentionally supplies only safe, inactive outputs. Functional TX, RX, control,
   and CDC logic is added in later phases behind this interface.
*)

open! Core
open! Hardcaml
open! Signal
open! Xgmii_of_hardcaml

module Config = struct
  let minimum_wire_frame_length = 64
  let maximum_length_field_value = 0xffff
  let default_max_supported_frame_length = 1518
  let default_tx_buffer_depth_bytes = 8192
  let default_rx_buffer_depth_bytes = 8192
  let default_descriptor_capacity = 4

  let validate_buffer ~name ~depth_bytes ~max_supported_frame_length =
    if depth_bytes < max_supported_frame_length || not (Int.is_pow2 depth_bytes)
    then
      raise_s
        [%message
          "Mac_10g_top.create: buffer depth must be a power of two and at least the \
           maximum supported wire-frame length"
            (name : string)
            (depth_bytes : int)
            (max_supported_frame_length : int)]
  ;;

  let validate
    ~tx_buffer_depth_bytes
    ~rx_buffer_depth_bytes
    ~descriptor_capacity
    ~max_supported_frame_length
    =
    if max_supported_frame_length < minimum_wire_frame_length
       || max_supported_frame_length > maximum_length_field_value
    then
      raise_s
        [%message
          "Mac_10g_top.create: maximum supported wire-frame length must be in 64..65535"
            (max_supported_frame_length : int)];
    validate_buffer
      ~name:"TX"
      ~depth_bytes:tx_buffer_depth_bytes
      ~max_supported_frame_length;
    validate_buffer
      ~name:"RX"
      ~depth_bytes:rx_buffer_depth_bytes
      ~max_supported_frame_length;
    if descriptor_capacity < 2 || not (Int.is_pow2 descriptor_capacity)
    then
      raise_s
        [%message
          "Mac_10g_top.create: descriptor capacity must be a power of two and at least 2"
            (descriptor_capacity : int)]
  ;;
end

module I = struct
  type 'a t =
    { (* AXI4-Stream TX ingress and XGMII TX, synchronous to tx_clock_i. *)
      tx_clock_i : 'a
    ; tx_reset_i : 'a
    ; s_axis_tx_tdata_i : 'a [@bits 64]
    ; s_axis_tx_tkeep_i : 'a [@bits 8]
    ; s_axis_tx_tvalid_i : 'a
    ; s_axis_tx_tlast_i : 'a
    ; s_axis_tx_tuser_i : 'a
    ; (* XGMII RX and AXI4-Stream RX egress, synchronous to rx_clock_i. *)
      rx_clock_i : 'a
    ; rx_reset_i : 'a
    ; xgmii_rxd_i : 'a [@bits 64]
    ; xgmii_rxc_i : 'a [@bits 8]
    ; m_axis_rx_tready_i : 'a
    ; (* AXI4-Lite control plane, synchronous to axi_clock_i. *)
      axi_clock_i : 'a
    ; axi_reset_i : 'a
    ; s_axi_awaddr_i : 'a [@bits 12]
    ; s_axi_awvalid_i : 'a
    ; s_axi_wdata_i : 'a [@bits 32]
    ; s_axi_wstrb_i : 'a [@bits 4]
    ; s_axi_wvalid_i : 'a
    ; s_axi_bready_i : 'a
    ; s_axi_araddr_i : 'a [@bits 12]
    ; s_axi_arvalid_i : 'a
    ; s_axi_rready_i : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* AXI4-Stream TX ingress and XGMII TX, synchronous to tx_clock_i. *)
      s_axis_tx_tready_o : 'a
    ; xgmii_txd_o : 'a [@bits 64]
    ; xgmii_txc_o : 'a [@bits 8]
    ; (* AXI4-Stream RX egress, synchronous to rx_clock_i. *)
      m_axis_rx_tdata_o : 'a [@bits 64]
    ; m_axis_rx_tkeep_o : 'a [@bits 8]
    ; m_axis_rx_tvalid_o : 'a
    ; m_axis_rx_tlast_o : 'a
    ; m_axis_rx_tuser_o : 'a
    ; (* AXI4-Lite control plane and interrupt, synchronous to axi_clock_i. *)
      s_axi_awready_o : 'a
    ; s_axi_wready_o : 'a
    ; s_axi_bresp_o : 'a [@bits 2]
    ; s_axi_bvalid_o : 'a
    ; s_axi_arready_o : 'a
    ; s_axi_rdata_o : 'a [@bits 32]
    ; s_axi_rresp_o : 'a [@bits 2]
    ; s_axi_rvalid_o : 'a
    ; irq_o : 'a
    }
  [@@deriving hardcaml]
end

let create
  ?(tx_buffer_depth_bytes = Config.default_tx_buffer_depth_bytes)
  ?(rx_buffer_depth_bytes = Config.default_rx_buffer_depth_bytes)
  ?(descriptor_capacity = Config.default_descriptor_capacity)
  ?(max_supported_frame_length = Config.default_max_supported_frame_length)
  (_scope : Scope.t)
  (_i : _ I.t)
  : _ O.t
  =
  Config.validate
    ~tx_buffer_depth_bytes
    ~rx_buffer_depth_bytes
    ~descriptor_capacity
    ~max_supported_frame_length;
  { O.s_axis_tx_tready_o = gnd
  ; xgmii_txd_o = Xgmii.idle_word.data
  ; xgmii_txc_o = Xgmii.idle_word.control
  ; m_axis_rx_tdata_o = zero 64
  ; m_axis_rx_tkeep_o = zero 8
  ; m_axis_rx_tvalid_o = gnd
  ; m_axis_rx_tlast_o = gnd
  ; m_axis_rx_tuser_o = gnd
  ; s_axi_awready_o = gnd
  ; s_axi_wready_o = gnd
  ; s_axi_bresp_o = zero 2
  ; s_axi_bvalid_o = gnd
  ; s_axi_arready_o = gnd
  ; s_axi_rdata_o = zero 32
  ; s_axi_rresp_o = zero 2
  ; s_axi_rvalid_o = gnd
  ; irq_o = gnd
  }
;;
