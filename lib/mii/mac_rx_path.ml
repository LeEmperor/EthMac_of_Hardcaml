(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_rx_path.ml" *)
(* Receive-side portion of the MII MAC. This circuit owns only [clock_i]-domain logic; the
   RX-to-consumer asynchronous FIFO remains in [Mac_top]. *)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock_i : 'a
    ; reset_i : 'a
    ; en_i : 'a
    ; rx_dv_i : 'a
    ; rx_er_i : 'a
    ; rx_data_i : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { stream_data_o : 'a [@bits 8]
    ; stream_valid_o : 'a
    ; stream_last_o : 'a
    ; stream_user_o : 'a
    ; in_preamble_o : 'a
    ; in_dst_mac_o : 'a
    ; in_payload_o : 'a
    ; frame_crc_ok_o : 'a
    ; frame_done_o : 'a
    ; rx_eth_type_o : 'a [@bits 16]
    ; keep_o : 'a
    }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) : _ O.t =
  let clock = i.I.clock_i in
  let reset = i.I.reset_i in
  let en = i.I.en_i in
  let spec = Reg_spec.create ~clock ~clear:reset () in
  (* These forward wires preserve the controller/datapath dependency without adding a
     register or changing the timing of the extracted logic. *)
  let byte_assembler_en = wire 1 in
  let raw_byte_out_valid = wire 1 in
  let payload_sel = wire 1 in
  let dst_mac_reg_en = wire 1 in
  let src_mac_reg_en = wire 1 in
  let eth_type_reg_en = wire 1 in
  let emit_payload = wire 1 in
  let fcs_present = wire 1 in
  let datapath =
    Rx_datapath.hierarchical
      ~instance:"rx_datapath"
      scope
      { Rx_datapath.I.rx_data = i.rx_data_i
      ; byte_assembler_en
      ; clock
      ; reset
      ; en
      ; payload_sel
      ; dst_mac_reg_en
      ; src_mac_reg_en
      ; eth_type_reg_en
      ; emit_payload
      ; fcs_present
      }
  in
  let controller =
    Rx_controller.hierarchical
      ~instance:"rx_controller"
      scope
      { Rx_controller.I.clock
      ; reset
      ; en
      ; rx_dv = i.rx_dv_i
      ; rx_er = i.rx_er_i
      ; rx_data_valid = raw_byte_out_valid
      ; rx_data = datapath.raw_byte_out
      }
  in
  raw_byte_out_valid <-- datapath.raw_byte_out_valid;
  byte_assembler_en <-- controller.byte_assembler_en;
  payload_sel <-- controller.payload_sel;
  dst_mac_reg_en <-- controller.dst_mac_reg_en;
  src_mac_reg_en <-- controller.src_mac_reg_en;
  eth_type_reg_en <-- controller.eth_type_reg_en;
  emit_payload <-- controller.emit_payload;
  fcs_present <-- controller.fcs_present;
  let frame_end = Helper_circuits.falling_edge_detector spec i.rx_dv_i in
  (* [rx_dv] drops before byte-valid fires for the final FCS byte. Extending the CRC
     enable through [frame_end] lets that byte settle before the result is sampled. *)
  let crc_en = ~:(controller.in_preamble) &: (i.rx_dv_i |: frame_end) &: en in
  let crc =
    Rx_crc.hierarchical
      ~instance:"rx_crc"
      scope
      { Rx_crc.I.clock
      ; reset
      ; en = crc_en
      ; rx_data = datapath.raw_byte_out
      ; rx_data_valid = datapath.raw_byte_out_valid
      }
  in
  let frame_end_d = reg spec frame_end in
  let frame_crc_ok = reg spec ~enable:frame_end_d crc.crc_valid in
  let stream_enable = datapath.payload_out_valid &: datapath.raw_byte_out_valid in
  let stream_data = reg spec datapath.payload_out in
  let stream_valid = reg spec stream_enable in
  let stream_last = frame_end_d &: stream_valid in
  { O.stream_data_o = stream_data
  ; stream_valid_o = stream_valid
  ; stream_last_o = stream_last
  ; stream_user_o = mux2 stream_last ~:(crc.crc_valid) gnd
  ; in_preamble_o = controller.in_preamble
  ; in_dst_mac_o = controller.in_dst_mac
  ; in_payload_o = controller.in_payload
  ; frame_crc_ok_o = frame_crc_ok
  ; frame_done_o = reg spec frame_end_d
  ; rx_eth_type_o = datapath.eth_type
  ; keep_o = datapath.keep |: controller.keep
  }
;;

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"mac_rx_path" create i
;;
