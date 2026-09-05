(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_rx_64_mac_top.ml" *)
(* RX-only UDP-over-MII composition with a 64-bit application stream.

   The existing protocol stack remains byte-wide, which matches the 100 Mb/s MII PHY.
   [Axis_width_adapter_8_to_64] is deliberately placed after UDP header removal: the
   application sees only UDP payload bytes, packed in the same lane order as a typical
   64-bit 10G MAC AXI-stream interface.
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { (* MII PHY receive interface, synchronous to [rx_clock_i]. [rx_reset_i] is
         synchronous at this module boundary. *)
      rx_clock_i : 'a
    ; rx_reset_i : 'a
    ; rx_dv_i : 'a
    ; rx_er_i : 'a
    ; rx_data_i : 'a [@bits 4]
    ; (* UDP application stream, synchronous to [tx_clock_i]. [tx_reset_i] is synchronous. *)
      tx_clock_i : 'a
    ; tx_reset_i : 'a
    ; en_i : 'a
    ; app_tready_i : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* Recovered UDP payload and metadata, synchronous to [tx_clock_i]. *)
      app_tdata_o : 'a [@bits 64]
    ; app_tkeep_o : 'a [@bits 8]
    ; app_tvalid_o : 'a
    ; app_tlast_o : 'a
    ; app_tfirst_o : 'a
    ; app_start_o : 'a
    ; src_port_o : 'a [@bits 16]
    ; dst_port_o : 'a [@bits 16]
    ; udp_length_o : 'a [@bits 16]
    ; payload_length_o : 'a [@bits 16]
    ; udp_checksum_o : 'a [@bits 16]
    ; src_ip_o : 'a [@bits 32]
    ; dst_ip_o : 'a [@bits 32]
    ; checksum_ok_o : 'a
    ; crc_error_o : 'a
    ; rx_frame_done_o : 'a
    ; ip_busy_o : 'a
    ; udp_busy_o : 'a
    ; (* MAC receive status, synchronous to [rx_clock_i]. *)
      frame_crc_ok_o : 'a
    ; in_payload_o : 'a
    ; frame_done_o : 'a
    }
  [@@deriving hardcaml]
end

module Metadata = struct
  type 'a t =
    { src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; udp_length : 'a [@bits 16]
    ; payload_length : 'a [@bits 16]
    ; udp_checksum : 'a [@bits 16]
    ; src_ip : 'a [@bits 32]
    ; dst_ip : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

let create ?(rx_fifo_for_sim = false) (scope : Scope.t) (i : _ I.t) : _ O.t =
  let narrow_ready = wire 1 in
  let narrow =
    Udp_rx_mac_top.hierarchical
      ~instance:"udp_rx_mac_top_8"
      ~rx_fifo_for_sim
      scope
      { Udp_rx_mac_top.I.rx_clock = i.rx_clock_i
      ; rx_reset = i.rx_reset_i
      ; tx_clock = i.tx_clock_i
      ; tx_reset = i.tx_reset_i
      ; en = i.en_i
      ; rx_dv = i.rx_dv_i
      ; rx_er = i.rx_er_i
      ; rx_data = i.rx_data_i
      ; app_tready = narrow_ready
      }
  in
  let wide =
    Axis_width_adapter_8_to_64.hierarchical
      ~instance:"udp_payload_8_to_64"
      scope
      { Axis_width_adapter_8_to_64.I.clock_i = i.tx_clock_i
      ; reset_i = i.tx_reset_i
      ; en_i = i.en_i
      ; s_tdata_i = narrow.app_tdata
      ; s_tvalid_i = narrow.app_tvalid
      ; s_tlast_i = narrow.app_tlast
      ; s_tfirst_i = narrow.app_tfirst
      ; m_tready_i = i.app_tready_i
      }
  in
  narrow_ready <-- wide.s_tready_o;
  (* Snapshot the sideband at the narrow frame start. This keeps it associated with the
     corresponding wide frame even if a following UDP header is parsed while the final
     wide beat is stalled.
  *)
  (* spec *)
  let spec = Reg_spec.create ~clock:i.tx_clock_i ~clear:i.tx_reset_i () in
  let metadata =
    Metadata.Of_signal.reg
      spec
      ~enable:narrow.app_start
      { Metadata.src_port = narrow.src_port
      ; dst_port = narrow.dst_port
      ; udp_length = narrow.udp_length
      ; payload_length = narrow.payload_length
      ; udp_checksum = narrow.udp_checksum
      ; src_ip = narrow.src_ip
      ; dst_ip = narrow.dst_ip
      }
  in
  { O.app_tdata_o = wide.m_tdata_o
  ; app_tkeep_o = wide.m_tkeep_o
  ; app_tvalid_o = wide.m_tvalid_o
  ; app_tlast_o = wide.m_tlast_o
  ; app_tfirst_o = wide.m_tfirst_o
  ; (* Unlike [app_tfirst_o], which remains asserted with a stalled first beat, this is a
       one-cycle transfer event. *)
    app_start_o = wide.m_tfirst_o &: i.app_tready_i
  ; src_port_o = metadata.src_port
  ; dst_port_o = metadata.dst_port
  ; udp_length_o = metadata.udp_length
  ; payload_length_o = metadata.payload_length
  ; udp_checksum_o = metadata.udp_checksum
  ; src_ip_o = metadata.src_ip
  ; dst_ip_o = metadata.dst_ip
  ; checksum_ok_o = narrow.checksum_ok
  ; crc_error_o = narrow.crc_error
  ; rx_frame_done_o = narrow.rx_frame_done
  ; ip_busy_o = narrow.ip_busy
  ; udp_busy_o = narrow.udp_busy
  ; frame_crc_ok_o = narrow.frame_crc_ok
  ; in_payload_o = narrow.in_payload
  ; frame_done_o = narrow.frame_done
  }
;;

let hierarchical ?instance ?(rx_fifo_for_sim = false) scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"udp_rx_64_mac_top" (create ~rx_fifo_for_sim) i
;;
