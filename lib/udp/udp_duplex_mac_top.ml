(*
 * Bohdan Purtell
 * University of Florida
 *
 * Module: Udp_duplex_mac_top
 *
 * Full-duplex UDP-over-MAC top — the *union* of [Udp_mac_top] (TX stack) and
 * [Udp_rx_mac_top] (RX stack) around ONE [Mac_top]. The two directions are
 * INDEPENDENT: this top owns no coupling between them (that is Harness #2's
 * RX->TX bridge; see UDP_FULL_DUPLEX_HARNESS_PLAN.md). It is the UDP mirror of the
 * bare-MAC [mac_validation_harness], which already drives a btn[3] TX burst and
 * a 1-byte/sec RX drain side-by-side on a single [Mac_top].
 *
 *     btn TX app ─→ Udp_tx ─→ Ipv4_tx ─→ Mac_top.s_axis ─→ PHY TX   (fpga -> laptop)
 *                                          (one Mac_top)
 *        app out ◀─ Udp_rx ◀─ Ipv4_rx ◀─ Mac_top.m_axis ◀─ PHY RX   (laptop -> fpga)
 *
 * Both composition stacks run in the tx_clock domain (the MAC captures PHY RX in
 * rx_clock, then its async RX FIFO presents [m_axis] in tx_clock), so the whole
 * thing is single-domain apart from the MAC's own internal RX->TX FIFO — no new CDC
 * is introduced here. MII is inherently full-duplex (separate TX/RX pins + clocks),
 * so simultaneous [tx_en]/[rx_dv] is fine.
 *
 * Everything below is lifted verbatim from the two single-direction tops; the only
 * merge work is (a) one shared [Mac_top] instead of two, with BOTH [s_axis] (TX)
 * and [m_axis] (RX) wired, and (b) disambiguating the two [udp_busy] outputs into
 * [tx_udp_busy] / [rx_udp_busy]. Endpoints stay the elaboration-time constants that
 * already agree with udp_app.py's golden values (ports 0x1234/0x1235, IPs .10/.1).
 *)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Ipv4_of_hardcaml

(* ── TX endpoints (mirror Udp_mac_top) ─────────────────────────────────────── *)
module Udp_cfg = struct
  let src_port = 0x1234
  let dst_port = 0x1235
end

module Ip_cfg = struct
  let src_ip = [ 192; 168; 1; 10 ]
  let dst_ip = [ 192; 168; 1; 1 ]
end

module Tx_path = Udp_ipv4_tx.Make (Udp_cfg) (Ip_cfg)

(* ── RX policy (mirror Udp_rx_mac_top): forward everything, report status ───── *)
module Ip_rx_cfg = struct
  let drop_on_bad_checksum = false
  let debug = false
end

module Udp_rx_cfg = struct
  let drop_on_port_mismatch = false
  let expected_dst_port = 0x1235
  let debug = false
end

module Rx_path = Udp_ipv4_rx.Make (Ip_rx_cfg) (Udp_rx_cfg)

module I = struct
  type 'a t =
    { (* two clock domains, mirroring Mac_top *)
      rx_clock : 'a
    ; rx_reset : 'a
    ; tx_clock : 'a
    ; tx_reset : 'a
    ; en : 'a
    ; (* PHY RX pins (rx_clock domain) *)
      rx_dv : 'a
    ; rx_er : 'a
    ; rx_data : 'a [@bits 4]
    ; (* UDP application TX side (tx_clock domain) — fpga -> laptop *)
      tx_start : 'a
    ; payload_len : 'a [@bits 16]
    ; payload_tdata : 'a [@bits 8]
    ; payload_tvalid : 'a
    ; (* recovered-UDP RX backpressure (tx_clock domain) — laptop -> fpga *)
      app_tready : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* ── TX side (fpga -> laptop) ── *)
      tx_d : 'a [@bits 4]
    ; tx_en : 'a
    ; tx_busy : 'a
    ; payload_tready : 'a
    ; tx_udp_busy : 'a (* Udp_tx emitting a datagram *)
    ; (* ── RX side (laptop -> fpga): recovered UDP application payload ── *)
      app_tdata : 'a [@bits 8]
    ; app_tvalid : 'a
    ; app_tlast : 'a
    ; app_tfirst : 'a
    ; app_start : 'a
    ; (* per-frame RX metadata (stable from the header parse through the frame) *)
      src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; udp_length : 'a [@bits 16]
    ; payload_length : 'a [@bits 16]
    ; udp_checksum : 'a [@bits 16] (* raw header field; NOT verified (stub) *)
    ; src_ip : 'a [@bits 32]
    ; dst_ip : 'a [@bits 32]
    ; (* per-frame RX status *)
      checksum_ok : 'a (* IPv4 header checksum verified *)
    ; crc_error : 'a (* held bad-frame verdict, padding-safe (see Udp_rx_mac_top) *)
    ; rx_frame_done : 'a (* tx-domain 1-cycle end-of-frame pulse *)
    ; ip_busy : 'a
    ; rx_udp_busy : 'a (* Udp_rx mid-datagram *)
    ; (* MAC RX status passthrough (rx_clock domain) — for the board LED/reg block *)
      frame_crc_ok : 'a
    ; in_payload : 'a
    ; frame_done : 'a
    }
  [@@deriving hardcaml]
end

let create ?(rx_fifo_for_sim = false) (scope : Scope.t) (i : _ I.t) : _ O.t =
  (* ── Wire stubs breaking every backpressure combinational loop ───────────── *)
  (* TX stack (mirrors Udp_mac_top) *)
  let wire_mac_tready = Signal.wire 1 in
  (* MAC.s_axis_tready -> Ipv4_tx.mac_tready *)
  (* RX stack (mirrors Udp_rx_mac_top) *)
  let wire_mac_rready = Signal.wire 1 in
  let tx_path =
    Tx_path.hierarchical
      ~instance:"udp_ipv4_tx"
      scope
      { Tx_path.I.clock_i = i.tx_clock
      ; reset_i = i.tx_reset
      ; en_i = i.en
      ; start_i = i.tx_start
      ; payload_len_i = i.payload_len
      ; payload_tdata_i = i.payload_tdata
      ; payload_tvalid_i = i.payload_tvalid
      ; mac_tready_i = wire_mac_tready
      }
  in
  (* ── L2: ONE shared MAC, both directions wired ───────────────────────────── *)
  let mac =
    Mac_top.hierarchical
      ~rx_fifo_for_sim
      ~ethertype:0x0800
      scope
      { Mac_top.I.rx_clock = i.rx_clock
      ; rx_reset = i.rx_reset
      ; tx_clock = i.tx_clock
      ; tx_reset = i.tx_reset
      ; en = i.en
      ; rx_dv = i.rx_dv
      ; rx_er = i.rx_er
      ; rx_data = i.rx_data (* RX: feed the recovered-payload chain (via the wire stub) *)
      ; m_axis_tready = wire_mac_rready (* TX: driven by the IPv4/UDP TX stack *)
      ; s_axis_tdata = tx_path.m_tdata_o
      ; s_axis_tvalid = tx_path.m_tvalid_o
      ; s_axis_tlast = tx_path.m_tlast_o
      ; s_axis_tuser = Signal.gnd
      ; tx_start = tx_path.tx_start_o
      }
  in
  let rx_path =
    Rx_path.hierarchical
      ~instance:"udp_ipv4_rx"
      scope
      { Rx_path.I.clock_i = i.tx_clock
      ; reset_i = i.tx_reset
      ; en_i = i.en
      ; rx_tdata_i = mac.m_axis_tdata
      ; rx_tvalid_i = mac.m_axis_tvalid
      ; rx_tlast_i = mac.m_axis_tlast
      ; rx_tuser_i = mac.m_axis_tuser
      ; rx_tfirst_i = mac.m_axis_tfirst
      ; rx_eth_type_i = mac.rx_eth_type
      ; app_tready_i = i.app_tready
      }
  in
  (* close every backpressure loop now that all blocks exist *)
  Signal.(wire_mac_tready <-- mac.s_axis_tready);
  Signal.(wire_mac_rready <-- rx_path.m_axis_tready_o);
  { O.tx_d (* TX side *) = mac.tx_d
  ; tx_en = mac.tx_en
  ; tx_busy = mac.tx_busy
  ; payload_tready = tx_path.payload_tready_o
  ; tx_udp_busy = tx_path.udp_busy_o (* RX side *)
  ; app_tdata = rx_path.app_tdata_o
  ; app_tvalid = rx_path.app_tvalid_o
  ; app_tlast = rx_path.app_tlast_o
  ; app_tfirst = rx_path.app_tfirst_o
  ; app_start = rx_path.app_start_o
  ; src_port = rx_path.src_port_o
  ; dst_port = rx_path.dst_port_o
  ; udp_length = rx_path.udp_length_o
  ; payload_length = rx_path.payload_length_o
  ; udp_checksum = rx_path.udp_checksum_o
  ; src_ip = rx_path.src_ip_o
  ; dst_ip = rx_path.dst_ip_o
  ; checksum_ok = rx_path.checksum_ok_o
  ; crc_error = rx_path.crc_error_o
  ; rx_frame_done = rx_path.frame_done_o
  ; ip_busy = rx_path.ip_busy_o
  ; rx_udp_busy = rx_path.udp_busy_o (* MAC RX status passthrough *)
  ; frame_crc_ok = mac.frame_crc_ok
  ; in_payload = mac.in_payload
  ; frame_done = mac.frame_done
  }
;;

let hierarchical ?instance ?(rx_fifo_for_sim = false) scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"udp_duplex_mac_top" (create ~rx_fifo_for_sim) i
;;
