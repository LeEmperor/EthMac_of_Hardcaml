(* Bohdan Purtell University of Florida

   Module: Udp_mac_top

   Composition layer: a UDP (L4) + IPv4 (L3) TX stack stacked on top of the MII Ethernet
   MAC (L2, Mac_top). This module owns the *wiring*, so the layering stays
   one-directional:

   Udp_tx ─(UDP datagram + meta)→ Ipv4_tx ─(Eth payload AXI-S)→ Mac_top.s_axis ─→ PHY

   Each layer only knows the one below it:
   - Udp_tx hands its datagram bytes + [{ip_start, l4_length, protocol}] to Ipv4_tx.
   - Ipv4_tx prepends the IPv4 header and drives Mac_top's s_axis.
   - The MAC knows nothing about IP or UDP — [mii_of_hardcaml] has no dependency on this
     library or on ipv4_of_hardcaml. "Including a UDP/IP stack" is a question of *what you
     instantiate around the MAC*, not a flag on the MAC.

   Backpressure flows the other way through two wire stubs that break the combinational
   loops: MAC.s_axis_tready → Ipv4_tx.mac_tready, and Ipv4_tx.l4_tready →
   Udp_tx.l4_tready.

   Everything here lives in the tx_clock domain (the MAC's TX side); the RX path is passed
   straight through untouched.

   Endpoints are elaboration-time constants (functor Config): src/dst IP live in the IPv4
   layer, src/dst port in the UDP layer. The MAC emits ethertype 0x0800 so a real IPv4
   host accepts the frame.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Ipv4_of_hardcaml

(* fixed endpoints for the first bring-up *)
module Udp_cfg = struct
  let src_port = 0x1234
  let dst_port = 0x1235
end

module Ip_cfg = struct
  let src_ip = [ 192; 168; 1; 10 ] (* 4 bytes, network order *)
  let dst_ip = [ 192; 168; 1; 1 ]
end

module Tx_path = Udp_ipv4_tx.Make (Udp_cfg) (Ip_cfg)

module I = struct
  type 'a t =
    { (* two clock domains, mirroring Mac_top *)
      rx_clock : 'a
    ; rx_reset : 'a
    ; tx_clock : 'a
    ; tx_reset : 'a
    ; en : 'a
    ; (* PHY RX pins *)
      rx_dv : 'a
    ; rx_er : 'a
    ; rx_data : 'a [@bits 4]
    ; (* RX AXI-S consumer backpressure *)
      m_axis_tready : 'a
    ; (* UDP application TX side (tx_clock domain) *)
      tx_start : 'a (* begin a datagram *)
    ; payload_len : 'a [@bits 16] (* application-data length, latched at tx_start *)
    ; payload_tdata : 'a [@bits 8]
    ; payload_tvalid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* RX AXI-S output (straight through from the MAC) *)
      m_axis_tdata : 'a [@bits 8]
    ; m_axis_tkeep : 'a
    ; m_axis_tlast : 'a
    ; m_axis_tvalid : 'a
    ; m_axis_tuser : 'a
    ; (* MII TX pins *)
      tx_d : 'a [@bits 4]
    ; tx_en : 'a
    ; tx_busy : 'a
    ; (* UDP application backpressure / status *)
      payload_tready : 'a
    ; udp_busy : 'a
    ; (* MAC RX status passthrough (rx_clock domain) — surfaced so board harnesses can
         reuse the MAC validation regs/LED status block. Straight from the internal
         Mac_top instance; the UDP layer neither consumes nor gates them. *)
      frame_crc_ok : 'a (* holds last RX frame's CRC result; 1 = good *)
    ; in_payload : 'a (* RX FSM is in the payload state *)
    ; frame_done : 'a (* 1-cycle pulse when an RX frame completes *)
    }
  [@@deriving hardcaml]
end

let create ?(rx_fifo_for_sim = false) (scope : Scope.t) (i : _ I.t) : _ O.t =
  (* Break the MAC-to-protocol backpressure loop with a wire stub. *)
  let wire_mac_tready = Signal.wire 1 in
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
  (* L2: Ethernet framing + FCS. ethertype 0x0800 = IPv4. *)
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
      ; rx_data = i.rx_data
      ; m_axis_tready =
          i.m_axis_tready
          (* the wiring: IPv4 byte stream → MAC's Ethernet-payload sink. ip_tx pulses
             tx_start as it opens the frame; the MAC latches that in start_pending and
             holds transmission until the whole datagram is buffered (store-and-forward —
             see the frames_buffered gate in mac_top.ml). The full frame — Ethernet
             framing, IPv4 header, UDP header, streamed app payload, MAC zero-padding, and
             FCS — is emitted byte-perfect (verified end-to-end in udp_mac_top_tb.ml). *)
      ; s_axis_tdata = tx_path.m_tdata_o
      ; s_axis_tvalid = tx_path.m_tvalid_o
      ; s_axis_tlast = tx_path.m_tlast_o
      ; s_axis_tuser = Signal.gnd
      ; tx_start = tx_path.tx_start_o
      }
  in
  Signal.(wire_mac_tready <-- mac.s_axis_tready);
  { O.m_axis_tdata = mac.m_axis_tdata
  ; m_axis_tkeep = mac.m_axis_tkeep
  ; m_axis_tlast = mac.m_axis_tlast
  ; m_axis_tvalid = mac.m_axis_tvalid
  ; m_axis_tuser = mac.m_axis_tuser
  ; tx_d = mac.tx_d
  ; tx_en = mac.tx_en
  ; tx_busy = mac.tx_busy
  ; payload_tready = tx_path.payload_tready_o
  ; udp_busy = tx_path.udp_busy_o
  ; frame_crc_ok = mac.frame_crc_ok
  ; in_payload = mac.in_payload
  ; frame_done = mac.frame_done
  }
;;

let hierarchical ?instance ?(rx_fifo_for_sim = false) scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"udp_mac_top" (create ~rx_fifo_for_sim) i
;;
