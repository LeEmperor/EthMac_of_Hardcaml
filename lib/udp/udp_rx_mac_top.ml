(* Bohdan Purtell University of Florida

   Module: Udp_rx_mac_top

   Composition layer: the *receive* mirror of [Udp_mac_top]. Stacks the IPv4 (L3) and UDP
   (L4) receive parsers on top of the MII Ethernet MAC (L2), so the wiring stays
   one-directional:

   PHY ─→ Mac_top.m_axis ─(Eth payload)→ Ipv4_rx ─(UDP datagram)→ Udp_rx ─→ app

   Each layer only knows the one below it:
   - The MAC hands up the Ethernet *payload* byte stream (m_axis) plus two sidebands the
     parsers need: [m_axis_tfirst] (SOF pulse on payload byte 0) and [rx_eth_type]
     (latched Ethernet type, stable per frame).
   - Ipv4_rx strips the 20-byte IPv4 header, verifies the header checksum, drops MAC
     zero-padding, and hands up the L4 datagram + [{protocol, src/dst ip}].
   - Udp_rx strips the 8-byte UDP header and hands up the application payload +
     [{src/dst port, lengths}].

   Backpressure flows the other way through two wire stubs that break the combinational
   loops: Udp_rx.m_axis_tready → Ipv4_rx.l4_tready, and Ipv4_rx.m_axis_tready →
   MAC.m_axis_tready. The application's [app_tready] gates the whole chain from the top.

   This is a dedicated RX-only top: the MAC's TX AXI-S sink is tied off (no frames are
   transmitted). It is the natural DUT for isolated RX bring-up — nothing on the TX side
   can interfere with the receive experiment. (The already-validated TX composition lives
   untouched in [Udp_mac_top]; a full-duplex top can merge the two later.)

   Clock domains: the MAC captures PHY data in [rx_clock], then its async RX FIFO presents
   [m_axis] in [tx_clock]. Ipv4_rx, Udp_rx, and the application RX stream therefore run in
   [tx_clock]. (The [rx_eth_type] sideband is currently sampled across the boundary
   combinationally — a CDC-hardening TODO carried over from Mac_top; inert in single-clock
   simulation with [rx_fifo_for_sim].)

   RX policy is fixed for first bring-up (no dropping): every well-formed IPv4/UDP frame
   is forwarded and its status reported. Flip the [Ip_rx_cfg]/[Udp_rx_cfg] knobs below to
   enforce checksum / port filtering.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Ipv4_of_hardcaml

(* Bring-up RX policy: forward everything, just report status. The expected dst port
   mirrors [Udp_mac_top]'s TX dst_port (0x1235) so a loopback agrees. *)
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
    ; (* application backpressure for the recovered UDP payload (tx_clock domain) *)
      app_tready : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* recovered UDP application payload stream (tx_clock domain) *)
      app_tdata : 'a [@bits 8]
    ; app_tvalid : 'a
    ; app_tlast : 'a
    ; app_tfirst : 'a (* SOF pulse on the first application byte *)
    ; app_start : 'a (* latch metadata here (pulses with app_tfirst) *)
    ; (* per-frame metadata (stable from the header parse through the frame) *)
      src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; udp_length : 'a [@bits 16]
    ; payload_length : 'a [@bits 16]
    ; udp_checksum : 'a [@bits 16] (* raw header field; NOT verified (stub) *)
    ; src_ip : 'a [@bits 32]
    ; dst_ip : 'a [@bits 32]
    ; (* per-frame status *)
      checksum_ok : 'a (* IPv4 header checksum verified *)
    ; crc_error : 'a
        (* app-visible bad-frame verdict: held level, latched from the frame-level
           late-status channel at [rx_frame_done]. Correct even when MAC padding delays
           the FCS verdict past the payload tlast (unlike the layers' tlast-aligned
           flags). *)
    ; rx_frame_done : 'a
        (* tx-domain 1-cycle end-of-frame pulse (aligned to m_axis); [crc_error] is
           refreshed on this edge *)
    ; ip_busy : 'a
    ; udp_busy : 'a
    ; (* MAC RX status passthrough (rx_clock domain) — surfaced so a board harness can
         reuse the MAC validation regs/LED status block, same as Udp_mac_top. *)
      frame_crc_ok : 'a
    ; in_payload : 'a
    ; frame_done : 'a
    }
  [@@deriving hardcaml]
end

let create ?(rx_fifo_for_sim = false) (scope : Scope.t) (i : _ I.t) : _ O.t =
  (* Break the protocol-to-MAC backpressure loop with a wire stub. *)
  let wire_mac_ready = Signal.wire 1 in
  (* L2: Ethernet framing + FCS check. ethertype only matters for TX framing (tied off
     here); RX filtering keys off the latched rx_eth_type instead. *)
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
          wire_mac_ready (* TX AXI-S sink tied off — this is an RX-only top *)
      ; s_axis_tdata = Signal.zero 8
      ; s_axis_tvalid = Signal.gnd
      ; s_axis_tlast = Signal.gnd
      ; s_axis_tuser = Signal.gnd
      ; tx_start = Signal.gnd
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
  Signal.(wire_mac_ready <-- rx_path.m_axis_tready_o);
  { O.app_tdata = rx_path.app_tdata_o
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
  ; udp_busy = rx_path.udp_busy_o
  ; frame_crc_ok = mac.frame_crc_ok
  ; in_payload = mac.in_payload
  ; frame_done = mac.frame_done
  }
;;

let hierarchical ?instance ?(rx_fifo_for_sim = false) scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"udp_rx_mac_top" (create ~rx_fifo_for_sim) i
;;
