(*
  Bohdan Purtell
  University of Florida

  Module: Udp_rx_mac_top

  Composition layer: the *receive* mirror of [Udp_mac_top]. Stacks the IPv4 (L3)
  and UDP (L4) receive parsers on top of the MII Ethernet MAC (L2), so the wiring
  stays one-directional:

      PHY ─→ Mac_top.m_axis ─(Eth payload)→ Ipv4_rx ─(UDP datagram)→ Udp_rx ─→ app

  Each layer only knows the one below it:
    - The MAC hands up the Ethernet *payload* byte stream (m_axis) plus two
      sidebands the parsers need: [m_axis_tfirst] (SOF pulse on payload byte 0)
      and [rx_eth_type] (latched Ethernet type, stable per frame).
    - Ipv4_rx strips the 20-byte IPv4 header, verifies the header checksum, drops
      MAC zero-padding, and hands up the L4 datagram + {protocol, src/dst ip}.
    - Udp_rx strips the 8-byte UDP header and hands up the application payload +
      {src/dst port, lengths}.

  Backpressure flows the other way through two wire stubs that break the
  combinational loops: Udp_rx.m_axis_tready → Ipv4_rx.l4_tready, and
  Ipv4_rx.m_axis_tready → MAC.m_axis_tready. The application's [app_tready] gates
  the whole chain from the top.

  This is a dedicated RX-only top: the MAC's TX AXI-S sink is tied off (no frames
  are transmitted). It is the natural DUT for isolated RX bring-up — nothing on
  the TX side can interfere with the receive experiment. (The already-validated
  TX composition lives untouched in [Udp_mac_top]; a full-duplex top can merge the
  two later.)

  Clock domains: the MAC captures PHY data in [rx_clock], then its async RX FIFO
  presents [m_axis] in [tx_clock]. Ipv4_rx, Udp_rx, and the application RX stream
  therefore run in [tx_clock]. (The [rx_eth_type] sideband is currently sampled
  across the boundary combinationally — a CDC-hardening TODO carried over from
  Mac_top; inert in single-clock simulation with [rx_fifo_for_sim].)

  RX policy is fixed for first bring-up (no dropping): every well-formed IPv4/UDP
  frame is forwarded and its status reported. Flip the [Ip_rx_cfg]/[Udp_rx_cfg]
  knobs below to enforce checksum / port filtering.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Ipv4_of_hardcaml

let () = Stdio.print_endline "=== Imported UDP RX + MAC Top ==="

(* Bring-up RX policy: forward everything, just report status. The expected dst
   port mirrors [Udp_mac_top]'s TX dst_port (0x1235) so a loopback agrees. *)
module Ip_rx_cfg = struct
  let drop_on_bad_checksum = false
  let debug = false
end

module Udp_rx_cfg = struct
  let drop_on_port_mismatch = false
  let expected_dst_port = 0x1235
  let debug = false
end

module Ip_rx  = Ipv4_rx.Make (Ip_rx_cfg)
module Udp_rxp = Udp_rx.Make (Udp_rx_cfg)

module I = struct
  type 'a t = {
    (* two clock domains, mirroring Mac_top *)
    rx_clock : 'a;
    rx_reset : 'a;
    tx_clock : 'a;
    tx_reset : 'a;
    en       : 'a;

    (* PHY RX pins (rx_clock domain) *)
    rx_dv   : 'a;
    rx_er   : 'a;
    rx_data : 'a [@bits 4];

    (* application backpressure for the recovered UDP payload (tx_clock domain) *)
    app_tready : 'a;
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    (* recovered UDP application payload stream (tx_clock domain) *)
    app_tdata  : 'a [@bits 8];
    app_tvalid : 'a;
    app_tlast  : 'a;
    app_tfirst : 'a;   (* SOF pulse on the first application byte *)
    app_start  : 'a;   (* latch metadata here (pulses with app_tfirst) *)

    (* per-frame metadata (stable from the header parse through the frame) *)
    src_port       : 'a [@bits 16];
    dst_port       : 'a [@bits 16];
    udp_length     : 'a [@bits 16];
    payload_length : 'a [@bits 16];
    udp_checksum   : 'a [@bits 16];  (* raw header field; NOT verified (stub) *)
    src_ip         : 'a [@bits 32];
    dst_ip         : 'a [@bits 32];

    (* per-frame status *)
    checksum_ok : 'a;  (* IPv4 header checksum verified *)
    crc_error     : 'a;  (* app-visible bad-frame verdict: held level, latched from the
                            frame-level late-status channel at [rx_frame_done]. Correct
                            even when MAC padding delays the FCS verdict past the
                            payload tlast (unlike the layers' tlast-aligned flags). *)
    rx_frame_done : 'a;  (* tx-domain 1-cycle end-of-frame pulse (aligned to m_axis);
                            [crc_error] is refreshed on this edge *)
    ip_busy       : 'a;
    udp_busy    : 'a;

    (* MAC RX status passthrough (rx_clock domain) — surfaced so a board harness
       can reuse the MAC validation regs/LED status block, same as Udp_mac_top. *)
    frame_crc_ok : 'a;
    in_payload   : 'a;
    frame_done   : 'a;
  } [@@deriving hardcaml]
end

let create ?(rx_fifo_for_sim = false) (scope : Scope.t) (i : _ I.t) : _ O.t =
  (* break the two backpressure combinational loops with wire stubs *)
  let wire_mac_ready = Signal.wire 1 in  (* Ipv4_rx.m_axis_tready -> MAC.m_axis_tready *)
  let wire_l4_ready  = Signal.wire 1 in  (* Udp_rx.m_axis_tready  -> Ipv4_rx.l4_tready  *)

  (* L2: Ethernet framing + FCS check. ethertype only matters for TX framing
     (tied off here); RX filtering keys off the latched rx_eth_type instead. *)
  let mac =
    Mac_top.create ~rx_fifo_for_sim ~ethertype:0x0800 scope
      { Mac_top.I.rx_clock = i.rx_clock
      ; rx_reset      = i.rx_reset
      ; tx_clock      = i.tx_clock
      ; tx_reset      = i.tx_reset
      ; en            = i.en
      ; rx_dv         = i.rx_dv
      ; rx_er         = i.rx_er
      ; rx_data       = i.rx_data
      ; m_axis_tready = wire_mac_ready
      (* TX AXI-S sink tied off — this is an RX-only top *)
      ; s_axis_tdata  = Signal.zero 8
      ; s_axis_tvalid = Signal.gnd
      ; s_axis_tlast  = Signal.gnd
      ; s_axis_tuser  = Signal.gnd
      ; tx_start      = Signal.gnd
      }
  in

  (* L3: strip the IPv4 header off the Ethernet payload. *)
  let ip =
    Ip_rx.create (Scope.sub_scope scope "ipv4_rx")
      { Ip_rx.I.clock = i.tx_clock
      ; reset       = i.tx_reset
      ; en          = i.en
      ; rx_tdata    = mac.m_axis_tdata
      ; rx_tvalid   = mac.m_axis_tvalid
      ; rx_tlast    = mac.m_axis_tlast
      ; rx_tuser    = mac.m_axis_tuser
      ; rx_tfirst   = mac.m_axis_tfirst
      ; rx_eth_type = mac.rx_eth_type
      ; l4_tready   = wire_l4_ready
      }
  in

  (* L4: strip the UDP header off the datagram, emit application payload. *)
  let udp =
    Udp_rxp.create (Scope.sub_scope scope "udp_rx")
      { Udp_rxp.I.clock = i.tx_clock
      ; reset       = i.tx_reset
      ; en          = i.en
      ; rx_tdata    = ip.m_tdata
      ; rx_tvalid   = ip.m_tvalid
      ; rx_tlast    = ip.m_tlast
      ; rx_tuser    = ip.crc_error
      ; rx_tfirst   = ip.m_tfirst
      ; ip_protocol = ip.protocol
      ; ip_src_ip   = ip.src_ip
      ; ip_dst_ip   = ip.dst_ip
      ; ip_frame_done  = ip.frame_done
      ; ip_frame_error = ip.frame_error
      ; app_tready  = i.app_tready
      }
  in

  (* close the backpressure loops now that all three blocks exist *)
  Signal.(wire_l4_ready  <-- udp.m_axis_tready);
  Signal.(wire_mac_ready <-- ip.m_axis_tready);

  (* Latch the frame-level bad-frame verdict at [frame_done] and hold it (like the
     MAC's own frame_crc_ok) so the application can sample it any time after the
     datagram drains — the fix for FCS status being dropped at the L3->L4 tlast.
     tx_clock domain: udp.frame_done rides mac.m_axis, presented in tx_clock. *)
  let spec_tx = Reg_spec.create ~clock:i.tx_clock ~clear:i.tx_reset () in
  let crc_error_held =
    Signal.reg_fb spec_tx ~width:1 ~enable:udp.frame_done ~f:(fun _ -> udp.frame_error)
    -- "crc_error_held"
  in

  { O.app_tdata   = udp.m_tdata
  ; app_tvalid    = udp.m_tvalid
  ; app_tlast     = udp.m_tlast
  ; app_tfirst    = udp.m_tfirst
  ; app_start     = udp.app_start
  ; src_port      = udp.src_port
  ; dst_port      = udp.dst_port
  ; udp_length    = udp.udp_length
  ; payload_length = udp.payload_length
  ; udp_checksum  = udp.udp_checksum
  ; src_ip        = udp.src_ip
  ; dst_ip        = udp.dst_ip
  ; checksum_ok   = ip.checksum_ok
  ; crc_error     = crc_error_held
  ; rx_frame_done = udp.frame_done
  ; ip_busy       = ip.busy
  ; udp_busy      = udp.busy
  ; frame_crc_ok  = mac.frame_crc_ok
  ; in_payload    = mac.in_payload
  ; frame_done    = mac.frame_done
  }
;;
