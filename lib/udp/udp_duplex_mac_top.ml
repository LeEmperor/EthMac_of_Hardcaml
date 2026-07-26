(*
  Bohdan Purtell
  University of Florida

  Module: Udp_duplex_mac_top

  Full-duplex UDP-over-MAC top — the *union* of [Udp_mac_top] (TX stack) and
  [Udp_rx_mac_top] (RX stack) around ONE [Mac_top]. The two directions are
  INDEPENDENT: this top owns no coupling between them (that is Harness #2's
  RX->TX bridge; see UDP_FULL_DUPLEX_HARNESS_PLAN.md). It is the UDP mirror of the
  bare-MAC [mac_top_validation_harness], which already drives a btn[3] TX burst and
  a 1-byte/sec RX drain side-by-side on a single [Mac_top].

      btn TX app ─→ Udp_tx ─→ Ipv4_tx ─→ Mac_top.s_axis ─→ PHY TX   (fpga -> laptop)
                                           (one Mac_top)
         app out ◀─ Udp_rx ◀─ Ipv4_rx ◀─ Mac_top.m_axis ◀─ PHY RX   (laptop -> fpga)

  Both composition stacks run in the tx_clock domain (the MAC captures PHY RX in
  rx_clock, then its async RX FIFO presents [m_axis] in tx_clock), so the whole
  thing is single-domain apart from the MAC's own internal RX->TX FIFO — no new CDC
  is introduced here. MII is inherently full-duplex (separate TX/RX pins + clocks),
  so simultaneous [tx_en]/[rx_dv] is fine.

  Everything below is lifted verbatim from the two single-direction tops; the only
  merge work is (a) one shared [Mac_top] instead of two, with BOTH [s_axis] (TX)
  and [m_axis] (RX) wired, and (b) disambiguating the two [udp_busy] outputs into
  [tx_udp_busy] / [rx_udp_busy]. Endpoints stay the elaboration-time constants that
  already agree with udp_app.py's golden values (ports 0x1234/0x1235, IPs .10/.1).
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Ipv4_of_hardcaml

let () = Stdio.print_endline "=== Imported UDP Duplex + MAC Top ==="

(* ── TX endpoints (mirror Udp_mac_top) ─────────────────────────────────────── *)
module Udp_cfg = struct
  let src_port = 0x1234
  let dst_port = 0x1235
end

module Ip_cfg = struct
  let src_ip = [ 192; 168; 1; 10 ]
  let dst_ip = [ 192; 168; 1; 1 ]
end

module Udp_txp = Udp_tx.Make (Udp_cfg)
module Ip_txp  = Ipv4_tx.Make (Ip_cfg)

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

module Ip_rxp  = Ipv4_rx.Make (Ip_rx_cfg)
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

    (* UDP application TX side (tx_clock domain) — fpga -> laptop *)
    tx_start       : 'a;
    payload_len    : 'a [@bits 16];
    payload_tdata  : 'a [@bits 8];
    payload_tvalid : 'a;

    (* recovered-UDP RX backpressure (tx_clock domain) — laptop -> fpga *)
    app_tready : 'a;
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    (* ── TX side (fpga -> laptop) ── *)
    tx_d           : 'a [@bits 4];
    tx_en          : 'a;
    tx_busy        : 'a;
    payload_tready : 'a;
    tx_udp_busy    : 'a;   (* Udp_tx emitting a datagram *)

    (* ── RX side (laptop -> fpga): recovered UDP application payload ── *)
    app_tdata  : 'a [@bits 8];
    app_tvalid : 'a;
    app_tlast  : 'a;
    app_tfirst : 'a;
    app_start  : 'a;

    (* per-frame RX metadata (stable from the header parse through the frame) *)
    src_port       : 'a [@bits 16];
    dst_port       : 'a [@bits 16];
    udp_length     : 'a [@bits 16];
    payload_length : 'a [@bits 16];
    udp_checksum   : 'a [@bits 16];  (* raw header field; NOT verified (stub) *)
    src_ip         : 'a [@bits 32];
    dst_ip         : 'a [@bits 32];

    (* per-frame RX status *)
    checksum_ok   : 'a;  (* IPv4 header checksum verified *)
    crc_error     : 'a;  (* held bad-frame verdict, padding-safe (see Udp_rx_mac_top) *)
    rx_frame_done : 'a;  (* tx-domain 1-cycle end-of-frame pulse *)
    ip_busy       : 'a;
    rx_udp_busy   : 'a;  (* Udp_rx mid-datagram *)

    (* MAC RX status passthrough (rx_clock domain) — for the board LED/reg block *)
    frame_crc_ok : 'a;
    in_payload   : 'a;
    frame_done   : 'a;
  } [@@deriving hardcaml]
end

let create ?(rx_fifo_for_sim = false) (scope : Scope.t) (i : _ I.t) : _ O.t =
  (* ── Wire stubs breaking every backpressure combinational loop ───────────── *)
  (* TX stack (mirrors Udp_mac_top) *)
  let wire_mac_tready = Signal.wire 1 in  (* MAC.s_axis_tready -> Ipv4_tx.mac_tready *)
  let wire_tx_l4_ready = Signal.wire 1 in (* Ipv4_tx.l4_tready -> Udp_tx.l4_tready   *)
  (* RX stack (mirrors Udp_rx_mac_top) *)
  let wire_mac_rready = Signal.wire 1 in  (* Ipv4_rx.m_axis_tready -> MAC.m_axis_tready *)
  let wire_rx_l4_ready = Signal.wire 1 in (* Udp_rx.m_axis_tready  -> Ipv4_rx.l4_tready  *)

  (* ── TX L4: UDP header + app payload ─────────────────────────────────────── *)
  let udp_tx =
    Udp_txp.create scope
      { Udp_txp.I.clock = i.tx_clock
      ; reset          = i.tx_reset
      ; en             = i.en
      ; start          = i.tx_start
      ; payload_len    = i.payload_len
      ; payload_tdata  = i.payload_tdata
      ; payload_tvalid = i.payload_tvalid
      ; l4_tready      = wire_tx_l4_ready
      }
  in

  (* ── TX L3: prepend the IPv4 header ──────────────────────────────────────── *)
  let ip_tx =
    Ip_txp.create scope
      { Ip_txp.I.clock = i.tx_clock
      ; reset       = i.tx_reset
      ; en          = i.en
      ; start       = udp_tx.ip_start
      ; l4_length   = udp_tx.l4_length
      ; protocol    = udp_tx.protocol
      ; l4_tdata    = udp_tx.m_tdata
      ; l4_tvalid   = udp_tx.m_tvalid
      ; l4_tlast    = udp_tx.m_tlast
      ; mac_tready  = wire_mac_tready
      }
  in

  (* ── L2: ONE shared MAC, both directions wired ───────────────────────────── *)
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
      (* RX: feed the recovered-payload chain (via the wire stub) *)
      ; m_axis_tready = wire_mac_rready
      (* TX: driven by the IPv4/UDP TX stack *)
      ; s_axis_tdata  = ip_tx.m_tdata
      ; s_axis_tvalid = ip_tx.m_tvalid
      ; s_axis_tlast  = ip_tx.m_tlast
      ; s_axis_tuser  = Signal.gnd
      ; tx_start      = ip_tx.tx_start
      }
  in

  (* ── RX L3: strip the IPv4 header off the Ethernet payload ────────────────── *)
  let ip_rx =
    Ip_rxp.create (Scope.sub_scope scope "ipv4_rx")
      { Ip_rxp.I.clock = i.tx_clock
      ; reset       = i.tx_reset
      ; en          = i.en
      ; rx_tdata    = mac.m_axis_tdata
      ; rx_tvalid   = mac.m_axis_tvalid
      ; rx_tlast    = mac.m_axis_tlast
      ; rx_tuser    = mac.m_axis_tuser
      ; rx_tfirst   = mac.m_axis_tfirst
      ; rx_eth_type = mac.rx_eth_type
      ; l4_tready   = wire_rx_l4_ready
      }
  in

  (* ── RX L4: strip the UDP header, emit application payload ────────────────── *)
  let udp_rx =
    Udp_rxp.create (Scope.sub_scope scope "udp_rx")
      { Udp_rxp.I.clock = i.tx_clock
      ; reset       = i.tx_reset
      ; en          = i.en
      ; rx_tdata    = ip_rx.m_tdata
      ; rx_tvalid   = ip_rx.m_tvalid
      ; rx_tlast    = ip_rx.m_tlast
      ; rx_tuser    = ip_rx.crc_error
      ; rx_tfirst   = ip_rx.m_tfirst
      ; ip_protocol = ip_rx.protocol
      ; ip_src_ip   = ip_rx.src_ip
      ; ip_dst_ip   = ip_rx.dst_ip
      ; ip_frame_done  = ip_rx.frame_done
      ; ip_frame_error = ip_rx.frame_error
      ; app_tready  = i.app_tready
      }
  in

  (* close every backpressure loop now that all blocks exist *)
  Signal.(wire_tx_l4_ready <-- ip_tx.l4_tready);
  Signal.(wire_mac_tready  <-- mac.s_axis_tready);
  Signal.(wire_rx_l4_ready <-- udp_rx.m_axis_tready);
  Signal.(wire_mac_rready  <-- ip_rx.m_axis_tready);

  (* Hold the frame-level bad-frame verdict at [frame_done] (see Udp_rx_mac_top) *)
  let spec_tx = Reg_spec.create ~clock:i.tx_clock ~clear:i.tx_reset () in
  let crc_error_held =
    Signal.reg_fb spec_tx ~width:1 ~enable:udp_rx.frame_done ~f:(fun _ -> udp_rx.frame_error)
    -- "crc_error_held"
  in

  { O.
    (* TX side *)
    tx_d           = mac.tx_d
  ; tx_en          = mac.tx_en
  ; tx_busy        = mac.tx_busy
  ; payload_tready = udp_tx.payload_tready
  ; tx_udp_busy    = udp_tx.busy
    (* RX side *)
  ; app_tdata      = udp_rx.m_tdata
  ; app_tvalid     = udp_rx.m_tvalid
  ; app_tlast      = udp_rx.m_tlast
  ; app_tfirst     = udp_rx.m_tfirst
  ; app_start      = udp_rx.app_start
  ; src_port       = udp_rx.src_port
  ; dst_port       = udp_rx.dst_port
  ; udp_length     = udp_rx.udp_length
  ; payload_length = udp_rx.payload_length
  ; udp_checksum   = udp_rx.udp_checksum
  ; src_ip         = udp_rx.src_ip
  ; dst_ip         = udp_rx.dst_ip
  ; checksum_ok    = ip_rx.checksum_ok
  ; crc_error      = crc_error_held
  ; rx_frame_done  = udp_rx.frame_done
  ; ip_busy        = ip_rx.busy
  ; rx_udp_busy    = udp_rx.busy
    (* MAC RX status passthrough *)
  ; frame_crc_ok   = mac.frame_crc_ok
  ; in_payload     = mac.in_payload
  ; frame_done     = mac.frame_done
  }
;;
