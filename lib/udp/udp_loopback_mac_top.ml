(*
  Bohdan Purtell
  University of Florida

  Module: Udp_loopback_mac_top

  Echo / loopback full-duplex UDP-over-MAC top (Harness #2 — see
  UDP_FULL_DUPLEX_HARNESS_PLAN.md). Same one-[Mac_top]/both-stacks skeleton as
  [Udp_duplex_mac_top], but instead of Phase 0's INDEPENDENT btn TX-stimulus + RX
  drain, an RX->TX **bridge FSM** feeds the recovered application stream straight
  back into the UDP TX application interface. A host sends one datagram; the FPGA
  re-wraps the recovered payload and echoes it back, so RX validation becomes
  host-asserted (send -> echo -> assert) instead of eyeballing LEDs.

      PHY RX ─→ Mac_top.m_axis ─→ Ipv4_rx ─→ Udp_rx ──┐  recovered app stream
                   (one Mac_top)                        │  (app_tdata/tvalid/tlast
                                                        │   + app_start + payload_length)
                                                  RX->TX bridge FSM
                                                        │  (tx_start + payload_len
                                                        ▼   + payload_tdata/tvalid)
      PHY TX ←─ Mac_top.tx_* ←── Ipv4_tx ←── Udp_tx ◀──┘

  Everything (RX-parse, bridge, TX-build) runs in the tx_clock domain: the MAC
  captures PHY RX in rx_clock, then its async RX FIFO presents [m_axis] in tx_clock,
  and the whole TX composition is tx_clock too. So the bridge is single-domain — no
  new CDC. MII is inherently full-duplex, so simultaneous [tx_en]/[rx_dv] is fine.
  The MAC's store-and-forward TX gate (frames_buffered, see
  [[mac-tx-fifo-streaming-limitation]]) buffers the whole re-wrapped frame before it
  goes on the wire, so the echo latency is store-and-forward.

  The endpoints line up for free: [Udp_tx]/[Ipv4_tx] emit src_port 0x1234 /
  dst_port 0x1235, src_ip 192.168.1.10 / dst_ip 192.168.1.1 — identical to
  udp_app.py's golden constants — so an echoed datagram is wire-shaped exactly like
  a TX-harness datagram (only the payload differs = whatever was sent).

  RX policy is forward-everything (drop_on_* = false), so a bad-FCS frame is still
  parsed and echoed with a freshly REGENERATED FCS: the corrupt payload survives
  but the outgoing FCS is valid, so the host's payload compare catches it. Gating
  the echo on the FCS verdict is impossible at [app_start] (the FCS result is only
  known at frame_done, after the payload has already streamed into Udp_tx), so it is
  out of scope for first bring-up — see the plan's "bad-FCS policy" open question.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Ipv4_of_hardcaml

let () = Stdio.print_endline "=== Imported UDP Loopback + MAC Top ==="

(* ── TX endpoints (mirror Udp_mac_top; agree with udp_app.py golden constants) ── *)
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

(* ── RX policy (mirror Udp_rx_mac_top): forward everything, report status ─────── *)
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

(* RX->TX bridge FSM (tx_clock) — mirrors the validated TX-harness Idle/Stream/Busy
   driver, but sourced from the recovered RX stream instead of a btn one-shot. *)
module Bridge_states = struct
  type t = Idle | Stream | Busy
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module I = struct
  type 'a t = {
    (* two clock domains, mirroring Mac_top *)
    rx_clock : 'a;
    rx_reset : 'a;
    tx_clock : 'a;
    tx_reset : 'a;
    en       : 'a;

    (* PHY RX pins (rx_clock domain) — the ONLY stimulus; the echo is RX-triggered *)
    rx_dv   : 'a;
    rx_er   : 'a;
    rx_data : 'a [@bits 4];
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    (* ── TX side (fpga -> laptop): the echo ── *)
    tx_d        : 'a [@bits 4];
    tx_en       : 'a;
    tx_busy     : 'a;
    tx_udp_busy : 'a;   (* Udp_tx emitting the echo datagram *)

    (* ── bridge status (for LEDs) ── *)
    bridge_active : 'a;  (* bridge is mid-echo (Stream or Busy) *)

    (* ── RX side (laptop -> fpga): recovered UDP payload + metadata (for LEDs) ── *)
    app_tdata  : 'a [@bits 8];
    app_tvalid : 'a;
    app_tlast  : 'a;
    app_start  : 'a;

    src_port       : 'a [@bits 16];
    dst_port       : 'a [@bits 16];
    payload_length : 'a [@bits 16];
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
  let spec_tx = Reg_spec.create ~clock:i.tx_clock ~clear:i.tx_reset () in

  (* ── Wire stubs breaking every combinational loop ─────────────────────────── *)
  (* TX-stack backpressure (mirrors Udp_mac_top) *)
  let wire_mac_tready  = Signal.wire 1 in   (* MAC.s_axis_tready -> Ipv4_tx.mac_tready *)
  let wire_tx_l4_ready = Signal.wire 1 in   (* Ipv4_tx.l4_tready -> Udp_tx.l4_tready   *)
  (* RX-stack backpressure (mirrors Udp_rx_mac_top) *)
  let wire_mac_rready  = Signal.wire 1 in   (* Ipv4_rx.m_axis_tready -> MAC.m_axis_tready *)
  let wire_rx_l4_ready = Signal.wire 1 in   (* Udp_rx.m_axis_tready  -> Ipv4_rx.l4_tready  *)
  (* RX->TX bridge FORWARD path (Udp_rx outputs -> Udp_tx inputs, via the FSM) *)
  let wire_b_start  = Signal.wire 1  in     (* bridge tx_start  -> Udp_tx.start   *)
  let wire_b_len    = Signal.wire 16 in     (* latched len      -> Udp_tx.payload_len *)
  let wire_b_tdata  = Signal.wire 8  in     (* RX app byte      -> Udp_tx.payload_tdata *)
  let wire_b_tvalid = Signal.wire 1  in     (* gated RX valid   -> Udp_tx.payload_tvalid *)
  (* RX->TX bridge BACKWARD path (Udp_tx.payload_tready -> Udp_rx.app_tready gate) *)
  let wire_rx_app_tready = Signal.wire 1 in

  (* ── TX L4: UDP header + app payload, fed from the bridge stubs ───────────── *)
  let udp_tx =
    Udp_txp.create scope
      { Udp_txp.I.clock = i.tx_clock
      ; reset          = i.tx_reset
      ; en             = i.en
      ; start          = wire_b_start
      ; payload_len    = wire_b_len
      ; payload_tdata  = wire_b_tdata
      ; payload_tvalid = wire_b_tvalid
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
      ; m_axis_tready = wire_mac_rready
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
      ; app_tready  = wire_rx_app_tready
      }
  in

  (* ── The RX->TX bridge FSM (the crux) ─────────────────────────────────────── *)
  (* Every input it needs is now live: udp_rx.* (forward stream + metadata),
     udp_tx.payload_tready (backpressure to splice), udp_tx.busy and mac.tx_busy
     (echo-drained gate). It drives the forward/backward bridge wire stubs. *)
  let sm = Always.State_machine.create (module Bridge_states) ~enable:vdd spec_tx in
  let len_reg = Always.Variable.reg ~enable:vdd ~width:16 spec_tx in  (* latched payload_length *)
  let started = Always.Variable.reg ~enable:vdd ~width:1  spec_tx in  (* first byte accepted yet? *)
  let b_tstart = Always.Variable.wire ~default:gnd () in

  let in_stream = sm.is Bridge_states.Stream -- "bridge_in_stream" in
  (* a payload byte is accepted iff we present it valid AND Udp_tx is ready *)
  let accept = in_stream &: udp_rx.m_tvalid &: udp_tx.payload_tready -- "bridge_accept" in

  Always.(compile [
    sm.switch [
      Bridge_states.Idle, [
        (* app_start is HELD while the first byte is stalled (Udp_rx.first_pend
           clears only on rx_tvalid & app_tready, and we drive app_tready low
           here), so latching len + advancing one cycle loses no byte. *)
        when_ (udp_rx.app_start) [
          len_reg <-- udp_rx.payload_length;
          started <--. 0;
          sm.set_next Bridge_states.Stream;
        ];
      ];
      Bridge_states.Stream, [
        (* hold tx_start high until the first byte is accepted; Udp_tx latches
           {start, payload_len} on that cycle, exactly as the TX harness does *)
        when_ (~:(started.value)) [ b_tstart <-- vdd ];
        when_ accept [
          started <-- vdd;
          when_ (udp_rx.m_tlast) [ sm.set_next Bridge_states.Busy ];
        ];
      ];
      Bridge_states.Busy, [
        (* echo fully pushed into Udp_tx AND off the MII pins before re-arming *)
        when_ (~:(udp_tx.busy) &: ~:(mac.tx_busy)) [
          sm.set_next Bridge_states.Idle;
        ];
      ];
    ];
  ]);

  let bridge_active = sm.is Bridge_states.Stream |: sm.is Bridge_states.Busy in

  (* ── Close every loop now that all blocks + the FSM exist ──────────────────── *)
  (* backpressure *)
  Signal.(wire_tx_l4_ready <-- ip_tx.l4_tready);
  Signal.(wire_mac_tready  <-- mac.s_axis_tready);
  Signal.(wire_rx_l4_ready <-- udp_rx.m_axis_tready);
  Signal.(wire_mac_rready  <-- ip_rx.m_axis_tready);
  (* bridge forward path (gate the RX stream into Udp_tx only while Streaming) *)
  Signal.(wire_b_start  <-- b_tstart.value);
  Signal.(wire_b_len    <-- len_reg.value);
  Signal.(wire_b_tdata  <-- udp_rx.m_tdata);
  Signal.(wire_b_tvalid <-- (in_stream &: udp_rx.m_tvalid));
  (* bridge backward path: splice Udp_tx backpressure to the RX chain (stall it
     into the 128-deep MAC RX FIFO whenever Udp_tx isn't ready), only in Stream *)
  Signal.(wire_rx_app_tready <-- (in_stream &: udp_tx.payload_tready));

  (* Hold the frame-level bad-frame verdict at [frame_done] (see Udp_rx_mac_top) *)
  let crc_error_held =
    Signal.reg_fb spec_tx ~width:1 ~enable:udp_rx.frame_done ~f:(fun _ -> udp_rx.frame_error)
    -- "crc_error_held"
  in

  { O.
    (* TX side (the echo) *)
    tx_d          = mac.tx_d
  ; tx_en         = mac.tx_en
  ; tx_busy       = mac.tx_busy
  ; tx_udp_busy   = udp_tx.busy
  ; bridge_active
    (* RX side *)
  ; app_tdata      = udp_rx.m_tdata
  ; app_tvalid     = udp_rx.m_tvalid
  ; app_tlast      = udp_rx.m_tlast
  ; app_start      = udp_rx.app_start
  ; src_port       = udp_rx.src_port
  ; dst_port       = udp_rx.dst_port
  ; payload_length = udp_rx.payload_length
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
