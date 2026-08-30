(*
 * Bohdan Purtell
 * University of Florida
 *
 * Module: Udp_loopback_mac_top
 *
 * Echo / loopback full-duplex UDP-over-MAC top (Harness #2 — see
 * UDP_FULL_DUPLEX_HARNESS_PLAN.md). Same one-[Mac_top]/both-stacks skeleton as
 * [Udp_duplex_mac_top], but instead of Phase 0's INDEPENDENT btn TX-stimulus + RX
 * drain, an RX->TX **bridge FSM** feeds the recovered application stream straight
 * back into the UDP TX application interface. A host sends one datagram; the FPGA
 * re-wraps the recovered payload and echoes it back, so RX validation becomes
 * host-asserted (send -> echo -> assert) instead of eyeballing LEDs.
 *
 *     PHY RX ─→ Mac_top.m_axis ─→ Ipv4_rx ─→ Udp_rx ──┐  recovered app stream
 *                  (one Mac_top)                        │  (app_tdata/tvalid/tlast
 *                                                       │   + app_start + payload_length)
 *                                                 RX->TX bridge FSM
 *                                                       │  (tx_start + payload_len
 *                                                       ▼   + payload_tdata/tvalid)
 *     PHY TX ←─ Mac_top.tx_* ←── Ipv4_tx ←── Udp_tx ◀──┘
 *
 * Everything (RX-parse, bridge, TX-build) runs in the tx_clock domain: the MAC
 * captures PHY RX in rx_clock, then its async RX FIFO presents [m_axis] in tx_clock,
 * and the whole TX composition is tx_clock too. So the bridge is single-domain — no
 * new CDC. MII is inherently full-duplex, so simultaneous [tx_en]/[rx_dv] is fine.
 * The MAC's store-and-forward TX gate (frames_buffered, see
 * [[mac-tx-fifo-streaming-limitation]]) buffers the whole re-wrapped frame before it
 * goes on the wire, so the echo latency is store-and-forward.
 *
 * The endpoints line up for free: [Udp_tx]/[Ipv4_tx] emit src_port 0x1234 /
 * dst_port 0x1235, src_ip 192.168.1.10 / dst_ip 192.168.1.1 — identical to
 * udp_app.py's golden constants — so an echoed datagram is wire-shaped exactly like
 * a TX-harness datagram (only the payload differs = whatever was sent).
 *
 * RX policy is forward-everything (drop_on_* = false), so a bad-FCS frame is still
 * parsed and echoed with a freshly REGENERATED FCS: the corrupt payload survives
 * but the outgoing FCS is valid, so the host's payload compare catches it. Gating
 * the echo on the FCS verdict is impossible at [app_start] (the FCS result is only
 * known at frame_done, after the payload has already streamed into Udp_tx), so it is
 * out of scope for first bring-up — see the plan's "bad-FCS policy" open question.
 *)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Ipv4_of_hardcaml

(* ── TX endpoints (mirror Udp_mac_top; agree with udp_app.py golden constants) ── *)
module Udp_cfg = struct
  let src_port = 0x1234
  let dst_port = 0x1235
end

module Ip_cfg = struct
  let src_ip = [ 192; 168; 1; 10 ]
  let dst_ip = [ 192; 168; 1; 1 ]
end

module Tx_path = Udp_ipv4_tx.Make (Udp_cfg) (Ip_cfg)

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

module Rx_path = Udp_ipv4_rx.Make (Ip_rx_cfg) (Udp_rx_cfg)

(* RX->TX bridge FSM (tx_clock) — mirrors the validated TX-harness Idle/Stream/Busy
   driver, but sourced from the recovered RX stream instead of a btn one-shot. *)
module Bridge_states = struct
  type t =
    | Idle
    | Stream
    | Busy
  [@@deriving sexp_of, compare ~localize, enumerate]
end

module I = struct
  type 'a t =
    { (* two clock domains, mirroring Mac_top *)
      rx_clock : 'a
    ; rx_reset : 'a
    ; tx_clock : 'a
    ; tx_reset : 'a
    ; en : 'a
    ; (* PHY RX pins (rx_clock domain) — the ONLY stimulus; the echo is RX-triggered *)
      rx_dv : 'a
    ; rx_er : 'a
    ; rx_data : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* ── TX side (fpga -> laptop): the echo ── *)
      tx_d : 'a [@bits 4]
    ; tx_en : 'a
    ; tx_busy : 'a
    ; tx_udp_busy : 'a (* Udp_tx emitting the echo datagram *)
    ; (* ── bridge status (for LEDs) ── *)
      bridge_active : 'a (* bridge is mid-echo (Stream or Busy) *)
    ; (* ── RX side (laptop -> fpga): recovered UDP payload + metadata (for LEDs) ── *)
      app_tdata : 'a [@bits 8]
    ; app_tvalid : 'a
    ; app_tlast : 'a
    ; app_start : 'a
    ; src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; payload_length : 'a [@bits 16]
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
  let spec_tx = Reg_spec.create ~clock:i.tx_clock ~clear:i.tx_reset () in
  (* ── Wire stubs breaking every combinational loop ─────────────────────────── *)
  (* TX-stack backpressure (mirrors Udp_mac_top) *)
  let wire_mac_tready = Signal.wire 1 in
  (* MAC.s_axis_tready -> Ipv4_tx.mac_tready *)
  (* RX-stack backpressure (mirrors Udp_rx_mac_top) *)
  let wire_mac_rready = Signal.wire 1 in
  (* RX->TX bridge FORWARD path (Udp_rx outputs -> Udp_tx inputs, via the FSM) *)
  let wire_b_start = Signal.wire 1 in
  (* bridge tx_start -> Udp_tx.start *)
  let wire_b_len = Signal.wire 16 in
  (* latched len -> Udp_tx.payload_len *)
  let wire_b_tdata = Signal.wire 8 in
  (* RX app byte -> Udp_tx.payload_tdata *)
  let wire_b_tvalid = Signal.wire 1 in
  (* gated RX valid -> Udp_tx.payload_tvalid *)
  (* RX->TX bridge BACKWARD path (Udp_tx.payload_tready -> Udp_rx.app_tready gate) *)
  let wire_rx_app_tready = Signal.wire 1 in
  let tx_path =
    Tx_path.hierarchical
      ~instance:"udp_ipv4_tx"
      scope
      { Tx_path.I.clock_i = i.tx_clock
      ; reset_i = i.tx_reset
      ; en_i = i.en
      ; start_i = wire_b_start
      ; payload_len_i = wire_b_len
      ; payload_tdata_i = wire_b_tdata
      ; payload_tvalid_i = wire_b_tvalid
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
      ; rx_data = i.rx_data
      ; m_axis_tready = wire_mac_rready
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
      ; app_tready_i = wire_rx_app_tready
      }
  in
  (* ── The RX->TX bridge FSM (the crux) ─────────────────────────────────────── *)
  (* Every input it needs is now live: rx_path.* (forward stream + metadata),
     tx_path.payload_tready_o (backpressure to splice), tx_path.udp_busy_o and mac.tx_busy
     (echo-drained gate). It drives the forward/backward bridge wire stubs. *)
  let sm = Always.State_machine.create (module Bridge_states) ~enable:vdd spec_tx in
  let len_reg = Always.Variable.reg ~enable:vdd ~width:16 spec_tx in
  (* latched payload_length *)
  let started = Always.Variable.reg ~enable:vdd ~width:1 spec_tx in
  (* first byte accepted yet? *)
  let b_tstart = Always.Variable.wire ~default:gnd () in
  let in_stream = sm.is Bridge_states.Stream -- "bridge_in_stream" in
  (* a payload byte is accepted iff we present it valid AND Udp_tx is ready *)
  let accept =
    in_stream &: rx_path.app_tvalid_o &: tx_path.payload_tready_o -- "bridge_accept"
  in
  Always.(
    compile
      [ sm.switch
          [ ( Bridge_states.Idle
            , [ (* app_start is HELD while the first byte is stalled (Udp_rx.first_pend
                   clears only on rx_tvalid & app_tready, and we drive app_tready low
                   here), so latching len + advancing one cycle loses no byte. *)
                when_
                  rx_path.app_start_o
                  [ len_reg <-- rx_path.payload_length_o
                  ; started <--. 0
                  ; sm.set_next Bridge_states.Stream
                  ]
              ] )
          ; ( Bridge_states.Stream
            , [ (* hold tx_start high until the first byte is accepted; Udp_tx latches
                   [{start, payload_len}] on that cycle, exactly as the TX harness does *)
                when_ ~:(started.value) [ b_tstart <-- vdd ]
              ; when_
                  accept
                  [ started <-- vdd
                  ; when_ rx_path.app_tlast_o [ sm.set_next Bridge_states.Busy ]
                  ]
              ] )
          ; ( Bridge_states.Busy
            , [ (* echo fully pushed into Udp_tx AND off the MII pins before re-arming *)
                when_
                  (~:(tx_path.udp_busy_o) &: ~:(mac.tx_busy))
                  [ sm.set_next Bridge_states.Idle ]
              ] )
          ]
      ]);
  let bridge_active = sm.is Bridge_states.Stream |: sm.is Bridge_states.Busy in
  (* ── Close every loop now that all blocks + the FSM exist ──────────────────── *)
  (* backpressure *)
  Signal.(wire_mac_tready <-- mac.s_axis_tready);
  Signal.(wire_mac_rready <-- rx_path.m_axis_tready_o);
  (* bridge forward path (gate the RX stream into Udp_tx only while Streaming) *)
  Signal.(wire_b_start <-- b_tstart.value);
  Signal.(wire_b_len <-- len_reg.value);
  Signal.(wire_b_tdata <-- rx_path.app_tdata_o);
  Signal.(wire_b_tvalid <-- (in_stream &: rx_path.app_tvalid_o));
  (* bridge backward path: splice Udp_tx backpressure to the RX chain (stall it into the
     128-deep MAC RX FIFO whenever Udp_tx isn't ready), only in Stream *)
  Signal.(wire_rx_app_tready <-- (in_stream &: tx_path.payload_tready_o));
  { O.tx_d (* TX side (the echo) *) = mac.tx_d
  ; tx_en = mac.tx_en
  ; tx_busy = mac.tx_busy
  ; tx_udp_busy = tx_path.udp_busy_o
  ; bridge_active (* RX side *)
  ; app_tdata = rx_path.app_tdata_o
  ; app_tvalid = rx_path.app_tvalid_o
  ; app_tlast = rx_path.app_tlast_o
  ; app_start = rx_path.app_start_o
  ; src_port = rx_path.src_port_o
  ; dst_port = rx_path.dst_port_o
  ; payload_length = rx_path.payload_length_o
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
  H.hierarchical ?instance ~scope ~name:"udp_loopback_mac_top" (create ~rx_fifo_for_sim) i
;;
