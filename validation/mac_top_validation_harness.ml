(*
 * Module: Mac_top_validation_harness
 * Board-level validation harness for the full-duplex MII MAC on the Arty A7-100T.
 *
 * This is the *active* top used for hardware bring-up. It lives outside lib/ on
 * purpose: lib/ holds the reusable MAC; this dir holds the throwaway scaffolding
 * that drives it on real silicon. It reuses Arty_board_top.I/O (the canonical pin
 * contract in lib/common) as its port interface and supplies its own [create].
 *
 * Structure
 * ─────────
 *   clk100mhz domain:
 *     Clk_div       → eth_ref_clk (25 MHz to PHY XI)
 *     phy_rst_cnt   → eth_rstn    (hold PHY in reset ~0.66 ms after power-on)
 *     Second_pulse  → heartbeat   → led0 toggle (0.5 Hz, eye-visible)
 *
 *   eth_tx_clk domain (MAC + everything it touches — TX data is registered on the
 *   same edge the PHY samples, per the DP83848 MII TX timing):
 *     TX trigger FSM→ one btn[3] press burst-fills the TX FIFO with a 46-byte
 *                     payload, then pulses tx_start (fill + transmit, atomic)
 *     Mac_top       → RX (PHY→AXI-S) + TX (AXI-S→PHY)
 *     Second_pulse  → 1 Hz RX drain (m_axis_tready)
 *     Regs (stub)   → AXI4-Lite status/control block, taps MAC status for readback
 *
 * Controls: btn[0] = active-high reset (raw async button, synchronized into each
 *                    clock domain), sw[0] = enable,
 *           btn[3] = one-shot "fill FIFO then transmit" trigger.
 *
 * LED map:
 *   led[3:0]  lower nibble of last drained RX byte
 *   led0_r    heartbeat toggle
 *   led1_r    tx_busy (TX frame in flight)       led1_g  phy_ready
 *   led2_g    last-frame CRC ok                  led2_b  in_payload (RX active)
 *   led3_r    last-frame CRC bad
 *)

open! Core
open! Hardcaml
open! Mii_of_hardcaml (* Mac_top lives in the wrapped mii library *)
open! Signal

(*
 * One-shot TX trigger FSM (see the sequencer block in [create]):
 * Idle  wait for a debounced/edged btn[3] press
 * Fill  burst-write the payload into the TX FIFO, one byte per cycle
 * Fire  pulse tx_start (whole payload now staged)
 * Busy  hold until the MAC reports tx_busy low, then re-arm
 *)
module Tx_trigger_states = struct
  type t =
    | Idle
    | Fill
    | Fire
    | Busy
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* Reuse the canonical board pin contract as the harness port interface. *)
module I = Arty_board_top.I
module O = Arty_board_top.O

let create (scope : Scope.t) (i : _ I.t) : _ O.t =
  let rst = Signal.bit i.I.btn ~pos:0 -- "rst" in
  (* btn[0]: raw async reset button *)
  let en = Signal.bit i.I.sw ~pos:0 -- "en" in
  (* sw[0]: active-high enable *)

  (* Per-domain reset synchronizers (see Board_scaffolding.reset_sync): one 2-FF chain per
     clock domain, fed from the raw async btn[0]. *)
  let sys_rst =
    Board_scaffolding.reset_sync ~clock:i.I.clk100mhz ~async_rst:rst -- "sys_rst"
  in
  let rx_rst =
    Board_scaffolding.reset_sync ~clock:i.I.eth_rx_clk ~async_rst:rst -- "rx_rst"
  in
  let tx_rst =
    Board_scaffolding.reset_sync ~clock:i.I.eth_tx_clk ~async_rst:rst -- "tx_rst"
  in
  let spec100 = Reg_spec.create ~clock:i.I.clk100mhz ~clear:sys_rst () in
  let spec_tx = Reg_spec.create ~clock:i.I.eth_tx_clk ~clear:tx_rst () in
  (* ==== 25 MHz reference clock to the PHY ==== *)
  let clk_div_inst =
    Board_scaffolding.eth_ref_clk ~scope ~clk100mhz:i.I.clk100mhz ~sys_rst ~en
  in
  (* ==== PHY hard reset: hold eth_rstn low ~0.66 ms, then release and hold high ==== *)
  let phy = Board_scaffolding.phy_hard_reset ~spec100 ~sys_rst in
  let phy_ready = phy.ready in
  (* ==== Heartbeat: 1 Hz pulse toggled into a 0.5 Hz square wave on led0 ==== *)
  let heartbeat =
    Board_scaffolding.heartbeat ~scope ~clk100mhz:i.I.clk100mhz ~sys_rst ~spec100
  in
  let heartbeat_toggle = heartbeat.toggle in
  (* ==== One-shot button-triggered TX ==== *)
  (* A single btn[3] press stages a full payload into the TX FIFO and then fires tx_start
     — the entire "fill + transmit" sequence from one press. The FSM runs in the
     eth_tx_clk domain so its FIFO writes are coherent with the MAC's TX FIFO write clock.
     btn[3] is synchronized in and reduced to a one-shot rising edge, so one press =
     exactly one frame; the FSM re-arms only after the MAC reports tx_busy low.

     payload_len MUST be >= 46 (minimum Ethernet payload). Below that the TX controller
     stalls in Payload waiting on FIFO data that never comes — the exact hang
     exercised/avoided in tx_path_tb.ml. *)
  let payload_len = 46 in
  let btn3 = Signal.bit i.I.btn ~pos:3 -- "btn3" in
  (* wire-back stubs break the FSM <-> MAC instantiation cycle *)
  let wire_tx_busy = Signal.wire 1 -- "wire_tx_busy" in
  let wire_tx_ready = Signal.wire 1 -- "wire_tx_ready" in
  (* 2-FF sync btn[3] into the tx-clk domain, then take a one-shot rising edge *)
  let btn3_tx = Signal.reg spec_tx (Signal.reg spec_tx btn3) -- "btn3_tx" in
  let btn3_edge = btn3_tx &: ~:(Signal.reg spec_tx btn3_tx) -- "btn3_edge" in
  (* phy_ready is a quasi-static level from the 100 MHz domain; 2-FF sync it *)
  let phy_ready_tx =
    Signal.reg spec_tx (Signal.reg spec_tx phy_ready) -- "dbg_phy_ready_tx"
  in
  let sm = Always.State_machine.create (module Tx_trigger_states) ~enable:vdd spec_tx in
  let fill_count = Always.Variable.reg ~enable:vdd ~width:6 spec_tx in
  let tx_tvalid = Always.Variable.wire ~default:gnd () in
  let tx_tlast = Always.Variable.wire ~default:gnd () in
  let tx_tstart = Always.Variable.wire ~default:gnd () in
  (* payload bytes: incrementing 0x01, 0x02, … 0x2E (mirrors tx_path_tb.ml) *)
  let fill_byte = uresize (fill_count.value +:. 1) ~width:8 -- "fill_byte" in
  Always.(
    compile
      [ sm.switch
          [ ( Tx_trigger_states.Idle
            , [ when_
                  (btn3_edge &: phy_ready_tx)
                  [ fill_count <--. 0; sm.set_next Tx_trigger_states.Fill ]
              ] )
          ; ( Tx_trigger_states.Fill
            , [ (* write one byte whenever the FIFO can accept it; tvalid and the count
                   advance together so each byte is written exactly once *)
                when_
                  wire_tx_ready
                  [ tx_tvalid <-- vdd
                  ; if_
                      (fill_count.value ==:. payload_len - 1)
                      [ tx_tlast <-- vdd
                      ; (* mark the final payload byte for the length-driven TX FSM *)
                        sm.set_next Tx_trigger_states.Fire
                      ]
                      [ fill_count <-- fill_count.value +:. 1 ]
                  ]
              ] )
          ; ( Tx_trigger_states.Fire
            , [ (* whole payload staged — kick off transmission *)
                tx_tstart <-- vdd
              ; sm.set_next Tx_trigger_states.Busy
              ] )
          ; ( Tx_trigger_states.Busy
            , [ when_ ~:wire_tx_busy [ sm.set_next Tx_trigger_states.Idle ] ] )
          ]
      ]);
  (* ── RX drain: pop one byte per second, show low nibble on the plain LEDs ─── *)
  let rx_drain = Board_scaffolding.rx_drain ~scope ~clock:i.I.eth_tx_clk ~reset:tx_rst in
  (* ── The MAC ─────────────────────────────────────────────────────────────── *)
  (* Two clock domains now: RX ingest on eth_rx_clk (source-synchronous to the PHY RX
     data), TX path on eth_tx_clk. Both resets are btn[0] for now — proper per-domain
     reset synchronizers land in a later step. *)
  let mac_inst =
    Mac_top.create
      scope
      { Mac_top.I.rx_clock = i.I.eth_rx_clk
      ; rx_reset = rx_rst
      ; tx_clock = i.I.eth_tx_clk
      ; tx_reset = tx_rst
      ; en
      ; rx_dv = i.I.eth_rx_dv
      ; rx_er = i.I.eth_rxerr
      ; rx_data = i.I.eth_rxd
      ; m_axis_tready = rx_drain.pulse
      ; s_axis_tdata = fill_byte
      ; s_axis_tvalid = tx_tvalid.value
      ; s_axis_tlast = tx_tlast.value
      ; s_axis_tuser = gnd
      ; tx_start = tx_tstart.value
      }
  in
  (* close the wire-back loop now that the MAC exists *)
  Signal.(wire_tx_busy <-- mac_inst.tx_busy);
  Signal.(wire_tx_ready <-- mac_inst.s_axis_tready);
  (* ── RX-status CDC into the tx_clk domain ──────────────────────────────────
     frame_crc_ok / in_payload / frame_done come out of the MAC in the rx_clk domain;
     everything that reads them here (regs_inst, LEDs) lives in tx_clk. Levels get a plain
     2-FF synchronizer; frame_done is a 1-cycle pulse, so it gets a toggle-based pulse
     synchronizer (a bare 2-FF would drop or stretch a single-cycle pulse across the
     crossing). *)
  let spec_rx = Reg_spec.create ~clock:i.I.eth_rx_clk ~clear:rx_rst () in
  let sync2_tx x = Board_scaffolding.sync2 ~spec:spec_tx x in
  let pulse_sync_tx p =
    Board_scaffolding.pulse_sync ~src_spec:spec_rx ~dst_spec:spec_tx p
  in
  let frame_crc_ok_tx = sync2_tx mac_inst.frame_crc_ok -- "frame_crc_ok_tx" in
  let in_payload_tx = sync2_tx mac_inst.in_payload -- "in_payload_tx" in
  let frame_done_tx = pulse_sync_tx mac_inst.frame_done -- "frame_done_tx" in
  (* ── Register block (STUB) ───────────────────────────────────────────────── *)
  (* AXI4-Lite slave tied off for now — no on-board master yet. It taps MAC status for
     eventual processor readback. When control feedback (soft_reset / tx_start_req) is
     wired back into the MAC, use the mac_top.ml wire-back pattern (Signal.wire stubs) to
     break the ordering/comb loop. *)
  let regs_inst =
    Mac_top_validation_harness_regs.create
      scope
      { Mac_top_validation_harness_regs.I.clock = i.I.eth_tx_clk
      ; reset = tx_rst
      ; s_axi_awaddr = zero 4
      ; s_axi_awvalid = gnd
      ; s_axi_wdata = zero 32
      ; s_axi_wstrb = zero 4
      ; s_axi_wvalid = gnd
      ; s_axi_bready = gnd
      ; s_axi_araddr = zero 4
      ; s_axi_arvalid = gnd
      ; s_axi_rready = gnd
      ; frame_done = frame_done_tx
      ; (* CDC-synchronized into tx_clk *)
        frame_crc_ok = frame_crc_ok_tx
      ; in_payload = in_payload_tx
      ; tx_en = mac_inst.tx_en
      ; (* already tx_clk domain *)
        rx_dv = i.I.eth_rx_dv
      }
  in
  (* ── Last drained RX byte → led[3:0] ─────────────────────────────────────── *)
  let last_byte =
    Signal.reg
      spec_tx
      ~enable:(rx_drain.pulse &: mac_inst.m_axis_tvalid)
      mac_inst.m_axis_tdata
  in
  (* ── Consume the AXI-S sideband (tlast/tuser) ────────────────────────────── These
     outputs come off the async-FIFO read port. If nothing downstream reads them, Vivado
     partially trims the FIFO's distributed RAM and leaves the read-address (DPRA) nets on
     the top RAM bit driverless — the "Route 35-13 Driverless net ram_reg_0_63_9_9/DPRA*"
     DRC failure. Latching tuser (CRC error) at tlast keeps those read bits connected to a
     real output pin, and doubles as a live CRC-error indicator from the drained stream. *)
  let rx_axis_crc_err =
    Signal.reg_fb
      spec_tx
      ~width:1
      ~enable:(rx_drain.pulse &: mac_inst.m_axis_tvalid &: mac_inst.m_axis_tlast)
      ~f:(fun _q -> mac_inst.m_axis_tuser)
    -- "rx_axis_crc_err"
  in
  (* keep debug OR-reductions alive so synthesis doesn't prune the submodules *)
  ignore (heartbeat.keep : Signal.t);
  ignore (rx_drain.keep : Signal.t);
  ignore (mac_inst.keep : Signal.t);
  ignore (regs_inst.keep : Signal.t);
  { O.led = Signal.select last_byte ~high:3 ~low:0
  ; led0_r = heartbeat_toggle
  ; led0_g = gnd
  ; led0_b = gnd
  ; led1_r = wire_tx_busy
  ; (* TX frame in flight (btn[3]-triggered) *)
    led1_g = phy_ready
  ; (* PHY out of hard reset *)
    led1_b = gnd
  ; led2_r = rx_axis_crc_err
  ; (* CRC error latched from AXI-S tuser@tlast (also anti-prunes FIFO sideband) *)
    led2_g = frame_crc_ok_tx
  ; led2_b = in_payload_tx
  ; led3_r = frame_done_tx &: ~:frame_crc_ok_tx
  ; led3_g = gnd
  ; led3_b = gnd
  ; uart_rxd_out = vdd
  ; eth_mdc = gnd
  ; eth_rstn = Signal.msb phy.cnt
  ; eth_ref_clk = clk_div_inst.dst_clk
  ; eth_tx_en = mac_inst.tx_en
  ; eth_txd = mac_inst.tx_d
  }
;;
