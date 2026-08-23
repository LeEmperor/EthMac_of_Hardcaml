(* Module: Board_scaffolding

   Shared Arty A7-100T board plumbing for the validation harnesses
   (Mac_top_validation_harness, Udp_mac_top_validation_harness). This is the
   domain-agnostic scaffolding every board bring-up top needs — per-domain reset
   synchronizers, the 25 MHz PHY reference clock, PHY hard-reset sequencing, the heartbeat
   LED, the RX-drain pulse, and clock-domain-crossing helpers — factored out so each
   harness only supplies its own stimulus FSM, the core it wraps, and its LED map.

   These are deliberately plain helper functions, NOT a Hardcaml sub-module: they build
   signals directly into the caller's circuit (no I/O record, no hierarchy boundary), so
   the caller keeps full control over signal-creation order and the emitted RTL is
   identical to hand-inlined plumbing. Call each helper at the point the equivalent inline
   code used to sit.
*)

open! Core
open! Hardcaml
open! Signal

(* Per-domain reset synchronizer. btn[0] is a raw asynchronous input, so drop it through a
   2-FF chain in the target clock domain. Async-assert (the FFs reset to 1 the instant
   [async_rst] goes high) and sync-deassert (0 shifts in, so reset releases 2 edges after
   [async_rst] releases). Gives clean per-domain reset recovery instead of feeding one
   async button into several clocks. *)
let reset_sync ~clock ~async_rst =
  let spec = Reg_spec.create ~clock ~reset:async_rst () in
  let ff0 = Signal.reg spec ~reset_to:(Bits.one 1) Signal.gnd in
  Signal.reg spec ~reset_to:(Bits.one 1) ff0
;;

(* 25 MHz reference clock to the PHY XI pin. Crude fabric divider — jitter is ugly but
   irrelevant at MII speeds. Returns the full Clk_div output; the caller drives
   eth_ref_clk from [.dst_clk]. *)
let eth_ref_clk ~scope ~clk100mhz ~sys_rst ~en =
  Clk_div.create scope { Clk_div.I.src_clk = clk100mhz; rst = sys_rst; en }
;;

module Phy_reset = struct
  type t =
    { cnt : Signal.t (* 17-bit saturating counter; MSB stuck at 1 once released *)
    ; ready : Signal.t (* = MSB(cnt): PHY is out of its ~0.66 ms hard reset *)
    }
end

(* PHY hard reset: hold eth_rstn low ~0.66 ms after power-on, then release and hold high.
   Drive eth_rstn from [MSB cnt] in the caller's output record; use [ready] as the "PHY
   up" status level. *)
let phy_hard_reset ~spec100 ~sys_rst =
  let cnt =
    Signal.reg_fb spec100 ~enable:vdd ~width:17 ~f:(fun q ->
      mux2 sys_rst (zero 17) (mux2 (msb q) q (q +:. 1)))
    -- "phy_rst_cnt"
  in
  { Phy_reset.cnt; ready = Signal.msb cnt -- "dbg_phy_ready" }
;;

module Heartbeat = struct
  type t =
    { toggle : Signal.t (* 0.5 Hz square wave for an eye-visible LED blink *)
    ; keep : Signal.t (* Second_pulse debug OR-reduction, forward for anti-prune *)
    }
end

(* 1 Hz heartbeat pulse toggled into a 0.5 Hz square wave. *)
let heartbeat ~scope ~clk100mhz ~sys_rst ~spec100 =
  let hb = Second_pulse.create scope { Second_pulse.I.clk = clk100mhz; rst = sys_rst } in
  let toggle =
    Signal.reg_fb spec100 ~enable:hb.pulse ~width:1 ~f:(fun q -> ~:q)
    -- "heartbeat_toggle"
  in
  { Heartbeat.toggle; keep = hb.keep }
;;

(* 1 Hz RX-drain pulse in the given (tx-side) domain: pops one RX byte per second so the
   drained value is eye-visible on the LEDs. Returns the Second_pulse output (use [.pulse]
   to gate m_axis_tready, [.keep] for anti-prune). *)
let rx_drain ~scope ~clock ~reset =
  Second_pulse.create
    ~clk_freq:25_000_000
    scope
    { Second_pulse.I.clk = clock; rst = reset }
;;

(* Plain 2-FF level synchronizer into [spec]'s domain. *)
let sync2 ~spec x = Signal.reg spec (Signal.reg spec x)

(* Toggle-based pulse synchronizer: a 1-cycle pulse in [src_spec]'s domain becomes a
   1-cycle pulse in [dst_spec]'s domain. A bare 2-FF level sync would drop or stretch a
   single-cycle pulse across the crossing, so convert to a toggle in the source domain,
   2-FF it across, and edge-detect on the far side. *)
let pulse_sync ~src_spec ~dst_spec src_pulse =
  let tog =
    Signal.reg_fb src_spec ~enable:vdd ~width:1 ~f:(fun q -> mux2 src_pulse ~:q q)
  in
  let tog_dst = Signal.reg dst_spec (Signal.reg dst_spec tog) in
  tog_dst ^: Signal.reg dst_spec tog_dst
;;
