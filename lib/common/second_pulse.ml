(* Bohdan Purtell Module: Second_pulse Single-cycle 1 Hz pulse generator. Ported from
   second_pulse.sv.

   [clk_freq] is how many clock cycles make a second, so the pulse fires on one cycle in
   every [clk_freq]. The counter is [ceil_log2 clk_freq] bits wide - 27 at 100 MHz - with
   a floor of one bit. The floor is the whole content of [counter_width] below:
   [ceil_log2 1] is zero, and a zero-width register is not something Hardcaml will build.
   A [clk_freq] of one is legal and degenerate rather than an error - a clock whose period
   is a second makes every cycle a second, and the pulse is high on every one - so the
   floor widens the counter instead of refusing the ratio. Zero and negative have no
   reading at all and are refused by name. See findings RTL-4.
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clk : 'a
    ; rst : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { pulse : 'a
    ; keep : 'a
    }
  [@@deriving hardcaml]
end

(* Elaboration-time check, so a nonsensical frequency fails where it is written rather
   than as a width error from inside Hardcaml. [Int.max 1] is what makes [clk_freq = 1]
   expressible: [Int.ceil_log2 1] is zero, and the bare formula would ask for a zero-width
   counter and surface as ("Width of signals must be >= 0" (width 0)) - a message that
   never mentions [clk_freq] and sends the reader into [Reg_spec]. One extra bit costs
   nothing and the terminal compare against [clk_freq - 1] is still exact. *)
let counter_width clk_freq =
  if clk_freq < 1
  then
    raise_s [%message "Second_pulse.create: clk_freq must be at least 1" (clk_freq : int)];
  Int.max 1 (Int.ceil_log2 clk_freq)
;;

(* [clk_freq] defaults to 100 MHz (Arty A7 system clock). Width is computed at elaboration
   time so no functor is needed. *)
let create ?(clk_freq = 100_000_000) (_scope : Scope.t) (i : _ I.t) : _ O.t =
  let open Always in
  let open Variable in
  let rising_edge = Reg_spec.create ~clock:i.I.clk ~clear:i.I.rst () in
  let width = counter_width clk_freq in
  let cnt = reg ~enable:vdd rising_edge ~width in
  let pulse = reg ~enable:vdd rising_edge ~width:1 in
  compile
    [ pulse <--. 0
    ; if_
        (cnt.value ==: of_int_trunc ~width (clk_freq - 1))
        [ cnt <--. 0; pulse <--. 1 ]
        [ cnt <-- cnt.value +:. 1 ]
    ];
  (* (--) names a signal node in-place (mutation) and returns the same signal. The naming
     causes the signal to appear under that label in VCD waveforms and Waveform.print
     output. ignore suppresses the OCaml "unused value" warning since (--) returns the
     signal for chaining but we don't need the reference here. Without these lines the
     internal counter would still exist in simulation (it is driven by compile above) but
     would appear with an auto-generated hierarchical name rather than the friendly "cnt"
     / "pulse" label. *)
  ignore (cnt.value -- "cnt" : Signal.t);
  ignore (pulse.value -- "pulse" : Signal.t);
  (*
   * reg_fb commentary
   * ─────────────────
   * The counter above could be expressed with Signal.reg_fb instead of the Always DSL:
   *
   *   let cnt = reg_fb rising_edge ~enable:vdd ~width
   *     ~f:(fun q -> mux2 (q ==: of_int_trunc ~width (clk_freq - 1))
   *                        (zero width) (q +:. 1)) in
   *   let pulse = reg rising_edge ~enable:vdd
   *     (cnt ==: of_int_trunc ~width (clk_freq - 1)) in
   *
   * reg_fb makes the feedback arc explicit: the closure maps q (current register
   * output) directly to the next-cycle input, and Hardcaml wires the loop for you.
   * No compile call, no Variable record, no default assignment.
   *
   * Why the Always block is kept here instead:
   * • This module has *two* coupled state elements (cnt + pulse) that share the same
   *   conditional branch.  Expressing them in one compile block mirrors the single
   *   always_ff block in the original SV and keeps the intent—"on max, reset both;
   *   otherwise increment"—in one place rather than two separate reg/reg_fb calls.
   * • The Always DSL supports priority-encoded defaults (pulse <--. 0 at the top,
   *   overridden inside the if_) naturally; reproducing that with reg_fb requires an
   *   explicit mux2 that is no cleaner.
   * • reg_fb shines for single-variable feedback loops (e.g. clk_div's 2-bit counter)
   *   where f captures everything in one tidy closure.  The trade-off flips once you
   *   have interdependent registers or non-trivial branching.
   *)
  { O.pulse = pulse.value; keep = pulse.value }
;;
