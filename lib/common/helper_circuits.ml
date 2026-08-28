(* Small combinational and register helpers shared across the MII, IPv4 and UDP blocks:
   edge detectors, a delay line, the two composed of both, and three byte-placement
   functions for header builders.

   Both detectors compare the current input against a one-cycle-old copy of it, so each is
   combinational in [x] and registered only in the history. Two consequences fall out of
   that, and both bite consumers rather than this file:

   - The output is a single-cycle pulse, live during the cycle the edge happens, not the
     cycle after. Sample it [before_edge]; at [after_edge] the history register has
     already taken this cycle's value and both detectors read zero unconditionally,
     whatever the input did.
   - The history register lives in the caller's [spec], so the caller's clear zeroes it.
     Out of clear the history reads zero, so an input that is already high looks like a
     rise whether or not it was one.

   The clear also reaches through [delay_by], which is what makes [rising_edge_delayed]
   and [falling_edge_delayed] worth reading twice. A clear landing on the same cycle as a
   detected edge kills the delay register that would have carried it, on that same edge -
   and the two directions then part ways, because the clear has also zeroed the history:

   - A rise whose input stays high is re-detected on the cycle after the clear, so the
     detector emits a two-cycle pulse and the delayed copy arrives exactly once, one cycle
     late. Deferred, not lost.
   - A fall is gone. A low input against a zeroed history is not an edge, so there is no
     second detection and no delayed pulse at all.

   This is what a clear is supposed to do - flush the pipeline - and it is deliberately
   not worked around here. The available workaround would be to build the delay chain on a
   clear-free spec, and that is worse: it would hand the consumer a delayed edge for a
   transition its own reset caused, describing a frame that no longer exists. Callers that
   need the edge across a reset have to hold it themselves. [rx_controller]'s
   [fcs_present] is the delayed consumer in this repo, and [mac_top]'s [frame_end] the
   plain one; the latter is a falling detector, so a clear coincident with the end of a
   frame swallows it. See findings RTL-6, and the goldens in
   [test/common/helper_circuits/].

   [delay_by spec ~n_cycles:0 x] is [x] itself, not a register - the base case of the
   recursion returns its argument, so a caller wiring it straight to a circuit output has
   to put a [wireof] in between.
*)

open! Core
open! Hardcaml
open! Signal
open! Always
open! Variable

(* High when the history register holds one and the input has gone to zero: the cycle the
   input falls, and only that cycle. *)
let falling_edge_detector spec x =
  let x_d = Signal.reg spec x in
  x_d &: ~:x
;;

(* The mirror of the above: the cycle the input goes high against a zero history. *)
let rising_edge_detector spec x =
  let x_d = Signal.reg spec x in
  ~:x_d &: x
;;

let delay_by spec ~n_cycles x =
  let rec loop n acc = if n = 0 then acc else loop (n - 1) (Signal.reg spec acc) in
  loop n_cycles x
;;

let falling_edge_delayed spec ~n_cycles x =
  let fell = falling_edge_detector spec x in
  delay_by spec ~n_cycles fell
;;

let rising_edge_delayed spec ~n_cycles x =
  let rose = rising_edge_detector spec x in
  delay_by spec ~n_cycles rose
;;

let const8 v = of_int_trunc ~width:8 v
let hi16 w = select w ~high:15 ~low:8

(* MSB-first on the wire *)
let lo16 w = select w ~high:7 ~low:0
