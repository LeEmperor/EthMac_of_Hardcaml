(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "clk_div_testbench.ml" *)

(* Testbench Support: Clk_div

   Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit,
   Quickcheck, and expect test suites.

   The block is a free-running counter whose MSB is the divided clock, so the whole of its
   behavior is "how many enabled cycles have elapsed since the last clear", and
   [expected_trace] below is that statement written as a fold. Everything in the two test
   files is checked against it.

   Two axes, not one. [Clk_div.create] takes [?divisor] (RTL-3), so the suite is a
   [Make_testbench] functor over the ratio in the same shape as [second_pulse]'s over
   [?clk_freq], and is instantiated at 2, 4, 8, 16 and 32. The enable schedule is the
   other axis and the sharper of the two: a free-running period measurement cannot
   distinguish a counter that occasionally drops or double-counts an enable, and the
   schedule property can, because the model tracks the count rather than the phase. The
   optional argument also means [include Dut] will not satisfy [Sim_fixture.S], so
   [create] is written out.

   Ratios are chosen to bracket the degenerate end as well as the ordinary one: 2 is the
   floor the RTL accepts, where the counter is one bit wide and the MSB *is* the counter,
   so a model that assumed a spare low bit would part company here. 4 is the value the
   ratio was hardcoded at, which keeps the phase-1 goldens readable as the same traces.

   Sampling. [dst_clk] is combinational off the counter register, so [after_edge] is the
   value the divided clock takes as a result of the cycle just driven; that is what an
   observation means here. [run_stimuli_across_edge] reports both sides so a suite can pin
   the one-cycle relationship rather than assume it.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Clk_div

(* One driven cycle's worth of stimulus. [rst] is the register spec's synchronous clear
   and [en] the counter's enable; the pair is the entire input space, [src_clk] aside. *)
module Stimulus = struct
  type t =
    { rst : bool
    ; en : bool
    }
  [@@deriving sexp, equal, compare]

  let run = { rst = false; en = true }
  let hold = { rst = false; en = false }
  let clear = { rst = true; en = false }
end

module Observation = struct
  type t =
    { stimulus : Stimulus.t
    ; dst_clk : bool
    }
  [@@deriving sexp, equal, compare]
end

(* The pure-OCaml model. The counter is [ceil_log2 divisor] bits wide, so [dst_clk] is
   high for the upper half of every [divisor]-enabled-cycle run.

   Clear beats enable: the register spec's clear is applied ahead of the enable term, so a
   cycle with [rst] high resets the counter even though [en] is low. That ordering is
   asserted directly in the Quickcheck suite rather than only implied by this fold. *)
let expected_counts ~divisor stimuli =
  let rec loop count = function
    | [] -> []
    | { Stimulus.rst; en } :: remaining ->
      let count = if rst then 0 else if en then (count + 1) % divisor else count in
      count :: loop count remaining
  in
  loop 0 stimuli
;;

(* [dst_clk] is the counter's MSB. *)
let dst_clk_of_count ~divisor count = count >= divisor / 2

let expected_trace ~divisor stimuli =
  List.map (expected_counts ~divisor stimuli) ~f:(dst_clk_of_count ~divisor)
;;

let expected_observations ~divisor stimuli : Observation.t list =
  List.map2_exn stimuli (expected_trace ~divisor stimuli) ~f:(fun stimulus dst_clk ->
    { Observation.stimulus; dst_clk })
;;

module Make_testbench (Config : sig
    val divisor : int
  end) =
struct
  let divisor = Config.divisor

  module Fixture = Sim_fixture.Make (struct
      module I = Dut.I
      module O = Dut.O

      (* Written out rather than [include Dut]: [Dut.create] carries an optional
         [?divisor] argument, and OxCaml will not erase it to match [S]'s [create]. *)
      let create scope inputs = Dut.create ~divisor:Config.divisor scope inputs
      let name = sprintf "Clk_div (divisor = %d)" Config.divisor
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit
  let inputs ~rst ~en = { Step.input_hold with rst = bit rst; en = bit en }
  let snapshot (output : Bits.t Dut.O.t) = Bits.to_bool output.dst_clk

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay ~num_cycles handler (inputs ~rst:true ~en:false)
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* Every scenario opens with a clear, so an observation list is always read as "starting
     from a counter of zero". *)
  let run_stimuli stimuli =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | ({ Stimulus.rst; en } as stimulus) :: remaining ->
          let dst_clk =
            Step.cycle handler (inputs ~rst ~en) |> Step.O_data.after_edge |> snapshot
          in
          { Observation.stimulus; dst_clk } :: loop handler remaining
      in
      loop handler stimuli
    in
    run_with_timeout ~timeout:(4 + List.length stimuli) ~testbench
  ;;

  (* Same drive, reporting both sides of the edge. [before_edge] is the previous cycle's
     [dst_clk] - the counter has not advanced yet - so a suite can assert the output is
     registered rather than combinational off [en]. *)
  let run_stimuli_across_edge stimuli =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | ({ Stimulus.rst; en } as stimulus) :: remaining ->
          let data = Step.cycle handler (inputs ~rst ~en) in
          let before = Step.O_data.before_edge data |> snapshot in
          let after = Step.O_data.after_edge data |> snapshot in
          (stimulus, before, after) :: loop handler remaining
      in
      loop handler stimuli
    in
    run_with_timeout ~timeout:(4 + List.length stimuli) ~testbench
  ;;

  let run_enabled_cycles num_cycles =
    run_stimuli (List.init num_cycles ~f:(fun _ -> Stimulus.run))
  ;;

  let expected_observations stimuli = expected_observations ~divisor stimuli
  let expected_trace stimuli = expected_trace ~divisor stimuli
end

(* The floor the RTL accepts: a one-bit counter, where the MSB is the whole counter. *)
module Div_2 = Make_testbench (struct
    let divisor = 2
  end)

(* The ratio the module hardcoded before [?divisor] existed, and the one every caller in
   [lib/] and [validation/] still gets by default. *)
module Div_4 = Make_testbench (struct
    let divisor = 4
  end)

module Div_8 = Make_testbench (struct
    let divisor = 8
  end)

module Div_16 = Make_testbench (struct
    let divisor = 16
  end)

module Div_32 = Make_testbench (struct
    let divisor = 32
  end)

(* The default instantiation, named so a suite that is not about the ratio does not have
   to pick one. *)
module Testbench = Div_4

(* Every instantiation behind one interface, so a property runs across the whole set
   rather than being written out per ratio. The [Stimulus] and [Observation] types live
   outside the functor precisely so these are all the same type. *)
type runner =
  { divisor : int
  ; run_stimuli : Stimulus.t list -> Observation.t list
  }

let runners =
  [ { divisor = Div_2.divisor; run_stimuli = Div_2.run_stimuli }
  ; { divisor = Div_4.divisor; run_stimuli = Div_4.run_stimuli }
  ; { divisor = Div_8.divisor; run_stimuli = Div_8.run_stimuli }
  ; { divisor = Div_16.divisor; run_stimuli = Div_16.run_stimuli }
  ; { divisor = Div_32.divisor; run_stimuli = Div_32.run_stimuli }
  ]
;;

(* Ratios the RTL rejects: not a power of two, and below the two-bit floor. Zero and a
   negative are in the list because [Int.is_pow2] is not the guard that stops them - the
   [< 2] test is - and a refactor that dropped one of the two conditions would still pass
   a list of odd numbers. *)
let illegal_divisors = [ -4; -1; 0; 1; 3; 5; 6; 7; 12; 100 ]

(* Elaboration only - no simulator, because the point is that [create] refuses to build
   the circuit at all. Returns the exception so a suite can pin the message rather than
   only the fact that something was raised. *)
let elaborate divisor =
  Or_error.try_with (fun () ->
    let scope = Scope.create ~flatten_design:true () in
    let (_ : Signal.t Dut.O.t) =
      Dut.create
        ~divisor
        scope
        { Dut.I.src_clk = Signal.input "src_clk" 1
        ; rst = Signal.input "rst" 1
        ; en = Signal.input "en" 1
        }
    in
    ())
;;
