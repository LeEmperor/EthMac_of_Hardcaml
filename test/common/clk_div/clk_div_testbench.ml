(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "clk_div_testbench.ml" *)

(* Testbench Support: Clk_div

   Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit,
   Quickcheck, and expect test suites.

   The block is a two-bit free-running counter whose MSB is the divided clock, so the
   whole of its behavior is "how many enabled cycles have elapsed since the last clear",
   and [expected_trace] below is that statement written as a fold. Everything in the two
   test files is checked against it.

   No divisor port. [lib/common/clk_div.ml] hardcodes [~width:2], so the division ratio is
   fixed at four and there is nothing to sweep across - the plan's "quickcheck the output
   period across divisor values" is instead a sweep across enable *schedules*, which is
   the only axis the RTL exposes. See findings RTL-3.

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

(* The pure-OCaml model. The counter is two bits wide, so [dst_clk] is high for the upper
   half of every four-enabled-cycle run.

   Clear beats enable: the register spec's clear is applied ahead of the enable term, so a
   cycle with [rst] high resets the counter even though [en] is low. That ordering is
   asserted directly in the Quickcheck suite rather than only implied by this fold. *)
let counter_max = 4

let expected_counts stimuli =
  let rec loop count = function
    | [] -> []
    | { Stimulus.rst; en } :: remaining ->
      let count = if rst then 0 else if en then (count + 1) % counter_max else count in
      count :: loop count remaining
  in
  loop 0 stimuli
;;

(* [dst_clk] is the counter's MSB. *)
let dst_clk_of_count count = count >= counter_max / 2
let expected_trace stimuli = List.map (expected_counts stimuli) ~f:dst_clk_of_count

let expected_observations stimuli : Observation.t list =
  List.map2_exn stimuli (expected_trace stimuli) ~f:(fun stimulus dst_clk ->
    { Observation.stimulus; dst_clk })
;;

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Clk_div"
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
end
