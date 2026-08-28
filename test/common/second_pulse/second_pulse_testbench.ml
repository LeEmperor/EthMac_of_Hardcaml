(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "second_pulse_testbench.ml" *)

(* Testbench Support: Second_pulse

   Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit,
   Quickcheck, and expect test suites.

   [Second_pulse.create] takes [?clk_freq], defaulting to 100_000_000 - one pulse every
   hundred million cycles, which no simulation is going to reach. The suite is therefore a
   [Make_testbench] functor over the divide value and is instantiated at a handful of
   small frequencies, exactly as [tx_datapath]'s is over its ethertype; the legacy harness
   made the same substitution with a bare [clk_freq_sim = 10]. The optional argument also
   means [include Dut] will not satisfy [Sim_fixture.S], so [create] is written out.

   Frequencies are chosen to separate two things a single value would confound: the
   counter is [Int.ceil_log2 clk_freq] bits wide, so a power of two makes the terminal
   count and the counter's natural wrap coincide, and a bug that reset on the wrap rather
   than on the compare would be invisible. 4, 8 and 16 are powers of two; 3, 5 and 10 are
   not and leave unused counter values above the terminal count.

   Sampling. Both [pulse] and the counter are registered, so [after_edge] is the pulse the
   cycle just driven produced. That is the convention the other register-output suites
   follow.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Second_pulse

(* [rst] is the only input besides the clock, so a stimulus is one boolean per cycle. *)
module Observation = struct
  type t =
    { rst : bool
    ; pulse : bool
    }
  [@@deriving sexp, equal, compare]
end

(* A whole run condensed to the cycles the pulse fired on, 1-based from the release of
   reset. This is the form the goldens print and the form the period property asserts:
   "every [clk_freq] cycles, once" is a statement about positions, not about a bit
   pattern. *)
module Pulse_summary = struct
  type t =
    { num_cycles : int
    ; pulse_cycles : int list
    }
  [@@deriving sexp, equal, compare]

  let of_observations observations =
    { num_cycles = List.length observations
    ; pulse_cycles =
        List.filter_mapi observations ~f:(fun index (observation : Observation.t) ->
          if observation.pulse then Some (index + 1) else None)
    }
  ;;
end

(* The pure-OCaml model: a counter that rolls at [clk_freq - 1] and a pulse register that
   is set by the same compare. Both carry the spec's clear, so a cycle with [rst] high
   zeroes the counter and forces the pulse low. *)
let expected_observations ~clk_freq rst_schedule : Observation.t list =
  let rec loop count = function
    | [] -> []
    | rst :: remaining ->
      let count, pulse =
        if rst
        then 0, false
        else if count = clk_freq - 1
        then 0, true
        else count + 1, false
      in
      { Observation.rst; pulse } :: loop count remaining
  in
  loop 0 rst_schedule
;;

let expected_summary ~clk_freq ~num_cycles =
  Pulse_summary.of_observations
    (expected_observations ~clk_freq (List.init num_cycles ~f:(fun _ -> false)))
;;

module Make_testbench (Config : sig
    val clk_freq : int
  end) =
struct
  let clk_freq = Config.clk_freq

  module Fixture = Sim_fixture.Make (struct
      module I = Dut.I
      module O = Dut.O

      (* Written out rather than [include Dut]: [Dut.create] carries an optional
         [?clk_freq] argument, and OxCaml will not erase it to match [S]'s [create]. *)
      let create scope inputs = Dut.create ~clk_freq:Config.clk_freq scope inputs
      let name = sprintf "Second_pulse (clk_freq = %d)" Config.clk_freq
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit
  let inputs ~rst = { Step.input_hold with rst = bit rst }

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay ~num_cycles handler (inputs ~rst:true)
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* One boolean per cycle, driving [rst]; every scenario opens with its own clear so an
     observation list always reads as "starting from a counter of zero". *)
  let run_stimuli rst_schedule =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | rst :: remaining ->
          let output = Step.cycle handler (inputs ~rst) |> Step.O_data.after_edge in
          { Observation.rst; pulse = Bits.to_bool output.Dut.O.pulse }
          :: loop handler remaining
      in
      loop handler rst_schedule
    in
    run_with_timeout ~timeout:(4 + List.length rst_schedule) ~testbench
  ;;

  let run_free ~num_cycles = run_stimuli (List.init num_cycles ~f:(fun _ -> false))
  let summary ~num_cycles = Pulse_summary.of_observations (run_free ~num_cycles)

  (* [keep] is the module's synthesis anti-pruning output. It is wired to the same
     register as [pulse], which a suite should freeze rather than assume: if it were ever
     re-pointed at a different internal, the two would part company here. *)
  let run_free_with_keep ~num_cycles =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) remaining =
        if remaining = 0
        then []
        else (
          let output = Step.cycle handler (inputs ~rst:false) |> Step.O_data.after_edge in
          (Bits.to_bool output.Dut.O.pulse, Bits.to_bool output.Dut.O.keep)
          :: loop handler (remaining - 1))
      in
      loop handler num_cycles
    in
    run_with_timeout ~timeout:(4 + num_cycles) ~testbench
  ;;
end

module Freq_3 = Make_testbench (struct
    let clk_freq = 3
  end)

module Freq_4 = Make_testbench (struct
    let clk_freq = 4
  end)

module Freq_5 = Make_testbench (struct
    let clk_freq = 5
  end)

module Freq_8 = Make_testbench (struct
    let clk_freq = 8
  end)

(* The frequency the legacy harness ran at, so its printed trace can be read against the
   goldens here. *)
module Freq_10 = Make_testbench (struct
    let clk_freq = 10
  end)

module Freq_16 = Make_testbench (struct
    let clk_freq = 16
  end)

(* Every instantiation behind one interface, so a property runs across the whole set
   rather than being written out per frequency. The [Observation] and [Pulse_summary]
   types live outside the functor precisely so these are all the same type. *)
type runner =
  { clk_freq : int
  ; run_stimuli : bool list -> Observation.t list
  }

let runners =
  [ { clk_freq = Freq_3.clk_freq; run_stimuli = Freq_3.run_stimuli }
  ; { clk_freq = Freq_4.clk_freq; run_stimuli = Freq_4.run_stimuli }
  ; { clk_freq = Freq_5.clk_freq; run_stimuli = Freq_5.run_stimuli }
  ; { clk_freq = Freq_8.clk_freq; run_stimuli = Freq_8.run_stimuli }
  ; { clk_freq = Freq_10.clk_freq; run_stimuli = Freq_10.run_stimuli }
  ; { clk_freq = Freq_16.clk_freq; run_stimuli = Freq_16.run_stimuli }
  ]
;;
