(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "second_pulse_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Second_pulse

   Typed examples and generated properties for the heartbeat pulse generator.

   The contract has three parts and each gets its own property: the pulse is one cycle
   wide, it fires every [clk_freq] cycles, and a clear restarts the phase rather than
   merely suppressing one pulse. All three are checked at six divide values, including
   both powers of two and non-powers, so a counter that rolled on its natural wrap rather
   than on the terminal compare cannot pass.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Second_pulse_testbench

let check ~clk_freq ~run_stimuli rst_schedule =
  [%test_result: Observation.t list]
    (run_stimuli rst_schedule)
    ~expect:(expected_observations ~clk_freq rst_schedule)
;;

let free_run ~run_stimuli ~num_cycles =
  Pulse_summary.of_observations (run_stimuli (List.init num_cycles ~f:(fun _ -> false)))
;;

let%test_unit "the pulse fires every clk_freq cycles, at every divide value" =
  List.iter runners ~f:(fun { clk_freq; run_stimuli } ->
    (* Three full periods plus a partial one, so the property is about the recurrence and
       not about a run that happens to end on a pulse. *)
    let num_cycles = (3 * clk_freq) + (clk_freq / 2) in
    [%test_result: Pulse_summary.t]
      (free_run ~run_stimuli ~num_cycles)
      ~expect:(expected_summary ~clk_freq ~num_cycles)
      ~message:(sprintf "clk_freq = %d" clk_freq))
;;

let%test_unit "the first pulse lands on cycle clk_freq, not clk_freq - 1 or + 1" =
  (* Off-by-one in the terminal compare is the failure this block is most likely to have,
     and it is invisible in a period measurement - every pulse would just shift together.
     Pinning the absolute position of the first pulse after a clear is what catches it. *)
  List.iter runners ~f:(fun { clk_freq; run_stimuli } ->
    let summary = free_run ~run_stimuli ~num_cycles:(2 * clk_freq) in
    [%test_result: int list]
      summary.pulse_cycles
      ~expect:[ clk_freq; 2 * clk_freq ]
      ~message:(sprintf "clk_freq = %d" clk_freq))
;;

let%test_unit "the pulse is exactly one cycle wide" =
  List.iter runners ~f:(fun { clk_freq; run_stimuli } ->
    let observations = run_stimuli (List.init (4 * clk_freq) ~f:(fun _ -> false)) in
    let longest_run =
      List.fold observations ~init:(0, 0) ~f:(fun (longest, current) observation ->
        let current = if observation.Observation.pulse then current + 1 else 0 in
        Int.max longest current, current)
      |> fst
    in
    [%test_result: int] longest_run ~expect:1 ~message:(sprintf "clk_freq = %d" clk_freq))
;;

let%test_unit "a clear restarts the phase" =
  (* Not just "no pulse during reset": the next pulse must be a full period after the
     clear, which is what distinguishes a counter that is zeroed from one that is merely
     gated. *)
  List.iter runners ~f:(fun { clk_freq; run_stimuli } ->
    let before = clk_freq - 1 in
    let rst_schedule =
      List.concat
        [ List.init before ~f:(fun _ -> false)
        ; [ true ]
        ; List.init (2 * clk_freq) ~f:(fun _ -> false)
        ]
    in
    let summary = Pulse_summary.of_observations (run_stimuli rst_schedule) in
    (* The clear lands on the cycle the pulse would otherwise have fired on, so the run
       that follows starts from zero: pulses at [before + 1 + clk_freq] and one period
       later, 1-based. *)
    [%test_result: int list]
      summary.pulse_cycles
      ~expect:[ before + 1 + clk_freq; before + 1 + (2 * clk_freq) ]
      ~message:(sprintf "clk_freq = %d" clk_freq))
;;

let%test_unit "keep tracks pulse" =
  (* [keep] exists only to stop synthesis pruning the internal; it is wired to the pulse
     register. Freeze that rather than assume it. *)
  List.iter [ Freq_5.run_free_with_keep; Freq_8.run_free_with_keep ] ~f:(fun run ->
    List.iter (run ~num_cycles:24) ~f:(fun (pulse, keep) ->
      [%test_result: bool] keep ~expect:pulse))
;;

let%test_unit "random reset schedules match the model, at every divide value" =
  List.iter runners ~f:(fun { clk_freq; run_stimuli } ->
    Quickcheck.test
      ~trials:20
      ~seed:(`Deterministic (sprintf "second-pulse-resets-%d" clk_freq))
      ~sexp_of:[%sexp_of: bool list]
      ~shrinker:(List.quickcheck_shrinker (Quickcheck.Shrinker.empty ()))
      ~shrink_attempts:(`Limit 100)
      ~f:(check ~clk_freq ~run_stimuli)
      (* Reset is rare on purpose: a schedule that clears often never reaches a terminal
         count, and it is the interaction of a clear with a nearly-complete period that is
         worth generating. *)
      (let open Quickcheck.Generator.Let_syntax in
       let%bind length = Int.gen_incl 1 (4 * clk_freq) in
       List.gen_with_length
         length
         (Quickcheck.Generator.weighted_union
            [ 9.0, Quickcheck.Generator.return false
            ; 1.0, Quickcheck.Generator.return true
            ])))
;;

let%test_unit "a random number of free-running cycles matches the model" =
  List.iter runners ~f:(fun { clk_freq; run_stimuli } ->
    Quickcheck.test
      ~trials:15
      ~seed:(`Deterministic (sprintf "second-pulse-free-%d" clk_freq))
      ~sexp_of:[%sexp_of: int]
      ~f:(fun num_cycles ->
        [%test_result: Pulse_summary.t]
          (free_run ~run_stimuli ~num_cycles)
          ~expect:(expected_summary ~clk_freq ~num_cycles)
          ~message:(sprintf "clk_freq = %d" clk_freq))
      (Int.gen_incl 1 (5 * clk_freq)))
;;
