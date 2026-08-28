(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "clk_div_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Clk_div

   Typed examples and generated properties for the fabric clock divider.

   The oracle is [Clk_div_testbench.expected_trace], a fold over the enable schedule. The
   headline property randomizes the schedule itself - arbitrary interleavings of run, hold
   and clear - rather than only running the counter free, because the enable is the one
   axis of this block's behavior that the fixed ratio leaves open. A divider that dropped
   or double-counted an enable would pass a free-running test and fail here.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Clk_div_testbench

let check stimuli =
  [%test_result: Observation.t list]
    (Testbench.run_stimuli stimuli)
    ~expect:(expected_observations stimuli)
;;

let trace stimuli =
  List.map (Testbench.run_stimuli stimuli) ~f:(fun (observation : Observation.t) ->
    observation.dst_clk)
;;

let%test_unit "the divided clock has a period of four enabled cycles" =
  (* Two full periods, phase included: the counter leaves a clear at zero, so the first
     high cycle is the second one. *)
  [%test_result: bool list]
    (trace (List.init 8 ~f:(fun _ -> Stimulus.run)))
    ~expect:[ false; true; true; false; false; true; true; false ]
;;

let%test_unit "the duty cycle is exactly half over whole periods" =
  List.iter [ 4; 8; 12; 40 ] ~f:(fun num_cycles ->
    let high =
      List.count (trace (List.init num_cycles ~f:(fun _ -> Stimulus.run))) ~f:Fn.id
    in
    [%test_result: int] high ~expect:(num_cycles / 2))
;;

let%test_unit "a disabled cycle freezes the counter" =
  (* Three runs then three holds: the output must sit at the value the third run left. *)
  let stimuli =
    List.init 3 ~f:(fun _ -> Stimulus.run) @ List.init 3 ~f:(fun _ -> Stimulus.hold)
  in
  [%test_result: bool list]
    (trace stimuli)
    ~expect:[ false; true; true; true; true; true ]
;;

let%test_unit "a disabled run only delays the waveform, never reshapes it" =
  (* Sixteen enabled cycles with holds sprinkled through must produce the same sequence
     of *transitions* as sixteen back-to-back enabled cycles - the holds stretch the
     waveform, they do not skip a phase. *)
  let free_running = trace (List.init 16 ~f:(fun _ -> Stimulus.run)) in
  let stretched =
    trace
      (List.concat_map (List.init 16 ~f:Fn.id) ~f:(fun index ->
         if index % 3 = 0 then [ Stimulus.hold; Stimulus.run ] else [ Stimulus.run ]))
  in
  let dedup values = List.remove_consecutive_duplicates values ~equal:Bool.equal in
  [%test_result: bool list] (dedup stretched) ~expect:(dedup free_running)
;;

let%test_unit "clear beats enable" =
  (* [rst] and [en] high on the same cycle: the register spec's clear is applied ahead of
     the enable term, so the counter zeroes rather than incrementing. Freezing this is the
     point - the model in the testbench folds the same priority, and everything else here
     inherits it. *)
  [%test_result: bool list]
    (trace
       [ Stimulus.run
       ; Stimulus.run
       ; { Stimulus.rst = true; en = true }
       ; Stimulus.run
       ; Stimulus.run
       ])
    ~expect:[ false; true; false; false; true ]
;;

let%test_unit "the output is registered, not combinational off en" =
  (* [before_edge] is the previous cycle's value; a divider that let [en] reach the output
     without a register would show the two sides of the edge disagreeing in a way the
     one-cycle shift below cannot explain. *)
  let stimuli = List.init 12 ~f:(fun _ -> Stimulus.run) in
  let across_edge = Testbench.run_stimuli_across_edge stimuli in
  let befores = List.map across_edge ~f:(fun (_, before, _) -> before) in
  let afters = List.map across_edge ~f:(fun (_, _, after) -> after) in
  [%test_result: bool list] befores ~expect:(false :: List.drop_last_exn afters)
;;

module Schedule = struct
  type t = Stimulus.t list [@@deriving sexp_of]

  (* Weighted so most cycles run: an all-random schedule spends most of its length
     clearing, which never exercises a full period. *)
  let stimulus_generator =
    Quickcheck.Generator.weighted_union
      [ 6.0, Quickcheck.Generator.return Stimulus.run
      ; 2.0, Quickcheck.Generator.return Stimulus.hold
      ; 1.0, Quickcheck.Generator.return Stimulus.clear
      ; 1.0, Quickcheck.Generator.return { Stimulus.rst = true; en = true }
      ]
  ;;

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 40 in
    List.gen_with_length length stimulus_generator
  ;;

  (* Only the list structure shrinks: a counter-example is most legible as the shortest
     schedule that still fails, and there is no smaller [Stimulus.t] to shrink toward. *)
  let quickcheck_shrinker = List.quickcheck_shrinker (Quickcheck.Shrinker.empty ())
end

let%test_unit "random enable schedules match the model" =
  Quickcheck.test
    ~trials:60
    ~seed:(`Deterministic "clk-div-schedules")
    ~sexp_of:[%sexp_of: Schedule.t]
    ~shrinker:Schedule.quickcheck_shrinker
    ~shrink_attempts:(`Limit 100)
    ~f:check
    Schedule.quickcheck_generator
;;

let%test_unit "free-running for a random number of cycles matches the model" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "clk-div-free-running")
    ~sexp_of:[%sexp_of: int]
    ~f:(fun num_cycles -> check (List.init num_cycles ~f:(fun _ -> Stimulus.run)))
    (Int.gen_incl 1 48)
;;
