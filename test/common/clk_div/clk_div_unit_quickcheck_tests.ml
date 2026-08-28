(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "clk_div_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Clk_div

   Typed examples and generated properties for the fabric clock divider.

   The oracle is [Clk_div_testbench.expected_trace], a fold over the enable schedule. Two
   axes are swept and they check different things. The divisor sweep is the plan's
   original property, writable now that [?divisor] exists (RTL-3): the output period is
   the ratio, and the duty cycle is half, at every ratio the RTL accepts. The enable
   schedule sweep randomizes arbitrary interleavings of run, hold and clear, which is the
   sharper of the two - a divider that dropped or double-counted an enable would pass a
   free-running period measurement at every ratio and fail here.

   The examples below are written against [Testbench], the divide-by-four instantiation,
   because four is the default every caller still gets; the properties run across
   [runners], which is all five.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Clk_div_testbench

let check stimuli =
  [%test_result: Observation.t list]
    (Testbench.run_stimuli stimuli)
    ~expect:(Testbench.expected_observations stimuli)
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

(* The ratio axis. Three periods free-running is enough to separate a divider that has the
   right period from one that only has the right first edge. *)
let free_running_trace runner ~num_cycles =
  List.map
    (runner.run_stimuli (List.init num_cycles ~f:(fun _ -> Stimulus.run)))
    ~f:(fun (observation : Observation.t) -> observation.dst_clk)
;;

let%test_unit "the output period is the divisor, at every ratio" =
  List.iter runners ~f:(fun runner ->
    let num_cycles = 3 * runner.divisor in
    [%test_result: bool list]
      ~message:(sprintf "divisor = %d" runner.divisor)
      (free_running_trace runner ~num_cycles)
      ~expect:
        (expected_trace
           ~divisor:runner.divisor
           (List.init num_cycles ~f:(fun _ -> Stimulus.run))))
;;

let%test_unit "every interior run of like values is half a period long, at every ratio" =
  (* The period stated as run lengths rather than as a golden list. The opening and
     closing runs are excluded because the window cuts them: the counter leaves a clear at
     zero and advances to one on the first driven cycle, so the trace starts partway
     through a phase, and it ends wherever the cycle count ran out. Every run between them
     is exactly [divisor / 2]. *)
  List.iter runners ~f:(fun runner ->
    let half = runner.divisor / 2 in
    let runs =
      free_running_trace runner ~num_cycles:(4 * runner.divisor)
      |> List.group ~break:(fun a b -> not (Bool.equal a b))
      |> List.map ~f:List.length
    in
    let interior = List.drop (List.drop_last_exn runs) 1 in
    [%test_result: int list]
      ~message:(sprintf "divisor = %d: interior runs" runner.divisor)
      interior
      ~expect:(List.map interior ~f:(fun _ -> half)))
;;

let%test_unit "the first rising edge lands half a period after the clear, at every ratio" =
  (* Phase, stated independently of the run lengths above. The first driven cycle leaves
     the counter at one, so the output goes high on the cycle that takes it to
     [divisor / 2] - index [divisor / 2 - 1], counting the first driven cycle as zero. At
     a divisor of two that is index zero: the output is high immediately. *)
  List.iter runners ~f:(fun runner ->
    let trace = free_running_trace runner ~num_cycles:(2 * runner.divisor) in
    [%test_result: int option]
      ~message:(sprintf "divisor = %d" runner.divisor)
      (List.findi trace ~f:(fun _ high -> high) |> Option.map ~f:fst)
      ~expect:(Some ((runner.divisor / 2) - 1)))
;;

let%test_unit "the duty cycle is exactly half over whole periods, at every ratio" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 1; 2; 5 ] ~f:(fun periods ->
      let num_cycles = periods * runner.divisor in
      let high = List.count (free_running_trace runner ~num_cycles) ~f:Fn.id in
      [%test_result: int]
        ~message:(sprintf "divisor = %d, %d periods" runner.divisor periods)
        high
        ~expect:(num_cycles / 2)))
;;

let%test_unit "a divisor of two divides on the counter's only bit" =
  (* The degenerate end: a one-bit counter, so [dst_clk] toggles every enabled cycle and
     the MSB is the whole register. A model that assumed a spare low bit under the MSB
     would produce a two-cycle-high pattern here. *)
  let trace =
    free_running_trace
      (List.find_exn runners ~f:(fun runner -> runner.divisor = 2))
      ~num_cycles:6
  in
  [%test_result: bool list] trace ~expect:[ true; false; true; false; true; false ]
;;

let%test_unit "illegal divisors are rejected at elaboration" =
  (* Not a power of two, or below the two-bit floor. The floor matters because a divisor
     of one asks for a zero-width register, which Hardcaml rejects with a message that
     says nothing about divisors. *)
  List.iter illegal_divisors ~f:(fun divisor ->
    match elaborate divisor with
    | Error _ -> ()
    | Ok () -> raise_s [%message "divisor was accepted" (divisor : int)])
;;

let%test_unit "every swept divisor elaborates" =
  List.iter runners ~f:(fun runner -> Or_error.ok_exn (elaborate runner.divisor))
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

let%test_unit "random enable schedules match the model at every ratio" =
  (* The two axes crossed: the schedule generator above, run against each instantiation.
     Trials are lower per ratio than the divide-by-four property's sixty because this is
     five simulations per trial; the seed is per-ratio so a failure names one. *)
  List.iter runners ~f:(fun runner ->
    Quickcheck.test
      ~trials:20
      ~seed:(`Deterministic (sprintf "clk-div-schedules-divisor-%d" runner.divisor))
      ~sexp_of:[%sexp_of: Schedule.t]
      ~shrinker:Schedule.quickcheck_shrinker
      ~shrink_attempts:(`Limit 100)
      ~f:(fun stimuli ->
        [%test_result: Observation.t list]
          ~message:(sprintf "divisor = %d" runner.divisor)
          (runner.run_stimuli stimuli)
          ~expect:(expected_observations ~divisor:runner.divisor stimuli))
      Schedule.quickcheck_generator)
;;
