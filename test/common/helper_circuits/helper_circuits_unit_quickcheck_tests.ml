(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "helper_circuits_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Helper_circuits

   Typed examples and generated properties for the edge detectors and the delay line.

   Net-new verification - these functions had no coverage at all. The oracle is
   [Helper_circuits_testbench.expected_observations], which reads each output off the
   input history rather than re-simulating, so it cannot fail the same way the RTL does.
   The headline property runs it against random bit streams; the examples pin the cases
   that a random stream reaches rarely and that a consumer depends on by name - a
   single-cycle pulse, a level that never moves, an edge in the first cycle after clear.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Helper_circuits_testbench

let check xs =
  [%test_result: Observation.t list]
    (Testbench.run_stimuli xs)
    ~expect:(expected_observations xs)
;;

let column xs ~f = List.map (Testbench.run_stimuli xs) ~f

let%test_unit "a rising edge is detected on the cycle the input goes high" =
  [%test_result: bool list]
    (column [ false; false; true; true; false ] ~f:Observation.rising)
    ~expect:[ false; false; true; false; false ]
;;

let%test_unit "a falling edge is detected on the cycle the input goes low" =
  [%test_result: bool list]
    (column [ false; true; true; false; false ] ~f:Observation.falling)
    ~expect:[ false; false; false; true; false ]
;;

let%test_unit "the detectors are one-cycle pulses, never levels" =
  (* The consumers gate on these for exactly one cycle; a detector that stayed high for
     the duration of the level would double-count every event downstream. Clear-free: a
     clear landing on a rise widens the pulse to two cycles, which is asserted separately
     below. *)
  let xs = Testbench.square_wave ~half_period:4 ~num_periods:3 in
  let observations = Testbench.run_stimuli xs in
  List.iter
    [ "rising", Observation.rising; "falling", Observation.falling ]
    ~f:(fun (name, get) ->
      let longest_run =
        List.fold observations ~init:(0, 0) ~f:(fun (longest, current) observation ->
          let current = if get observation then current + 1 else 0 in
          Int.max longest current, current)
        |> fst
      in
      [%test_result: int] longest_run ~expect:1 ~message:name)
;;

let%test_unit "a one-cycle pulse produces one rising and one falling edge, adjacent" =
  let observations = Testbench.run_stimuli [ false; true; false; false ] in
  [%test_result: bool list]
    (List.map observations ~f:Observation.rising)
    ~expect:[ false; true; false; false ];
  [%test_result: bool list]
    (List.map observations ~f:Observation.falling)
    ~expect:[ false; false; true; false ]
;;

let%test_unit "a constant input produces no edges" =
  List.iter [ false; true ] ~f:(fun level ->
    let xs = List.init 8 ~f:(fun _ -> level) in
    let observations = Testbench.run_stimuli xs in
    (* An input that is high from the first cycle after clear *does* rise, because the
       register was cleared to zero - so the constant-high case skips that first cycle. *)
    let steady = List.drop observations 1 in
    List.iter steady ~f:(fun observation ->
      [%test_result: bool] observation.Observation.rising ~expect:false;
      [%test_result: bool] observation.Observation.falling ~expect:false))
;;

let%test_unit "an input already high in the first cycle after clear reads as a rise" =
  (* The clear zeroes the detector's register, so the first high cycle looks like an edge
     whether or not it was one. This is a real property of the block, not an artifact of
     the fixture: a consumer that comes out of reset with its input already asserted gets
     a pulse. Freeze it rather than let a future reader assume otherwise. *)
  [%test_result: bool list]
    (column [ true; true; true ] ~f:Observation.rising)
    ~expect:[ true; false; false ]
;;

let%test_unit "delay_by 0 is the identity" =
  let xs = [ false; true; true; false; true; false; false; true ] in
  [%test_result: bool list] (column xs ~f:Observation.delayed_0) ~expect:xs
;;

let%test_unit "delay_by n shifts the stream by exactly n cycles" =
  let xs = [ false; true; true; false; true; false; false; true; false; true ] in
  let shifted_by n =
    List.init (List.length xs) ~f:(fun index ->
      if index < n then false else List.nth_exn xs (index - n))
  in
  [%test_result: bool list] (column xs ~f:Observation.delayed_1) ~expect:(shifted_by 1);
  [%test_result: bool list]
    (column xs ~f:Observation.delayed_n)
    ~expect:(shifted_by delay_depth)
;;

let%test_unit "the delayed detectors are the plain detectors, shifted" =
  (* [rising_edge_delayed] is defined as [delay_by n (rising_edge_detector x)]. Asserting
     the composition against the two parts, in one simulation, is what makes that
     definition testable rather than merely readable. *)
  let xs = Testbench.square_wave ~half_period:3 ~num_periods:4 in
  let observations = Testbench.run_stimuli xs in
  let shifted get =
    List.init (List.length observations) ~f:(fun index ->
      if index < edge_delay_depth
      then false
      else get (List.nth_exn observations (index - edge_delay_depth)))
  in
  [%test_result: bool list]
    (List.map observations ~f:Observation.rising_delayed)
    ~expect:(shifted Observation.rising);
  [%test_result: bool list]
    (List.map observations ~f:Observation.falling_delayed)
    ~expect:(shifted Observation.falling)
;;

let%test_unit "a clear flushes the delay line" =
  let observations =
    Testbench.run_with_clear
      ~xs_before:[ true; true; true; true ]
      ~xs_after:[ false; false; false; false ]
  in
  (* The clear is synchronous, and these observations are [before_edge]: during the clear
     cycle itself the registers still hold what they held, and the flush is visible from
     the cycle after. Both halves are asserted - the second is the flush, the first is the
     one-cycle latency that a reader chasing a delay-line bug needs to know about. *)
  let clear_cycle = List.nth_exn observations 4 in
  [%test_result: bool] clear_cycle.Observation.delayed_1 ~expect:true;
  [%test_result: bool] clear_cycle.Observation.delayed_n ~expect:true;
  let after_clear = List.drop observations 5 in
  List.iter after_clear ~f:(fun observation ->
    [%test_result: bool] observation.Observation.delayed_1 ~expect:false;
    [%test_result: bool] observation.Observation.delayed_n ~expect:false)
;;

(* The cycles an output pulses on. The clear cases below ask "how many pulses, and where",
   which a whole-column comparison answers less directly than a list of indices. *)
let pulse_cycles observations ~f =
  List.filter_mapi observations ~f:(fun index observation ->
    if f observation then Some index else None)
;;

let clear_case ~before ~after ~clear ~f =
  pulse_cycles
    (Testbench.run_scheduled (Testbench.edge_with_clear ~before ~after ~clear ~tail:5))
    ~f
;;

let%test_unit "a falling edge coincident with a clear is lost" =
  (* RTL-6. The detector is combinational, so [falling] is high during the clear cycle -
     but the delay chain behind it carries the same spec and is cleared on that same edge,
     so the pulse never reaches [falling_delayed]. The clear-free control is half the
     test: an assertion that the clear case emits nothing is also satisfied by an output
     tied to zero, or by a stimulus with no edge in it. *)
  [%test_result: int list]
    (clear_case ~before:true ~after:false ~clear:false ~f:Observation.falling_delayed)
    ~expect:[ Testbench.edge_cycle + edge_delay_depth ];
  [%test_result: int list]
    (clear_case ~before:true ~after:false ~clear:true ~f:Observation.falling_delayed)
    ~expect:[];
  [%test_result: int list]
    (clear_case ~before:true ~after:false ~clear:true ~f:Observation.falling)
    ~expect:[ Testbench.edge_cycle ]
;;

let%test_unit "a rising edge coincident with a clear is deferred one cycle, not lost" =
  (* The other half of RTL-6, and the half the finding did not say: a rise is not lost the
     way a fall is. The clear zeroes the detector's history register, so on the next cycle
     a still-high input is compared against zero and reads as a fresh rise, which the
     delay chain then carries normally. A consumer of [rising_edge_delayed] whose input
     stays asserted across its own reset gets a late strobe, not a missing one - and
     exactly one, because the clear kills the first of the detector's two pulses on the
     same edge that produces the second. *)
  [%test_result: int list]
    (clear_case ~before:false ~after:true ~clear:false ~f:Observation.rising_delayed)
    ~expect:[ Testbench.edge_cycle + edge_delay_depth ];
  [%test_result: int list]
    (clear_case ~before:false ~after:true ~clear:true ~f:Observation.rising_delayed)
    ~expect:[ Testbench.edge_cycle + edge_delay_depth + 1 ]
;;

let%test_unit "a clear widens a rise to two cycles and swallows a fall" =
  (* The mechanism the two cases above share, asserted on the plain detectors so that a
     reader who finds one of them failing can tell which half moved. This is also the
     exception to "the detectors are one-cycle pulses": that property holds clear-free,
     and a consumer counting pulses across a clear sees two. *)
  [%test_result: int list]
    (clear_case ~before:false ~after:true ~clear:true ~f:Observation.rising)
    ~expect:[ Testbench.edge_cycle; Testbench.edge_cycle + 1 ];
  [%test_result: int list]
    (clear_case ~before:true ~after:false ~clear:true ~f:Observation.falling)
    ~expect:[ Testbench.edge_cycle ]
;;

let%test_unit "after_edge is degenerate for the detectors" =
  (* Not a property of the design, a property of where you sample it: at [after_edge] the
     detector's register has already taken this cycle's input, so both detectors read zero
     no matter what the input did. This is why every scenario here samples [before_edge],
     and asserting it keeps that reasoning from decaying into folklore. *)
  let xs = Testbench.square_wave ~half_period:2 ~num_periods:4 in
  List.iter (Testbench.run_across_edge xs) ~f:(fun (_before, after) ->
    [%test_result: bool] after.Observation.rising ~expect:false;
    [%test_result: bool] after.Observation.falling ~expect:false)
;;

let%test_unit "random bit streams match the model" =
  Quickcheck.test
    ~trials:60
    ~seed:(`Deterministic "helper-circuits-streams")
    ~sexp_of:[%sexp_of: bool list]
    ~shrinker:(List.quickcheck_shrinker (Quickcheck.Shrinker.empty ()))
    ~shrink_attempts:(`Limit 100)
    ~f:check
    (let open Quickcheck.Generator.Let_syntax in
     let%bind length = Int.gen_incl 1 32 in
     List.gen_with_length length Bool.quickcheck_generator)
;;

let%test_unit "random square waves match the model" =
  (* A uniform random stream toggles roughly every other cycle, which never exercises a
     level held longer than the delay line is deep. Square waves do. *)
  Quickcheck.test
    ~trials:24
    ~seed:(`Deterministic "helper-circuits-square-waves")
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (half_period, num_periods) ->
      check (Testbench.square_wave ~half_period ~num_periods))
    (Quickcheck.Generator.both (Int.gen_incl 1 6) (Int.gen_incl 1 4))
;;

(* The word helpers in the same module. They are not edge detectors and share no state
   with them, but they live in [helper_circuits.ml], had no coverage either, and the ipv4
   and udp header builders in Phase 3 depend on the byte order they impose. *)

let%test_unit "hi16 and lo16 split a word into its high and low bytes" =
  let words = [ 0x0000; 0x00FF; 0xFF00; 0x1234; 0xABCD; 0xFFFF ] in
  [%test_result: Word_observation.t list]
    (Testbench.run_words words)
    ~expect:(List.map words ~f:expected_word_observation)
;;

let%test_unit "hi16 and lo16 are not interchangeable" =
  (* Stated as its own case because a swap is the failure that matters: the ipv4 and udp
     header builders emit [hi16 x] then [lo16 x] to put a length or a checksum on the wire
     most significant byte first, and a swap there produces a well-formed header with the
     wrong number in it. *)
  match Testbench.run_words [ 0x1234 ] with
  | [ observation ] ->
    [%test_result: int] observation.hi_byte ~expect:0x12;
    [%test_result: int] observation.lo_byte ~expect:0x34
  | observations ->
    raise_s [%message "expected one observation" (List.length observations : int)]
;;

let%test_unit "const8 is eight bits wide and truncates silently" =
  let value, truncated, width = Testbench.run_constants () in
  [%test_result: int] value ~expect:const_byte_value;
  [%test_result: int] width ~expect:8;
  [%test_result: int] truncated ~expect:(const_byte_overflowing_value land 0xFF)
;;

let%test_unit "random words split correctly" =
  Quickcheck.test
    ~trials:60
    ~seed:(`Deterministic "helper-circuits-words")
    ~sexp_of:[%sexp_of: int]
    ~shrinker:Int.quickcheck_shrinker
    ~shrink_attempts:(`Limit 100)
    ~f:(fun word ->
      [%test_result: Word_observation.t list]
        (Testbench.run_words [ word ])
        ~expect:[ expected_word_observation word ])
    (Int.gen_incl 0x0000 0xFFFF)
;;
