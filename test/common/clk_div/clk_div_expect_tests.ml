(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "clk_div_expect_tests.ml" *)

(* Expect Test Suite: Clk_div

   Golden traces of the divided clock as a waveform-shaped string, one character per
   cycle. The period, the freeze under [en] low, and the restart under [rst] are all
   things you should be able to read off the row rather than decode from a list of
   booleans.

   The first group is the divide-by-four instantiation, which is what every caller gets by
   default; the ratio sweep at the bottom prints one row per divisor so the periods can be
   compared by eye against each other, which is the form a divisor mistake is most visible
   in. See findings RTL-3 for how the ratio became an argument.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Clk_div_testbench

(* One character per driven cycle, so a golden reads like a waveform row. *)
let trace_string observations =
  String.of_list
    (List.map observations ~f:(fun (observation : Observation.t) ->
       if observation.dst_clk then '_' else '.'))
;;

let print_trace observations = print_endline (trace_string observations)

let%expect_test "twelve enabled cycles are three divide-by-four periods" =
  print_trace (Testbench.run_enabled_cycles 12);
  [%expect {| .__..__..__. |}]
;;

let%expect_test "the counter freezes while en is low and resumes where it left off" =
  (* The legacy harness ran 20 enabled cycles, 4 frozen, then 8 more; this is the same
     shape shortened to one reviewable row. *)
  let stimuli =
    List.concat
      [ List.init 5 ~f:(fun _ -> Stimulus.run)
      ; List.init 4 ~f:(fun _ -> Stimulus.hold)
      ; List.init 5 ~f:(fun _ -> Stimulus.run)
      ]
  in
  print_trace (Testbench.run_stimuli stimuli);
  [%expect {| .__......__.._ |}]
;;

let%expect_test "a clear mid-period restarts the count" =
  let stimuli =
    List.concat
      [ List.init 3 ~f:(fun _ -> Stimulus.run)
      ; [ Stimulus.clear ]
      ; List.init 6 ~f:(fun _ -> Stimulus.run)
      ]
  in
  print_trace (Testbench.run_stimuli stimuli);
  [%expect {| .__..__.._ |}]
;;

let%expect_test "the observation record, in full, for one period" =
  print_s [%sexp (Testbench.run_enabled_cycles 4 : Observation.t list)];
  [%expect
    {|
    (((stimulus ((rst false) (en true))) (dst_clk false))
     ((stimulus ((rst false) (en true))) (dst_clk true))
     ((stimulus ((rst false) (en true))) (dst_clk true))
     ((stimulus ((rst false) (en true))) (dst_clk false)))
    |}]
;;

(* One row per ratio over the same 32-cycle window, so the periods stack up and can be
   read against each other. Every row starts from a clear, so every one begins partway
   into its first low phase - the opening low run is half a period minus the cycle the
   clear already accounted for. *)
let%expect_test "the period tracks the divisor" =
  List.iter runners ~f:(fun runner ->
    printf
      "%3d  %s\n"
      runner.divisor
      (trace_string (runner.run_stimuli (List.init 32 ~f:(fun _ -> Stimulus.run)))));
  [%expect
    {|
     2  _._._._._._._._._._._._._._._._.
     4  .__..__..__..__..__..__..__..__.
     8  ...____....____....____....____.
    16  .......________........________.
    32  ...............________________.
    |}]
;;

(* The ratio does not change what a hold or a clear means: the freeze is still "the
   waveform stops where it is", and the clear still restarts the count from zero, at a
   ratio where a phase is four cycles rather than two. *)
let%expect_test "freeze and restart at a divisor of eight" =
  let stimuli =
    List.concat
      [ List.init 6 ~f:(fun _ -> Stimulus.run)
      ; List.init 4 ~f:(fun _ -> Stimulus.hold)
      ; List.init 4 ~f:(fun _ -> Stimulus.run)
      ; [ Stimulus.clear ]
      ; List.init 8 ~f:(fun _ -> Stimulus.run)
      ]
  in
  print_trace (Div_8.run_stimuli stimuli);
  [%expect {| ...________.......____. |}]
;;

(* The rejection message itself, not just the fact of a rejection. A divisor of one is the
   case worth reading: it would otherwise surface as Hardcaml's zero-width register error,
   which says nothing about divisors and sends the reader into [Reg_spec]. *)
let%expect_test "illegal divisors are rejected by name" =
  List.iter illegal_divisors ~f:(fun divisor ->
    match elaborate divisor with
    | Ok () -> printf "%4d  accepted\n" divisor
    | Error error -> printf "%4d  %s\n" divisor (Error.to_string_mach error));
  [%expect
    {|
     -4  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor -4))
     -1  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor -1))
      0  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 0))
      1  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 1))
      3  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 3))
      5  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 5))
      6  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 6))
      7  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 7))
     12  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 12))
    100  ("Clk_div.create: divisor must be a power of two and at least 2"(divisor 100))
    |}]
;;
