(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "second_pulse_expect_tests.ml" *)

(* Expect Test Suite: Second_pulse

   Golden traces of the pulse position. The waveform row is the point: the reviewable
   claim about this block is "one cycle high, [clk_freq - 1] cycles low, forever", and a
   row of characters says that where a list of booleans does not. The bottom group is the
   floor of the domain - the row at [clk_freq = 1], where the low phase has no cycles left
   in it, and the message [create] raises below that. See findings RTL-4.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Second_pulse_testbench

(* One character per driven cycle. *)
let print_trace observations =
  print_endline
    (String.of_list
       (List.map observations ~f:(fun (observation : Observation.t) ->
          match observation.rst, observation.pulse with
          | true, _ -> 'R'
          | false, true -> '|'
          | false, false -> '.')))
;;

let%expect_test "three periods at clk_freq = 10, the frequency the legacy harness ran" =
  print_trace (Freq_10.run_free ~num_cycles:35);
  [%expect {| .........|.........|.........|..... |}]
;;

let%expect_test "the pulse position, as cycle numbers" =
  print_s [%sexp (Freq_10.summary ~num_cycles:35 : Pulse_summary.t)];
  [%expect {| ((num_cycles 35) (pulse_cycles (10 20 30))) |}]
;;

let%expect_test "a power-of-two and a non-power-of-two divide side by side" =
  (* clk_freq = 8 fills its 3-bit counter exactly; clk_freq = 5 leaves 5, 6 and 7 unused.
     A counter that rolled on its natural wrap instead of on the terminal compare would
     agree with the first row and not the second. *)
  print_endline "clk_freq = 8:";
  print_trace (Freq_8.run_free ~num_cycles:24);
  print_endline "clk_freq = 5:";
  print_trace (Freq_5.run_free ~num_cycles:24);
  [%expect
    {|
    clk_freq = 8:
    .......|.......|.......|
    clk_freq = 5:
    ....|....|....|....|....
    |}]
;;

let%expect_test "a clear mid-period restarts the phase" =
  let rst_schedule =
    List.concat
      [ List.init 3 ~f:(fun _ -> false); [ true ]; List.init 16 ~f:(fun _ -> false) ]
  in
  print_trace (Freq_5.run_stimuli rst_schedule);
  [%expect {| ...R....|....|....|. |}]
;;

let%expect_test "the observation record, in full, across one period" =
  print_s [%sexp (Freq_3.run_free ~num_cycles:4 : Observation.t list)];
  [%expect
    {|
    (((rst false) (pulse false)) ((rst false) (pulse false))
     ((rst false) (pulse true)) ((rst false) (pulse false)))
    |}]
;;

(* The floor of the domain, one row per frequency, so the low phase can be watched
   shrinking to nothing. [clk_freq = 1] is the case RTL-4 was filed against: the counter
   width is [ceil_log2 1 = 0] before the floor, so this row did not exist at all until
   [Second_pulse.counter_width] gained its [Int.max 1]. *)
let%expect_test "the smallest frequencies, down to a pulse every cycle" =
  List.iter [ Freq_1.run_free; Freq_2.run_free; Freq_3.run_free ] ~f:(fun run ->
    print_trace (run ~num_cycles:12));
  [%expect {|
    ||||||||||||
    .|.|.|.|.|.|
    ..|..|..|..|
    |}]
;;

(* The rejection message itself, not just the fact of a rejection. Zero is the case worth
   reading: [Int.ceil_log2] would otherwise raise about its own argument, from inside
   Core, with nothing in the message tying it to a frequency. *)
let%expect_test "illegal frequencies are rejected by name" =
  List.iter illegal_clk_freqs ~f:(fun clk_freq ->
    match elaborate clk_freq with
    | Ok () -> printf "%5d  accepted\n" clk_freq
    | Error error -> printf "%5d  %s\n" clk_freq (Error.to_string_mach error));
  [%expect
    {|
    -100  ("Second_pulse.create: clk_freq must be at least 1"(clk_freq -100))
      -4  ("Second_pulse.create: clk_freq must be at least 1"(clk_freq -4))
      -1  ("Second_pulse.create: clk_freq must be at least 1"(clk_freq -1))
       0  ("Second_pulse.create: clk_freq must be at least 1"(clk_freq 0))
    |}]
;;
