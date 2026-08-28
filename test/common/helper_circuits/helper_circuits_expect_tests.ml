(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "helper_circuits_expect_tests.ml" *)

(* Expect Test Suite: Helper_circuits

   Golden waveform tables: one labelled row per output, one column per cycle. These
   helpers are read far more often than they are changed, and the thing a reader needs is
   the alignment between the input row and the rows below it - which cycle the pulse lands
   on, and how far the delayed copies trail. A sexp list of records does not show that;
   stacked rows do.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Helper_circuits_testbench

let row label values =
  printf
    "%-16s %s\n"
    label
    (String.of_list (List.map values ~f:(fun value -> if value then '#' else '.')))
;;

let print_waves observations =
  row "x" (List.map observations ~f:Observation.x);
  List.iter output_names ~f:(fun (name, get) -> row name (List.map observations ~f:get))
;;

let%expect_test "a square wave through every helper" =
  print_waves
    (Testbench.run_stimuli (Testbench.square_wave ~half_period:3 ~num_periods:3));
  [%expect
    {|
    x                ...###...###...###
    rising           ...#.....#.....#..
    falling          ......#.....#.....
    delayed_0        ...###...###...###
    delayed_1        ....###...###...##
    delayed_n        ......###...###...
    rising_delayed   .....#.....#.....#
    falling_delayed  ........#.....#...
    |}]
;;

let%expect_test "a single-cycle pulse" =
  print_waves (Testbench.run_stimuli [ false; false; true; false; false; false; false ]);
  [%expect
    {|
    x                ..#....
    rising           ..#....
    falling          ...#...
    delayed_0        ..#....
    delayed_1        ...#...
    delayed_n        .....#.
    rising_delayed   ....#..
    falling_delayed  .....#.
    |}]
;;

let%expect_test "an input already high out of clear" =
  print_waves (Testbench.run_stimuli [ true; true; true; false; false ]);
  [%expect
    {|
    x                ###..
    rising           #....
    falling          ...#.
    delayed_0        ###..
    delayed_1        .###.
    delayed_n        ...##
    rising_delayed   ..#..
    falling_delayed  .....
    |}]
;;

let%expect_test "a clear mid-stream flushes the delay line" =
  (* The empty [falling_delayed] row is the interesting part. [x] falls on the clear
     cycle, so [falling] is high during it - but the two-deep delay chain behind it is
     cleared on that same edge, and the edge never propagates. A clear does not merely
     flush the line, it cancels whatever the detectors produced on the cycle it lands on.
     Consumers that reset mid-frame get no delayed edge for the transition the reset
     itself caused. *)
  print_waves
    (Testbench.run_with_clear
       ~xs_before:[ true; true; true; true ]
       ~xs_after:[ false; true; true; false; false ]);
  [%expect
    {|
    x                ####..##..
    rising           #.....#...
    falling          ....#...#.
    delayed_0        ####..##..
    delayed_1        .####..##.
    delayed_n        ...##....#
    rising_delayed   ..#.....#.
    falling_delayed  ..........
    |}]
;;

let%expect_test "the observation record, in full, across one edge" =
  print_s
    [%sexp
      (List.map (Testbench.run_stimuli [ false; true; false ]) ~f:compact
       : Compact_observation.t list)];
  [%expect
    {|
    (((x false) (active_outputs ()))
     ((x true) (active_outputs (rising delayed_0)))
     ((x false) (active_outputs (falling delayed_1))))
    |}]
;;

let%expect_test "hi16 and lo16 split a word, most significant byte first" =
  List.iter
    (Testbench.run_words [ 0x0000; 0x00FF; 0xFF00; 0x1234; 0xABCD ])
    ~f:(fun w ->
      printf "0x%04x -> %02x %02x\n" w.Word_observation.word w.hi_byte w.lo_byte);
  [%expect
    {|
    0x0000 -> 00 00
    0x00ff -> 00 ff
    0xff00 -> ff 00
    0x1234 -> 12 34
    0xabcd -> ab cd
    |}]
;;

let%expect_test "a clear coincident with an edge, against its clear-free control" =
  (* RTL-6, in the form that shows the mechanism. The clear lands on the cycle the input
     changes, and the pair of runs is what makes the two rows readable: with no clear the
     delayed pulse trails the detector by [edge_delay_depth] in both directions, and with
     one the rise arrives a cycle later than the control while the fall never arrives at
     all.

     The reason for the asymmetry is in the [rising] row: the clear zeroes the detector's
     history register, so on the following cycle the still-high input is compared against
     zero and reads as a second rise, which the delay chain carries normally. A fall has
     no such second chance - a low input against a zeroed history is not an edge - so it
     is the direction that is genuinely lost. *)
  let show label stimulus =
    printf "%s\n" label;
    print_waves (Testbench.run_scheduled stimulus)
  in
  show
    "rise, clear on the rise cycle"
    (Testbench.edge_with_clear ~before:false ~after:true ~clear:true ~tail:5);
  show
    "rise, no clear"
    (Testbench.edge_with_clear ~before:false ~after:true ~clear:false ~tail:5);
  show
    "fall, clear on the fall cycle"
    (Testbench.edge_with_clear ~before:true ~after:false ~clear:true ~tail:5);
  show
    "fall, no clear"
    (Testbench.edge_with_clear ~before:true ~after:false ~clear:false ~tail:5);
  [%expect
    {|
    rise, clear on the rise cycle
    x                ..######
    rising           ..##....
    falling          ........
    delayed_0        ..######
    delayed_1        ....####
    delayed_n        ......##
    rising_delayed   .....#..
    falling_delayed  ........
    rise, no clear
    x                ..######
    rising           ..#.....
    falling          ........
    delayed_0        ..######
    delayed_1        ...#####
    delayed_n        .....###
    rising_delayed   ....#...
    falling_delayed  ........
    fall, clear on the fall cycle
    x                ##......
    rising           #.......
    falling          ..#.....
    delayed_0        ##......
    delayed_1        .##.....
    delayed_n        ........
    rising_delayed   ..#.....
    falling_delayed  ........
    fall, no clear
    x                ##......
    rising           #.......
    falling          ..#.....
    delayed_0        ##......
    delayed_1        .##.....
    delayed_n        ...##...
    rising_delayed   ..#.....
    falling_delayed  ....#...
    |}]
;;
