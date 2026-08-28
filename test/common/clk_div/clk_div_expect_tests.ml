(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "clk_div_expect_tests.ml" *)

(* Expect Test Suite: Clk_div

   Golden traces of the divided clock as a waveform-shaped string, one character per
   cycle. The divide-by-four period, the freeze under [en] low, and the restart under
   [rst] are all things you should be able to read off the row rather than decode from a
   list of booleans.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Clk_div_testbench

(* One character per driven cycle, so a golden reads like a waveform row. *)
let print_trace observations =
  print_endline
    (String.of_list
       (List.map observations ~f:(fun (observation : Observation.t) ->
          if observation.dst_clk then '_' else '.')))
;;

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
