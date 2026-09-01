(* University of Florida *)
(* Author: Bohdan Purtell *)

open! Core
open! Tx_testbench

let%expect_test "short-frame TX phase trace" =
  Testbench.run (Beat.of_frame (List.range 0 14))
  |> List.filter_mapi ~f:(fun cycle snapshot ->
    if cycle < 4 || snapshot.Snapshot.control <> 0xff || snapshot.state <> 0
    then Some (cycle, snapshot.state, snapshot.control, snapshot.frames, snapshot.bytes)
    else None)
  |> List.iter ~f:(fun row -> print_s [%sexp (row : int * int * int * int * int)]);
  [%expect
    {|
    (0 0 255 0 0)
    (1 0 255 0 0)
    (2 0 1 0 0)
    (3 1 0 0 0)
    (4 1 0 0 0)
    (5 2 0 0 0)
    (6 2 0 0 0)
    (7 2 0 0 0)
    (8 2 0 0 0)
    (9 2 0 0 0)
    (10 2 0 0 0)
    (11 3 255 0 0)
    (12 4 255 1 64)
    (13 4 255 1 64)
    |}]
;;
