(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Expect Test Suite: Pcs_tx_scrambler

   Reviewable trace of a continuous idle stream starting from the reset seed.
*)

open! Core
open! Pcs_tx_scrambler_testbench

let%expect_test "continuous idle payload changes every block" =
  let testbench = Testbench.create () in
  Testbench.reset testbench;
  for block = 0 to 3 do
    let scrambled =
      Testbench.scramble_hex testbench ~payload:"000000000000001e" ~header:0b01
    in
    print_s [%sexp (block : int), (Scrambled.printable scrambled : Sexp.t)]
  done;
  [%expect
    {|
    (0 ((data 64'h7bfff0800000001e) (header 1)))
    (1 ((data 64'h85cff0fffff8401e) (header 1)))
    (2 ((data 64'h85d84f0118079ee1) (header 1)))
    (3 ((data 64'h85d815fee8479ee9) (header 1)))
    |}]
;;
