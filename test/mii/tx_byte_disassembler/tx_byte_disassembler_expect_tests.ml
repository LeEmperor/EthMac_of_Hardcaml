(* 
   University of Florida 
   Author: Bohdan Purtell

   Expect Test Suite: Tx_byte_disassembler

   Readable transaction traces showing the two active output beats and the idle state
   after the high nibble.

   Highkey really don't know what else to test here lmao. Maybe rst assertion midway or random byte drops?
*)

open! Core
open! Tx_byte_disassembler_testbench

let%expect_test "disassembles 0xab low nibble first" =
  let observations = Testbench.run_bytes [ 0xAB ] in
  print_s [%sexp (observations : Observation.t list)];
  [%expect
    {|
    (((ready_during_lo true) (ready_during_hi false) (tx_en_during_lo true)
      (tx_en_during_hi true) (lo_nibble (11)) (hi_nibble (10))
      (after_hi ((ready true) (tx_en false) (tx_d 0)))))
    |}]
;;

let%expect_test "disassembles bytes back-to-back" =
  let observations = Testbench.run_bytes [ 0x12; 0xF0 ] in
  print_s [%sexp (observations : Observation.t list)];
  [%expect
    {|
    (((ready_during_lo true) (ready_during_hi false) (tx_en_during_lo true)
      (tx_en_during_hi true) (lo_nibble (2)) (hi_nibble (1))
      (after_hi ((ready true) (tx_en false) (tx_d 0))))
     ((ready_during_lo true) (ready_during_hi false) (tx_en_during_lo true)
      (tx_en_during_hi true) (lo_nibble (0)) (hi_nibble (15))
      (after_hi ((ready true) (tx_en false) (tx_d 0)))))
    |}]
;;
