(* 
   University of Florida 
   Author: Bohdan Purtell

   Expect Test Suite: Tx_byte_disassembler

   Readable transaction traces showing the two active output beats and the idle state
   after the high nibble.

   Control-path traces cover reset interruption and a valid pulse while the DUT is busy.
*)

open! Core
open! Tx_byte_disassembler_testbench

(* test 1 : johnathan *)
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

(* test 2 : ashley *)
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

(* test 3 : ashleight *)
let%expect_test "reset while busy discards the interrupted byte" =
  let observation =
    Testbench.run_reset_while_busy ~interrupted_byte:0xAB ~following_byte:0x3C
  in
  print_s [%sexp (observation : Reset_while_busy_observation.t)];
  [%expect
    {|
    ((low_before_reset ((ready true) (tx_en true) (tx_d 11)))
     (after_reset ((ready true) (tx_en false) (tx_d 0)))
     (following_byte
      ((ready_during_lo true) (ready_during_hi false) (tx_en_during_lo true)
       (tx_en_during_hi true) (lo_nibble (12)) (hi_nibble (3))
       (after_hi ((ready true) (tx_en false) (tx_d 0))))))
    |}]
;;

(* test 4 : donald *)
let%expect_test "a valid pulse while busy is ignored" =
  let observation =
    Testbench.run_valid_pulse_while_busy
      ~accepted_byte:0xAB
      ~offered_while_busy:0x4D
  in
  print_s [%sexp (observation : Busy_valid_observation.t)];
  [%expect
    {|
    ((accepted_low ((ready true) (tx_en true) (tx_d 11)))
     (accepted_high_while_busy ((ready false) (tx_en true) (tx_d 10)))
     (after_busy_pulse ((ready true) (tx_en false) (tx_d 0))))
    |}]
;;
