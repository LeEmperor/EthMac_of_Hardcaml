(*
  University of Florida
  Author: Bohdan Purtell

  Expect Test Suite: ""


*)

(* Golden-output tests for cycle-by-cycle behavior that is useful to read as a trace. *)

open! Core
open! Rx_byte_assembler_testbench

let%expect_test "assembles 0xAB low nibble first" =
  let observations = Testbench.run_bytes [ 0xAB ] in
  print_s [%sexp (observations : Observation.t list)];
  [%expect
    {|
    (((valid_after_low_nibble false) (valid_after_high_nibble true)
      (completed_byte (171))))
    |}]
;;

let%expect_test "disabled cycles do not consume nibbles" =
  let snapshots = Testbench.run_with_disabled_cycles 0xAB [ 0x1; 0x2; 0x3 ] in
  print_s [%sexp (snapshots : Output_snapshot.t list)];
  [%expect
    {|
    (((byte_out 11) (byte_valid false)) ((byte_out 11) (byte_valid false))
     ((byte_out 11) (byte_valid false)) ((byte_out 11) (byte_valid false))
     ((byte_out 171) (byte_valid true)))
    |}]
;;

let%expect_test "reset discards a partial byte" =
  let snapshots = Testbench.run_reset_mid_byte ~discarded_low:0xA ~byte:0xDC in
  print_s [%sexp (snapshots : Output_snapshot.t list)];
  [%expect
    {|
    (((byte_out 10) (byte_valid false)) ((byte_out 0) (byte_valid false))
     ((byte_out 12) (byte_valid false)) ((byte_out 220) (byte_valid true)))
    |}]
;;
