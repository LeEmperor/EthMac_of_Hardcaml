(*
  University of Florida
  Author: Bohdan Purtell

  Expect Test Suite: Rx_byte_assembler

  Golden-output tests presenting cycle-by-cycle behavior as readable traces,
  including disabled cycles and reset during a partially assembled byte.

  Tags: { "ACTIVE"
          ; "TEST"
          ; "EXPECT_TEST"
          ; "EXPECT"
          ; "PERSONAL_REFERENCE"
        }
*)

open! Core
open! Rx_byte_assembler_testbench

(* basic example *)
let%expect_test "assembles 0xAB low nibble first" =
  let observations = Testbench.run_bytes [ 0xAB ] in
  print_s [%sexp (observations : Observation.t list)]; (* ppx is so cool *)
  (* one day i will have the courage to use emacs ocaml mode lmao *)
  [%expect
    {|
    (((valid_after_low_nibble false) (valid_after_high_nibble true)
      (completed_byte (171))))
    |}]
  (* God I hate these expect sexps: running the test and then pasting in the output is actually kinda goated; i think theres a tool that does that called like tuareg or something but it doesn't have a neovim port (i haven't looked); might have to write one myself *)
;;

(* null test *)
let%expect_test "disabled cycles do not consume nibbles" =
  let snapshots = Testbench.run_with_disabled_cycles 0xAB [ 0x1; 0x2; 0x3 ] in
  print_s [%sexp (snapshots : Output_snapshot.t list)]; (* introduction of snapshot concept that is essentially and unfinished Observation *)
  [%expect
    {|
    (((byte_out 11) (byte_valid false)) ((byte_out 11) (byte_valid false))
     ((byte_out 11) (byte_valid false)) ((byte_out 11) (byte_valid false))
     ((byte_out 171) (byte_valid true)))
    |}]
;;

(* reset sees partial *)
let%expect_test "reset discards a partial byte" =
  let snapshots = Testbench.run_reset_mid_byte ~discarded_low:0xA ~byte:0xDC in
  print_s [%sexp (snapshots : Output_snapshot.t list)];
  [%expect
    {|
    (((byte_out 10) (byte_valid false)) ((byte_out 0) (byte_valid false))
     ((byte_out 12) (byte_valid false)) ((byte_out 220) (byte_valid true)))
    |}]
;;
