(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "shared_primitives_expect_tests.ml" *)
(* Compact golden examples for the Phase-1 combinational primitives. *)

open! Core
open! Hardcaml_verif
open! Shared_primitives_testbench

let%expect_test "check vector split across masked words" =
  let first =
    Testbench.run
      ~crc:Crc32.init
      ~bytes:[ 0x31; 0x32; 0x33; 0x34; 0x35; 0x36; 0x37; 0x38 ]
      ~valid_bytes:0xff
      ~keep:0xff
      ~last:false
  in
  let second =
    Testbench.run
      ~crc:first.next_crc
      ~bytes:[ 0x39 ]
      ~valid_bytes:0x01
      ~keep:0x01
      ~last:true
  in
  printf
    "first_crc=0x%08x final_crc=0x%08x fcs=0x%08x\n"
    first.next_crc
    second.next_crc
    (second.next_crc lxor 0xffffffff);
  print_s [%sexp (second : Output_snapshot.t)];
  [%expect
    {|
    first_crc=0x651f2550 final_crc=0x340bc6d9 fcs=0xcbf43926
    ((next_crc 873187033) (next_fcs 3421780262) (valid_residue false)
     (keep_contiguous true) (keep_count 1) (keep_round_trip 1) (beat_legal true))
    |}]
;;
