(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Expect Test Suite: Pcs_tx_encoder

   Compact, reviewable output for canonical data, idle, start, and ordered-set blocks.
*)

open! Core
open! Pcs_tx_encoder_testbench

let print_encoding testbench name lanes =
  let encoded = Testbench.encode testbench lanes in
  print_s [%sexp (name : string), (Encoded.printable encoded : Sexp.t)]
;;

let%expect_test "canonical Clause 49 mappings" =
  let testbench = Testbench.create () in
  print_encoding
    testbench
    "data"
    (List.map [ 0x00; 0x11; 0x22; 0x33; 0x44; 0x55; 0x66; 0x77 ] ~f:Lane.data);
  print_encoding testbench "idle" (List.init 8 ~f:(fun _ -> Lane.idle));
  print_encoding
    testbench
    "start-0"
    (Lane.start :: List.map [ 0x11; 0x22; 0x33; 0x44; 0x55; 0x66; 0x77 ] ~f:Lane.data);
  print_encoding
    testbench
    "ordered-sets"
    [ Lane.ordered_set
    ; Lane.data 0x00
    ; Lane.data 0x00
    ; Lane.data 0x01
    ; Lane.ordered_set
    ; Lane.data 0x00
    ; Lane.data 0x00
    ; Lane.data 0x01
    ];
  [%expect
    {|
    (data ((payload 64'h7766554433221100) (header 2) (bad_xgmii false)))
    (idle ((payload 64'h000000000000001e) (header 1) (bad_xgmii false)))
    (start-0 ((payload 64'h7766554433221178) (header 1) (bad_xgmii false)))
    (ordered-sets ((payload 64'h0100000001000055) (header 1) (bad_xgmii false)))
    |}]
;;
