(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_crc_expect_tests.ml" *)

(* Expect Test Suite: Rx_crc

   Golden cycle traces of the accumulator walking a short frame and its FCS. Printed in
   hex, because every constant that matters here - 0xFFFFFFFF at reload, 0xDEBB20E3 at the
   residue - is quoted in hex by the standard and by the RTL.

   Long frames belong in the Quickcheck suite; four bytes and their FCS is enough to see
   the whole shape.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Rx_crc_testbench

let payload = [ 0xDE; 0xAD; 0xBE; 0xEF ]

let%expect_test "accumulator walks a frame and lands on the residue" =
  let trace =
    Testbench.run_bytes_with_fcs payload ~fcs:(Crc32.fcs_bytes payload)
    |> List.map ~f:Testbench.compact
  in
  print_s [%sexp (trace : Compact_observation.t list)];
  [%expect
    {|
    (((phase (Payload 0)) (byte 0xde) (crc_out 0x4c96efa1) (crc_valid false))
     ((phase (Payload 1)) (byte 0xad) (crc_out 0x09fadac4) (crc_valid false))
     ((phase (Payload 2)) (byte 0xbe) (crc_out 0xb0d962f8) (crc_valid false))
     ((phase (Payload 3)) (byte 0xef) (crc_out 0x83635ca5) (crc_valid false))
     ((phase (Fcs 0)) (byte 0x5a) (crc_out 0x2d818cd1) (crc_valid false))
     ((phase (Fcs 1)) (byte 0xa3) (crc_out 0xbe26919c) (crc_valid false))
     ((phase (Fcs 2)) (byte 0x9c) (crc_out 0x00be2691) (crc_valid false))
     ((phase (Fcs 3)) (byte 0x7c) (crc_out 0xdebb20e3) (crc_valid true)))
    |}]
;;

let%expect_test "a corrupted final byte never reaches the residue" =
  let corrupted = [ 0xDE; 0xAD; 0xBE; 0xEE ] in
  let trace =
    Testbench.run_bytes_with_fcs corrupted ~fcs:(Crc32.fcs_bytes payload)
    |> List.map ~f:Testbench.compact
  in
  print_s [%sexp (trace : Compact_observation.t list)];
  [%expect
    {|
    (((phase (Payload 0)) (byte 0xde) (crc_out 0x4c96efa1) (crc_valid false))
     ((phase (Payload 1)) (byte 0xad) (crc_out 0x09fadac4) (crc_valid false))
     ((phase (Payload 2)) (byte 0xbe) (crc_out 0xb0d962f8) (crc_valid false))
     ((phase (Payload 3)) (byte 0xee) (crc_out 0xf4646c33) (crc_valid false))
     ((phase (Fcs 0)) (byte 0x5a) (crc_out 0x349abd90) (crc_valid false))
     ((phase (Fcs 1)) (byte 0xa3) (crc_out 0xbfe4fbab) (crc_valid false))
     ((phase (Fcs 2)) (byte 0x9c) (crc_out 0xb80241f4) (crc_valid false))
     ((phase (Fcs 3)) (byte 0x7c) (crc_out 0xe3db0953) (crc_valid false)))
    |}]
;;

let%expect_test "dropping en reloads the accumulator" =
  let after_disabled_cycle, observations =
    Testbench.run_enable_drop
      ~discarded:[ 0x11; 0x22 ]
      ~bytes:payload
      ~fcs:(Crc32.fcs_bytes payload)
  in
  let trace = List.map observations ~f:Testbench.compact in
  let after_disabled_cycle = sprintf "0x%08x" after_disabled_cycle.crc_out in
  print_s [%sexp { after_disabled_cycle : string; trace : Compact_observation.t list }];
  [%expect
    {|
    ((after_disabled_cycle 0xffffffff)
     (trace
      (((phase (Payload 0)) (byte 0xde) (crc_out 0x4c96efa1) (crc_valid false))
       ((phase (Payload 1)) (byte 0xad) (crc_out 0x09fadac4) (crc_valid false))
       ((phase (Payload 2)) (byte 0xbe) (crc_out 0xb0d962f8) (crc_valid false))
       ((phase (Payload 3)) (byte 0xef) (crc_out 0x83635ca5) (crc_valid false))
       ((phase (Fcs 0)) (byte 0x5a) (crc_out 0x2d818cd1) (crc_valid false))
       ((phase (Fcs 1)) (byte 0xa3) (crc_out 0xbe26919c) (crc_valid false))
       ((phase (Fcs 2)) (byte 0x9c) (crc_out 0x00be2691) (crc_valid false))
       ((phase (Fcs 3)) (byte 0x7c) (crc_out 0xdebb20e3) (crc_valid true)))))
    |}]
;;

let%expect_test "a valid gap holds the accumulator" =
  let before_gap, during_gap, after_gap =
    Testbench.run_valid_gap ~before:[ 0xDE; 0xAD; 0xBE ] ~after:[ 0xEF ]
  in
  let hex (snapshot : Output_snapshot.t) = sprintf "0x%08x" snapshot.crc_out in
  print_s
    [%sexp
      { before_gap = (hex before_gap : string)
      ; during_gap = (hex during_gap : string)
      ; after_gap = (hex after_gap : string)
      }];
  [%expect {| ((before_gap 0xb0d962f8) (during_gap 0xb0d962f8) (after_gap 0x83635ca5)) |}]
;;
