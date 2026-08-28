(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_crc_expect_tests.ml" *)

(* Expect Test Suite: Tx_crc

   Golden traces of the accumulator over a short payload and of the [byte_sel] mux walking
   the four FCS bytes out in transmission order. Printed in hex so the emitted bytes line
   up visually with the FCS word above them.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Tx_crc_testbench

let payload = [ 0xDE; 0xAD; 0xBE; 0xEF ]
let hex_bytes bytes = List.map bytes ~f:(sprintf "0x%02x")
let compact_trace observations = List.map observations ~f:Testbench.compact

let%expect_test "byte_sel mux ordering across the four FCS bytes" =
  let trace = compact_trace (Testbench.run_bytes_then_read_fcs payload) in
  print_s [%sexp (trace : Compact_observation.t list)];
  [%expect
    {|
    (((phase (Payload 0)) (byte 0xde) (crc_out 0x4c96efa1) (fcs_byte 0x5e))
     ((phase (Payload 1)) (byte 0xad) (crc_out 0x09fadac4) (fcs_byte 0x3b))
     ((phase (Payload 2)) (byte 0xbe) (crc_out 0xb0d962f8) (fcs_byte 0x07))
     ((phase (Payload 3)) (byte 0xef) (crc_out 0x83635ca5) (fcs_byte 0x5a))
     ((phase (Fcs_read 0)) (byte 0x00) (crc_out 0x83635ca5) (fcs_byte 0x5a))
     ((phase (Fcs_read 1)) (byte 0x00) (crc_out 0x83635ca5) (fcs_byte 0xa3))
     ((phase (Fcs_read 2)) (byte 0x00) (crc_out 0x83635ca5) (fcs_byte 0x9c))
     ((phase (Fcs_read 3)) (byte 0x00) (crc_out 0x83635ca5) (fcs_byte 0x7c)))
    |}]
;;

let%expect_test "the standard check vector emits 26 39 f4 cb" =
  let check_vector = [ 0x31; 0x32; 0x33; 0x34; 0x35; 0x36; 0x37; 0x38; 0x39 ] in
  let observations = Testbench.run_bytes_then_read_fcs check_vector in
  let final =
    Testbench.final_snapshot (List.take observations (List.length check_vector))
  in
  print_s
    [%sexp
      { crc_out = (sprintf "0x%08x" final.crc_out : string)
      ; fcs_word = (sprintf "0x%08x" (final.crc_out lxor 0xFFFFFFFF) : string)
      ; emitted_bytes = (hex_bytes (Testbench.fcs_bytes_of observations) : string list)
      }];
  [%expect
    {|
    ((crc_out 0x340bc6d9) (fcs_word 0xcbf43926)
     (emitted_bytes (0x26 0x39 0xf4 0xcb)))
    |}]
;;

let%expect_test "dropping en reloads the accumulator" =
  let after_disabled_cycle, observations =
    Testbench.run_enable_drop ~discarded:[ 0x11; 0x22 ] ~bytes:payload
  in
  print_s
    [%sexp
      { after_disabled_cycle = (sprintf "0x%08x" after_disabled_cycle.crc_out : string)
      ; emitted_bytes = (hex_bytes (Testbench.fcs_bytes_of observations) : string list)
      }];
  [%expect
    {| ((after_disabled_cycle 0xffffffff) (emitted_bytes (0x5a 0xa3 0x9c 0x7c))) |}]
;;
