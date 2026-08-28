(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_controller_expect_tests.ml" *)

(* Expect Test Suite: Rx_controller

   Golden cycle traces for the receive-header state sequence and recovery behavior. Only
   asserted outputs are printed, keeping the traces readable while retaining the exact
   state/register-enable timing.

   I love the tuareg and expect test thingy in Emacs. Second most boring thing is pasting
   expect sexps - close first of PCB-level validation; rip Vignesh.
*)

open! Core
open! Rx_controller_testbench

let compact_snapshot output = Testbench.active_outputs output

let%expect_test "walks from preamble through payload" =
  let frame =
    Frame.create
      ~preamble_length:2
      ~destination_mac:[ 0x00; 0x11; 0x22; 0x33; 0x44; 0x55 ]
      ~source_mac:[ 0x66; 0x77; 0x88; 0x99; 0xAA; 0xBB ]
      ~eth_type:[ 0x08; 0x00 ]
      ~payload:[ 0xDE; 0xAD ]
      ()
  in
  let trace = List.map (Testbench.run_frame frame) ~f:Testbench.compact in
  print_s [%sexp (trace : Compact_observation.t list)];
  [%expect
    {|
    (((phase (Preamble 0)) (byte 85)
      (active_outputs (byte_assembler_en fcs_present in_preamble)))
     ((phase (Preamble 1)) (byte 85)
      (active_outputs (byte_assembler_en in_preamble)))
     ((phase Sfd) (byte 213) (active_outputs (byte_assembler_en in_dst_mac)))
     ((phase (Destination_mac 0)) (byte 0)
      (active_outputs (byte_assembler_en dst_mac_reg_en in_dst_mac)))
     ((phase (Destination_mac 1)) (byte 17)
      (active_outputs (byte_assembler_en dst_mac_reg_en in_dst_mac)))
     ((phase (Destination_mac 2)) (byte 34)
      (active_outputs (byte_assembler_en dst_mac_reg_en in_dst_mac)))
     ((phase (Destination_mac 3)) (byte 51)
      (active_outputs (byte_assembler_en dst_mac_reg_en in_dst_mac)))
     ((phase (Destination_mac 4)) (byte 68)
      (active_outputs (byte_assembler_en dst_mac_reg_en in_dst_mac)))
     ((phase (Destination_mac 5)) (byte 85)
      (active_outputs (byte_assembler_en dst_mac_reg_en)))
     ((phase (Source_mac 0)) (byte 102)
      (active_outputs (byte_assembler_en src_mac_reg_en)))
     ((phase (Source_mac 1)) (byte 119)
      (active_outputs (byte_assembler_en src_mac_reg_en)))
     ((phase (Source_mac 2)) (byte 136)
      (active_outputs (byte_assembler_en src_mac_reg_en)))
     ((phase (Source_mac 3)) (byte 153)
      (active_outputs (byte_assembler_en src_mac_reg_en)))
     ((phase (Source_mac 4)) (byte 170)
      (active_outputs (byte_assembler_en src_mac_reg_en)))
     ((phase (Source_mac 5)) (byte 187)
      (active_outputs (byte_assembler_en src_mac_reg_en)))
     ((phase (Eth_type 0)) (byte 8)
      (active_outputs (byte_assembler_en eth_type_reg_en)))
     ((phase (Eth_type 1)) (byte 0)
      (active_outputs
       (byte_assembler_en eth_type_reg_en payload_sel emit_payload in_payload)))
     ((phase (Payload 0)) (byte 222)
      (active_outputs (byte_assembler_en payload_sel emit_payload in_payload)))
     ((phase (Payload 1)) (byte 173)
      (active_outputs (byte_assembler_en payload_sel emit_payload in_payload))))
    |}]
;;

let%expect_test "an invalid cycle pauses the destination counter" =
  let observation = Testbench.run_destination_pause () in
  let compact =
    [ "before_pause", compact_snapshot observation.before_pause
    ; "during_pause", compact_snapshot observation.during_pause
    ; ( "after_sixth_destination_byte"
      , compact_snapshot observation.after_sixth_destination_byte )
    ; "after_first_source_byte", compact_snapshot observation.after_first_source_byte
    ]
  in
  print_s [%sexp (compact : (string * string list) list)];
  [%expect
    {|
    ((before_pause (byte_assembler_en dst_mac_reg_en in_dst_mac))
     (during_pause (byte_assembler_en dst_mac_reg_en in_dst_mac))
     (after_sixth_destination_byte (byte_assembler_en dst_mac_reg_en))
     (after_first_source_byte (byte_assembler_en src_mac_reg_en)))
    |}]
;;

let%expect_test "reset discards a partial header" =
  let observation = Testbench.run_reset_mid_frame () in
  let compact =
    [ "before_reset", compact_snapshot observation.before_reset
    ; "after_reset", compact_snapshot observation.after_reset
    ; "after_next_preamble", compact_snapshot observation.after_next_preamble
    ]
  in
  print_s [%sexp (compact : (string * string list) list)];
  [%expect
    {|
    ((before_reset (byte_assembler_en dst_mac_reg_en in_dst_mac))
     (after_reset ())
     (after_next_preamble (byte_assembler_en fcs_present in_preamble)))
    |}]
;;

let%expect_test "rx_er aborts payload processing" =
  let observation = Testbench.run_payload_error () in
  let compact =
    [ "before_error", compact_snapshot observation.before_error
    ; "after_error", compact_snapshot observation.after_error
    ; "following_idle_cycle", compact_snapshot observation.following_idle_cycle
    ]
  in
  print_s [%sexp (compact : (string * string list) list)];
  [%expect
    {|
    ((before_error
      (byte_assembler_en eth_type_reg_en payload_sel emit_payload in_payload))
     (after_error (byte_assembler_en)) (following_idle_cycle ()))
    |}]
;;
