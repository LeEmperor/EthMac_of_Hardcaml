(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_datapath_expect_tests.ml" *)

(* Expect Test Suite: Rx_datapath

   Golden traces of the header-register capture and of the four-deep FCS-strip pipeline.
   One row per assembled byte, sampled on the cycle [raw_byte_out_valid] is high, which is
   the cycle [mac_top] gates its FIFO write on.

   The pipeline trace is the readable form of the whole mechanism: the first four rows
   emit nothing while the pipeline fills, then payload bytes come out four behind the
   bytes going in, and the four FCS bytes are still in flight when [emit_payload] drops.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Rx_datapath_testbench

let%expect_test "ethertype shifts in MSB-first" =
  let trace, eth_type = Testbench.run_eth_type_capture [ 0x45; 0x21 ] in
  let latched_eth_type = sprintf "0x%04x" eth_type in
  let trace = List.map trace ~f:Testbench.compact in
  print_s [%sexp { trace : Compact_observation.t list; latched_eth_type : string }];
  [%expect
    {|
    ((trace
      (((phase (Eth_type 0)) (byte_in 0x45) (raw_byte_out 0x45)
        (payload_out 0x00) (emitted false))
       ((phase (Eth_type 1)) (byte_in 0x21) (raw_byte_out 0x21)
        (payload_out 0x00) (emitted false))))
     (latched_eth_type 0x4521))
    |}]
;;

let%expect_test "the FCS-strip pipeline emits the payload and swallows the FCS" =
  let observation =
    Testbench.run_payload_strip
      ~payload:[ 0x11; 0x22; 0x33; 0x44; 0x55 ]
      ~fcs:[ 0xAA; 0xBB; 0xCC; 0xDD ]
  in
  let trace = List.map observation.trace ~f:Testbench.compact in
  let collected = List.map observation.collected ~f:(sprintf "0x%02x") in
  print_s [%sexp { trace : Compact_observation.t list; collected : string list }];
  [%expect
    {|
    ((trace
      (((phase (Payload 0)) (byte_in 0x11) (raw_byte_out 0x11) (payload_out 0x00)
        (emitted false))
       ((phase (Payload 1)) (byte_in 0x22) (raw_byte_out 0x22) (payload_out 0x00)
        (emitted false))
       ((phase (Payload 2)) (byte_in 0x33) (raw_byte_out 0x33) (payload_out 0x00)
        (emitted false))
       ((phase (Payload 3)) (byte_in 0x44) (raw_byte_out 0x44) (payload_out 0x00)
        (emitted false))
       ((phase (Payload 4)) (byte_in 0x55) (raw_byte_out 0x55) (payload_out 0x11)
        (emitted true))
       ((phase (Fcs 0)) (byte_in 0xaa) (raw_byte_out 0xaa) (payload_out 0x22)
        (emitted true))
       ((phase (Fcs 1)) (byte_in 0xbb) (raw_byte_out 0xbb) (payload_out 0x33)
        (emitted true))
       ((phase (Fcs 2)) (byte_in 0xcc) (raw_byte_out 0xcc) (payload_out 0x44)
        (emitted true))
       ((phase (Fcs 3)) (byte_in 0xdd) (raw_byte_out 0xdd) (payload_out 0x55)
        (emitted true))
       ((phase (Drain 0)) (byte_in 0x00) (raw_byte_out 0x00) (payload_out 0x00)
        (emitted false))
       ((phase (Drain 1)) (byte_in 0x00) (raw_byte_out 0x00) (payload_out 0x00)
        (emitted false))
       ((phase (Drain 2)) (byte_in 0x00) (raw_byte_out 0x00) (payload_out 0x00)
        (emitted false))
       ((phase (Drain 3)) (byte_in 0x00) (raw_byte_out 0x00) (payload_out 0x00)
        (emitted false))
       ((phase (Drain 4)) (byte_in 0x00) (raw_byte_out 0x00) (payload_out 0x00)
        (emitted false))
       ((phase (Drain 5)) (byte_in 0x00) (raw_byte_out 0x00) (payload_out 0x00)
        (emitted false))))
     (collected (0x11 0x22 0x33 0x44 0x55)))
    |}]
;;
