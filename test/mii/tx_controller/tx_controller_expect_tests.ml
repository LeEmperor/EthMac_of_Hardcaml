(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_controller_expect_tests.ml" *)

(* Expect Test Suite: Tx_controller

   Golden traces of the transmit state walk and its control lines. Only asserted outputs
   are printed, which keeps the traces readable while retaining the exact state and
   counter timing - the same compaction [rx_controller]'s suite uses.

   A whole frame is 60-plus cycles even at the minimum payload, which is not a reviewable
   diff, so the full walk is summarized as cycles-per-state and only the interesting
   stretches are printed cycle by cycle: the launch handshake, the header counters, and
   the pad-to-FCS handover.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Tx_controller_testbench

let print_trace trace =
  print_s [%sexp (List.map trace ~f:Testbench.compact : Compact_observation.t list)]
;;

let%expect_test "cycles per state across three payload lengths" =
  let summary payload_length =
    payload_length, Testbench.cycles_per_state (Testbench.run_frame ~payload_length)
  in
  let summaries = List.map [ 1; 46; 100 ] ~f:summary in
  print_s [%sexp (summaries : (int * (State.t * int) list) list)];
  [%expect
    {|
    ((1
      ((Idle 2) (Preamble 7) (Sfd 1) (Dst_mac 6) (Src_mac 6) (Eth_type 2)
       (Payload 46) (Fcs 4)))
     (46
      ((Idle 2) (Preamble 7) (Sfd 1) (Dst_mac 6) (Src_mac 6) (Eth_type 2)
       (Payload 46) (Fcs 4)))
     (100
      ((Idle 2) (Preamble 7) (Sfd 1) (Dst_mac 6) (Src_mac 6) (Eth_type 2)
       (Payload 100) (Fcs 4))))
    |}]
;;

let%expect_test "launch handshake through the SFD" =
  let observation = Testbench.run_frame ~payload_length:46 in
  (* The launch cycle plus the seven preamble bytes and the SFD. *)
  print_trace (List.take observation.trace 9);
  [%expect
    {|
    (((phase Launch) (state Idle) (mac_byte_sel 0) (active_outputs ()))
     ((phase (In_frame 0)) (state Preamble) (mac_byte_sel 0)
      (active_outputs (tx_busy)))
     ((phase (In_frame 1)) (state Preamble) (mac_byte_sel 1)
      (active_outputs (tx_busy)))
     ((phase (In_frame 2)) (state Preamble) (mac_byte_sel 2)
      (active_outputs (tx_busy)))
     ((phase (In_frame 3)) (state Preamble) (mac_byte_sel 3)
      (active_outputs (tx_busy)))
     ((phase (In_frame 4)) (state Preamble) (mac_byte_sel 4)
      (active_outputs (tx_busy)))
     ((phase (In_frame 5)) (state Preamble) (mac_byte_sel 5)
      (active_outputs (tx_busy)))
     ((phase (In_frame 6)) (state Preamble) (mac_byte_sel 6)
      (active_outputs (tx_busy)))
     ((phase (In_frame 7)) (state Sfd) (mac_byte_sel 0)
      (active_outputs (tx_busy))))
    |}]
;;

let%expect_test "header counters walk the two MACs and the ethertype" =
  let observation = Testbench.run_frame ~payload_length:46 in
  let header =
    List.filter observation.trace ~f:(fun (item : Observation.t) ->
      match item.output.state with
      | State.Dst_mac | State.Src_mac | State.Eth_type -> true
      | _ -> false)
  in
  print_trace header;
  [%expect
    {|
    (((phase (In_frame 8)) (state Dst_mac) (mac_byte_sel 0)
      (active_outputs (tx_busy)))
     ((phase (In_frame 9)) (state Dst_mac) (mac_byte_sel 1)
      (active_outputs (tx_busy)))
     ((phase (In_frame 10)) (state Dst_mac) (mac_byte_sel 2)
      (active_outputs (tx_busy)))
     ((phase (In_frame 11)) (state Dst_mac) (mac_byte_sel 3)
      (active_outputs (tx_busy)))
     ((phase (In_frame 12)) (state Dst_mac) (mac_byte_sel 4)
      (active_outputs (tx_busy)))
     ((phase (In_frame 13)) (state Dst_mac) (mac_byte_sel 5)
      (active_outputs (tx_busy)))
     ((phase (In_frame 14)) (state Src_mac) (mac_byte_sel 0)
      (active_outputs (tx_busy)))
     ((phase (In_frame 15)) (state Src_mac) (mac_byte_sel 1)
      (active_outputs (tx_busy)))
     ((phase (In_frame 16)) (state Src_mac) (mac_byte_sel 2)
      (active_outputs (tx_busy)))
     ((phase (In_frame 17)) (state Src_mac) (mac_byte_sel 3)
      (active_outputs (tx_busy)))
     ((phase (In_frame 18)) (state Src_mac) (mac_byte_sel 4)
      (active_outputs (tx_busy)))
     ((phase (In_frame 19)) (state Src_mac) (mac_byte_sel 5)
      (active_outputs (tx_busy)))
     ((phase (In_frame 20)) (state Eth_type) (mac_byte_sel 0)
      (active_outputs (tx_busy)))
     ((phase (In_frame 21)) (state Eth_type) (mac_byte_sel 1)
      (active_outputs (tx_busy))))
    |}]
;;

let%expect_test "a short datagram pads to the minimum and hands over to the FCS" =
  let observation = Testbench.run_frame ~payload_length:3 in
  (* The three real payload bytes are the first payload cycles; the pad runs to index 45
     and the Fcs state follows. Printing only the payload and FCS cycles keeps the
     handover visible without the header. *)
  let payload_and_fcs =
    List.filter observation.trace ~f:(fun (item : Observation.t) ->
      match item.output.state with
      | State.Payload | State.Fcs -> true
      | _ -> false)
  in
  let first_pad_cycles = List.take payload_and_fcs 6 in
  let final_cycles = List.drop payload_and_fcs (List.length payload_and_fcs - 6) in
  print_endline "entering the pad:";
  print_trace first_pad_cycles;
  print_endline "pad handing over to the FCS:";
  print_trace final_cycles;
  [%expect
    {|
    entering the pad:
    (((phase (In_frame 22)) (state Payload) (mac_byte_sel 0)
      (active_outputs (tx_busy)))
     ((phase (In_frame 23)) (state Payload) (mac_byte_sel 1)
      (active_outputs (tx_busy)))
     ((phase (In_frame 24)) (state Payload) (mac_byte_sel 2)
      (active_outputs (tx_busy)))
     ((phase (In_frame 25)) (state Payload) (mac_byte_sel 3)
      (active_outputs (tx_busy pad)))
     ((phase (In_frame 26)) (state Payload) (mac_byte_sel 4)
      (active_outputs (tx_busy pad)))
     ((phase (In_frame 27)) (state Payload) (mac_byte_sel 5)
      (active_outputs (tx_busy pad))))
    pad handing over to the FCS:
    (((phase (In_frame 66)) (state Payload) (mac_byte_sel 4)
      (active_outputs (tx_busy pad)))
     ((phase (In_frame 67)) (state Payload) (mac_byte_sel 5)
      (active_outputs (crc_en tx_busy pad)))
     ((phase (In_frame 68)) (state Fcs) (mac_byte_sel 0)
      (active_outputs (crc_en tx_busy)))
     ((phase (In_frame 69)) (state Fcs) (mac_byte_sel 1)
      (active_outputs (crc_en tx_busy)))
     ((phase (In_frame 70)) (state Fcs) (mac_byte_sel 2)
      (active_outputs (crc_en tx_busy)))
     ((phase (In_frame 71)) (state Fcs) (mac_byte_sel 3)
      (active_outputs (tx_busy))))
    |}]
;;

let%expect_test "a start pulse held against a closed store-and-forward gate" =
  let snapshots = Testbench.run_start_without_frame_ready ~num_cycles:4 in
  print_s [%sexp (snapshots : Output_snapshot.t list)];
  [%expect
    {|
    (((byte_mux_sel Idle) (mac_byte_sel 0) (crc_en false) (state Idle)
      (tx_busy false) (pad false))
     ((byte_mux_sel Idle) (mac_byte_sel 0) (crc_en false) (state Idle)
      (tx_busy false) (pad false))
     ((byte_mux_sel Idle) (mac_byte_sel 0) (crc_en false) (state Idle)
      (tx_busy false) (pad false))
     ((byte_mux_sel Idle) (mac_byte_sel 0) (crc_en false) (state Idle)
      (tx_busy false) (pad false)))
    |}]
;;

let%expect_test "a latched start launches when frame_ready rises" =
  let after_start, while_waiting, at_frame_ready, after_launch =
    Testbench.run_deferred_launch ~cycles_before_frame_ready:3
  in
  let states snapshots =
    List.map (snapshots : Output_snapshot.t list) ~f:(fun snapshot -> snapshot.state)
  in
  print_s
    [%sexp
      { after_start = (after_start.state : State.t)
      ; while_waiting = (states while_waiting : State.t list)
      ; at_frame_ready = (at_frame_ready.state : State.t)
      ; after_launch = (after_launch.state : State.t)
      ; busy_after_launch = (after_launch.tx_busy : bool)
      }];
  [%expect
    {|
    ((after_start Idle) (while_waiting (Idle Idle Idle)) (at_frame_ready Idle)
     (after_launch Preamble) (busy_after_launch true))
    |}]
;;
