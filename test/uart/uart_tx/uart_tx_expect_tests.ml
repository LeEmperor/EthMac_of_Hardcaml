(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "uart_tx_expect_tests.ml" *)

(* Expect Test Suite: Uart_tx

   Golden traces of the serial line, rendered as the receiver would see it: the idle
   cycles, then one group per symbol, then the idle cycles after. Read left to right it is
   the frame - a space start bit, eight data bits least significant first, a mark stop bit
   - and the grouping is what makes the bit order checkable by eye.

   Tags: [{ "ACTIVE" ; "TEST" ; "EXPECT_TEST" }]
*)

open! Core
open! Uart_tx_testbench

let print_frame ?cycles_per_symbol ?stall_cycles byte =
  let frame = Testbench.run_byte ?cycles_per_symbol ?stall_cycles byte in
  printf "0x%02x: %s\n" byte (frame_to_string frame)
;;

let%expect_test "0x55 and 0xAA, the same bits in the opposite order" =
  print_frame ~cycles_per_symbol:2 0x55;
  print_frame ~cycles_per_symbol:2 0xAA;
  [%expect
    {|
    0x55: 111 00 11 00 11 00 11 00 11 00 11 1111
    0xaa: 111 00 00 11 00 11 00 11 00 11 11 1111
    |}]
;;

let%expect_test "the all-zero and all-one bytes" =
  (* 0x00 makes the data field look like eight more start bits and 0xFF like eight more
     stop bits; the symbol grouping is the only thing separating them. *)
  print_frame ~cycles_per_symbol:2 0x00;
  print_frame ~cycles_per_symbol:2 0xFF;
  [%expect
    {|
    0x00: 111 00 00 00 00 00 00 00 00 00 11 1111
    0xff: 111 00 11 11 11 11 11 11 11 11 11 1111
    |}]
;;

let%expect_test "a single set bit at each end" =
  print_frame ~cycles_per_symbol:1 0x01;
  print_frame ~cycles_per_symbol:1 0x80;
  [%expect
    {|
    0x01: 111 0 1 0 0 0 0 0 0 0 1 1111
    0x80: 111 0 0 0 0 0 0 0 0 1 1 1111
    |}]
;;

let%expect_test "widening the tick spacing stretches the frame without reshaping it" =
  List.iter [ 1; 2; 4 ] ~f:(fun cycles_per_symbol -> print_frame ~cycles_per_symbol 0x3C);
  [%expect
    {|
    0x3c: 111 0 0 0 1 1 1 1 0 0 1 1111
    0x3c: 111 00 00 00 11 11 11 11 00 00 11 1111
    0x3c: 111 0000 0000 0000 1111 1111 1111 1111 0000 0000 1111 1111
    |}]
;;

let%expect_test "stall cycles lengthen a symbol, leaving the frame intact" =
  print_frame ~cycles_per_symbol:2 ~stall_cycles:2 0x3C;
  [%expect {| 0x3c: 111 0000 0000 0000 1111 1111 1111 1111 0000 0000 1111 1111 |}]
;;

let%expect_test "the decoded frame, in full" =
  print_s [%sexp (Testbench.decode_byte 0x96 : Uart_receiver.Decoded.t)];
  [%expect
    {|
    ((start_bit false) (data_bits (false true true false true false false true))
     (stop_bit true) (byte (150)) (unstable_symbols ()))
    |}]
;;

let%expect_test "the FSM states, in full" =
  (* Every state here is reachable. An enumerated state that no transition targets - there
     was one, [DONE] - is dead weight a reader has to rule out, and it also has no arm in
     the output switch, so it would fall through to the mark default if it were ever
     reached. *)
  print_s [%sexp (Dut.States.all : Dut.States.t list)];
  [%expect {| (IDLE START PAYLOAD STOP) |}]
;;

let%expect_test "keep across an idle stretch and a frame" =
  (* Not a status line: [keep] is an OR-reduce of the bit counter and the state, so it
     rises with the frame and stays high afterwards - the counter rests at seven rather
     than returning to zero. *)
  printf "%s\n" (keep_trace_to_string (Testbench.run_keep ~num_idle:4 ~num_frame:14 0x55));
  [%expect {| idle 0000 | frame 01111111111111 |}]
;;
