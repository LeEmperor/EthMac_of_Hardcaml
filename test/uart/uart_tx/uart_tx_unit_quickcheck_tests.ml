(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "uart_tx_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Uart_tx

   Typed examples and generated properties for the UART transmitter.

   The headline property is the round trip: a random byte transmitted, then decoded by
   [Uart_receiver], must come back unchanged, with a space start bit and a mark stop bit
   and no symbol that moved mid-interval. It is run across tick spacings and across enable
   stalls, because the whole point of a tick-driven transmitter is that neither changes
   the frame it sends - only how long it takes to send it.

   The examples pin the bytes whose bit patterns a random draw is least likely to
   distinguish: 0x00 and 0xFF (which make the data field indistinguishable from the start
   and stop bits respectively), and 0x55 / 0xAA (which differ only by bit order, so a
   transmitter that sent most significant bit first would pass one and fail the other).

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Uart_tx_testbench

let check ?cycles_per_symbol ?stall_cycles byte =
  [%test_result: Uart_receiver.Decoded.t]
    (Testbench.decode_byte ?cycles_per_symbol ?stall_cycles byte)
    ~expect:(Uart_receiver.expected byte)
    ~message:(sprintf "byte = 0x%02x" byte)
;;

let%test_unit "0x55 round-trips" = check 0x55
let%test_unit "0xAA round-trips" = check 0xAA
let%test_unit "0x00 round-trips" = check 0x00
let%test_unit "0xFF round-trips" = check 0xFF
let%test_unit "0x01 round-trips" = check 0x01
let%test_unit "0x80 round-trips" = check 0x80

let%test_unit "the data bits go out least significant first" =
  (* 0x01 and 0x80 are one bit each at opposite ends; asserting the raw symbol values
     rather than only the decoded byte states the wire order directly, so a decoder that
     was wrong in the same direction as the RTL could not hide it. *)
  let symbol_values byte = List.map (Testbench.run_byte byte).symbols ~f:Symbol.value in
  [%test_result: bool list]
    (symbol_values 0x01)
    ~expect:
      [ false (* start *)
      ; true
      ; false
      ; false
      ; false
      ; false
      ; false
      ; false
      ; false
      ; true (* stop *)
      ];
  [%test_result: bool list]
    (symbol_values 0x80)
    ~expect:
      [ false (* start *)
      ; false
      ; false
      ; false
      ; false
      ; false
      ; false
      ; false
      ; true
      ; true (* stop *)
      ]
;;

let%test_unit "the line idles at mark with no request" =
  List.iter (Testbench.run_idle ~num_cycles:12) ~f:(fun value ->
    [%test_result: bool] value ~expect:true)
;;

let%test_unit "the line is mark before and after the frame" =
  List.iter [ 0x00; 0xFF; 0x3C ] ~f:(fun byte ->
    let frame = Testbench.run_byte byte in
    List.iter (frame.leading_line @ frame.trailing_line) ~f:(fun value ->
      [%test_result: bool] value ~expect:true ~message:(sprintf "byte = 0x%02x" byte)))
;;

let%test_unit "every symbol is stable across its whole interval" =
  (* A transmitter that moved the line between ticks would still decode correctly under a
     single mid-bit sample; the interval-wide check is what rejects it. *)
  List.iter [ 2; 3; 5; 8 ] ~f:(fun cycles_per_symbol ->
    let frame = Testbench.run_byte ~cycles_per_symbol 0x5A in
    List.iteri frame.symbols ~f:(fun index symbol ->
      [%test_result: bool]
        (Symbol.is_stable symbol)
        ~expect:true
        ~message:(sprintf "cycles_per_symbol = %d, symbol %d" cycles_per_symbol index)))
;;

let%test_unit "the frame is independent of the tick spacing" =
  (* The block advances on [tick], not on the clock. Widening the interval must stretch
     the frame without reshaping it. *)
  List.iter [ 1; 2; 3; 6; 10 ] ~f:(fun cycles_per_symbol -> check ~cycles_per_symbol 0x5A)
;;

let%test_unit "disabling the enable stalls the frame without corrupting it" =
  (* [en] gates the state machine and the bit counter both. Cycles with it low must be
     invisible in the decoded frame. *)
  List.iter [ 1; 2; 4 ] ~f:(fun stall_cycles ->
    check ~cycles_per_symbol:3 ~stall_cycles 0x96)
;;

let%test_unit "the transmitter returns to idle and does not send a second frame" =
  (* [d_in_valid] is asserted for one cycle only. Running well past the stop bit must show
     mark the whole way - a transmitter that re-triggered off a latched request would drop
     the line for a start bit somewhere in the trailing window. *)
  let frame = Testbench.run_byte ~num_trailing:40 0x00 in
  List.iter frame.trailing_line ~f:(fun value -> [%test_result: bool] value ~expect:true)
;;

let%test_unit "keep is tied low" =
  List.iter (Testbench.run_keep ~num_cycles:12 0xFF) ~f:(fun value ->
    [%test_result: bool] value ~expect:false)
;;

let%test_unit "random bytes round-trip" =
  Quickcheck.test
    ~trials:60
    ~seed:(`Deterministic "uart-tx-round-trip")
    ~sexp_of:[%sexp_of: int]
    ~shrinker:Int.quickcheck_shrinker
    ~shrink_attempts:(`Limit 100)
    ~f:(fun byte -> check byte)
    Generators.byte
;;

let%test_unit "random bytes round-trip at random tick spacings, with random stalls" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "uart-tx-timing")
    ~sexp_of:[%sexp_of: int * int * int]
    ~f:(fun (byte, cycles_per_symbol, stall_cycles) ->
      check ~cycles_per_symbol ~stall_cycles byte)
    (let open Quickcheck.Generator.Let_syntax in
     let%bind byte = Generators.byte in
     let%bind cycles_per_symbol = Int.gen_incl 1 8 in
     let%map stall_cycles = Int.gen_incl 0 4 in
     byte, cycles_per_symbol, stall_cycles)
;;
