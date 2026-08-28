(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_top_expect_tests.ml" *)
(* Short RX and TX golden traces for the complete MII MAC. *)

open! Core
open! Mac_top_testbench

let hex bytes = List.map bytes ~f:(sprintf "%02x") |> String.concat ~sep:" "

let%expect_test "TX: one short payload is padded into a minimum frame" =
  let observation = transmit [ 0xde; 0xad; 0xbe; 0xef ] in
  print_s
    [%sexp
      { byte_count = (List.length observation.frame : int)
      ; prefix = (hex (List.take observation.frame 30) : string)
      ; payload_tail = (hex (List.sub observation.frame ~pos:64 ~len:4) : string)
      ; fcs =
          (hex (List.drop observation.frame (List.length observation.frame - 4)) : string)
      ; tx_en_edges = ((observation.tx_en_rises, observation.tx_en_falls) : int * int)
      }];
  [%expect
    {|
    ((byte_count 72)
     (prefix
      "55 55 55 55 55 55 55 d5 ff ff ff ff ff ff 02 00 00 00 00 01 08 00 de ad be ef 00 00 00 00")
     (payload_tail "00 00 00 00") (fcs "9a a7 9c ca") (tx_en_edges (1 1)))
    |}]
;;

let%expect_test "RX: one good frame becomes one AXI payload" =
  let observation = receive_payload [ 0x10; 0x20; 0x30; 0x40; 0x50 ] in
  print_s [%sexp (observation : Rx_observation.t)];
  [%expect
    {|
    ((payload (16 32 48 64 80)) (tfirst_indices (0)) (tlast_indices (4))
     (tuser_on_last (false)) (frame_crc_ok true) (rx_eth_type 2048))
    |}]
;;
