(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_loopback_mac_top_expect_tests.ml" *)
(* Short golden frame used by the loopback echo side. *)

open! Core
open! Udp_loopback_mac_top_testbench

let hex bytes = List.map bytes ~f:(sprintf "%02x") |> String.concat ~sep:" "

let%expect_test "a four-byte echo is a minimum Ethernet frame" =
  let frame = (List.nth_exn (run_all ()) 2).tx_frame in
  print_s
    [%sexp
      { byte_count = (List.length frame : int)
      ; prefix = (hex (List.take frame 30) : string)
      ; fcs = (hex (List.drop frame (List.length frame - 4)) : string)
      }];
  [%expect
    {|
    ((byte_count 72)
     (prefix
      "55 55 55 55 55 55 55 d5 ff ff ff ff ff ff 02 00 00 00 00 01 08 00 45 00 00 20 00 00 40 00")
     (fcs "19 cb 13 81"))
    |}]
;;
