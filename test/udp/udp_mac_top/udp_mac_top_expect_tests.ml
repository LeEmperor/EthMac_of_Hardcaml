(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_mac_top_expect_tests.ml" *)
(* Short golden summaries for the composed UDP and IPv4 transmit headers. *)

open! Core
open! Udp_mac_top_testbench

let hex bytes = List.map bytes ~f:(sprintf "%02x") |> String.concat ~sep:" "

let%expect_test "the fourth no-reset frame is complete on the MII wire" =
  let observation = List.last_exn (run_all ()) in
  print_s
    [%sexp
      { byte_count = (List.length observation.frame : int)
      ; prefix = (hex (List.take observation.frame 30) : string)
      ; fcs =
          (hex (List.drop observation.frame (List.length observation.frame - 4)) : string)
      ; tx_en_edges = ((observation.tx_en_rises, observation.tx_en_falls) : int * int)
      }];
  [%expect
    {|
    ((byte_count 72)
     (prefix
      "55 55 55 55 55 55 55 d5 ff ff ff ff ff ff 02 00 00 00 00 01 08 00 45 00 00 2e 00 00 40 00")
     (fcs "1f c7 b2 e9") (tx_en_edges (1 1)))
    |}]
;;
