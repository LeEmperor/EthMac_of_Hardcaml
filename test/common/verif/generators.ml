(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "generators.ml" *)
(* Quickcheck generators for the value types the networking suites randomize over.

   Every suite was rolling its own [Int.gen_incl 0x00 0xFF]; these are the shared ones.
   Lengths default to small ranges because each generated element costs simulated clock
   cycles — pass the optional bounds explicitly when a suite wants a wider sweep.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "COMMON_ITEMS" }]
*)

open! Core

let byte : int Quickcheck.Generator.t = Int.gen_incl 0x00 0xFF

let byte_list
  ?(min_length = 1)
  ?(max_length = 16) ()
  : int list Quickcheck.Generator.t =

  (* kid named monad *)
  let open Quickcheck.Generator.Let_syntax in

  (* randomized sequence creator; might need seeding passthroughs but we'll do that later *)
  let%bind length = Int.gen_incl min_length max_length in
  List.gen_with_length length byte
  [@@ocamlformat "disable"]

(* goons v1 *)
let mac_address : int list Quickcheck.Generator.t = List.gen_with_length 6 byte
let ipv4_address : int list Quickcheck.Generator.t = List.gen_with_length 4 byte
let port : int Quickcheck.Generator.t = Int.gen_incl 0x0000 0xFFFF

(* emacs auto-annotate is suprisingly wonderful *)
let payload_length ?(min_length = 0) ?(max_length = 64) () : int Quickcheck.Generator.t =
  Int.gen_incl min_length max_length
;;

(* randomize, and then construct into an Eth_frame "object" *)
let eth_frame
  ?(min_preamble_length = 1)
  ?(max_preamble_length = 10)
  ?(min_payload_length = 0)
  ?(max_payload_length = 16)
  ()
  : Eth_frame.t Quickcheck.Generator.t
  =

  (* cousin named monad *)
  let open Quickcheck.Generator.Let_syntax in

  (* randomized eth frame creation *)
  let%bind preamble_length = Int.gen_incl min_preamble_length max_preamble_length in
  let%bind destination_mac = mac_address in
  let%bind source_mac = mac_address in
  let%bind eth_type = List.gen_with_length 2 byte in
  let%bind payload_length =
    payload_length ~min_length:min_payload_length ~max_length:max_payload_length ()
  in

  (* lets get the payload moving lads! [TF2 reference] *)
  let%map payload = List.gen_with_length payload_length byte in

  (* instantiate based on the common infra we just wrote for the Eth_frame abstractions *)
  Eth_frame.create ~preamble_length ~destination_mac ~source_mac ~eth_type ~payload ()
  [@@ocamlformat "disable"]
