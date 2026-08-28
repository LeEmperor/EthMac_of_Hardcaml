(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_datapath_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Rx_datapath

   Typed examples and generated properties covering the ethertype register capture and the
   four-deep FCS-strip pipeline.

   The two facts the superseded [rx_datapath_legacy_assertion_test.ml] asserted - 0x45
   0x21 latching as 0x4521, and a five-byte payload surviving a four-byte FCS intact - are
   the first two cases below. The Quickcheck properties generalize the second: for any
   payload length the collected bytes are the payload, in order, with no FCS byte leaking
   through and no payload byte lost.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Rx_datapath_testbench

let eth_type_of bytes = snd (Testbench.run_eth_type_capture bytes)

let%test_unit "ethertype latches MSB-first" =
  [%test_result: int] (eth_type_of [ 0x45; 0x21 ]) ~expect:0x4521
;;

let%test_unit "ethertype latches IPv4" =
  [%test_result: int] (eth_type_of [ 0x08; 0x00 ]) ~expect:0x0800
;;

let%test_unit "a five-byte payload survives its FCS" =
  let payload = [ 0x11; 0x22; 0x33; 0x44; 0x55 ] in
  let observation =
    Testbench.run_payload_strip ~payload ~fcs:[ 0xAA; 0xBB; 0xCC; 0xDD ]
  in
  [%test_result: int list] observation.collected ~expect:payload
;;

(* Four bytes of payload is the boundary case: the pipeline is exactly full of them when
   the FCS arrives, so every one must still come out and none of the FCS may. *)
let%test_unit "a payload the depth of the pipeline survives its FCS" =
  let payload = [ 0xDE; 0xAD; 0xBE; 0xEF ] in
  let observation =
    Testbench.run_payload_strip ~payload ~fcs:[ 0x01; 0x02; 0x03; 0x04 ]
  in
  [%test_result: int list] observation.collected ~expect:payload
;;

(* A payload shorter than the pipeline is under the 46-byte Ethernet minimum and cannot
   reach this block from a conformant PHY, but the strip logic has no length condition in
   it, so the property should still hold. *)
let%test_unit "a payload shorter than the pipeline survives its FCS" =
  let payload = [ 0xAB; 0xCD ] in
  let observation =
    Testbench.run_payload_strip ~payload ~fcs:[ 0x01; 0x02; 0x03; 0x04 ]
  in
  [%test_result: int list] observation.collected ~expect:payload
;;

let%test_unit "emission lags the input by exactly the pipeline depth" =
  (* Distinct from the length checks above: an FCS byte chosen to collide with a payload
     byte would slip past a pure equality on the collected list if the pipeline were a
     stage short. Pairing each emitted byte with the cycle that emitted it pins the
     latency at four, which is what makes the FCS unreachable rather than merely absent.
     Payload byte 0 leaves on the cycle payload byte 4 arrives, so the four cycles that
     carry the FCS in are the ones carrying the last four payload bytes out. *)
  let payload = [ 0x11; 0x22; 0x33; 0x44; 0x55; 0x66 ] in
  let observation =
    Testbench.run_payload_strip ~payload ~fcs:[ 0x11; 0x22; 0x33; 0x44 ]
  in
  let emissions =
    List.filter_map observation.trace ~f:(fun (item : Observation.t) ->
      Option.some_if (Testbench.emitted item.output) (item.phase, item.output.payload_out))
  in
  [%test_result: (Phase.t * int) list]
    emissions
    ~expect:
      [ Phase.Payload 4, 0x11
      ; Phase.Payload 5, 0x22
      ; Phase.Fcs 0, 0x33
      ; Phase.Fcs 1, 0x44
      ; Phase.Fcs 2, 0x55
      ; Phase.Fcs 3, 0x66
      ]
;;

let%test_unit "ethertype latches random byte pairs" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "rx-datapath-eth-type")
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun bytes ->
      let high, low =
        match bytes with
        | [ high; low ] -> high, low
        | _ -> failwith "generator must produce exactly two bytes"
      in
      [%test_result: int] (eth_type_of bytes) ~expect:((high lsl 8) lor low))
    (List.gen_with_length 2 Generators.byte)
;;

let%test_unit "payload in equals payload out for random lengths" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "rx-datapath-fcs-strip")
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:(List.quickcheck_shrinker Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:(fun payload ->
      let fcs = Crc32.fcs_bytes payload in
      let observation = Testbench.run_payload_strip ~payload ~fcs in
      [%test_result: int list] observation.collected ~expect:payload)
    (Generators.byte_list ~min_length:1 ~max_length:24 ())
;;

let%test_unit "random frames' payloads survive their real FCS" =
  Quickcheck.test
    ~trials:30
    ~seed:(`Deterministic "rx-datapath-frames")
    ~sexp_of:[%sexp_of: Eth_frame.t]
    ~f:(fun frame ->
      (* The controller holds [emit_payload] from the ethertype through the FCS, so the
         span this block sees as payload is the frame's payload alone. *)
      let observation =
        Testbench.run_payload_strip
          ~payload:frame.payload
          ~fcs:(Eth_frame.fcs_bytes frame)
      in
      [%test_result: int list] observation.collected ~expect:frame.payload)
    (Generators.eth_frame ~min_payload_length:1 ~max_payload_length:16 ())
;;
