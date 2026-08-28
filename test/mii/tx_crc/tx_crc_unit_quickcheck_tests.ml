(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_crc_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Tx_crc

   Typed examples and generated properties covering the raw accumulator, the four FCS
   bytes read out through the [byte_sel] mux, and the [~en] reload.

   The oracle is [Hardcaml_verif.Crc32]: [Crc32.bytes] for [crc_out] and [Crc32.fcs_bytes]
   for the emitted bytes. The superseded [tx_crc_legacy_assertion_test.ml] checked the
   same three things - the standard vector's FCS 0xCBF43926 read out as 0x26 0x39 0xF4
   0xCB, recovery after an [en] drop, and an arbitrary payload against the software model.

   A frame produced here is what [Rx_crc] must accept: feeding a span and then the bytes
   this block emits for it is what lands a receiver on [Crc32.residue]. That composition
   is checked directly in the mac_top loopback suite (phase 4); here the tie is the shared
   [Crc32] model.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Tx_crc_testbench

let check_vector = [ 0x31; 0x32; 0x33; 0x34; 0x35; 0x36; 0x37; 0x38; 0x39 ]

let emitted_fcs_bytes bytes =
  Testbench.fcs_bytes_of (Testbench.run_bytes_then_read_fcs bytes)
;;

let%test_unit "raw accumulator matches the software model for the check vector" =
  let final = Testbench.(final_snapshot (run_bytes check_vector)) in
  [%test_result: int] final.crc_out ~expect:(Crc32.bytes check_vector)
;;

let%test_unit "the check vector's FCS word is the standard's 0xCBF43926" =
  let final = Testbench.(final_snapshot (run_bytes check_vector)) in
  [%test_result: int] (final.crc_out lxor 0xFFFFFFFF) ~expect:0xCBF43926
;;

let%test_unit "byte_sel walks the FCS little-endian" =
  [%test_result: int list]
    (emitted_fcs_bytes check_vector)
    ~expect:[ 0x26; 0x39; 0xF4; 0xCB ];
  [%test_result: int list]
    (emitted_fcs_bytes check_vector)
    ~expect:(Crc32.fcs_bytes check_vector)
;;

let%test_unit "an arbitrary payload matches the software model" =
  let payload = [ 0xDE; 0xAD; 0xBE; 0xEF; 0xCA; 0xFE ] in
  [%test_result: int list] (emitted_fcs_bytes payload) ~expect:(Crc32.fcs_bytes payload)
;;

let%test_unit "dropping en reloads the accumulator mid-frame" =
  let after_disabled_cycle, observations =
    Testbench.run_enable_drop ~discarded:[ 0xDE; 0xAD ] ~bytes:check_vector
  in
  [%test_result: int] after_disabled_cycle.crc_out ~expect:0xFFFFFFFF;
  (* ~0xFFFFFFFF is 0, so a parked-at-reload accumulator emits 0x00 on every byte_sel. *)
  [%test_result: int] after_disabled_cycle.fcs_byte ~expect:0x00;
  [%test_result: int list]
    (Testbench.fcs_bytes_of observations)
    ~expect:(Crc32.fcs_bytes check_vector)
;;

let%test_unit "reading the FCS does not disturb the accumulator" =
  (* All four read cycles hold [data_valid] low, so every one of them must report the same
     [crc_out] the last payload byte left behind. *)
  let observations = Testbench.run_bytes_then_read_fcs check_vector in
  let expect = Crc32.bytes check_vector in
  List.iter observations ~f:(fun (observation : Observation.t) ->
    match observation.phase with
    | Phase.Fcs_read _ -> [%test_result: int] observation.output.crc_out ~expect
    | Phase.Payload _ -> ())
;;

let%test_unit "emitted FCS bytes match the software model for random payloads" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "tx-crc-fcs-bytes")
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:(List.quickcheck_shrinker Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:(fun bytes ->
      [%test_result: int list] (emitted_fcs_bytes bytes) ~expect:(Crc32.fcs_bytes bytes))
    (Generators.byte_list ~min_length:1 ~max_length:16 ())
;;

let%test_unit "emitted FCS bytes match the software model for random frames" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "tx-crc-frame-fcs")
    ~sexp_of:[%sexp_of: Eth_frame.t]
    ~f:(fun frame ->
      [%test_result: int list]
        (emitted_fcs_bytes (Eth_frame.crc_covered_bytes frame))
        ~expect:(Eth_frame.fcs_bytes frame))
    (Generators.eth_frame ~max_payload_length:16 ())
;;
