(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_crc_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Rx_crc

   Typed examples and generated properties covering the raw accumulator, the
   frame-plus-FCS residue check that drives [crc_valid], rejection of a corrupted frame,
   the [~en] reload, and accumulator hold across a [rx_data_valid] gap.

   The oracle throughout is [Hardcaml_verif.Crc32]. Note which of its two views applies:
   [Crc32.bytes] is the raw accumulator that [crc_out] holds, [Crc32.fcs_bytes] is the
   inverted word a transmitter appends. Feeding a span and then its own [fcs_bytes] is
   what lands the accumulator on [Crc32.residue].

   The three constants asserted below - residue 0xDEBB20E3 for "123456789" plus its FCS,
   rejection of an all-zero FCS, and recovery after an [en] drop - are the same three the
   superseded [rx_crc_legacy_assertion_test.ml] checked.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Rx_crc_testbench

(* The standard CRC-32 check vector, "123456789". *)
let check_vector = [ 0x31; 0x32; 0x33; 0x34; 0x35; 0x36; 0x37; 0x38; 0x39 ]
let final_of_bytes bytes = Testbench.(final_snapshot (run_bytes bytes))

let final_of_bytes_with_fcs bytes ~fcs =
  Testbench.(final_snapshot (run_bytes_with_fcs bytes ~fcs))
;;

let%test_unit "raw accumulator matches the software model for the check vector" =
  [%test_result: int]
    (final_of_bytes check_vector).crc_out
    ~expect:(Crc32.bytes check_vector)
;;

let%test_unit "the check vector reaches the standard residue once its FCS is clocked in" =
  let final = final_of_bytes_with_fcs check_vector ~fcs:(Crc32.fcs_bytes check_vector) in
  [%test_result: int] final.crc_out ~expect:Crc32.residue;
  [%test_result: bool] final.crc_valid ~expect:true
;;

(* The legacy harness spelled these out as literals rather than deriving them, so keep
   them literal here: they are the standard's numbers, not the model's. *)
let%test_unit "the standard's published constants" =
  [%test_result: int] Crc32.residue ~expect:0xDEBB20E3;
  [%test_result: int] (Crc32.fcs check_vector) ~expect:0xCBF43926;
  [%test_result: int list]
    (Crc32.fcs_bytes check_vector)
    ~expect:[ 0x26; 0x39; 0xF4; 0xCB ]
;;

let%test_unit "an all-zero FCS is rejected" =
  let final = final_of_bytes_with_fcs check_vector ~fcs:[ 0x00; 0x00; 0x00; 0x00 ] in
  [%test_result: bool] final.crc_valid ~expect:false
;;

let%test_unit "dropping en reloads the accumulator mid-frame" =
  let after_disabled_cycle, observations =
    Testbench.run_enable_drop
      ~discarded:[ 0xDE; 0xAD ]
      ~bytes:check_vector
      ~fcs:(Crc32.fcs_bytes check_vector)
  in
  (* The disabled cycle itself reloads 0xFFFFFFFF, so the discarded bytes leave no trace. *)
  [%test_result: int] after_disabled_cycle.crc_out ~expect:0xFFFFFFFF;
  let final = Testbench.final_snapshot observations in
  [%test_result: int] final.crc_out ~expect:Crc32.residue;
  [%test_result: bool] final.crc_valid ~expect:true
;;

let%test_unit "a cycle without rx_data_valid holds the accumulator" =
  let before_gap, during_gap, after_gap =
    Testbench.run_valid_gap ~before:[ 0xDE; 0xAD; 0xBE ] ~after:[ 0xEF ]
  in
  [%test_result: Output_snapshot.t] during_gap ~expect:before_gap;
  [%test_result: int] before_gap.crc_out ~expect:(Crc32.bytes [ 0xDE; 0xAD; 0xBE ]);
  (* The gap must not have consumed the 0xEE riding on rx_data while valid was low. *)
  [%test_result: int] after_gap.crc_out ~expect:(Crc32.bytes [ 0xDE; 0xAD; 0xBE; 0xEF ])
;;

let%test_unit "crc_valid only rises on the residue" =
  (* Every intermediate accumulator of a real frame must leave crc_valid low; only the
     cycle that clocks the last FCS byte may raise it. *)
  let observations =
    Testbench.run_bytes_with_fcs check_vector ~fcs:(Crc32.fcs_bytes check_vector)
  in
  let valid_cycles =
    List.filter_mapi observations ~f:(fun index (observation : Observation.t) ->
      Option.some_if observation.output.crc_valid index)
  in
  [%test_result: int list] valid_cycles ~expect:[ List.length observations - 1 ]
;;

let%test_unit "raw accumulator matches the software model for random byte spans" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "rx-crc-raw-accumulator")
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:(List.quickcheck_shrinker Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:(fun bytes ->
      [%test_result: int] (final_of_bytes bytes).crc_out ~expect:(Crc32.bytes bytes))
    (Generators.byte_list ~min_length:1 ~max_length:16 ())
;;

let%test_unit "random frames reach the residue and assert crc_valid" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "rx-crc-residue")
    ~sexp_of:[%sexp_of: Eth_frame.t]
    ~f:(fun frame ->
      let covered = Eth_frame.crc_covered_bytes frame in
      let final = final_of_bytes_with_fcs covered ~fcs:(Eth_frame.fcs_bytes frame) in
      [%test_result: int] final.crc_out ~expect:Crc32.residue;
      [%test_result: bool] final.crc_valid ~expect:true)
    (Generators.eth_frame ~max_payload_length:16 ())
;;

(* A single corrupted byte changes the accumulator: the difference a one-byte delta
   contributes is a nonzero polynomial times a power of x, which the CRC polynomial never
   divides. So a corrupted frame must never satisfy the residue check. *)
module Corruption = struct
  type t =
    { bytes : int list
    ; index : int
    ; mask : int
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind bytes = Generators.byte_list ~min_length:1 ~max_length:16 () in
    let%bind index = Int.gen_incl 0 (List.length bytes - 1) in
    let%map mask = Int.gen_incl 0x01 0xFF in
    { bytes; index; mask }
  ;;
end

let%test_unit "a single corrupted byte clears crc_valid" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "rx-crc-corruption")
    ~sexp_of:[%sexp_of: Corruption.t]
    ~f:(fun { Corruption.bytes; index; mask } ->
      let fcs = Crc32.fcs_bytes bytes in
      let corrupted =
        List.mapi bytes ~f:(fun position byte ->
          if position = index then byte lxor mask else byte)
      in
      let final = final_of_bytes_with_fcs corrupted ~fcs in
      [%test_result: bool] final.crc_valid ~expect:false)
    Corruption.quickcheck_generator
;;
