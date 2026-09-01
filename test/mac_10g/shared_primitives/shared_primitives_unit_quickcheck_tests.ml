(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "shared_primitives_unit_quickcheck_tests.ml" *)
(* Exhaustive keep-mask checks and generated masked-CRC agreement. *)

open! Core
open! Hardcaml_verif
open! Shared_primitives_testbench

let popcount value =
  List.range 0 8 |> List.count ~f:(fun bit -> value land (1 lsl bit) <> 0)
;;

let contiguous value =
  List.range 1 9 |> List.exists ~f:(fun count -> value = (1 lsl count) - 1)
;;

let%test_unit "all 256 keep masks have the modeled count and validity" =
  List.iter (List.range 0 256) ~f:(fun keep ->
    List.iter [ false; true ] ~f:(fun last ->
      let o = Testbench.run ~crc:Crc32.init ~bytes:[] ~valid_bytes:0 ~keep ~last in
      [%test_result: int] o.keep_count ~expect:(popcount keep);
      [%test_result: bool] o.keep_contiguous ~expect:(contiguous keep);
      [%test_result: bool]
        o.beat_legal
        ~expect:(if last then contiguous keep else keep = 0xff)))
;;

let%test_unit "contiguous keep masks round-trip through their byte count" =
  List.iter (List.range 1 9) ~f:(fun count ->
    let keep = (1 lsl count) - 1 in
    let o = Testbench.run ~crc:Crc32.init ~bytes:[] ~valid_bytes:0 ~keep ~last:true in
    [%test_result: int] o.keep_round_trip ~expect:keep)
;;

let%test_unit "masked update consumes enabled lanes in wire order" =
  let bytes = [ 0x10; 0x21; 0x32; 0x43; 0x54; 0x65; 0x76; 0x87 ] in
  List.iter (List.range 0 256) ~f:(fun valid_bytes ->
    let o = Testbench.run ~crc:Crc32.init ~bytes ~valid_bytes ~keep:0xff ~last:false in
    [%test_result: int] o.next_crc ~expect:(Crc32.masked_bytes bytes ~valid_bytes))
;;

let%test_unit "FCS inversion and receive residue helpers use Ethernet conventions" =
  let frame_and_fcs =
    [ 0x31; 0x32; 0x33; 0x34; 0x35; 0x36; 0x37; 0x38; 0x39 ]
    |> fun frame -> frame @ Crc32.fcs_bytes frame
  in
  let #(first, second) = List.split_n frame_and_fcs 8 in
  let first_update =
    Testbench.run ~crc:Crc32.init ~bytes:first ~valid_bytes:0xff ~keep:0xff ~last:false
  in
  let valid_bytes = (1 lsl List.length second) - 1 in
  let final =
    Testbench.run
      ~crc:first_update.next_crc
      ~bytes:second
      ~valid_bytes
      ~keep:valid_bytes
      ~last:true
  in
  [%test_result: int] final.next_crc ~expect:Crc32.residue;
  [%test_result: bool] final.valid_residue ~expect:true;
  [%test_result: int] final.next_fcs ~expect:(Crc32.residue lxor 0xffffffff)
;;

let%test_unit "masked CRC agrees for generated data, masks, and starting states" =
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind bytes = Generators.byte_list ~min_length:8 ~max_length:8 () in
    let%bind valid_bytes = Int.gen_incl 0 255 in
    let%bind crc_low = Int.gen_incl 0 0xffff in
    let%map crc_high = Int.gen_incl 0 0xffff in
    (crc_high lsl 16) lor crc_low, bytes, valid_bytes
  in
  Quickcheck.test
    ~trials:100
    ~seed:(`Deterministic "mac-10g-masked-crc")
    ~sexp_of:[%sexp_of: int * int list * int]
    ~f:(fun (crc, bytes, valid_bytes) ->
      let o = Testbench.run ~crc ~bytes ~valid_bytes ~keep:0xff ~last:false in
      [%test_result: int]
        o.next_crc
        ~expect:(Crc32.masked_bytes ~init:crc bytes ~valid_bytes))
    generator
;;
