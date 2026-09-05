(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Unit and Quickcheck Test Suite: Pcs_tx_scrambler

   Exact known-answer vectors and generated multi-block streams covering reset, header
   bypass, and state continuity.
*)

open! Core
open! Hardcaml
open! Pcs_tx_scrambler_testbench

let expect_exact actual ~data ~header =
  let expected : Scrambled.t = { data = Bits.of_hex ~width:64 data; header } in
  if not (Scrambled.equal actual expected)
  then
    raise_s
      [%message
        "unexpected known-answer scrambling"
          (Scrambled.printable actual : Sexp.t)
          (Scrambled.printable expected : Sexp.t)]
;;

let%test_unit "first block after reset has a fixed known answer" =
  let testbench = Testbench.create () in
  Testbench.reset testbench;
  Testbench.scramble_hex testbench ~payload:"0000000000000000" ~header:0b10
  |> expect_exact ~data:"03ffff8000000000" ~header:0b10
;;

let%test_unit "sync headers bypass the scrambler" =
  let testbench = Testbench.create () in
  Testbench.reset testbench;
  let data_header =
    Testbench.scramble_hex testbench ~payload:"000000000000001e" ~header:0b10
  in
  let control_header =
    Testbench.scramble_hex testbench ~payload:"000000000000001e" ~header:0b01
  in
  [%test_result: int] data_header.header ~expect:0b10;
  [%test_result: int] control_header.header ~expect:0b01
;;

let%test_unit "state is continuous across unlike back-to-back blocks" =
  let testbench = Testbench.create () in
  Testbench.reset testbench;
  Testbench.scramble_hex testbench ~payload:"0000000000000000" ~header:0b10
  |> expect_exact ~data:"03ffff8000000000" ~header:0b10;
  Testbench.scramble_hex testbench ~payload:"000000000000001e" ~header:0b01
  |> expect_exact ~data:"87eff0ffffffc01e" ~header:0b01;
  Testbench.scramble_hex testbench ~payload:"7766554433221100" ~header:0b10
  |> expect_exact ~data:"1bb115443b2591ff" ~header:0b10
;;

let%test_unit "reset restores the initial state" =
  let testbench = Testbench.create () in
  Testbench.reset testbench;
  let first = Testbench.scramble_hex testbench ~payload:"fedcba9876543210" ~header:0b10 in
  ignore
    (Testbench.scramble_hex testbench ~payload:"0123456789abcdef" ~header:0b01
     : Scrambled.t);
  Testbench.reset testbench;
  let after_reset =
    Testbench.scramble_hex testbench ~payload:"fedcba9876543210" ~header:0b10
  in
  if not (Scrambled.equal first after_reset)
  then
    raise_s
      [%message
        "reset did not restore scrambler state"
          (Scrambled.printable first : Sexp.t)
          (Scrambled.printable after_reset : Sexp.t)]
;;

module Generators = struct
  let payload =
    List.gen_with_length 8 (Int.gen_incl 0x00 0xff)
    |> Quickcheck.Generator.map ~f:(fun bytes ->
      Bits.concat_lsb (List.map bytes ~f:(fun byte -> Bits.of_int_trunc ~width:8 byte)))
  ;;

  let header = Quickcheck.Generator.of_list [ 0b01; 0b10 ]

  let stream =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 32 in
    List.gen_with_length length (Quickcheck.Generator.both payload header)
  ;;
end

let%test_unit "random multi-block streams match the serial reference model" =
  Quickcheck.test
    ~trials:200
    ~seed:(`Deterministic "pcs-tx-scrambler-stream")
    ~sexp_of:[%sexp_of: (Bits.t * int) list]
    ~f:(fun blocks ->
      let testbench = Testbench.create () in
      Testbench.reset testbench;
      List.iter blocks ~f:(fun (payload, header) ->
        ignore (Testbench.scramble testbench ~payload ~header : Scrambled.t)))
    Generators.stream
;;
