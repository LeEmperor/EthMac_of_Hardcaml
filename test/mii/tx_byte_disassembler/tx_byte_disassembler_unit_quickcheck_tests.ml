(* 
   University of Florida 
   Author: Bohdan Purtell

   Unit and Quickcheck Test Suite: Tx_byte_disassembler

   Typed examples and generated properties covering low-nibble-first byte disassembly,
   back-to-back traffic, and return to idle.
*)

open! Core
open! Tx_byte_disassembler_testbench

module Generators = struct
  let byte : int Quickcheck.Generator.t = 
    Int.gen_incl 0x00 0xFF
  ;;

  let byte_sequence : int list Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 16 in
    List.gen_with_length length byte
  ;;
end

(* I wonder if there's a way to generate expected_observation functions? *)
let expected_observation byte : Observation.t =
  let (lo, hi) = Testbench.Byte_transaction.to_nibbles byte in

  { ready_during_lo = true
  ; ready_during_hi = false
  ; tx_en_during_lo = true
  ; tx_en_during_hi = true
  ; lo_nibble = Some lo
  ; hi_nibble = Some hi
  ; after_hi  = {  ready = true (* holy ugly nested records Batman *)
                ; tx_en = false
                ; tx_d = 0 
               }
  }
;;

let check_bytes bytes =
  let actual = Testbench.run_bytes bytes in
  let expect = List.map bytes ~f:expected_observation in
  [%test_result: Observation.t list] actual ~expect
;;

let%test_unit "disassembles 0x00" = check_bytes [ 0x00 ]
let%test_unit "disassembles 0x0f" = check_bytes [ 0x0F ]
let%test_unit "disassembles 0xf0" = check_bytes [ 0xF0 ]
let%test_unit "disassembles 0xff" = check_bytes [ 0xFF ]
let%test_unit "sends the low nibble first" = check_bytes [ 0x12; 0xAB ]

let%test_unit "disassembles back-to-back bytes" =
  check_bytes [ 0x00; 0x12; 0xAB; 0xF0; 0xFF ]
;;

let%test_unit "disassembles random byte sequences" =
  Quickcheck.test
    ~trials:100
    ~seed:(`Deterministic "tralalero-tralala")
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:(List.quickcheck_shrinker Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:check_bytes
    Generators.byte_sequence
;;
