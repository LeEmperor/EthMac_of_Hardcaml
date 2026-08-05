(*
  University of Florida
  Author: Bohdan Purtell

  Unit and Quickcheck Test Suite: Rx_byte_assembler

  Typed example assertions and generated properties covering byte assembly for individual values, back-to-back traffic, and randomized byte sequences.

  Tags: { "ACTIVE"
          ; "TEST"
          ; "QUICKCHECk"
          ; "UNIT"
          ; "UNIT_TEST"
          ; "UNITTEST"
          ; "PERSONAL_REFERENCE"
        }
*)

open! Core
open! Rx_byte_assembler_testbench

(* fascinating composition*)
module Generators = struct
  (* random variable byte B *)
  let byte : int Quickcheck.Generator.t = (* ability to discern types here has been finicky sometimes; maybe need to re-build ocamllsp or something tbh *)
    Int.gen_incl 0x00 0xFF

    (* need to import the weighted random sequence thing in eventually - would probablybe a fun exercise to write one my own for some Caltrain ride *)
  let byte_sequence : int list Quickcheck.Generator.t =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 16 in
    List.gen_with_length length byte
  ;;
end

(* henchmen (helper) 1*)
let expected_observation byte : Observation.t =
  { valid_after_low_nibble = false
  ; valid_after_high_nibble = true
  ; completed_byte = Some byte
  }
;;

(* goon #2 *)
let check_bytes bytes =
  let actual = Testbench.run_bytes bytes in
  let expect = List.map bytes ~f:expected_observation in
  [%test_result: Observation.t list] actual ~expect
;;

let%test_unit "assembles 0" = check_bytes [ 0x00 ]
let%test_unit "assembles 1" = check_bytes [ 0x01 ]
let%test_unit "assembles 16" = check_bytes [ 0x10 ]

let%test_unit "assembles back-to-back" =
  check_bytes [ 0x00; 0xAB; 0xFF ]
;;

let%test_unit "assembles 0xFF" = check_bytes [ 0xFF ]
let%test_unit "assembles 0x7F" = check_bytes [ 0x7F ]

let%test_unit "assembles random byte sequences" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "tungtungtung-sahur")
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:(List.quickcheck_shrinker Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:check_bytes
    Generators.byte_sequence
;;
