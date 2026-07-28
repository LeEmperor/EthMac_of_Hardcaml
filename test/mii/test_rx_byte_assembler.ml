(*
  Jane Street Capital
  Author: Bohdan Purtell

  Test: "test_rx_byte_assembler"

  Desc:
    Originally written as a learning experience into inline, expect, and quickcheck tests in the Janestreet style.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench

let () = Stdio.print_endline "=== Imported Test: test_rx_byte_assembler ===";;

module Dut = Rx_byte_assembler

(* An "item" as UVM might treat one. Back-end agnostic TLM-item that emitted *)
module Observation = struct
  type t = 
    { valid_after_low_nibble : bool (* should be false *)
      ; valid_after_high_nibble : bool (* should be true *)
      ; completed_byte : int option
    } [@@deriving sexp, equal, compare]
end

(* expects to see the byte value completely with a valid only pulsed at the high nibble *)
let expected_observation byte : Observation.t = (* eventually i will get used to this silly record formatting *)
  { valid_after_low_nibble = false
  ; valid_after_high_nibble = true
  ; completed_byte = Some byte
  }
;;

module Generators = struct
  (* a byte, represented as a "random" variable almost, we define it's constraints *)
  let byte : int Quickcheck.Generator.t = 
    Int.gen_incl 0x00 0xFF
  ;;

  let byte2 = 
    Quickcheck.Generator.weighted_union
    [ 
      (4.0, Quickcheck.Generator.of_list
        [ 0x00
        ; 0x01
        ; 0x0F
        ; 0x10
        ; 0x7F
        ; 0x80
        ; 0xFE
        ; 0xFF ])
    ; 6.0, Int.gen_incl 0x00 0xFF
    ]
  ;;

  (* define a random-len (constrained) sequence of these random variables *)
  (* this is so mathematical its very cool *)
  let byte_sequence : int list Quickcheck.Generator.t = 
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Int.gen_incl 1 16 in
    List.gen_with_length length byte
  ;;
end

module Testbench = struct
  (* the simulation backend *)
  module Sim = 
    Cyclesim.With_interface (Dut.I) (Dut.O)

  (* step tb handler - Cyclsim based *)
  module Step = 
    Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)

  module Byte_transaction = struct
    type t = int [@@deriving compare, equal, sexp]

    let to_nibbles byte = 
      let low = byte land 0xF in
      let high = (byte lsr 4) land 0xF in
      (low, high)
    ;;

    (* should a command type go in the byte transaction module? or elsewhere? *)
    type command = 
      | Reset
      | Send_byte of int
      | Disable_for of int
    [@@deriving sexp]
  end

  let bit value = 
    if value then Bits.vdd else Bits.gnd
  ;;

  (* way to abstract some input mapper over the I shape? perhaps a functor? *)
  let inputs ~reset ~en ~rx_data = 
    { Step.input_hold with
      reset = bit reset
      ; en = bit en
      ; rx_data = Bits.of_int_trunc ~width:4 rx_data
    }
  ;;

  let drive_nibble (handler : Step.Handler.t @ local) nibble = 
    let step_result = 
      Step.cycle
        handler
        (inputs ~reset:false ~en:true ~rx_data:nibble)
    in
    Step.O_data.after_edge step_result
  ;;

  let drive_byte (handler : Step.Handler.t @ local) byte = 
    let lo, hi = Byte_transaction.to_nibbles byte in
    let after_lo_nibble = drive_nibble handler lo in
    let after_hi_nibble = drive_nibble handler hi in
    (* (drive_nibble handler lo, drive_nibble handler hi) *)
      (* execution order in tuples is NOT guaranteed; can we make it guaranteed? *)
    (after_lo_nibble, after_hi_nibble)
  ;;

  (* apply reset and discard outputs *)
  let reset ?(num_cycles=1) (handler : Step.Handler.t @ local) = 
    Step.delay
      ~num_cycles:num_cycles
      handler
      (inputs ~reset:true ~en:false ~rx_data:0)
  ;;

  let observe_byte ~after_low_nibble ~after_high_nibble  = 
    let valid_after_low_nibble = 
      Bits.to_bool after_low_nibble.Dut.O.byte_valid
    in

    let valid_after_high_nibble =
      Bits.to_bool after_high_nibble.Dut.O.byte_valid
    in

    { Observation.valid_after_low_nibble
      ; valid_after_high_nibble
      ; completed_byte =
        if valid_after_high_nibble 
          then Some (Bits.to_int_trunc after_high_nibble.byte_out)
          else None
    }
  ;;

  let drive_and_observe_byte (handler : Step.Handler.t @ local) byte =
    let after_low_nibble, after_high_nibble = drive_byte handler byte in
    observe_byte ~after_low_nibble ~after_high_nibble 
  ;;

  (* fine to keep inside the testbench api, but later should be abastracted out for Evsim usage *)
  (* think of this as the test component in UVM *)
  let scenario ~bytes (handler : Step.Handler.t @ local) _initial_outputs = 
    reset handler;

    let rec loop (handler : Step.Handler.t @ local) = function
      | [] -> [] (* if empty *)
      | byte :: remaining_bytes -> (* else if matcheable as a (a :: (b ..)) *)
          let observation = drive_and_observe_byte handler byte in
          observation :: loop handler remaining_bytes
    in
    loop handler bytes
  ;;

  let create_simulator () =
    let scope = 
      Scope.create
        ~flatten_design:true
        ~auto_label_hierarchical_ports:true
        ()
    in
    Sim.create (Dut.create scope) (* how would this work across multiparamerization schemes with Configs passing to the Make schemes? *)
  ;;

  (* can ultimately be thought of the task run() from UVM *)
  (* very interesting function*)
  let run_bytes bytes =
    let simulator = create_simulator () in
    let timeout = 4 + (2 * List.length bytes) in
    match 
      Step.run_with_timeout
        ~timeout
        ()
        ~simulator
        (* ~testbench:(scenario ~bytes:bytes) *)
        ~testbench:(fun handler initial_outputs ->
          scenario ~bytes handler initial_outputs
        )
    with
    | Some observations -> observations
    | None -> failwith "Rx_byte_assembler testbench timed out run_bytes"
  ;;

  let run_byte byte =
    match run_bytes [byte] with
    | [observation] -> observation
    | _ -> failwith "Rx_byte_assembler testbench failed, expected exactly one byte observation."
  ;;

end

(* let%expect_test "example: assembling 0xAB" =  *)
(*   let observations = Testbench.run_bytes [ 0xAB ] in *)
(*   print_s [%sexp (observations: Observation.t list)]; *)
(*   [%expect *)
(*   {| *)
(*     (((valid_after_low_nibble false) (valid_after_high_nibble true) *)
(*       (completed_byte (171)))) *)
(*   |} *)
(*   ] *)
(* ;; *)

(* ---------------------------- Test Sequences ------------------------------- *)
(* is it possible to define a module thta contains a set of constants *)
module Testcases = struct

  (* re-used constants *)
  let zero            = 0x00;;
  let one             = 0x01;;
  let max             = 0xFF;;
  let max_minus_one   = 0xFF - 1;;
  let max_plus_one    = 0x00;;
  let ab              = 0xAB;;

  (* let tc3 =  *)
  (*   [ 0x00 *)
  (*   ; 0x01 *)
  (*   ; 0x0F *)
  (*   ; 0x10 *)
  (*   ; 0xAB *)
  (*   ; 0xFE *)
  (*   ; 0xFF *)
  (*   ] *)
  (* ;; *)

  (* custom write a ppx that lets these compose together instead of manualy having to add them all together to something? *)
  let tc1 = [ zero ];;
  let tc2 = [ one ];;
  let tc3 = [ zero ; one ];;
  let tc4 = [ one ; zero ];;
  let tc5 = [ max ];;
  let tc6 = [ zero ; max ];;
  let tc7 = [ max_plus_one ; max ; max_minus_one ];;
  let tc8 = [ zero
    ; zero
    ; one
    ; one
    ; max_plus_one
    ; max
    ; max
    ; max_minus_one
    ; max_plus_one
  ];;

  let all = 
    [ tc1
    ; tc2
    ; tc3
    ; tc4
    ; tc5
    ; tc6
    ; tc7
    ; tc8
    ];;

  (* is there a way we can fold all of these into 1 thing? *)
end

let check_bytes bytes = 
  let actual = Testbench.run_bytes bytes in
  let expect =
    List.map bytes ~f:expected_observation
  in
  [%test_result: Observation.t list] actual ~expect
;;

let run_tests cases =
  List.iter cases ~f:check_bytes
;;

(* ---------------------------- Unit Tests ------------------------------- *)
(* functionally typed tests, better for longer sequences *)

(* im pretty proud of this recursive function, so I'm going to leave it here *)
(* let run_tests (cases : int list list) = *)
(*   let rec loop cases =  *)
(*     match cases with *)
(*     | [] -> () *)
(*     | case :: remaining_cases -> *)
(*         let (bytes : int list) = case in *)
(*         let actual = Testbench.run_bytes bytes in *)
(*         let expect =  *)
(*           List.map bytes ~f:expected_observation in *)
(**)
(*         [%test_result: Observation.t list] actual ~expect; *)
(*         loop remaining_cases *)
(*   in *)
(*   loop cases *)
(* ;; *)

(* is it best to have (1) giant test fail? or individual? is it possible to have an alcotest-like system with suites composed of individual tests? i quite like 'suite' systems where a suite can fail but individual tests inside might not all fail - perhaps i can port my own version to work with the test_unit extension expressions? *)
(* let%test_unit "test_suite 1 : individual byte assemblies" = *)
(*   Testcases.all |> run_tests *)
(* ;; *)

(* demonstration of individual unit tests *)
let%test_unit "assembles 0"     = check_bytes [0x00];;
let%test_unit "assembles 1"     = check_bytes [0x01];;
let%test_unit "assembles 16"    = check_bytes [0x10];;
let%test_unit "assembles back-to-back" = check_bytes [ 0x00; 0xAB; 0xFF ];;
let%test_unit "assembles 0xFF"  = check_bytes [0xFF];;
let%test_unit "assembles 0x7F " = check_bytes [0x7F];;

(* demonstration of quickcheck-generated mabobs *)
(* let%test_unit "random sequence of mabobs" = *)
(*   Quickcheck.test *)
(*     ~trials:50 *)
(*     ~seed:(`Deterministic "tralalero-tralala") *)
(*     ~sexp_of:[%sexp_of: int list] *)
(*     ~f:check_bytes *)
(*     Generators.byte_sequence *)
(* ;; *)

(* demonstration of a shrinker *)
let%test_unit "random sequence of mabobs (w/ shrinker)" =
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "tungtungtung-sahur")
    ~sexp_of:[%sexp_of: int list]
    ~shrinker:
      (List.quickcheck_shrinker Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:check_bytes
    Generators.byte_sequence
;;
