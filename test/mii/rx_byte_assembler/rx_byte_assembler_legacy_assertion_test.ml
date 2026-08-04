(*
  University of Florida
  Author: Bohdan Purtell

  Legacy Assertion Test: Rx_byte_assembler

  Deprecated standalone simulation and manual assertion harness. The unit,
  Quickcheck, and expect test suites supersede this test.

  Don't use anything here lmao; eventually will have dunetest de-active this modules usage, but I think some of the integration suites use it elsewhere.

  Tags: { "DEPRECATED"
          ; "ASSERTION_TEST"
        }
*)

open! Core
open! Hardcaml
open! Mii_of_hardcaml

let () = print_endline "=== Running MAC RX Byte Assembler Testbench ==="

module Sim = Cyclesim.With_interface (Rx_byte_assembler.I) (Rx_byte_assembler.O)

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Rx_byte_assembler.create scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let ( <-- ) r v = r := Bits.of_int_trunc ~width:(Bits.width !r) v in
  let cycle () = Cyclesim.cycle sim in

  let all_ok = ref true in
  let check name cond =
    if not cond then all_ok := false;
    printf "  %-40s: %s\n" name (if cond then "PASS" else "FAIL")
  in

  (* reset *)
  i.Rx_byte_assembler.I.reset <-- 1;
  i.en <-- 0;
  i.rx_data <-- 0;
  cycle ();
  i.reset <-- 0;
  i.en <-- 1;

  (* Feed a byte as (lo nibble, hi nibble) — MII order — and assert the assembler
     stays quiet mid-pair, then strobes byte_valid with the reassembled byte. *)
  let send_byte_check byte =
    let lo = byte land 0xF in
    let hi = (byte lsr 4) land 0xF in
    i.rx_data <-- lo;
    cycle ();
    check
      (sprintf "byte %02x: mid-pair byte_valid=0" byte)
      (not (Bits.to_bool !(o.Rx_byte_assembler.O.byte_valid)));
    i.rx_data <-- hi;
    cycle ();
    check (sprintf "byte %02x: complete byte_valid=1" byte) (Bits.to_bool !(o.byte_valid));
    check
      (sprintf "byte %02x: byte_out correct" byte)
      (Bits.to_int_trunc !(o.byte_out) = byte)
  in
  List.iter [ 0xAB; 0x12; 0xFF; 0x00; 0x5D ] ~f:send_byte_check;

  (* en low: assembler must not latch a new nibble or raise byte_valid *)
  i.en <-- 0;
  i.rx_data <-- 0x7;
  cycle ();
  check "en=0: byte_valid stays 0" (not (Bits.to_bool !(o.byte_valid)));

  printf "\n=== %s ===\n" (if !all_ok then "ALL PASS" else "FAILURES PRESENT");
  print_endline "=== SIMULATION COMPLETE ===";
  if not !all_ok then exit 1
;;
