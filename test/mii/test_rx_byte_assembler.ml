(*
  University of Florida
  Author: Bohdan Purtell

  Test: "test_rx_byte_assembler"
  Classification: unit

  Desc:
    Originally written as a learning experience into inline, expect, and quickcheck tests in the Janestreet style.
*)

open! Core
open! Hardcaml
open! Signal

let () = Stdio.print_endline "=== Imported Test: test_rx_byte_assembler ===";;

module Byte_transaction = struct
  type t = int [@@deriving compare, equal, sexp]

  let to_nibble byte = 
    let low = byte land 0xF in
    let high = (byte lsr 4) land 0xF in
    [low; high]
  ;;

  (* should a command type go in the byte transaction module? or elsewhere? *)
  type command = 
    | Reset
    | Send_byte of int
    | Disable_for of int
  [@@deriving sexp]
end

