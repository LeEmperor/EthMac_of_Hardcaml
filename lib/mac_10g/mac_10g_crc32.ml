(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_crc32.ml" *)
(* Masked, reflected Ethernet CRC-32 update for the 64-bit 10G datapath. Enabled byte
   lanes are consumed in increasing lane order; disabled lanes do not affect the state. *)

open! Core
open! Hardcaml
open! Signal

(* constants for crc-32 *)
let polynomial = of_int_trunc ~width:32 0xedb88320
let initial = of_int_trunc ~width:32 0xffffffff
let residue = of_int_trunc ~width:32 0xdebb20e3

(* strings together individual LFSR entries *)
let update_bit crc input_bit =
  let feedback = lsb crc ^: input_bit in
  (* shifted xor fold *)
  let shifted = srl crc ~by:1 in
  (* formatter be damned *)
  mux2 feedback (* sel *) (shifted ^: polynomial) (* shifted fold *) shifted (* shifted *)
;;

(* fold for entire bit list *)
let update_byte crc byte = List.fold (bits_lsb byte) ~init:crc ~f:update_bit

(* generalized function across bytes *)
let update crc ~data ~valid_bytes =
  if width crc <> 32
  then invalid_argf "Mac_10g_crc32.update: CRC width is %d, expected 32" (width crc) ();
  if width data <> 64
  then invalid_argf "Mac_10g_crc32.update: data width is %d, expected 64" (width data) ();
  if width valid_bytes <> 8
  then
    invalid_argf
      "Mac_10g_crc32.update: valid-byte mask width is %d, expected 8"
      (width valid_bytes)
      ();
  List.fold (List.range 0 8) ~init:crc ~f:(fun crc lane ->
    let byte = select data ~high:((8 * lane) + 7) ~low:(8 * lane) in
    mux2 (bit valid_bytes ~pos:lane) (update_byte crc byte) crc)
;;

let fcs crc = ~:crc
let has_valid_residue crc = crc ==: residue

(* current *)
module I = struct
  type 'a t =
    { crc_i : 'a [@bits 32]
    ; data_i : 'a [@bits 64]
    ; valid_bytes_i : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

(* output next *)
module O = struct
  type 'a t =
    { next_crc_o : 'a [@bits 32]
    ; next_fcs_o : 'a [@bits 32]
    ; valid_residue_o : 'a
    }
  [@@deriving hardcaml]
end

(* need hierarchy maintainencenes *)
let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  let next_crc = update i.crc_i ~data:i.data_i ~valid_bytes:i.valid_bytes_i in
  { O.next_crc_o = next_crc
  ; next_fcs_o = fcs next_crc
  ; valid_residue_o = has_valid_residue next_crc
  }
;;

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"mac_10g_crc32" create i
;;
