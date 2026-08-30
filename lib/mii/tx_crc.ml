(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_crc.ml" *)
(* CRC-32 generator for the transmit path. Accumulates transmitted bytes and exposes the
   FCS (the bitwise complement of the accumulator in little-endian byte order) through a
   byte-select mux so [Tx_datapath] can emit one FCS byte per clock in the [Fcs] state. *)

open! Core
open! Hardcaml
open! Signal
open! Always
open! Variable

module I = struct
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; en : 'a (* dropping resets the accumulator to 0xFFFFFFFF *)
    ; data : 'a [@bits 8]
    ; data_valid : 'a (* accumulate data into CRC this cycle *)
    ; byte_sel : 'a [@bits 2] (* selects which FCS byte to expose: 0=LSB..3=MSB *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { fcs_byte : 'a [@bits 8] (* ~crc_reg byte indexed by byte_sel *)
    ; crc_out : 'a [@bits 32] (* raw accumulator for debug / mac_top read-back *)
    ; keep : 'a
    }
  [@@deriving hardcaml]
end

let const_crc_poly = Signal.of_int_trunc ~width:32 0xEDB88320

let crc_bit current_crc input_bit =
  let lsb = current_crc.:[0, 0] in
  let feedback = lsb ^: input_bit in
  let shifted = srl current_crc ~by:1 in
  mux2 feedback (shifted ^: const_crc_poly) shifted
;;

let crc_byte current_crc input_byte =
  List.fold (Signal.bits_lsb input_byte) ~init:current_crc ~f:crc_bit
;;

let create (scope : Scope.t) i : _ O.t =
  let _scope = Scope.sub_scope scope "tx_crc_scope" in
  let clock = i.I.clock in
  let reset = i.I.reset in
  let en = i.I.en in
  let data = i.I.data in
  let data_valid = i.I.data_valid in
  let byte_sel = i.I.byte_sel in
  let rising_edge = Reg_spec.create ~clock () in
  let crc_init = Signal.of_int_trunc ~width:32 0xFFFFFFFF in
  let crc_reg = reg ~enable:vdd ~width:32 rising_edge in
  compile
    [ if_
        (reset |: ~:en)
        [ crc_reg <-- crc_init ]
        [ when_ data_valid [ crc_reg <-- crc_byte crc_reg.value data ] ]
    ];
  (*
   * FCS = bitwise-NOT of accumulator, transmitted little-endian:
   * byte 0 (byte_sel=0) = bits [7:0]  of ~crc
   * byte 3 (byte_sel=3) = bits [31:24] of ~crc
   *)
  let fcs = ~:(crc_reg.value) in
  { fcs_byte =
      mux
        byte_sel
        [ select fcs ~high:7 ~low:0
        ; select fcs ~high:15 ~low:8
        ; select fcs ~high:23 ~low:16
        ; select fcs ~high:31 ~low:24
        ]
  ; crc_out = crc_reg.value
  ; keep = lsb crc_reg.value
  }
;;

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"tx_crc" create i
;;
