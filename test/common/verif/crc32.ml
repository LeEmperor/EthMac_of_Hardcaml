(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "crc32.ml" *)
(* Software reference model for the Ethernet CRC-32 implemented by [Rx_crc] / [Tx_crc].

   Reflected polynomial 0xEDB88320, accumulator initialized to 0xFFFFFFFF, bits fed
   least-significant first. Two views of the same accumulator matter to the hardware:

   - [bytes] returns the raw accumulator, which is what [Rx_crc.crc_out] holds and what
     must equal [residue] once a frame and its FCS have both been fed through.
   - [fcs] returns the accumulator after the final inversion, i.e. the 32-bit FCS word
     that [Tx_crc] exposes one byte at a time; [fcs_bytes] splits it into the four
     little-endian bytes in transmission order.

   Tags: [{ "ACTIVE" ; "TEST" ; "REFERENCE_MODEL" ; "COMMON_ITEMS" }]
*)

open! Core

let polynomial = 0xEDB88320
let init = 0xFFFFFFFF

(* The accumulator value a receiver lands on after clocking a frame followed by its own
   FCS.

   Equal to ~0x2144DF1C, the standard residue quoted without the output inversion, and the
   constant [Rx_crc] compares [crc_out] against.
*)
let residue = 0xDEBB20E3

(* Mirrors [Rx_crc.crc_bit] / [Tx_crc.crc_bit]: xor the incoming bit into the LSB of the
   accumulator, shift right, and fold the polynomial back in when that bit was set.

   This was such a fun function back when I wrote it.
*)
let bit crc input_bit =
  let lsb = crc land 1 in
  let feedback = lsb lxor (input_bit land 1) in
  let shifted = crc lsr 1 in
  if feedback = 1 then shifted lxor polynomial else shifted
;;

(* Probably a nicer Array/List pair conversion on this like I've done with BASE-R
   scramblers before, but this does just fine for now.
*)
let byte crc input_byte =
  let crc = ref crc in
  for index = 0 to 7 do
    crc := bit !crc ((input_byte lsr index) land 1)
  done;
  !crc
;;

(* Raw accumulator, no final inversion. *)
(* if we name a fold function an origami is that cool *)
let bytes ?(init = init) input_bytes = List.fold input_bytes ~init ~f:byte

(* A software-only model of the 10G masked word primitive. [data] is in XGMII/AXI lane
   order, so list element zero corresponds to mask bit zero. *)
let masked_bytes ?(init = init) data ~valid_bytes =
  List.foldi data ~init ~f:(fun lane crc input_byte ->
    if valid_bytes land (1 lsl lane) <> 0 then byte crc input_byte else crc)
;;

(* The FCS word: the accumulator after the final inversion. *)
let fcs input_bytes = bytes input_bytes lxor 0xFFFFFFFF

(* Wire order: least significant FCS byte first. *)
let fcs_bytes input_bytes =
  let value = fcs input_bytes in
  List.init 4 ~f:(fun index -> (value lsr (8 * index)) land 0xFF)
;;
