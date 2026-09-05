(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "base_r_block.ml" *)
(* A semantic 64b/66b block at the boundary between the PCS and a transceiver gearbox.

   The two-bit sync header is separate because this is how the UltraScale+ GTY gearbox
   exposes the block. The PCS scrambles [data], but never [header].

   Bit zero is the first bit of each field transmitted by the semantic PCS stream. Thus,
   an on-wire data header of 01 is represented as 2'b10, and an on-wire control header of
   10 is represented as 2'b01. A device-specific adapter may reverse these values if its
   transceiver port uses a different convention.
*)

open! Hardcaml

module Sync_header = struct
  let data = 0b10
  let control = 0b01
end

type 'a t =
  { data : 'a [@bits 64]
  ; header : 'a [@bits 2]
  }
[@@deriving hardcaml]
