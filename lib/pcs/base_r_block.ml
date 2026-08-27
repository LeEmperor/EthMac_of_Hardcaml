(* A semantic 64b/66b block at the boundary between the PCS and a transceiver gearbox.

   The two-bit sync header is separate because this is how the UltraScale+ GTY gearbox
   exposes the block. The PCS scrambles [data], but never [header]. *)

open! Hardcaml

type 'a t =
  { data : 'a [@bits 64]
  ; header : 'a [@bits 2]
  }
[@@deriving hardcaml]
