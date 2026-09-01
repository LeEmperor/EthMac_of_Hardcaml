(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_axis.ml" *)
(* Direction-neutral 64-bit AXI4-Stream frame beat and keep-mask helpers used by the 10G
   MAC datapaths. Byte lane 0 is the least-significant byte. *)

open! Core
open! Hardcaml
open! Signal

module Beat = struct
  type 'a t =
    { data : 'a [@bits 64]
    ; keep : 'a [@bits 8]
    ; last : 'a
    ; user : 'a
    }
  [@@deriving hardcaml]
end

let keep_is_contiguous keep =
  List.range 1 9
  |> List.map ~f:(fun byte_count -> keep ==:. (1 lsl byte_count) - 1)
  |> reduce ~f:( |: )
;;

let keep_byte_count keep =
  List.fold (bits_lsb keep) ~init:(zero 4) ~f:(fun count enabled ->
    count +: uresize enabled ~width:4)
;;

let keep_of_byte_count byte_count =
  mux byte_count (List.init 9 ~f:(fun count -> of_int_trunc ~width:8 ((1 lsl count) - 1)))
;;

let beat_has_legal_keep ~keep ~last = mux2 last (keep_is_contiguous keep) (keep ==:. 0xff)
