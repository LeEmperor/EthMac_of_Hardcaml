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

(* from lanes 0 to anything is contiguous -> no holes, no leading gap *)
let keep_is_contiguous keep =
  List.range 1 9 (* generate a list of [1,9) *)
  |> List.map ~f:(fun byte_count -> keep ==:. (1 lsl byte_count) - 1)
    (* plug each vlaue of [1,8] -> creates a cascading ladder of 1s from 0 to 8 -> all 1s
       of a certain base max *)
  |> reduce ~f:( |: ) (* OR-fold the entire thing *)
;;

(* sizing is too small to matter here *)
(* let keep_is_contiguous keep = *)
(* let k = uresize keep ~width:9 in *)
(* k &: k +:. 1 ==:. 0 &: (keep <>:. 0) *)
(* ;; *)

let keep_byte_count keep =
  List.fold
    (bits_lsb keep) (* list of 8 1b signals from a single 8b word *)
    ~init:(zero 4) (* sign extend to 4b *)
    ~f:(fun count enabled -> count +: uresize enabled ~width:4)
;;

let keep_of_byte_count byte_count =
  mux byte_count (List.init 9 ~f:(fun count -> of_int_trunc ~width:8 ((1 lsl count) - 1)))
;;

let beat_has_legal_keep ~keep ~last = mux2 last (keep_is_contiguous keep) (keep ==:. 0xff)
