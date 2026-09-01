(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_axis.ml" *)
(* Direction-neutral 64-bit AXI4-Stream frame beat and keep-mask helpers used by the 10G
   MAC datapaths. Byte lane 0 is the least-significant byte.

   There's a high shot that the Jane Street AXIS library has some of this stuff, but I
   didn't find out about that until just recently, and will continue doing my own AXI
   stuff.
*)

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

(* sign extend to 4, adding as you go -> builds a popcount: number of 1's in a word *)
(* if word[i] is high, it contributes *)
let keep_byte_count (keep : t) =
  List.fold
    (bits_lsb keep)
    (* list of 8 1b signals from a single 8b word *)
    (* bits_lsb and msb are commutative -> funny enough the implementation does List.rev *)
    ~init:(zero 4) (* accumulate starting at 0 *)
    ~f:(fun count enabled -> count +: uresize enabled ~width:4)
;;

[@@@ocamlformat "disable"]
let keep_of_byte_count (byte_count : t) =
  mux
    byte_count (* sel on the byte_count fed *)
    (List.init 9 ~f:(fun count -> (* 9 entries; 0 is valid because of FCS, where nothing is getting added to a payload *)
         of_int_trunc ~width:8 ((1 lsl count) - 1) (* generates the ascending 1s pyramid *)
       )
    )
    (* if we have 2 bytes, then we need (2) bits in the resulting keep mask -> this selects 0b0000_0011 from that pyramid *)

(* mux is a tad more readable i fear *)
(* let keep_of_byte_count byte_count = *)
(*   concat_lsb (List.init 8 ~f:(fun lane -> byte_count >:. lane)) *)

let beat_has_legal_keep ~keep ~last =
  mux2
    last
    (keep_is_contiguous keep)
    (keep ==:. 0xff)
[@@@ocamlformat "enable"]
