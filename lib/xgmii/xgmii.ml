(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "xgmii.ml" *)
(* Internal 64-bit SDR XGMII representation and lane-oriented helpers. The original XGMII
   spec is apparently DDR, but no one actually adheres to that or something lmao.

   Lane 0 occupies data[7:0] and is earliest in time. See [docs/xgmii_contract.md] for the
   complete interface contract.
*)

open! Core
open! Hardcaml
open! Signal

(* [Word] is a direction-neutral value shared by producers and consumers. Its fields are
   therefore not top-level input or output ports and intentionally do not carry [_i] or
   [_o] suffixes. *)
module Word = struct
  type 'a t =
    { data : 'a [@bits 64]
    ; control : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

(* XGMII spec control characters -> these are what sit in the txc line, and help the PCS
   to create the little 2b header that is attached to the 64b bus -> making it 66b in
   total
*)
module Control_character = struct
  let idle = 0x07 (* /I - '11' header *)
  let start = 0xfb (* /S - often has a specific encoding *)
  let terminate = 0xfd (* /T - can occur in any lane *)
  let error = 0xfe (* oof *)

  let sequence_ordered_set =
    0x9c (* link faulting encoding; not entirely sure what this does *)
  ;;
end

(* helper for the lane boundaries -> might be able to use in elaboration, but namely will
   be grabbed by the verification suites
*)
let check_lane lane =
  if lane < 0 || lane >= 8
  then invalid_argf "XGMII lane %d is outside the range 0..7" lane ()
;;

(* helper for grabbing a specific 8b from the word (Which itself is a vector) *)
let lane_byte (word : Signal.t Word.t) lane =
  check_lane lane;
  select word.data ~high:((8 * lane) + 7) ~low:(8 * lane)
;;

(* does the same as above but for control mask items (stuff from txc) *)
let lane_is_control (word : Signal.t Word.t) lane =
  check_lane lane;
  bit word.control ~pos:lane
;;

(* cates togethers the bytes into a list -> probably a better naming scheme out there but
   lmao *)
let of_lane_bytes bytes ~control =
  if List.length bytes <> 8
  then invalid_argf "an XGMII word requires 8 lane bytes, got %d" (List.length bytes) ();
  { Word.data = concat_lsb (List.map bytes ~f:(of_int_trunc ~width:8))
  ; control = of_int_trunc ~width:8 control
  }
;;

(* accessor overide for overriding stuff in a single lane *)
let set_lane (word : Signal.t Word.t) ~lane ~byte ~is_control =
  check_lane lane;
  { Word.data =
      concat_lsb (List.init 8 ~f:(fun n -> if n = lane then byte else lane_byte word n))
  ; control =
      concat_lsb
        (List.init 8 ~f:(fun n -> if n = lane then is_control else lane_is_control word n))
  }
;;

(* below are some common helpers for constructing idle and error words as they are
   somewhat annoying to have in the verif suite itself *)
let idle_word =
  of_lane_bytes (List.init 8 ~f:(Fn.const Control_character.idle)) ~control:0xff
;;

let error_word =
  of_lane_bytes (List.init 8 ~f:(Fn.const Control_character.error)) ~control:0xff
;;

let local_fault_word =
  of_lane_bytes
    [ Control_character.sequence_ordered_set
    ; 0x00
    ; 0x00
    ; 0x01
    ; Control_character.sequence_ordered_set
    ; 0x00
    ; 0x00
    ; 0x01
    ]
    ~control:0x11
;;

let remote_fault_word =
  of_lane_bytes
    [ Control_character.sequence_ordered_set
    ; 0x00
    ; 0x00
    ; 0x02
    ; Control_character.sequence_ordered_set
    ; 0x00
    ; 0x00
    ; 0x02
    ]
    ~control:0x11
;;
