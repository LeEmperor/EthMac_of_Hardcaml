(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "xgmii.ml" *)
(* Shared 64-bit SDR XGMII representation and lane-oriented helpers.

   Lane 0 occupies data[7:0], control[0], and is earliest in time. This module is
   direction-neutral so it can be shared by the MAC, PCS, and verification code.
*)

open! Core
open! Hardcaml
open! Signal

module Word = struct
  type 'a t =
    { data : 'a [@bits 64]
    ; control : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module Control_character = struct
  let idle = 0x07 (* /I *)
  let start = 0xfb (* /S *)
  let terminate = 0xfd (* /T *)
  let error = 0xfe (* /E *)
  let sequence_ordered_set = 0x9c (* /O *)
end

let check_lane lane =
  if lane < 0 || lane >= 8
  then invalid_argf "XGMII lane %d is outside the range 0..7" lane ()
;;

let is_legal_start_lane lane = lane = 0 || lane = 4

let lane_byte (word : Signal.t Word.t) lane =
  check_lane lane;
  select word.data ~high:((8 * lane) + 7) ~low:(8 * lane)
;;

let lane_is_control (word : Signal.t Word.t) lane =
  check_lane lane;
  bit word.control ~pos:lane
;;

let of_lane_bytes bytes ~control =
  if List.length bytes <> 8
  then invalid_argf "an XGMII word requires 8 lane bytes, got %d" (List.length bytes) ();
  { Word.data = concat_lsb (List.map bytes ~f:(of_int_trunc ~width:8))
  ; control = of_int_trunc ~width:8 control
  }
;;

let set_lane (word : Signal.t Word.t) ~lane ~byte ~is_control =
  check_lane lane;
  if width byte <> 8
  then invalid_argf "an XGMII lane byte must be 8 bits, got %d" (width byte) ();
  if width is_control <> 1
  then
    invalid_argf "an XGMII lane control value must be 1 bit, got %d" (width is_control) ();
  { Word.data =
      concat_lsb (List.init 8 ~f:(fun n -> if n = lane then byte else lane_byte word n))
  ; control =
      concat_lsb
        (List.init 8 ~f:(fun n -> if n = lane then is_control else lane_is_control word n))
  }
;;

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
