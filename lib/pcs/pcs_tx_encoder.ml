(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "pcs_tx_encoder.ml" *)
(* Combinational Clause 49 transmit encoder for the internal 64-bit SDR XGMII interface.

   The output payload is the unscrambled 64-bit block payload. Payload bit zero, sync
   header bit zero, and XGMII lane zero are earliest in the semantic serial stream. An
   illegal XGMII word is replaced by a legal all-error control block and reported through
   [tx_bad_xgmii_o].
*)

open! Core
open! Hardcaml
open! Signal
open! Xgmii_of_hardcaml

module I = struct
  type 'a t =
    { xgmii_txd_i : 'a [@bits 64]
    ; xgmii_txc_i : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { encoded_payload_o : 'a [@bits 64]
    ; encoded_header_o : 'a [@bits 2]
    ; tx_bad_xgmii_o : 'a
    }
  [@@deriving hardcaml]
end

module Block_type = struct
  let control = 0x1e
  let control_ordered_set_4 = 0x2d
  let start_4 = 0x33
  let ordered_set_control = 0x4b
  let ordered_sets = 0x55
  let ordered_set_start = 0x66
  let start_0 = 0x78
  let terminate = [| 0x87; 0x99; 0xaa; 0xb4; 0xcc; 0xd2; 0xe1; 0xff |]
end

let const width value = of_int_trunc ~width value
let all signals = List.fold signals ~init:vdd ~f:( &: )
let any signals = List.fold signals ~init:gnd ~f:( |: )
let payload fields = concat_lsb fields

let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  (* aliases *)
  let word : Signal.t Xgmii.Word.t = { data = i.xgmii_txd_i; control = i.xgmii_txc_i } in

  (* 1 morbillion helpers *)
  let byte lane = Xgmii.lane_byte word lane in
  let is_control lane = Xgmii.lane_is_control word lane in
  let is_data lane = ~:(is_control lane) in
  let carries lane character = is_control lane &: (byte lane ==:. character) in
  let is_idle lane = carries lane Xgmii.Control_character.idle in
  let is_error lane = carries lane Xgmii.Control_character.error in
  let is_regular_control lane = is_idle lane |: is_error lane in
  let is_start lane = carries lane Xgmii.Control_character.start in
  let is_terminate lane = carries lane Xgmii.Control_character.terminate in
  let is_ordered_set lane = carries lane Xgmii.Control_character.sequence_ordered_set in
  let control_code lane = mux2 (is_error lane) (const 7 0x1e) (zero 7) in
  let regular_controls lanes = all (List.map lanes ~f:is_regular_control) in
  let data_lanes lanes = all (List.map lanes ~f:is_data) in
  let bytes lanes = List.map lanes ~f:byte in
  let control_codes lanes = List.map lanes ~f:control_code in
  let block_type value = const 8 value in
  let ordered_set_code = zero 4 in
  let reserved width = zero width in
  let all_data = data_lanes (List.range 0 8) in
  let all_control = regular_controls (List.range 0 8) in
  let start_0 = is_start 0 &: data_lanes (List.range 1 8) in
  let start_4 =
    regular_controls (List.range 0 4) &: is_start 4 &: data_lanes (List.range 5 8)
  in
  let ordered_set_4 =
    regular_controls (List.range 0 4) &: is_ordered_set 4 &: data_lanes (List.range 5 8)
  in
  let ordered_set_prefix = is_ordered_set 0 &: data_lanes (List.range 1 4) in
  let ordered_set_control = ordered_set_prefix &: regular_controls (List.range 4 8) in
  let ordered_sets =
    ordered_set_prefix &: is_ordered_set 4 &: data_lanes (List.range 5 8)
  in
  let ordered_set_start =
    ordered_set_prefix &: is_start 4 &: data_lanes (List.range 5 8)
  in
  let terminate_valid lane =
    data_lanes (List.range 0 lane)
    &: is_terminate lane
    &: regular_controls (List.range (lane + 1) 8)
  in
  let terminate_valids = List.init 8 ~f:terminate_valid in
  let control_payload =
    payload (block_type Block_type.control :: control_codes (List.range 0 8))
  in
  let start_0_payload =
    payload (block_type Block_type.start_0 :: bytes (List.range 1 8))
  in
  let start_4_payload =
    payload
      ([ block_type Block_type.start_4 ]
       @ control_codes (List.range 0 4)
       @ [ reserved 4 ]
       @ bytes (List.range 5 8))
  in
  let ordered_set_4_payload =
    payload
      ([ block_type Block_type.control_ordered_set_4 ]
       @ control_codes (List.range 0 4)
       @ [ ordered_set_code ]
       @ bytes (List.range 5 8))
  in
  let ordered_set_control_payload =
    payload
      ([ block_type Block_type.ordered_set_control ]
       @ bytes (List.range 1 4)
       @ [ ordered_set_code ]
       @ control_codes (List.range 4 8))
  in
  let ordered_sets_payload =
    payload
      ([ block_type Block_type.ordered_sets ]
       @ bytes (List.range 1 4)
       @ [ ordered_set_code; ordered_set_code ]
       @ bytes (List.range 5 8))
  in
  let ordered_set_start_payload =
    payload
      ([ block_type Block_type.ordered_set_start ]
       @ bytes (List.range 1 4)
       @ [ ordered_set_code; reserved 4 ]
       @ bytes (List.range 5 8))
  in
  let terminate_payload lane =
    payload
      ([ block_type Block_type.terminate.(lane) ]
       @ bytes (List.range 0 lane)
       @ (if lane = 7 then [] else [ reserved (7 - lane) ])
       @ control_codes (List.range (lane + 1) 8))
  in
  let candidates =
    [ all_data, i.xgmii_txd_i
    ; all_control, control_payload
    ; start_0, start_0_payload
    ; start_4, start_4_payload
    ; ordered_set_4, ordered_set_4_payload
    ; ordered_set_control, ordered_set_control_payload
    ; ordered_sets, ordered_sets_payload
    ; ordered_set_start, ordered_set_start_payload
    ]
    @ List.map2_exn
        terminate_valids
        (List.init 8 ~f:terminate_payload)
        ~f:(fun valid data -> valid, data)
  in
  let error_control_code = const 7 0x1e in
  let error_payload =
    payload (block_type Block_type.control :: List.init 8 ~f:(Fn.const error_control_code))
  in
  let encoded_payload =
    List.fold_right candidates ~init:error_payload ~f:(fun (valid, data) selected ->
      mux2 valid data selected)
  in
  let valid = any (List.map candidates ~f:fst) in
  { O.encoded_payload_o = encoded_payload
  ; encoded_header_o =
      mux2
        all_data
        (const 2 Base_r_block.Sync_header.data)
        (const 2 Base_r_block.Sync_header.control)
  ; tx_bad_xgmii_o = ~:valid
  }
  [@@ocamlformat "disable"]
