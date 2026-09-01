(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_tx.ml" *)
(* Functional lane-0 10G XGMII transmitter with padding, FCS, termination, and a
   conservative interpacket gap. *)

open! Core
open! Hardcaml
open! Signal
open! Xgmii_of_hardcaml

module I = struct
  type 'a t =
    { clock_i : 'a
    ; reset_i : 'a
    ; enable_i : 'a
    ; counters_clear_i : 'a
    ; buffer_data_i : 'a [@bits 64]
    ; buffer_keep_i : 'a [@bits 8]
    ; buffer_valid_i : 'a
    ; buffer_last_i : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { buffer_ready_o : 'a
    ; xgmii_txd_o : 'a [@bits 64]
    ; xgmii_txc_o : 'a [@bits 8]
    ; state_o : 'a [@bits 3]
    ; frame_pulse_o : 'a
    ; underflow_pulse_o : 'a
    ; underflow_sticky_o : 'a
    ; frames_o : 'a [@bits 64]
    ; bytes_o : 'a [@bits 64]
    ; underflows_o : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

let state_wait = 0
let state_body = 1
let state_pad = 2
let state_fcs = 3
let state_ifg = 4
let byte data lane = select data ~high:((8 * lane) + 7) ~low:(8 * lane)

let fcs_byte fcs index =
  mux (select index ~high:1 ~low:0) (List.init 4 ~f:(fun lane -> byte fcs lane))
;;

let terminal_word ~prefix_data ~prefix_count ~fcs =
  let term_fits = prefix_count <=:. 3 in
  let term_position = prefix_count +:. 4 in
  let lane_values =
    List.init 8 ~f:(fun lane ->
      let lane_signal = of_int_trunc ~width:4 lane in
      let in_prefix = lane_signal <: prefix_count in
      let fcs_offset = lane_signal -: prefix_count in
      let in_fcs = lane_signal >=: prefix_count &: (fcs_offset <:. 4) in
      let is_term = term_fits &: (term_position ==:. lane) in
      let after_term = term_fits &: (term_position <:. lane) in
      let data =
        mux2
          in_prefix
          (byte prefix_data lane)
          (mux2
             in_fcs
             (fcs_byte fcs fcs_offset)
             (mux2
                is_term
                (of_int_trunc ~width:8 Xgmii.Control_character.terminate)
                (of_int_trunc ~width:8 Xgmii.Control_character.idle)))
      in
      data, is_term |: after_term)
  in
  ( { Xgmii.Word.data = concat_lsb (List.map lane_values ~f:fst)
    ; control = concat_lsb (List.map lane_values ~f:snd)
    }
  , term_fits )
;;

let fcs_word ~fcs ~index =
  let remaining = of_int_trunc ~width:4 4 -: uresize index ~width:4 in
  let lane_values =
    List.init 8 ~f:(fun lane ->
      let lane_signal = of_int_trunc ~width:4 lane in
      let in_fcs = lane_signal <: remaining in
      let is_term = lane_signal ==: remaining in
      let source_index = uresize index ~width:4 +: lane_signal in
      let data =
        mux2
          in_fcs
          (fcs_byte fcs source_index)
          (mux2
             is_term
             (of_int_trunc ~width:8 Xgmii.Control_character.terminate)
             (of_int_trunc ~width:8 Xgmii.Control_character.idle))
      in
      data, ~:in_fcs)
  in
  { Xgmii.Word.data = concat_lsb (List.map lane_values ~f:fst)
  ; control = concat_lsb (List.map lane_values ~f:snd)
  }
;;

let create (scope : Scope.t) (i : _ I.t) : _ O.t =
  let ( -- ) = Scope.naming scope in
  let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in
  let reg_var width = Always.Variable.reg ~enable:vdd ~width spec in
  let state = reg_var 3 in
  let crc = reg_var 32 in
  let covered_count = reg_var 17 in
  let stored_fcs = reg_var 32 in
  let fcs_index = reg_var 3 in
  let pending_frame_length = reg_var 17 in
  let ifg_words = reg_var 2 in
  let frames = reg_var 64 in
  let bytes = reg_var 64 in
  let underflows = reg_var 64 in
  let underflow_sticky = reg_var 1 in
  let is_wait = state.value ==:. state_wait in
  let is_body = state.value ==:. state_body in
  let is_pad = state.value ==:. state_pad in
  let is_fcs = state.value ==:. state_fcs in
  let is_ifg = state.value ==:. state_ifg in
  let start_word =
    Xgmii.of_lane_bytes
      ([ Xgmii.Control_character.start ] @ List.init 6 ~f:(Fn.const 0x55) @ [ 0xd5 ])
      ~control:0x01
  in
  let masked_body_data =
    concat_lsb
      (List.init 8 ~f:(fun lane ->
         mux2 (bit i.buffer_keep_i ~pos:lane) (byte i.buffer_data_i lane) (zero 8)))
  in
  let body_count = Mac_10g_axis.keep_byte_count i.buffer_keep_i in
  let count_after_body = covered_count.value +: uresize body_count ~width:17 in
  let pad_needed =
    mux2
      (count_after_body <:. 60)
      (of_int_trunc ~width:17 60 -: count_after_body)
      (zero 17)
  in
  let body_space = of_int_trunc ~width:4 8 -: body_count in
  let body_pad_count =
    mux2
      (pad_needed <=: uresize body_space ~width:17)
      (select pad_needed ~high:3 ~low:0)
      body_space
  in
  let body_prefix_count = body_count +: body_pad_count in
  let body_crc_count = mux2 i.buffer_last_i body_prefix_count body_count in
  let body_crc_mask = Mac_10g_axis.keep_of_byte_count body_crc_count in
  let body_next_crc =
    Mac_10g_crc32.update crc.value ~data:masked_body_data ~valid_bytes:body_crc_mask
  in
  let body_final_fcs = Mac_10g_crc32.fcs body_next_crc in
  let body_padding_complete = pad_needed <=: uresize body_space ~width:17 in
  let body_terminal_word, body_term_fits =
    terminal_word
      ~prefix_data:masked_body_data
      ~prefix_count:body_prefix_count
      ~fcs:body_final_fcs
  in
  let body_completed_length = count_after_body +: pad_needed +:. 4 in
  let pad_remaining = of_int_trunc ~width:17 60 -: covered_count.value in
  let pad_count =
    mux2
      (pad_remaining <=:. 8)
      (select pad_remaining ~high:3 ~low:0)
      (of_int_trunc ~width:4 8)
  in
  let pad_mask = Mac_10g_axis.keep_of_byte_count pad_count in
  let pad_next_crc =
    Mac_10g_crc32.update crc.value ~data:(zero 64) ~valid_bytes:pad_mask
  in
  let pad_final_fcs = Mac_10g_crc32.fcs pad_next_crc in
  let pad_terminal_word, pad_term_fits =
    terminal_word ~prefix_data:(zero 64) ~prefix_count:pad_count ~fcs:pad_final_fcs
  in
  let stored_fcs_word = fcs_word ~fcs:stored_fcs.value ~index:fcs_index.value in
  let body_underflow = i.enable_i &: is_body &: ~:(i.buffer_valid_i) -- "underflow" in
  let frame_pulse = Always.Variable.wire ~default:gnd () in
  let frame_length_pulse = Always.Variable.wire ~default:(zero 17) () in
  let output_word =
    let body_word =
      mux2
        i.buffer_valid_i
        (mux2
           i.buffer_last_i
           (mux2 body_padding_complete body_terminal_word.data masked_body_data)
           i.buffer_data_i)
        Xgmii.error_word.data
    in
    let body_control =
      mux2
        i.buffer_valid_i
        (mux2
           (i.buffer_last_i &: body_padding_complete)
           body_terminal_word.control
           (zero 8))
        Xgmii.error_word.control
    in
    let wait_start = i.enable_i &: i.buffer_valid_i in
    { Xgmii.Word.data =
        mux
          state.value
          [ mux2 wait_start start_word.data Xgmii.idle_word.data
          ; body_word
          ; mux2 (pad_remaining <=:. 8) pad_terminal_word.data (zero 64)
          ; stored_fcs_word.data
          ; Xgmii.idle_word.data
          ; Xgmii.idle_word.data
          ; Xgmii.idle_word.data
          ; Xgmii.idle_word.data
          ]
    ; control =
        mux
          state.value
          [ mux2 wait_start start_word.control Xgmii.idle_word.control
          ; body_control
          ; mux2 (pad_remaining <=:. 8) pad_terminal_word.control (zero 8)
          ; stored_fcs_word.control
          ; Xgmii.idle_word.control
          ; Xgmii.idle_word.control
          ; Xgmii.idle_word.control
          ; Xgmii.idle_word.control
          ]
    }
  in
  Always.(
    compile
      [ if_
          ~:(i.enable_i)
          [ state <--. state_wait
          ; crc <-- Mac_10g_crc32.initial
          ; covered_count <--. 0
          ; ifg_words <--. 0
          ]
          [ when_
              (is_wait &: i.buffer_valid_i)
              [ state <--. state_body
              ; crc <-- Mac_10g_crc32.initial
              ; covered_count <--. 0
              ]
          ; when_
              is_body
              [ if_
                  ~:(i.buffer_valid_i)
                  [ state <--. state_ifg; ifg_words <--. 2 ]
                  [ crc <-- body_next_crc
                  ; covered_count
                    <-- covered_count.value +: uresize body_crc_count ~width:17
                  ; when_
                      i.buffer_last_i
                      [ if_
                          ~:body_padding_complete
                          [ state <--. state_pad ]
                          [ if_
                              body_term_fits
                              [ state <--. state_ifg
                              ; ifg_words <--. 2
                              ; frame_pulse <-- vdd
                              ; frame_length_pulse <-- body_completed_length
                              ]
                              [ state <--. state_fcs
                              ; stored_fcs <-- body_final_fcs
                              ; fcs_index
                                <-- uresize
                                      (of_int_trunc ~width:4 8 -: body_prefix_count)
                                      ~width:3
                              ; pending_frame_length <-- body_completed_length
                              ]
                          ]
                      ]
                  ]
              ]
          ; when_
              is_pad
              [ crc <-- pad_next_crc
              ; covered_count <-- covered_count.value +: uresize pad_count ~width:17
              ; if_
                  (pad_remaining <=:. 8)
                  [ if_
                      pad_term_fits
                      [ state <--. state_ifg
                      ; ifg_words <--. 2
                      ; frame_pulse <-- vdd
                      ; frame_length_pulse <--. 64
                      ]
                      [ state <--. state_fcs
                      ; stored_fcs <-- pad_final_fcs
                      ; fcs_index
                        <-- uresize (of_int_trunc ~width:4 8 -: pad_count) ~width:3
                      ; pending_frame_length <--. 64
                      ]
                  ]
                  [ state <--. state_pad ]
              ]
          ; when_
              is_fcs
              [ state <--. state_ifg
              ; ifg_words <--. 2
              ; frame_pulse <-- vdd
              ; frame_length_pulse <-- pending_frame_length.value
              ]
          ; when_
              is_ifg
              [ if_
                  (ifg_words.value <=:. 1)
                  [ state <--. state_wait; ifg_words <--. 0 ]
                  [ ifg_words <-- ifg_words.value -:. 1 ]
              ]
          ]
      ; if_
          i.counters_clear_i
          [ frames <--. 0; bytes <--. 0; underflows <--. 0; underflow_sticky <--. 0 ]
          [ when_
              frame_pulse.value
              [ frames <-- frames.value +:. 1
              ; bytes <-- bytes.value +: uresize frame_length_pulse.value ~width:64
              ]
          ; when_
              body_underflow
              [ underflows <-- underflows.value +:. 1; underflow_sticky <--. 1 ]
          ]
      ]);
  { O.buffer_ready_o = i.enable_i &: is_body
  ; xgmii_txd_o = output_word.data
  ; xgmii_txc_o = output_word.control
  ; state_o = state.value
  ; frame_pulse_o = frame_pulse.value
  ; underflow_pulse_o = body_underflow
  ; underflow_sticky_o = underflow_sticky.value
  ; frames_o = frames.value
  ; bytes_o = bytes.value
  ; underflows_o = underflows.value
  }
;;

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"mac_10g_tx" create i
;;
