(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_tx_ingress.ml" *)
(* AXI4-Stream frame validation and transactional TX-buffer control. *)

open! Core
open! Hardcaml
open! Signal

module type Config = sig
  val max_supported_frame_length : int
  val buffer_length_width : int
end

module Make (Config : Config) = struct
  let () =
    if Config.max_supported_frame_length < 64
       || Config.max_supported_frame_length > 0xffff
    then invalid_arg "Mac_10g_tx_ingress: max_supported_frame_length must be in 64..65535";
    if Config.buffer_length_width < 4 || Config.buffer_length_width > 17
    then invalid_arg "Mac_10g_tx_ingress: buffer_length_width must be in 4..17"
  ;;

  module I = struct
    type 'a t =
      { clock_i : 'a
      ; reset_i : 'a
      ; enable_i : 'a
      ; counters_clear_i : 'a
      ; axis_data_i : 'a [@bits 64]
      ; axis_keep_i : 'a [@bits 8]
      ; axis_valid_i : 'a
      ; axis_last_i : 'a
      ; axis_user_i : 'a
      ; buffer_write_ready_i : 'a
      ; buffer_commit_ready_i : 'a
      ; buffer_frame_length_i : 'a [@bits Config.buffer_length_width]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { axis_ready_o : 'a
      ; buffer_write_data_o : 'a [@bits 64]
      ; buffer_write_keep_o : 'a [@bits 8]
      ; buffer_write_valid_o : 'a
      ; buffer_commit_o : 'a
      ; buffer_rollback_o : 'a
      ; frame_commit_pulse_o : 'a
      ; drop_pulse_o : 'a
      ; malformed_pulse_o : 'a
      ; drops_o : 'a [@bits 64]
      ; malformed_frames_o : 'a [@bits 64]
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =
    let ( -- ) = Scope.naming scope in
    let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in
    let reg_var width = Always.Variable.reg ~enable:vdd ~width spec in
    let dropping = reg_var 1 in
    let malformed = reg_var 1 in
    let pending_commit = reg_var 1 in
    let drops = reg_var 64 in
    let malformed_frames = reg_var 64 in
    let keep_legal =
      Mac_10g_axis.beat_has_legal_keep ~keep:i.axis_keep_i ~last:i.axis_last_i
    in
    let beat_count = Mac_10g_axis.keep_byte_count i.axis_keep_i in
    let frame_length_after_beat =
      uresize i.buffer_frame_length_i ~width:17 +: uresize beat_count ~width:17
    in
    let length_legal =
      frame_length_after_beat
      >=:. 14
      &: (frame_length_after_beat <=:. Config.max_supported_frame_length - 4)
    in
    let final_rejected = i.axis_last_i &: (i.axis_user_i |: ~:length_legal) in
    let reject_current = ~:keep_legal |: final_rejected in
    let ready =
      i.enable_i
      &: ~:(pending_commit.value)
      &: mux2 (dropping.value |: reject_current) vdd i.buffer_write_ready_i
         -- "axis_ready"
    in
    let accepted = i.axis_valid_i &: ready -- "axis_accepted" in
    let accepted_reject = accepted &: reject_current in
    let accepted_good = accepted &: ~:(dropping.value) &: ~:reject_current in
    let commit_request = accepted_good &: i.axis_last_i in
    let commit = pending_commit.value |: commit_request -- "buffer_commit" in
    let rollback = accepted_reject &: ~:(dropping.value) -- "buffer_rollback" in
    let completed_drop =
      accepted &: i.axis_last_i &: (dropping.value |: reject_current) -- "completed_drop"
    in
    let completed_malformed =
      completed_drop
      &: (malformed.value |: ~:keep_legal |: ~:length_legal) -- "completed_malformed"
    in
    Always.(
      compile
        [ if_
            i.counters_clear_i
            [ drops <--. 0; malformed_frames <--. 0 ]
            [ when_ completed_drop [ drops <-- drops.value +:. 1 ]
            ; when_
                completed_malformed
                [ malformed_frames <-- malformed_frames.value +:. 1 ]
            ]
        ; when_ (commit_request &: ~:(i.buffer_commit_ready_i)) [ pending_commit <--. 1 ]
        ; when_
            (pending_commit.value &: i.buffer_commit_ready_i)
            [ pending_commit <--. 0 ]
        ; when_
            accepted
            [ if_
                i.axis_last_i
                [ dropping <--. 0; malformed <--. 0 ]
                [ when_
                    (~:(dropping.value) &: reject_current)
                    [ dropping <--. 1; malformed <-- (~:keep_legal |: ~:length_legal) ]
                ]
            ]
        ]);
    { O.axis_ready_o = ready
    ; buffer_write_data_o = i.axis_data_i
    ; buffer_write_keep_o = i.axis_keep_i
    ; buffer_write_valid_o = accepted_good
    ; buffer_commit_o = commit
    ; buffer_rollback_o = rollback
    ; frame_commit_pulse_o = commit &: i.buffer_commit_ready_i
    ; drop_pulse_o = completed_drop
    ; malformed_pulse_o = completed_malformed
    ; drops_o = drops.value
    ; malformed_frames_o = malformed_frames.value
    }
  ;;

  let hierarchical ?instance scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ?instance
      ~scope
      ~name:(sprintf "mac_10g_tx_ingress_%d" Config.max_supported_frame_length)
      create
      i
  ;;
end
