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

(* states - alwyas FSM exists for a reason but not a good one?? *)
let state_wait = 0
let state_body = 1
let state_pad = 2
let state_fcs = 3
let state_ifg = 4

(* and to think some poor soul would do this in SystemVerilog; it's me; I'm the poor soul *)
let byte data lane = select data ~high:((8 * lane) + 7) ~low:(8 * lane)

(* very surprised the formatter didn't shoot me in the head here lmao; i think its because
   of the @@@ attributes elsewhere but not too sure; *)
(* goes unused in a few places - gets inlined hwere it should be used; might want to
   verify this individual function *)
let fcs_byte (fcs : t) (index : t) =
  mux
    (select index ~high:1 ~low:0)
    (* verilog-style slice on index; grabs index[1:0] -> uses that as the mux select *)
    (List.init 4 ~f:(fun lane -> byte fcs lane) (* forms 4 items, of which? *))
;;

(* slices the fcs into 4 bytes -> fcs[7:0], fcs[15:8], fcs[23:16], fcs[31:24] = byte fcs
   0, 1, 2, 3 *)

(* forms a byte mux *)

(* can i close the frame in this word? *)
let terminal_word
  ~(prefix_data : t)
  (* 64b *)
  (* composes a partial data word, and its length *)
  ~(prefix_count : t)
  (* 4b - how many lanes are real? *)
  (* with the fcs as well *)
  ~(fcs : t)
  (* finished 32b fcs of whole frame *)
  =
  (* our budget is prefix_count + 4 (FCS) + 1 (/T) <= 8 *)
  (* means we have 5 items that are tagged onto the end of a payload beat *)
  let term_fits = prefix_count <=:. 3 in
  (* prefix is payload items, check that we have at most 3 *)
  let term_position = prefix_count +:. 4 in
  (* where is the term ending? it doesn't always end perfectly; may need revisiting later
     when we consider inter-frame leaving and not wasting IDLE things *)

  (* consider example final word: P P P F F F F T 0 1 2 3 4 5 6 7

     prefix = 3; we have (3) payload bytes that the word needs to finish off consuming 4
     F(CS) bytes that are necessary to exist 1 T byte, which is for XGMII recognition of
     the end of a frame;
  *)

  (* each of these are the (data, control) pair -> we need to extract them at the end *)
  let (lane_values : (t * t) list) =
    (* pair (tuple) list -> the word and it's control status *)

    (* each lane, where lane is an integer in iteration *)
    List.init 8 ~f:(fun lane ->
      (*might need to worry about "lane" name shadowing; *)

      (* int arg of "fun lane" into signal representation *)
      let (lane_signal : t) = of_int_trunc ~width:4 lane in
      (* forms [0; 1; 2; 3; 4; 5; 6; 7] as their Signal.t forms *)

      (* the lot of these in_thing's are mutually exclusive -> can this be formally
         proven? *)
      (* because of this the prio mux might be killable *)
      (* could one-hot flatten it -> might matter for 100G applications *)

      (* does the lane hold real prefix data? (non fcs data) *)
      let in_prefix = lane_signal <: prefix_count in
      (* for example: lane's 0, 1, 2 are all "in-prefix" -> [0, 1, 2] <: 3*)

      (* where does the offset begin? *)
      (* consider lane 3 of the example frame; fcs_offset = 3 - 3 => 0

         this technically wraps as it is subtraction; which is good for other comparators
      *)
      let fcs_offset = lane_signal -: prefix_count in
      (* lane 3: 3 >= 3 : true fcs_offset = (3 -: 3) = 0 0 <:. 4 : true

         true AND true => in_fcs = true

         lane 2: 2 >= 3 : false -> wholly evaluates to false, correct as we're in payload
         in this case
      *)
      let in_fcs = lane_signal >=: prefix_count &: (fcs_offset <:. 4) in
      (* is the lane we're looking at specifically the TERM? *)
      let is_term = term_fits &: (term_position ==:. lane) in
      (* are we after the term? probably easier to read other way around *)
      let after_term = term_fits &: (term_position <:. lane) in
      (* let after_term = term_fits &: (lane >:. term_position) in *)
      (* for some reason this isn't valid *)

      (* one of my worst chain muxes ever I fear *)
      let data =
        mux2
          in_prefix (* is the data payload data? *)
          (byte prefix_data lane)
          (* yes : grab the byte out of the prefix_data (which is not specifically a
             single byte, in example is 3 bytes of taste) *)
          (mux2
             (* no : need to select between FCS (and which part of FCS), as well as /T and
                /I *)
             in_fcs
             (* are we in the fcs? *)
             (* yes *)
             (mux
                (select fcs_offset ~high:1 ~low:0) (* slice fcs_offset[1:0] *)
                (List.init 4 ~f:(fun lane -> byte fcs lane))
                (* select into bytes of the 4B fcs *))
             (* no *)
             (mux2
                is_term (* is it a term? *)
                (* yes *)
                (of_int_trunc ~width:8 Xgmii.Control_character.terminate)
                (* /T *)
                (* no - works off the assumption that in_fcs isn't high for something that
                   should be an idle character *)
                (of_int_trunc ~width:8 Xgmii.Control_character.idle)))
        (* /I *)
      in
      data, is_term |: after_term)
  in
  (* the resulting data-control pair, with the term-fits booln to follow it - aka only use
     this if the term actually fits *)
  ( { Xgmii.Word.data =
        concat_lsb (List.map lane_values ~f:fst)
        (* first *)
        (* form together the "data" into a scalar *)
    ; control =
        concat_lsb (List.map lane_values ~f:snd)
        (* second *)
        (* form together the "control" into a scalar *)
    }
  , term_fits (* does the term fit? - timing might be a problem here *) )
;;

(**)
let fcs_word ~fcs ~index =
  let remaining = of_int_trunc ~width:4 4 -: uresize index ~width:4 in
  (* same lane_values idiom as earlier *)
  let lane_values =
    List.init 8 ~f:(fun lane ->
      let lane_signal = of_int_trunc ~width:4 lane in
      let in_fcs = lane_signal <: remaining in
      let is_term = lane_signal ==: remaining in
      (* 4b select signal; *)
      (* out of range is copmuted but not used *)
      let (source_index : t) = uresize index ~width:4 +: lane_signal in
      (* fun operator precedence puzzle here if you're not careful remember, application
         first over infix operation hardcaml derives it's +: behaviour from : in the
         precedence rankings

         footgun - |: and &: are NOT tighter
      *)

      (* mux tree! *)
      let data =
        mux2
          in_fcs (* in fcs? *)
          (* yes *)
          (mux
             (select source_index ~high:1 ~low:0)
             (List.init 4 ~f:(fun lane -> byte fcs lane))
             (* slice into the FCS and map it out *))
          (* no *)
          (mux2
             (* is it THE term? *)
             is_term
             (* yes -> emit the termination character *)
             (of_int_trunc ~width:8 Xgmii.Control_character.terminate)
             (* no -> all others are idle characters *)
             (of_int_trunc ~width:8 Xgmii.Control_character.idle))
      in
      data, ~:in_fcs)
  in
  { Xgmii.Word.data = concat_lsb (List.map lane_values ~f:fst)
  ; control = concat_lsb (List.map lane_values ~f:snd)
  }
;;

module I_Regs = struct
  type 'a t = { bruh : 'a } [@@deriving hardcaml]
end

[@@@ocamlformat "disable"]
let create (scope : Scope.t) (i : _ I.t) : _ O.t =
  (* spec *)
  let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in

  let ( -- ) = Scope.naming scope in

  (* fun helper -> makes an Always Reg out of something -> I think my I_Regs approach is better *)
  (* debugability on those might be a bit difficult; we'll see *)
  let reg_var width = Always.Variable.reg ~enable:vdd ~width spec in

  (* new approach to doing state; may move back to a module States declaration *)
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

  (* one day i will write a ppx that does this for me *)
  let is_wait = state.value ==:. state_wait in
  let is_body = state.value ==:. state_body in
  let is_pad  = state.value ==:. state_pad in
  let is_fcs  = state.value ==:. state_fcs in
  let is_ifg  = state.value ==:. state_ifg in

  (* thank God I wrote those helper functions *)
  let start_word =
    Xgmii.of_lane_bytes (* build us a word map out of ints *)
      ([ Xgmii.Control_character.start ] @  (* /S *)
       List.init 6 ~f:(Fn.const 0x55) @     (* 0x55 *)
       [ 0xd5 ]                             (* SFD *)
      )
      ~control:0x01 (* /S is a control char - use a 0x55, but that as txc[where[/S]] means /S in XGMII *)
  in

  (* combo assignment on the body data and whether or not the keep lane corresponds with it;
    one may consider moving this to a common function library?

    for
      0 0 0 1 1 1 1 1

      the masked_body_data =
        8*{0} @:
        8*{0} @:
        8*{0} @:
        buffer_data_i[39:32] @:
        buffer_data_i[31:24] @:
        buffer_data_i[23:16] @:
        buffer_data_i[15:8] @:
        buffer_data_i[7:0] @:
  *)
  let masked_body_data =
    concat_lsb (* concat fold entire list *)
      (List.init 8 ~f:(fun lane -> (* for each lane *)
        mux2
          (* is the word the buffer gave us "keepable" -> is the byte have a corresponding 1 in the keep mask *)
          (bit i.buffer_keep_i ~pos:lane)  (* select on the lane's keep in the mask *)

          (* yes grab the data of the lane *)
          (byte i.buffer_data_i lane)       (* lane data *)

          (* no - zero *)
          (zero 8)                          (* zilch *)
        )
      )
  in

  (* no protection against, non-contiguous keep groups; *)
  (* "popcount", number of 1s in the buffer_keep_i mask *)
  (* gives us the size of the "body" of bytes that we're committing in this XGMII beat *)
  (* for [0 0 0 1 1 1 1 1 ], body_count = 5 *)
  (* = popcount(keep) *)
  let body_count = Mac_10g_axis.keep_byte_count i.buffer_keep_i in

  (* running CRC-covered body count; "once this body word is accepted, how many frame bytes WILL
    be covered" *)
  (* covered_count is the main stateful item that we need to keep track of between cycles *)
  let count_after_body = covered_count.value +: (uresize body_count ~width:17) in
  (* consider where covered_count is 0 - the initial buffer beat,
      after cosnider where it is 8 - we've churned through some initial addressing items

      therefore, count_after_body, assuming another good 64b full-kept beat, forms a 16
      for this function
  *)

  (* the name implies what it does; or does it? *)
  (* consider on a full beat that isn't the START_WORD -> DA[5:0], SA[1:0] *)
  let pad_needed =
    mux2
      (* is count after covering the new body <= 60? *)
      (count_after_body <:. 60) (* inline comparator -> against a constant so . ; very cool functionality *)

      (* yes - pad amount required is the difference of 60 an the count *)
      (of_int_trunc ~width:17 60 -: count_after_body)

      (* no - zero *)
      (zero 17)
  in

  (* 4b, starts at 8; difference between 8 and body size
    we have 8B of space; how many are left taking into account the buffer_keep_i popcount?

  for example with keep mask of
      0 0 0 1 1 1 1 1 -> popcount is 5
      means that IF we do need to pad, then we have (8) - (popcount = 5) = 3 bytes to pad
  *)
  (* howmany pad bytes could be added without spilling *)
  let (body_space : t) = (of_int_trunc ~width:4 8) -: body_count in

  (* min(pad_needed, body_space) *)
  (* if we need to pad,   *)
  let body_pad_count =
    mux2
      (* is the pad less than a certain size? *)
      (* in the example, we consider 0 and 8
          0: pad_needed ->
      *)
      (pad_needed <=: uresize body_space ~width:17)

      (* yes - pad_needed[3:0] *)
      (select pad_needed ~high:3 ~low:0)

      (* no - *)
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

  (* Always assignment logic -> ive shifted around on whether or not to use the Always DSL,
      but I think for "stateful" assignment reasons that it is completely necessary for more
      complex control loops; something has to consume all the helper functions we create right?
  *)

  Always.(
    compile
      [ if_ ~:(i.enable_i)
        [ state           <--. state_wait (* this approach to state is a tad more debuggable,
                                                but there are probably good reasons why State_machine
                                              was written; we'll see. *)
        ; crc             <-- Mac_10g_crc32.initial
        ; covered_count   <--. 0
        ; ifg_words       <--. 0
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

  { O.
    buffer_ready_o      = i.enable_i &: is_body
  ; xgmii_txd_o         = output_word.data
  ; xgmii_txc_o         = output_word.control
  ; state_o             = state.value
  ; frame_pulse_o       = frame_pulse.value
  ; underflow_pulse_o   = body_underflow
  ; underflow_sticky_o  = underflow_sticky.value
  ; frames_o            = frames.value
  ; bytes_o             = bytes.value
  ; underflows_o        = underflows.value
  }
[@@@ocamlformat "enable"]

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"mac_10g_tx" create i
;;
