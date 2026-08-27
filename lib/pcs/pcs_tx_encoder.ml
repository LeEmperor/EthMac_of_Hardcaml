(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "pcs_tx_encoder.ml" *)
(* Combinational Clause 49 transmit encoder for the internal 64-bit SDR XGMII interface.

   The output payload is the unscrambled 64-bit block payload. Payload bit zero, sync
   header bit zero, and XGMII lane zero are earliest in the semantic serial stream. An
   illegal XGMII word is replaced by a legal all-error control block and reported through
   [tx_bad_xgmii_o].

   Vocabulary:
   - Block = 66b-wide vector that we send downstream.
   - Control Block = some lanes are control items
     - /S/ D1 D2 D3 D4 D5 D6 D7
     - D0 D1 D2 /T/ /I/ /I/ /I/ /I/

   One must remember that control codes are only 7b!

   Also, ordered sets are encoded as 0x9C, but translated into either sequence sets or
   signal sets depending on the generator of the sequence.
   - Sequence sets -> Ethernet (0x9C)
   - Signal sets -> Fibre (0x5C)
     - preserves signal reconstruction from the Q-code to tell us who faulted
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

(* When a given lane-set contains a control item, the 64b payload in the 66b block is
   actually to be interpreted diffferently.

   The first 8b of the 64b payload are now "block-type."
*)
module Block_type = struct
  let control = 0x1e (* Control *)
  let control_ordered_set_4 = 0x2d (* Control + Set *)
  let ordered_set_control = 0x4b (* Set + Control *)
  let start_4 = 0x33 (* Control + Start *)
  let ordered_sets = 0x55 (* Set + Set *)
  let ordered_set_start = 0x66 (* Set + Start *)
  let start_0 = 0x78 (* Start *)
  let terminate = [| 0x87; 0x99; 0xaa; 0xb4; 0xcc; 0xd2; 0xe1; 0xff |]
end

let const width value = of_int_trunc ~width value
let all signals = List.fold signals ~init:vdd ~f:( &: )
let any signals = List.fold signals ~init:gnd ~f:( |: )
let payload fields = concat_lsb fields

(* alot of this could've been done with the Always DSL I feel, but I should move away from
   that when possible; problem is that my debugability from the I_Regs + I_Wires
   structuring with of_always that I used to do was quite good at adding debugability
   since I could string prepend all the signals in the net;

   much to bother more Jane Street devs about
*)
let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  (* aliases *)
  let word : Signal.t Xgmii.Word.t = { data = i.xgmii_txd_i; control = i.xgmii_txc_i } in

  (* 1 morbillion helpers *)
  (* get the byte carried by a lane in the presented beat *)
  let byte lane = Xgmii.lane_byte word lane in

  (* extract out the control bit for a given lane, based on a given word *)
  (* clever inversion on is_control -> not sure if is_control acting as an inverse of
    is_data would be better, but is_control is probably more proactive for signal discernments so I'm going with this
  *)
  (* if we look at the control mask, and the bit is set for that lane, then we know the lane's data is control data *)
  let is_control lane = Xgmii.lane_is_control word lane in
  let is_data lane = ~:(is_control lane) in 

  (* if a lane is a control lane, then discern which control character is sitting in the lane *)
  (* definitely some functor match magic that could occur here, but alas *)
  let carries lane character = is_control lane &: (byte lane ==:. character) in

  (* is_X -> probably can be functor'd but I'm a bum *)
  let is_idle lane = carries lane Xgmii.Control_character.idle in
  let is_error lane = carries lane Xgmii.Control_character.error in

  (* can the item be represented with generic Clause 49 fields? *)
  let is_regular_control lane = is_idle lane |: is_error lane in
  let is_start lane = carries lane Xgmii.Control_character.start in
  let is_terminate lane = carries lane Xgmii.Control_character.terminate in

  (* is the byte on a lane the ordered_set delimiter? *)
  let is_ordered_set lane = carries lane Xgmii.Control_character.sequence_ordered_set in

  (* manual C-field encoding - very interesting part of the spec imo w*)
  let control_code lane = mux2 (is_error lane) (const 7 0x1e) (Signal.zero 7) in

  (* is the entire Word.data.t regular control words? namely for idle detection *)
  let regular_controls lanes = all (List.map lanes ~f:is_regular_control) in

  (* are all the lanes data? *)
  let data_lanes lanes = all (List.map lanes ~f:is_data) in

  (* splicer *)
  let bytes lanes = List.map lanes ~f:byte in
  let control_codes lanes = List.map lanes ~f:control_code in

  (* forms a Signal const of a given int with width 8 *)
  let block_type value = const 8 value in
  let ordered_set_code = zero 4 in
  let reserved width = zero width in

  (* List.range is like range(0, 8) in python - too much kaggle recently lmao *)
  (* essentially we're checking lanes [0, 8) if they're data *)
  let all_data = data_lanes (List.range 0 8) in
  let all_control = regular_controls (List.range 0 8) in (* specifically for generic detection *)

  (* smattering of combinational gates to decode locations of ordered sets, data, and control groups *)
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

  (* where is the terminate? -> every late before lane is data, every lane after is generic control *)
  let terminate_valid lane =
    data_lanes (List.range 0 lane)
    &: is_terminate lane
    &: regular_controls (List.range (lane + 1) 8)
  in
  let terminate_valids = List.init 8 ~f:terminate_valid in (* bit mask of valids on the lane set *)
  let control_payload =
    payload (block_type Block_type.control :: control_codes (List.range 0 8))
  in

  (* on case start 0, encode the data payload as all following bytes *)
  let start_0_payload =
    payload (block_type Block_type.start_0 :: bytes (List.range 1 8))
  in

  (* on case start 4, set the control codes for 0-3, then the data for 5-8 *)
  let start_4_payload =
    payload
      ([ block_type Block_type.start_4 ]
      (* wget the block type constant from Block_type, type 33 : 8b *)
       @ control_codes (List.range 0 4) (* cated of 4 7b control words : 28b*)
       @ [ reserved 4 ] (* 4b *)
       @ bytes (List.range 5 8)) (* 24b *)
  in

  (* ordered set into payload group *)
  let ordered_set_4_payload =
    payload
      ([ block_type Block_type.control_ordered_set_4 ] (* 8b *)
       @ control_codes (List.range 0 4) (* 28b *)
       @ [ ordered_set_code ] (* 4b -> table 49-1 encodes 0x9C as 0x00 *)
       @ bytes (List.range 5 8)) (* 24b *)
  in

  (* ordered set into data *)
  let ordered_set_control_payload =
    payload
      ([ block_type Block_type.ordered_set_control ] (* 8b *)
       @ bytes (List.range 1 4) (* 24b *)
       @ [ ordered_set_code ] (* 4b *)
       @ control_codes (List.range 4 8)) (* 28b *)
  in

  (* double ordered sets *)
  let ordered_sets_payload =
    payload
      ([ block_type Block_type.ordered_sets ]
       @ bytes (List.range 1 4)
       @ [ ordered_set_code; ordered_set_code ]
       @ bytes (List.range 5 8))
  in

  (* o set into start payload *)
  let ordered_set_start_payload =
    payload
      ([ block_type Block_type.ordered_set_start ]
       @ bytes (List.range 1 4)
       @ [ ordered_set_code; reserved 4 ]
       @ bytes (List.range 5 8))
  in

  let terminate_payload lane =
    payload
      ([ block_type Block_type.terminate.(lane) ] (* get which lane we're terminating on *)
       @ bytes (List.range 0 lane) (* payload up to the terminate point *)
       @ (if lane = 7 then [] else [ reserved (7 - lane) ])
       @ control_codes (List.range (lane + 1) 8)) (* stuff with control codes *)
  in

  (* fat cate of all possible data outs -> oof mux *)
  let candidates = (* ting, ting2 *)
    [ (all_data, i.xgmii_txd_i) (* forms a tuple on the spot -> keep this in mind for later *)
    ; all_control, control_payload
    ; start_0, start_0_payload
    ; start_4, start_4_payload
    ; ordered_set_4, ordered_set_4_payload
    ; ordered_set_control, ordered_set_control_payload
    ; ordered_sets, ordered_sets_payload
    ; ordered_set_start, ordered_set_start_payload
    ]
    @ List.map2_exn (* termination logic *)
        terminate_valids
        (List.init 8 ~f:terminate_payload)
        ~f:(fun valid data -> valid, data) (* very functional moment w*)
  in
      (* the List.map2_exn does the below basically as a composition *)
        (* ; terminate_0_valid,          terminate_0_payload *)
        (* ; terminate_1_valid,          terminate_1_payload *)
        (* ; terminate_2_valid,          terminate_2_payload *)
        (* ; terminate_3_valid,          terminate_3_payload *)
        (* ; terminate_4_valid,          terminate_4_payload *)
        (* ; terminate_5_valid,          terminate_5_payload *)
        (* ; terminate_6_valid,          terminate_6_payload *)
        (* ; terminate_7_valid,          terminate_7_payload *)
        (* ] *)

  let error_control_code = const 7 0x1e in
  let error_payload =
    payload (block_type Block_type.control ::
             List.init 8 ~f:(Fn.const error_control_code))
  in

  (* giant prio mux -> if we have the guarantee that only 1 thing is true,
    can we get nice binary reductions?

    priority_select_with_default -> balanced tree implements
    onehot_select -> or reduction balancing
      FORMAL VERIFICATINON MOMENT YIPPEEEE
      FORMAL VERIFICATINON MOMENT YIPPEEEEEEE -> guarantee the thingy
  *)

  (* final mux chain composition; if this meets timing it's crazy *)
  let encoded_payload =
    List.fold_right candidates ~init:error_payload ~f:( (* fold_right : prio mux chain *)
      fun (valid, data) selected -> (* take in a tuple composed of valid@data, and else the selection on selected *)
      mux2 valid data selected)
      (*
            let rec fold_right list ~init ~f =
                match list with
                | [] -> init
                | item :: remaining ->
                f item (fold_right remaining ~init ~f)
            ;;

        here we pass error_payload as the "init", which means at the very end of the if else chain the
        ultimate "else" will be the error_payload we composed a bit earlier
      *)
  in

  (* or fold to see if anyone is valid at all *)
  let valid = any (List.map candidates ~f:fst) in (* fst - first *)
  { O.encoded_payload_o = encoded_payload
  ; encoded_header_o = (* quick in-place mux for BASE-R 2b header *)
      mux2
        all_data
        (const 2 Base_r_block.Sync_header.data)
        (const 2 Base_r_block.Sync_header.control)
  ; tx_bad_xgmii_o = ~:valid
  }
  [@@ocamlformat "disable"]
(* me vs the ocamlformatter *)
