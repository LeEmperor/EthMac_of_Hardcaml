(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "axis_width_adapter_8_to_64.ml" *)
(* Pack a frame-oriented, byte-wide AXI stream into 64-bit beats.

   Byte order follows the convention used by common 10G MAC AXI-stream interfaces: the
   earliest byte on [s_tdata_i] is placed in [m_tdata_o][7:0], and [m_tkeep_o][0]
   qualifies it. A short final beat is emitted as soon as [s_tlast_i] is accepted, with a
   contiguous low-bit [m_tkeep_o].

   The adapter contains one output holding register. Once a complete (or final partial)
   word is pending it backpressures the byte stream until that word is accepted. It can
   consume the first byte of the next word on the same edge that the pending word is
   accepted, so there is no forced bubble on the narrow input.
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { (* Byte-wide source and 64-bit sink, synchronous to [clock_i]. [reset_i] is
         synchronous. *)
      clock_i : 'a
    ; reset_i : 'a
    ; en_i : 'a
    ; s_tdata_i : 'a [@bits 8]
    ; s_tvalid_i : 'a
    ; s_tlast_i : 'a
    ; s_tfirst_i : 'a
    ; m_tready_i : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { s_tready_o : 'a
    ; m_tdata_o : 'a [@bits 64]
    ; m_tkeep_o : 'a [@bits 8]
    ; m_tvalid_o : 'a
    ; m_tlast_o : 'a
    ; m_tfirst_o : 'a
    }
  [@@deriving hardcaml]
end

module Regs = struct
  type 'a t =
    { data : 'a [@bits 64]
    ; keep : 'a [@bits 8]
    ; lane : 'a [@bits 3]
    ; valid : 'a
    ; last : 'a
    ; first : 'a
    }
  [@@deriving hardcaml]
end

[@@@ocamlformat "disable"]
let create (scope : Scope.t) (i : _ I.t) : _ O.t =

  (* small unit; Always being here is fine *)
  let open Always in
  let open Variable in

  (* spec *)
  let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in

  (* running with my module'd approach for internal debugabilities *)
  let r = Regs.Of_always.reg ~enable:i.en_i spec in
  Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) r;

  (* ready valid handshake *)
  let output_can_advance =
    ~:(r.valid.value) |:
    i.m_tready_i
  in

  (* pass ready *)
  let s_tready = i.en_i &: output_can_advance in

  (* so many handshakes we might be a campaign *)
  let input_transfer = i.s_tvalid_i &: s_tready in
  let output_transfer = r.valid.value &: i.m_tready_i in

  (* helpers *)
  let lane_is_zero = r.lane.value ==:. 0 in
  let lane_is_seven = r.lane.value ==:. 7 in

  let base_data =
    mux2
      (* if lane is zero *)
      (* lane being 0 distinguishes a new 64b word *)
      lane_is_zero

      (* yes - zero out the data *)
      (zero 64)

      (* no - pass the data *)
      r.data.value
  in

  (* standard keep *)
  let base_keep =
    mux2

      (* if lane is zero *)
      lane_is_zero

      (* yes - zero the keep *)
      (zero 8)

      (* no - pass *)
      r.keep.value
  in

  (* 0-extend the incoming byte *)
  let byte = uresize i.s_tdata_i ~width:64 in

  (* think for example of a frame composed of 0x11 0x22 0x33

      active edge 1 => 0x0000_0000_0000_0011 becomes r.data
      active edge 2 => 0x0000_0000_0000_0022
        transforms into  0000_0000_0000_2200 (* byte << 8 *)
        r.data = r.data |: (byte << 8)

        list iter this fully combinationally, and simply decide who's actually writing in r.data later
    *)
  let data_with_byte =
    mux
      (* select on the lane value *)
      r.lane.value (* this is selecting the large mux that feeds the r.data value; essentially a glorified counter *)

      (* need to widen first before we shift otherwise we would truncate all the useful stuff *)
      (* create 8 items of data, each being shifted by the lane *)
      (List.init 8 ~f:(fun lane ->
           base_data |: (sll byte ~by:(lane * 8))
         )
      )
  in

  (* keep assign with the data *)
  let keep_with_byte =
    mux
      (* selecting on the lane counter *)
      r.lane.value
      (List.init 8 ~f:(fun lane ->
        base_keep |: of_int_trunc ~width:8 (1 lsl lane) (* shift left *)
        )
      )
  in

  (* register assignments on edge *)
  compile
    [ when_ output_transfer [ r.valid <--. 0 ]
    ; when_
        (* when handshake good *)
        input_transfer
        [ r.data <-- data_with_byte (* write the data accum *)
        ; r.keep <-- keep_with_byte (* and it's keep mask *)
        ; when_ lane_is_zero [ r.first <-- i.s_tfirst_i ] (* assert the first beat *)
        ; if_
            (i.s_tlast_i |: lane_is_seven) (* might need to verify against this explicit property *)
            (* true *)
            [ r.lane <--. 0;
              r.valid <--. 1;
              r.last <-- i.s_tlast_i
            ]

            (* false *)
            [ r.lane <-- r.lane.value +:. 1 ]
        ]
    ];

  { O.s_tready_o = s_tready
  ; m_tdata_o = r.data.value
  ; m_tkeep_o = r.keep.value
  ; m_tvalid_o = i.en_i &: r.valid.value
  ; m_tlast_o = i.en_i &: r.valid.value &: r.last.value
  ; m_tfirst_o = i.en_i &: r.valid.value &: r.first.value
  }
[@@@ocamlformat "enable"]

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"axis_width_adapter_8_to_64" create i
;;
