(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Testbench Support: "Pcs_tx_encoder"

   Combinational DUT harness and an independent software model of the Clause 49 mappings.
*)

open! Core
open! Hardcaml
open! Pcs_of_hardcaml
module Dut = Pcs_tx_encoder

module Lane = struct
  type t =
    { byte : int
    ; is_control : bool
    }
  [@@deriving sexp]

  let data byte = { byte; is_control = false }
  let control byte = { byte; is_control = true }
  let idle = control 0x07
  let start = control 0xfb
  let terminate = control 0xfd
  let error = control 0xfe
  let ordered_set = control 0x9c
end

module Encoded = struct
  type t =
    { payload : Bits.t
    ; header : int
    ; bad_xgmii : bool
    }
  [@@deriving sexp_of]

  let equal a b =
    Bits.equal a.payload b.payload
    && a.header = b.header
    && Bool.equal a.bad_xgmii b.bad_xgmii
  ;;

  let printable t =
    [%sexp
      { payload = (Bits.Hex.to_string t.payload : string)
      ; header = (t.header : int)
      ; bad_xgmii = (t.bad_xgmii : bool)
      }]
  ;;
end

module Reference = struct
  let check_word lanes =
    if List.length lanes <> 8
    then
      failwithf "an XGMII test word requires eight lanes, got %d" (List.length lanes) ()
  ;;

  let lane lanes index = List.nth_exn lanes index
  let is_data lanes index = not (lane lanes index).Lane.is_control

  let carries lanes index byte =
    let lane : Lane.t = lane lanes index in
    lane.is_control && lane.byte = byte
  ;;

  let control_code lanes index =
    if carries lanes index 0x07
    then Some 0x00
    else if carries lanes index 0xfe
    then Some 0x1e
    else None
  ;;

  let all_indices indices ~f = List.for_all indices ~f
  let data_lanes lanes indices = all_indices indices ~f:(is_data lanes)

  let regular_controls lanes indices =
    all_indices indices ~f:(fun index -> Option.is_some (control_code lanes index))
  ;;

  let field width value = Bits.of_int_trunc ~width value

  let pack fields =
    Bits.concat_lsb (List.map fields ~f:(fun (width, value) -> field width value))
  ;;

  let bytes lanes indices =
    List.map indices ~f:(fun index -> 8, (lane lanes index).Lane.byte)
  ;;

  let control_codes lanes indices =
    List.map indices ~f:(fun index -> 7, Option.value_exn (control_code lanes index))
  ;;

  let control_payload lanes = pack ((8, 0x1e) :: control_codes lanes (List.range 0 8))
  let error_payload = pack ((8, 0x1e) :: List.init 8 ~f:(fun _ -> 7, 0x1e))

  let terminate_lane lanes =
    List.find (List.range 0 8) ~f:(fun index ->
      data_lanes lanes (List.range 0 index)
      && carries lanes index 0xfd
      && regular_controls lanes (List.range (index + 1) 8))
  ;;

  let terminate_types = [| 0x87; 0x99; 0xaa; 0xb4; 0xcc; 0xd2; 0xe1; 0xff |]

  let terminate_payload lanes index =
    pack
      ([ 8, terminate_types.(index) ]
       @ bytes lanes (List.range 0 index)
       @ (if index = 7 then [] else [ 7 - index, 0 ])
       @ control_codes lanes (List.range (index + 1) 8))
  ;;

  let encode lanes : Encoded.t =
    check_word lanes;
    let block payload header = { Encoded.payload; header; bad_xgmii = false } in
    if data_lanes lanes (List.range 0 8)
    then block (pack (bytes lanes (List.range 0 8))) 0b10
    else if regular_controls lanes (List.range 0 8)
    then block (control_payload lanes) 0b01
    else if carries lanes 0 0xfb && data_lanes lanes (List.range 1 8)
    then block (pack ([ 8, 0x78 ] @ bytes lanes (List.range 1 8))) 0b01
    else if regular_controls lanes (List.range 0 4)
            && carries lanes 4 0xfb
            && data_lanes lanes (List.range 5 8)
    then
      block
        (pack
           ([ 8, 0x33 ]
            @ control_codes lanes (List.range 0 4)
            @ [ 4, 0 ]
            @ bytes lanes (List.range 5 8)))
        0b01
    else if regular_controls lanes (List.range 0 4)
            && carries lanes 4 0x9c
            && data_lanes lanes (List.range 5 8)
    then
      block
        (pack
           ([ 8, 0x2d ]
            @ control_codes lanes (List.range 0 4)
            @ [ 4, 0 ]
            @ bytes lanes (List.range 5 8)))
        0b01
    else if carries lanes 0 0x9c
            && data_lanes lanes (List.range 1 4)
            && regular_controls lanes (List.range 4 8)
    then
      block
        (pack
           ([ 8, 0x4b ]
            @ bytes lanes (List.range 1 4)
            @ [ 4, 0 ]
            @ control_codes lanes (List.range 4 8)))
        0b01
    else if carries lanes 0 0x9c
            && data_lanes lanes (List.range 1 4)
            && carries lanes 4 0x9c
            && data_lanes lanes (List.range 5 8)
    then
      block
        (pack
           ([ 8, 0x55 ]
            @ bytes lanes (List.range 1 4)
            @ [ 4, 0; 4, 0 ]
            @ bytes lanes (List.range 5 8)))
        0b01
    else if carries lanes 0 0x9c
            && data_lanes lanes (List.range 1 4)
            && carries lanes 4 0xfb
            && data_lanes lanes (List.range 5 8)
    then
      block
        (pack
           ([ 8, 0x66 ]
            @ bytes lanes (List.range 1 4)
            @ [ 4, 0; 4, 0 ]
            @ bytes lanes (List.range 5 8)))
        0b01
    else (
      match terminate_lane lanes with
      | Some index -> block (terminate_payload lanes index) 0b01
      | None -> { payload = error_payload; header = 0b01; bad_xgmii = true })
  ;;
end

module Testbench = struct
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)

  type t =
    { sim : Sim.t
    ; inputs : Bits.t ref Dut.I.t
    ; outputs : Bits.t ref Dut.O.t
    }

  let create () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let sim = Sim.create (Dut.create scope) in
    { sim; inputs = Cyclesim.inputs sim; outputs = Cyclesim.outputs sim }
  ;;

  let encode t lanes : Encoded.t =
    Reference.check_word lanes;
    t.inputs.xgmii_txd_i
    := Bits.concat_lsb
         (List.map lanes ~f:(fun (lane : Lane.t) -> Bits.of_int_trunc ~width:8 lane.byte));
    t.inputs.xgmii_txc_i
    := Bits.of_int_trunc
         ~width:8
         (List.foldi lanes ~init:0 ~f:(fun index control lane ->
            if lane.Lane.is_control then control lor (1 lsl index) else control));
    Cyclesim.cycle t.sim;
    { payload = !(t.outputs.encoded_payload_o)
    ; header = Bits.to_int_trunc !(t.outputs.encoded_header_o)
    ; bad_xgmii = Bits.to_bool !(t.outputs.tx_bad_xgmii_o)
    }
  ;;

  let check t lanes =
    let actual = encode t lanes in
    let expected = Reference.encode lanes in
    if not (Encoded.equal actual expected)
    then
      raise_s
        [%message
          "PCS TX encoder mismatch"
            (lanes : Lane.t list)
            (Encoded.printable actual : Sexp.t)
            (Encoded.printable expected : Sexp.t)]
  ;;
end
