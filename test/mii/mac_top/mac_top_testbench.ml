(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_top_testbench.ml" *)
(* Shared frame drivers, observations, and reference model for the complete MII MAC. *)

open! Core
open! Hardcaml
open! Mii_of_hardcaml
open! Hardcaml_verif
module Dut = Mac_top
module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)

let destination_mac = [ 0xff; 0xff; 0xff; 0xff; 0xff; 0xff ]
let source_mac = [ 0x02; 0x00; 0x00; 0x00; 0x00; 0x01 ]
let eth_type = [ 0x08; 0x00 ]
let preamble = List.init 7 ~f:(Fn.const 0x55) @ [ 0xd5 ]
let minimum_payload_length = 46

module Tx_observation = struct
  type t =
    { frame : int list
    ; tx_en_rises : int
    ; tx_en_falls : int
    ; nibble_count : int
    ; busy_cleared : bool
    }
  [@@deriving sexp, equal, compare]
end

module Rx_observation = struct
  type t =
    { payload : int list
    ; tfirst_indices : int list
    ; tlast_indices : int list
    ; tuser_on_last : bool option
    ; frame_crc_ok : bool
    ; rx_eth_type : int
    }
  [@@deriving sexp, equal, compare]
end

let create_simulator () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  Sim.create (Dut.create ~rx_fifo_for_sim:true ~ethertype:0x0800 scope)
;;

let set signal value = signal := Bits.of_int_trunc ~width:(Bits.width !signal) value
let bit signal = Bits.to_bool !signal
let int signal = Bits.to_int_trunc !signal

let reset sim =
  let i : _ Dut.I.t = Cyclesim.inputs sim in
  set i.rx_reset 1;
  set i.tx_reset 1;
  set i.en 0;
  set i.rx_dv 0;
  set i.rx_er 0;
  set i.rx_data 0;
  set i.m_axis_tready 0;
  set i.s_axis_tdata 0;
  set i.s_axis_tvalid 0;
  set i.s_axis_tlast 0;
  set i.s_axis_tuser 0;
  set i.tx_start 0;
  Cyclesim.cycle sim;
  set i.rx_reset 0;
  set i.tx_reset 0;
  set i.en 1;
  Cyclesim.cycle sim
;;

let padded_payload payload =
  payload
  @ List.init (Int.max 0 (minimum_payload_length - List.length payload)) ~f:(Fn.const 0)
;;

let expected_tx_frame payload =
  let payload = padded_payload payload in
  let covered = destination_mac @ source_mac @ eth_type @ payload in
  preamble @ covered @ Crc32.fcs_bytes covered
;;

let bytes_of_nibbles nibbles =
  let rec loop acc = function
    | low :: high :: rest -> loop (((high lsl 4) lor low) :: acc) rest
    | [] -> List.rev acc
    | [ _ ] -> failwith "Mac_top emitted an incomplete nibble pair"
  in
  loop [] nibbles
;;

let transmit ?sim payload =
  if List.is_empty payload then invalid_arg "Mac_top TX needs one tlast-marked input byte";
  let sim = Option.value sim ~default:(create_simulator ()) in
  let i : _ Dut.I.t = Cyclesim.inputs sim in
  let o : _ Dut.O.t = Cyclesim.outputs sim in
  reset sim;
  List.iteri payload ~f:(fun index byte ->
    set i.s_axis_tdata byte;
    set i.s_axis_tvalid 1;
    set i.s_axis_tlast (if index = List.length payload - 1 then 1 else 0);
    Cyclesim.cycle sim);
  set i.s_axis_tvalid 0;
  set i.s_axis_tlast 0;
  set i.tx_start 1;
  Cyclesim.cycle sim;
  set i.tx_start 0;
  let nibbles = ref [] in
  let rises = ref 0 in
  let falls = ref 0 in
  let previous = ref false in
  let saw_frame = ref false in
  let idle_after = ref 0 in
  let cycles = ref 0 in
  while ((not !saw_frame) || !idle_after < 3) && !cycles < 4096 do
    let enabled = bit o.tx_en in
    if enabled && not !previous then incr rises;
    if (not enabled) && !previous
    then (
      incr falls;
      saw_frame := true);
    if enabled
    then (
      idle_after := 0;
      nibbles := int o.tx_d :: !nibbles)
    else if !saw_frame
    then incr idle_after;
    previous := enabled;
    Cyclesim.cycle sim;
    incr cycles
  done;
  if !cycles >= 4096 then failwith "Mac_top TX timed out";
  let nibbles = List.rev !nibbles in
  { Tx_observation.frame = bytes_of_nibbles nibbles
  ; tx_en_rises = !rises
  ; tx_en_falls = !falls
  ; nibble_count = List.length nibbles
  ; busy_cleared = not (bit o.tx_busy)
  }
;;

let send_byte sim byte =
  let i : _ Dut.I.t = Cyclesim.inputs sim in
  set i.rx_data (byte land 0x0f);
  Cyclesim.cycle sim;
  set i.rx_data ((byte lsr 4) land 0x0f);
  Cyclesim.cycle sim
;;

let receive_frame ?sim frame =
  let sim = Option.value sim ~default:(create_simulator ()) in
  let i : _ Dut.I.t = Cyclesim.inputs sim in
  let o : _ Dut.O.t = Cyclesim.outputs sim in
  reset sim;
  set i.rx_dv 1;
  List.iter frame ~f:(send_byte sim);
  set i.rx_dv 0;
  Cyclesim.cycle sim;
  set i.m_axis_tready 1;
  let payload = ref [] in
  let firsts = ref [] in
  let lasts = ref [] in
  let user = ref None in
  let done_ = ref false in
  let cycles = ref 0 in
  while ((not !done_) || !cycles < 3) && !cycles < 4096 do
    if bit o.m_axis_tvalid
    then (
      let index = List.length !payload in
      if bit o.m_axis_tfirst then firsts := index :: !firsts;
      if bit o.m_axis_tlast
      then (
        lasts := index :: !lasts;
        user := Some (bit o.m_axis_tuser);
        done_ := true);
      payload := int o.m_axis_tdata :: !payload);
    Cyclesim.cycle sim;
    incr cycles
  done;
  if not !done_ then failwith "Mac_top RX timed out";
  { Rx_observation.payload = List.rev !payload
  ; tfirst_indices = List.rev !firsts
  ; tlast_indices = List.rev !lasts
  ; tuser_on_last = !user
  ; frame_crc_ok = bit o.frame_crc_ok
  ; rx_eth_type = int o.rx_eth_type
  }
;;

let receive_payload ?(corrupt_fcs = false) payload =
  let covered = destination_mac @ source_mac @ eth_type @ payload in
  let fcs = Crc32.fcs_bytes covered in
  let fcs = if corrupt_fcs then List.map fcs ~f:(fun byte -> byte lxor 0xff) else fcs in
  receive_frame (preamble @ covered @ fcs)
;;

let loopback payload =
  let transmitted = transmit payload in
  transmitted, receive_frame transmitted.frame
;;

let make_payload length =
  List.init length ~f:(fun index -> ((index * 37) + 0x41) land 0xff)
;;
