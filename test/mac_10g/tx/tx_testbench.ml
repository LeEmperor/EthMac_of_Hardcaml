(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_testbench.ml" *)
(* Composed Phase-2 TX fixture and byte-oriented XGMII decoder. *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml_verif
open! Mac_10g_of_hardcaml

module Buffer = Mac_10g_packet_buffer.Make (struct
    let depth_bytes = 256
    let descriptor_capacity = 4
    let error_width = 1
  end)

module Ingress = Mac_10g_tx_ingress.Make (struct
    let max_supported_frame_length = 255
    let buffer_length_width = Buffer.length_width
  end)

module Dut = struct
  module I = struct
    type 'a t =
      { clock_i : 'a
      ; reset_i : 'a
      ; enable_i : 'a
      ; counters_clear_i : 'a
      ; data_i : 'a [@bits 64]
      ; keep_i : 'a [@bits 8]
      ; valid_i : 'a
      ; last_i : 'a
      ; user_i : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ready_o : 'a
      ; txd_o : 'a [@bits 64]
      ; txc_o : 'a [@bits 8]
      ; state_o : 'a [@bits 3]
      ; frames_o : 'a [@bits 64]
      ; bytes_o : 'a [@bits 64]
      ; drops_o : 'a [@bits 64]
      ; malformed_o : 'a [@bits 64]
      ; underflows_o : 'a [@bits 64]
      ; underflow_sticky_o : 'a
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) : _ O.t =
    let buffer_write_data = wire 64 in
    let buffer_write_keep = wire 8 in
    let buffer_write_valid = wire 1 in
    let buffer_commit = wire 1 in
    let buffer_rollback = wire 1 in
    let buffer_read_ready = wire 1 in
    let buffer =
      Buffer.create
        (Scope.sub_scope scope "buffer")
        { Buffer.I.clock_i = i.clock_i
        ; reset_i = i.reset_i
        ; write_data_i = buffer_write_data
        ; write_keep_i = buffer_write_keep
        ; write_valid_i = buffer_write_valid
        ; commit_i = buffer_commit
        ; rollback_i = buffer_rollback
        ; commit_error_i = zero 1
        ; read_ready_i = buffer_read_ready
        }
    in
    let ingress =
      Ingress.create
        (Scope.sub_scope scope "ingress")
        { Ingress.I.clock_i = i.clock_i
        ; reset_i = i.reset_i
        ; enable_i = i.enable_i
        ; counters_clear_i = i.counters_clear_i
        ; axis_data_i = i.data_i
        ; axis_keep_i = i.keep_i
        ; axis_valid_i = i.valid_i
        ; axis_last_i = i.last_i
        ; axis_user_i = i.user_i
        ; buffer_write_ready_i = buffer.write_ready_o
        ; buffer_commit_ready_i = buffer.commit_ready_o
        ; buffer_frame_length_i = buffer.current_frame_length_o
        }
    in
    let tx =
      Mac_10g_tx.create
        (Scope.sub_scope scope "formatter")
        { Mac_10g_tx.I.clock_i = i.clock_i
        ; reset_i = i.reset_i
        ; enable_i = i.enable_i
        ; counters_clear_i = i.counters_clear_i
        ; buffer_data_i = buffer.read_data_o
        ; buffer_keep_i = buffer.read_keep_o
        ; buffer_valid_i = buffer.read_valid_o
        ; buffer_last_i = buffer.read_last_o
        }
    in
    Signal.(buffer_write_data <-- ingress.buffer_write_data_o);
    Signal.(buffer_write_keep <-- ingress.buffer_write_keep_o);
    Signal.(buffer_write_valid <-- ingress.buffer_write_valid_o);
    Signal.(buffer_commit <-- ingress.buffer_commit_o);
    Signal.(buffer_rollback <-- ingress.buffer_rollback_o);
    Signal.(buffer_read_ready <-- tx.buffer_ready_o);
    { O.ready_o = ingress.axis_ready_o
    ; txd_o = tx.xgmii_txd_o
    ; txc_o = tx.xgmii_txc_o
    ; state_o = tx.state_o
    ; frames_o = tx.frames_o
    ; bytes_o = tx.bytes_o
    ; drops_o = ingress.drops_o
    ; malformed_o = ingress.malformed_frames_o
    ; underflows_o = tx.underflows_o
    ; underflow_sticky_o = tx.underflow_sticky_o
    }
  ;;
end

module Beat = struct
  type t =
    { bytes : int list
    ; keep : int
    ; last : bool
    ; user : bool
    }
  [@@deriving sexp, equal, compare]

  let of_frame ?(user = false) bytes =
    let rec loop = function
      | [] -> []
      | remaining ->
        let #(beat, rest) = List.split_n remaining 8 in
        { bytes = beat
        ; keep = (1 lsl List.length beat) - 1
        ; last = List.is_empty rest
        ; user = user && List.is_empty rest
        }
        :: loop rest
    in
    loop bytes
  ;;
end

module Snapshot = struct
  type t =
    { lanes : int list
    ; control : int
    ; state : int
    ; frames : int
    ; bytes : int
    ; drops : int
    ; malformed : int
    ; underflows : int
    ; underflow_sticky : bool
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)

  let inputs
    ?(reset = false)
    ?(enable = true)
    ?(counters_clear = false)
    ?(valid = false)
    beat
    =
    let beat =
      Option.value beat ~default:{ Beat.bytes = []; keep = 0; last = false; user = false }
    in
    let data =
      List.init 8 ~f:(fun lane -> List.nth beat.bytes lane |> Option.value ~default:0)
      |> List.map ~f:(Bits.of_int_trunc ~width:8)
      |> Bits.concat_lsb
    in
    { Dut.I.clock_i = Bits.empty
    ; reset_i = Bits_conv.bit reset
    ; enable_i = Bits_conv.bit enable
    ; counters_clear_i = Bits_conv.bit counters_clear
    ; data_i = data
    ; keep_i = Bits.of_int_trunc ~width:8 beat.keep
    ; valid_i = Bits_conv.bit valid
    ; last_i = Bits_conv.bit beat.last
    ; user_i = Bits_conv.bit beat.user
    }
  ;;

  let drive refs values =
    Dut.I.iter2 refs values ~f:(fun reference value ->
      if not (Bits.is_empty value) then reference := value)
  ;;

  let snapshot (o : Bits.t Dut.O.t) =
    { Snapshot.lanes =
        List.init 8 ~f:(fun lane ->
          Bits.select o.txd_o ~high:((8 * lane) + 7) ~low:(8 * lane) |> Bits.to_int_trunc)
    ; control = Bits.to_int_trunc o.txc_o
    ; state = Bits.to_int_trunc o.state_o
    ; frames = Bits.to_int_trunc o.frames_o
    ; bytes = Bits.to_int_trunc o.bytes_o
    ; drops = Bits.to_int_trunc o.drops_o
    ; malformed = Bits.to_int_trunc o.malformed_o
    ; underflows = Bits.to_int_trunc o.underflows_o
    ; underflow_sticky = Bits_conv.to_bool o.underflow_sticky_o
    }
  ;;

  let run ?(gaps = fun _ -> false) ?(clear_counters = false) beats =
    let simulator = Sim.create (Dut.create (Scope.create ~flatten_design:true ())) in
    let input_refs = Cyclesim.inputs simulator in
    let output_refs = Cyclesim.outputs ~clock_edge:Before simulator in
    let raw values =
      drive input_refs values;
      Cyclesim.cycle simulator
    in
    raw (inputs ~reset:true None);
    raw (inputs ~reset:true None);
    raw (inputs None);
    let beat_index = ref 0 in
    let snapshots = ref [] in
    let cycle = ref 0 in
    while
      (!beat_index < List.length beats
       ||
       match !snapshots with
       | [] -> true
       | last :: _ ->
         last.Snapshot.frames + last.drops
         < List.count beats ~f:(fun (b : Beat.t) -> b.last))
      && !cycle < 4000
    do
      let beat = List.nth beats !beat_index in
      let valid = Option.is_some beat && not (gaps !cycle) in
      drive input_refs (inputs ~valid beat);
      Cyclesim.cycle_before_clock_edge simulator;
      let outputs = Dut.O.map output_refs ~f:( ! ) in
      snapshots := snapshot outputs :: !snapshots;
      let accepted = valid && Bits_conv.to_bool outputs.ready_o in
      Cyclesim.cycle_at_clock_edge simulator;
      Cyclesim.cycle_after_clock_edge simulator;
      if accepted then Int.incr beat_index;
      Int.incr cycle
    done;
    if !cycle = 4000 then failwith "TX testbench timed out";
    (* Capture the registered counter update following the final terminate/drop cycle. *)
    drive input_refs (inputs None);
    Cyclesim.cycle_before_clock_edge simulator;
    snapshots := snapshot (Dut.O.map output_refs ~f:( ! )) :: !snapshots;
    if clear_counters
    then (
      raw (inputs ~counters_clear:true None);
      drive input_refs (inputs None);
      Cyclesim.cycle_before_clock_edge simulator;
      snapshots := snapshot (Dut.O.map output_refs ~f:( ! )) :: !snapshots);
    List.rev !snapshots
  ;;
end

let decode_frames snapshots =
  let frames = ref [] in
  let current = ref None in
  let preamble_remaining = ref 0 in
  List.iter snapshots ~f:(fun ({ Snapshot.lanes; control; _ } : Snapshot.t) ->
    List.iteri lanes ~f:(fun lane value ->
      let is_control = control land (1 lsl lane) <> 0 in
      match !current with
      | None ->
        if is_control && value = 0xfb
        then (
          current := Some [];
          preamble_remaining := 7)
      | Some bytes ->
        if !preamble_remaining > 0
        then Int.decr preamble_remaining
        else if is_control && value = 0xfd
        then (
          frames := List.rev bytes :: !frames;
          current := None)
        else if not is_control
        then current := Some (value :: bytes)));
  List.rev !frames
;;

let termination_lanes snapshots =
  List.filter_map snapshots ~f:(fun ({ Snapshot.lanes; control; _ } : Snapshot.t) ->
    List.findi lanes ~f:(fun lane value -> control land (1 lsl lane) <> 0 && value = 0xfd)
    |> Option.map ~f:fst)
;;

let interframe_idle_counts snapshots =
  let counts = ref [] in
  let after_terminate = ref false in
  let idle_count = ref 0 in
  List.iter snapshots ~f:(fun ({ Snapshot.lanes; control; _ } : Snapshot.t) ->
    List.iteri lanes ~f:(fun lane value ->
      let is_control = control land (1 lsl lane) <> 0 in
      if is_control && value = 0xfd
      then (
        after_terminate := true;
        idle_count := 0)
      else if !after_terminate && is_control && value = 0x07
      then Int.incr idle_count
      else if !after_terminate && is_control && value = 0xfb
      then (
        counts := !idle_count :: !counts;
        after_terminate := false)));
  List.rev !counts
;;

let expected_wire_frame bytes =
  let crc_bytes =
    bytes @ List.init (Int.max 0 (60 - List.length bytes)) ~f:(Fn.const 0)
  in
  crc_bytes @ Crc32.fcs_bytes crc_bytes
;;
