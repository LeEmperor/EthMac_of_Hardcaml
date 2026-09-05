(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "packet_buffer_testbench.ml" *)
(* Shared transactional-buffer drivers, observations, and byte-oriented reference model. *)

open! Core
open! Hardcaml
open! Hardcaml_verif
open! Mac_10g_of_hardcaml

module Dut = Mac_10g_packet_buffer.Make (struct
    let depth_bytes = 32
    let descriptor_capacity = 4
    let error_width = 4
  end)

module Action = struct
  type t =
    { write_bytes : int list
    ; commit : bool
    ; rollback : bool
    ; error : int
    ; read_ready : bool
    }
  [@@deriving sexp, equal, compare]

  let idle =
    { write_bytes = []; commit = false; rollback = false; error = 0; read_ready = false }
  ;;

  let write ?(commit = false) ?(error = 0) write_bytes =
    { idle with write_bytes; commit; error }
  ;;

  let rollback = { idle with rollback = true }
  let read = { idle with read_ready = true }
end

module Output_snapshot = struct
  type t =
    { write_ready : bool
    ; commit_ready : bool
    ; current_frame_length : int
    ; read_bytes : int list
    ; read_keep : int
    ; read_valid : bool
    ; read_last : bool
    ; read_error : int
    ; bytes_used : int
    ; descriptors_used : int
    }
  [@@deriving sexp, equal, compare]
end

module Cycle_observation = struct
  type t =
    { action : Action.t
    ; before_edge : Output_snapshot.t
    ; after_edge : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Reference = struct
  type descriptor =
    { bytes : int list
    ; error : int
    }
  [@@deriving sexp, equal, compare]

  (* Frame-level model deliberately does not reproduce pointers or RAM banks. It models
     only the externally promised transaction: speculative bytes disappear on rollback,
     and become an ordered descriptor on commit. *)
  type t =
    { speculative : int list
    ; committed : descriptor list
    }
  [@@deriving sexp, equal, compare]

  let empty = { speculative = []; committed = [] }

  let apply t (action : Action.t) =
    let speculative =
      if action.rollback then [] else t.speculative @ action.write_bytes
    in
    if action.rollback
    then { t with speculative = [] }
    else if action.commit && not (List.is_empty speculative)
    then
      { speculative = []
      ; committed = t.committed @ [ { bytes = speculative; error = action.error } ]
      }
    else { t with speculative }
  ;;
end

module Testbench = struct
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)

  let input_of_action ?(reset = false) (action : Action.t) =
    if List.length action.write_bytes > 8
    then invalid_arg "a packet-buffer write action is at most eight bytes";
    let data =
      List.init 8 ~f:(fun lane ->
        List.nth action.write_bytes lane |> Option.value ~default:0)
      |> List.map ~f:(Bits.of_int_trunc ~width:8)
      |> Bits.concat_lsb
    in
    let keep = (1 lsl List.length action.write_bytes) - 1 in
    { Dut.I.clock_i = Bits.empty
    ; reset_i = Bits_conv.bit reset
    ; write_data_i = data
    ; write_keep_i = Bits.of_int_trunc ~width:8 keep
    ; write_valid_i = Bits_conv.bit (not (List.is_empty action.write_bytes))
    ; commit_i = Bits_conv.bit action.commit
    ; rollback_i = Bits_conv.bit action.rollback
    ; commit_error_i = Bits.of_int_trunc ~width:4 action.error
    ; read_ready_i = Bits_conv.bit action.read_ready
    }
  ;;

  let snapshot (o : Bits.t Dut.O.t) : Output_snapshot.t =
    let keep = Bits.to_int_trunc o.read_keep_o in
    let count =
      List.range 0 8 |> List.count ~f:(fun lane -> keep land (1 lsl lane) <> 0)
    in
    let read_bytes =
      List.init count ~f:(fun lane ->
        Bits.select o.read_data_o ~high:((8 * lane) + 7) ~low:(8 * lane)
        |> Bits.to_int_trunc)
    in
    { write_ready = Bits_conv.to_bool o.write_ready_o
    ; commit_ready = Bits_conv.to_bool o.commit_ready_o
    ; current_frame_length = Bits.to_int_trunc o.current_frame_length_o
    ; read_bytes
    ; read_keep = keep
    ; read_valid = Bits_conv.to_bool o.read_valid_o
    ; read_last = Bits_conv.to_bool o.read_last_o
    ; read_error = Bits.to_int_trunc o.read_error_o
    ; bytes_used = Bits.to_int_trunc o.bytes_used_o
    ; descriptors_used = Bits.to_int_trunc o.descriptors_used_o
    }
  ;;

  let drive_inputs refs values =
    Dut.I.iter2 refs values ~f:(fun reference value ->
      if not (Bits.is_empty value) then reference := value)
  ;;

  let cycle simulator inputs before_outputs after_outputs action =
    drive_inputs inputs (input_of_action action);
    Cyclesim.cycle_before_clock_edge simulator;
    let before_edge = Dut.O.map before_outputs ~f:( ! ) |> snapshot in
    Cyclesim.cycle_at_clock_edge simulator;
    Cyclesim.cycle_after_clock_edge simulator;
    { Cycle_observation.action
    ; before_edge
    ; after_edge = Dut.O.map after_outputs ~f:( ! ) |> snapshot
    }
  ;;

  let run actions =
    let simulator = Sim.create (Dut.create (Scope.create ~flatten_design:true ())) in
    let inputs = Cyclesim.inputs simulator in
    let before_outputs = Cyclesim.outputs ~clock_edge:Before simulator in
    let after_outputs = Cyclesim.outputs ~clock_edge:After simulator in
    let raw_cycle values =
      drive_inputs inputs values;
      Cyclesim.cycle simulator
    in
    raw_cycle (input_of_action ~reset:true Action.idle);
    raw_cycle (input_of_action ~reset:true Action.idle);
    raw_cycle (input_of_action Action.idle);
    List.map actions ~f:(cycle simulator inputs before_outputs after_outputs)
  ;;

  let writes_for_frame ?(error = 0) bytes =
    let rec loop = function
      | [] -> []
      | bytes ->
        let #(beat, rest) = List.split_n bytes 8 in
        Action.write ~commit:(List.is_empty rest) ~error beat :: loop rest
    in
    loop bytes
  ;;

  let reads_for_frame bytes =
    List.init ((List.length bytes + 7) / 8) ~f:(Fn.const Action.read)
  ;;

  let accepted_read_beats observations =
    List.filter_map
      observations
      ~f:(fun ({ action; before_edge; _ } : Cycle_observation.t) ->
        if action.read_ready && before_edge.read_valid
        then Some (before_edge.read_bytes, before_edge.read_last, before_edge.read_error)
        else None)
  ;;

  let accepted_read_bytes observations =
    accepted_read_beats observations |> List.concat_map ~f:(fun (bytes, _, _) -> bytes)
  ;;
end
