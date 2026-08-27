(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "pcs_tx_scrambler.ml" *)
(* Stateful 64-bit parallel Clause 49 transmit scrambler.

   Payload bit zero is processed first. Before a block, state bit zero is the most
   recently transmitted scrambled payload bit, so state bits 38 and 57 represent the x^39
   and x^58 taps. The state resets synchronously to all ones and advances once for every
   clock edge outside reset. The two-bit sync header bypasses the scrambler.
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock_i : 'a
    ; reset_i : 'a
    ; encoded_payload_i : 'a [@bits 64]
    ; encoded_header_i : 'a [@bits 2]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { scrambled_data_o : 'a [@bits 64]
    ; scrambled_header_o : 'a [@bits 2]
    }
  [@@deriving hardcaml]
end

let state_width = 58
let reset_state = ones state_width

(* given a payload, and a current state represented by a Signal.t *)
let parallel_step ~payload ~state =
  (* base array set *)
  let scrambled_bits = Array.create ~len:64 gnd in

  (* hardcaml joyous *)
  for bit_index = 0 to 63 do

    (* set xor set 39 *)
    let tap_39 =
      if bit_index >= 39
      then scrambled_bits.(bit_index - 39)
      else bit state ~pos:(38 - bit_index)
    in

    (* and set 58 *)
    let tap_58 =
      if bit_index >= 58
      then scrambled_bits.(bit_index - 58)
      else bit state ~pos:(57 - bit_index)
    in

    (* xor wrap those monkeys *)
    scrambled_bits.(bit_index) <- bit payload ~pos:bit_index ^: tap_39 ^: tap_58
  done;

  (* fold entire thing : need array/list converters since arrays have nice indexing for tap FIRs *)
  let scrambled_data = concat_lsb (Array.to_list scrambled_bits) in

  (* moving away from Always DSL *)
  let next_state =
    concat_lsb
      (List.init state_width ~f:(fun state_index -> scrambled_bits.(63 - state_index)))
  in

  (* tuple! *)
  scrambled_data, next_state
  [@@ocamlformat "disable"]

let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
  (* spec *)
  let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in

  (* next_state *)
  let next_state_wire = Signal.wire state_width -- "scrambler_next_state" in

  (* current state *)
  let state = reg spec ~clear_to:reset_state next_state_wire -- "scrambler_current_state" in

  (* assignment on structured bindings *)
  let scrambled_data, next_state = parallel_step ~payload:i.encoded_payload_i ~state in
  next_state_wire <-- next_state;

  { O.
    scrambled_data_o = scrambled_data
    ; scrambled_header_o = i.encoded_header_i
  }
  [@@ocamlformat "disable"]
