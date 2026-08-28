(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "eth_frame.ml" *)
(* A validated Ethernet II frame description shared by the MII suites.

   The record is direction-neutral (formatting guide section 4), so no field carries an
   [_i] or [_o] suffix. [create] rejects malformed frames at construction time rather than
   letting a bad vector reach the DUT, which keeps Quickcheck shrinkers from reporting
   failures that are really generator bugs.

   Byte-order note: [crc_covered_bytes] is exactly the span the FCS is computed over
   (destination MAC through payload) — preamble and SFD are outside it.

   Tags: [{ "ACTIVE" ; "TEST" ; "REFERENCE_MODEL" ; "COMMON_ITEMS" }]
*)

open! Core

type t =
  { preamble_length : int
  ; destination_mac : int list
  ; source_mac : int list
  ; eth_type : int list
  ; payload : int list
  }
[@@deriving sexp, equal, compare]

let preamble_byte = 0x55
let sfd_byte = 0xD5

[@@@ocamlformat "disable"]

(* on todady's episode of Bo vs the formatter *)
let create
  ?(preamble_length     = 7)
  ?(destination_mac     = [ 0x00; 0x11; 0x22; 0x33; 0x44; 0x55 ])
  ?(source_mac          = [ 0x66; 0x77; 0x88; 0x99; 0xAA; 0xBB ])
  ?(eth_type            = [ 0x08; 0x00 ])
  ?(payload             = [])
  () (* we love placement unit! *)

  = (* we can get away with alot of sw here, it is verif after all *)

  (* preamble length check *)
  if preamble_length < 1 then invalid_arg "preamble_length must be positive";

  (* bunch fo joyous checks*)
  if List.length destination_mac <> 6 (* wonder if ocaml has a spaceship operator *)
  then invalid_arg "destination_mac must contain 6 bytes";

  if List.length source_mac <> 6 then invalid_arg "source_mac must contain 6 bytes";
  if List.length eth_type <> 2 then invalid_arg "eth_type must contain 2 bytes";

  (* cate those fuckers, and check against overflows and whether or not the int values are within bounds *)
  (* definitely a formal verif point right here *)
  let all_bytes = destination_mac @ source_mac @ eth_type @ payload in
  if List.exists all_bytes ~f:(fun byte -> byte < 0 || byte > 0xFF)
  then invalid_arg "frame bytes must be between 0x00 and 0xff";

  { preamble_length
  ; destination_mac
  ; source_mac
  ; eth_type
  ; payload
  }
;;
[@@@ocamlformat "enable"]

let byte_count t = t.preamble_length + 1 + 6 + 6 + 2 + List.length t.payload

(* The span the FCS covers - everything from the destination MAC onwards *)
let crc_covered_bytes t = t.destination_mac @ t.source_mac @ t.eth_type @ t.payload

(* The full wire frame minus the FCS - preamble, SFD, then the covered span *)
let to_bytes t =
  List.init t.preamble_length ~f:(fun _ -> preamble_byte)
  @ (sfd_byte :: crc_covered_bytes t)
;;

let fcs_bytes t = Crc32.fcs_bytes (crc_covered_bytes t)
let with_fcs t = to_bytes t @ fcs_bytes t
