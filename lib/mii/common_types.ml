(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "common_types.ml" *)
(* Common types and functions shared by the MII MAC modules, serving a role similar to a
   SystemVerilog package. *)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits

module States = struct
  type t =
    | Idle
    | Preamble
    | Sfd
    | Dst_mac
    | Src_mac
    | Eth_type
    | Payload
    | Fcs
  [@@deriving sexp_of, compare ~localize, enumerate]

  let width = Int.ceil_log2 (List.length all)

  let to_signal t =
    List.findi_exn all ~f:(fun _ v -> compare v t = 0)
    |> fst
    |> Signal.of_int_trunc ~width
  ;;

  (* let byte_source_of_state = function *)
  (* | Idle -> zero 8 *)
  (* | Preamble -> of_int_trunc ~width:8 55 *)
  (* | Sfd -> of_int_trunc ~width:8 D5 *)
  (* | Dst_mac -> dst_mac_mux *)
  (* | Src_mac -> *)
  (* | Eth_type -> *)
  (* | Payload -> *)
  (* | Fcs -> *)
end
