(* Bohdan Purtell University of Florida

   Interface: Rx_controller This module serves interface to an FSM controller for a
   Hardcaml ethernet MAC.
*)

open! Core
open! Hardcaml

module I : sig
  type 'a t =
    { (* spec *)
      clock : 'a
    ; reset : 'a
    ; en : 'a
    ; (* control lines *)
      rx_dv : 'a
    ; rx_er : 'a
    ; (* data line -> rxd presents Preamble/SFD *)
      rx_data : 'a [@bits 8]
    ; rx_data_valid : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { byte_assembler_en : 'a
    ; dst_mac_reg_en : 'a
    ; src_mac_reg_en : 'a
    ; eth_type_reg_en : 'a
    ; payload_sel : 'a
    ; emit_payload : 'a
    ; fcs_present : 'a
    ; (* FSM state indicators *)
      in_preamble : 'a
    ; in_dst_mac : 'a
    ; in_payload : 'a
    ; (* debug lines *)
      keep : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
