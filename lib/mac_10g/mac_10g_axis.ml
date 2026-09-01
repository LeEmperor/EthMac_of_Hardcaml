(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_axis.ml" *)
(* Direction-neutral 64-bit AXI4-Stream frame beat used by the 10G MAC datapaths. *)

open! Hardcaml

module Beat = struct
  type 'a t =
    { data : 'a [@bits 64]
    ; keep : 'a [@bits 8]
    ; last : 'a
    ; user : 'a
    }
  [@@deriving hardcaml]
end
