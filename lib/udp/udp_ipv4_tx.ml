(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_ipv4_tx.ml" *)
(* Reusable single-clock UDP-over-IPv4 transmit composition. Endpoint policy remains an
   elaboration-time choice made by the parent through the two configuration functors. *)

open! Core
open! Hardcaml
open! Signal
open! Ipv4_of_hardcaml

module Make (Udp_config : Udp_tx.Config) (Ipv4_config : Ipv4_tx.Config) = struct
  module Udp = Udp_tx.Make (Udp_config)
  module Ip = Ipv4_tx.Make (Ipv4_config)

  module I = struct
    type 'a t =
      { clock_i : 'a
      ; reset_i : 'a
      ; en_i : 'a
      ; start_i : 'a
      ; payload_len_i : 'a [@bits 16]
      ; payload_tdata_i : 'a [@bits 8]
      ; payload_tvalid_i : 'a
      ; mac_tready_i : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { m_tdata_o : 'a [@bits 8]
      ; m_tvalid_o : 'a
      ; m_tlast_o : 'a
      ; tx_start_o : 'a
      ; payload_tready_o : 'a
      ; udp_busy_o : 'a
      ; ip_busy_o : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =
    let wire_l4_tready = wire 1 in
    let udp =
      Udp.hierarchical
        ~instance:"udp_tx"
        scope
        { Udp.I.clock = i.clock_i
        ; reset = i.reset_i
        ; en = i.en_i
        ; start = i.start_i
        ; payload_len = i.payload_len_i
        ; payload_tdata = i.payload_tdata_i
        ; payload_tvalid = i.payload_tvalid_i
        ; l4_tready = wire_l4_tready
        }
    in
    let ip =
      Ip.hierarchical
        ~instance:"ipv4_tx"
        scope
        { Ip.I.clock = i.clock_i
        ; reset = i.reset_i
        ; en = i.en_i
        ; start = udp.ip_start
        ; l4_length = udp.l4_length
        ; protocol = udp.protocol
        ; l4_tdata = udp.m_tdata
        ; l4_tvalid = udp.m_tvalid
        ; l4_tlast = udp.m_tlast
        ; mac_tready = i.mac_tready_i
        }
    in
    wire_l4_tready <-- ip.l4_tready;
    { O.m_tdata_o = ip.m_tdata
    ; m_tvalid_o = ip.m_tvalid
    ; m_tlast_o = ip.m_tlast
    ; tx_start_o = ip.tx_start
    ; payload_tready_o = udp.payload_tready
    ; udp_busy_o = udp.busy
    ; ip_busy_o = ip.busy
    }
  ;;

  let hierarchical ?instance scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name:"udp_ipv4_tx" create i
  ;;
end
