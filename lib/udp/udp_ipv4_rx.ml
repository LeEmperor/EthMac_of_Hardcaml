(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_ipv4_rx.ml" *)
(* Reusable single-clock IPv4/UDP receive composition. The Ethernet payload and its
   frame-level status enter from the MAC; the stripped application stream leaves here. *)

open! Core
open! Hardcaml
open! Signal
open! Ipv4_of_hardcaml

module Make (Ipv4_config : Ipv4_rx.Config) (Udp_config : Udp_rx.Config) = struct
  module Ip = Ipv4_rx.Make (Ipv4_config)
  module Udp = Udp_rx.Make (Udp_config)

  module I = struct
    type 'a t =
      { clock_i : 'a
      ; reset_i : 'a
      ; en_i : 'a
      ; rx_tdata_i : 'a [@bits 8]
      ; rx_tvalid_i : 'a
      ; rx_tlast_i : 'a
      ; rx_tuser_i : 'a
      ; rx_tfirst_i : 'a
      ; rx_eth_type_i : 'a [@bits 16]
      ; app_tready_i : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { m_axis_tready_o : 'a
      ; app_tdata_o : 'a [@bits 8]
      ; app_tvalid_o : 'a
      ; app_tlast_o : 'a
      ; app_tfirst_o : 'a
      ; app_start_o : 'a
      ; src_port_o : 'a [@bits 16]
      ; dst_port_o : 'a [@bits 16]
      ; udp_length_o : 'a [@bits 16]
      ; payload_length_o : 'a [@bits 16]
      ; udp_checksum_o : 'a [@bits 16]
      ; src_ip_o : 'a [@bits 32]
      ; dst_ip_o : 'a [@bits 32]
      ; checksum_ok_o : 'a
      ; crc_error_o : 'a
      ; frame_done_o : 'a
      ; ip_busy_o : 'a
      ; udp_busy_o : 'a
      }
    [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =
    let wire_l4_tready = wire 1 in
    let ip =
      Ip.hierarchical
        ~instance:"ipv4_rx"
        scope
        { Ip.I.clock = i.clock_i
        ; reset = i.reset_i
        ; en = i.en_i
        ; rx_tdata = i.rx_tdata_i
        ; rx_tvalid = i.rx_tvalid_i
        ; rx_tlast = i.rx_tlast_i
        ; rx_tuser = i.rx_tuser_i
        ; rx_tfirst = i.rx_tfirst_i
        ; rx_eth_type = i.rx_eth_type_i
        ; l4_tready = wire_l4_tready
        }
    in
    let udp =
      Udp.hierarchical
        ~instance:"udp_rx"
        scope
        { Udp.I.clock = i.clock_i
        ; reset = i.reset_i
        ; en = i.en_i
        ; rx_tdata = ip.m_tdata
        ; rx_tvalid = ip.m_tvalid
        ; rx_tlast = ip.m_tlast
        ; rx_tuser = ip.crc_error
        ; rx_tfirst = ip.m_tfirst
        ; ip_protocol = ip.protocol
        ; ip_src_ip = ip.src_ip
        ; ip_dst_ip = ip.dst_ip
        ; ip_frame_done = ip.frame_done
        ; ip_frame_error = ip.frame_error
        ; app_tready = i.app_tready_i
        }
    in
    wire_l4_tready <-- udp.m_axis_tready;
    let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in
    let crc_error =
      reg_fb spec ~width:1 ~enable:udp.frame_done ~f:(fun _ -> udp.frame_error)
      -- "crc_error_held"
    in
    { O.m_axis_tready_o = ip.m_axis_tready
    ; app_tdata_o = udp.m_tdata
    ; app_tvalid_o = udp.m_tvalid
    ; app_tlast_o = udp.m_tlast
    ; app_tfirst_o = udp.m_tfirst
    ; app_start_o = udp.app_start
    ; src_port_o = udp.src_port
    ; dst_port_o = udp.dst_port
    ; udp_length_o = udp.udp_length
    ; payload_length_o = udp.payload_length
    ; udp_checksum_o = udp.udp_checksum
    ; src_ip_o = udp.src_ip
    ; dst_ip_o = udp.dst_ip
    ; checksum_ok_o = ip.checksum_ok
    ; crc_error_o = crc_error
    ; frame_done_o = udp.frame_done
    ; ip_busy_o = ip.busy
    ; udp_busy_o = udp.busy
    }
  ;;

  let hierarchical ?instance scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name:"udp_ipv4_rx" create i
  ;;
end
