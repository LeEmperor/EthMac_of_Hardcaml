(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_datapath.ml" *)
(* Byte-level datapath for the Hardcaml Ethernet MAC transmit path. *)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits
open! Common_types

module I = struct
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; en : 'a
    ; s_axis_tdata : 'a [@bits 8]
    ; s_axis_tvalid : 'a
    ; s_axis_tuser : 'a
    ; fcs_byte : 'a [@bits 8] (* from tx_crc; muxed out during Fcs state *)
    ; (* byte_mux_sel = sm.current from tx_controller *)
      byte_mux_sel : 'a [@bits 3]
    ; (* bottom bits of controller byte_counter — selects which MAC/eth-type byte *)
      mac_byte_sel : 'a [@bits 3]
    ; (* 1 while the controller is zero-padding a sub-minimum payload *)
      pad : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { byte_out : 'a [@bits 8]
    ; s_axis_tready : 'a
    ; keep : 'a
    }
  [@@deriving hardcaml]
end

let create ?(ethertype = 0x9999) (scope : Scope.t) i : _ O.t =
  let _scope : Scope.t = Scope.sub_scope scope "tx_datapath_scope" in
  let _clock = i.I.clock in
  let _rst = i.I.reset in
  let _en = i.I.en in
  let mac_byte_sel = i.I.mac_byte_sel in
  (* dst: broadcast so the laptop accepts it on whichever port is cabled to the Arty *)
  let const_dst_mac =
    List.map ~f:(of_int_trunc ~width:8) [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]
  in
  (* src: locally-administered MAC for the Arty (02:xx = not a burned-in OUI) *)
  let const_src_mac =
    List.map ~f:(of_int_trunc ~width:8) [ 0x02; 0x00; 0x00; 0x00; 0x00; 0x01 ]
  in
  (* ethertype parameterized via [?ethertype] (default 0x9999, a custom/unknown type the
     kernel ignores — easy to sniff with `tcpdump -i <iface> ether proto 0x9999`). IPv4
     stacks pass 0x0800 so a real host accepts the frame. *)
  let const_eth_type =
    List.map
      ~f:(of_int_trunc ~width:8)
      [ (ethertype lsr 8) land 0xff; ethertype land 0xff ]
  in
  let dst_mac_mux = mux mac_byte_sel const_dst_mac in
  let src_mac_mux = mux mac_byte_sel const_src_mac in
  let eth_type_mux = mux mac_byte_sel const_eth_type in
  let byte_source_of_state : States.t -> Signal.t = function
    | Idle -> of_int_trunc ~width:8 0
    | Preamble -> of_int_trunc ~width:8 0x55
    | Sfd -> of_int_trunc ~width:8 0xD5
    | Dst_mac -> dst_mac_mux
    | Src_mac -> src_mac_mux
    | Eth_type -> eth_type_mux
    (* during padding the FIFO is not popped; emit 0x00 so the CRC covers the zero pad
       bytes required to reach the 46-byte minimum payload *)
    | Payload -> mux2 i.I.pad (of_int_trunc ~width:8 0) i.I.s_axis_tdata
    | Fcs -> i.I.fcs_byte
  in
  (* Explicit list in States declaration order — exhaustiveness is enforced by the pattern
     match in byte_source_of_state above. *)
  let byte_mux =
    Signal.mux
      i.I.byte_mux_sel
      [ byte_source_of_state Idle
      ; byte_source_of_state Preamble
      ; byte_source_of_state Sfd
      ; byte_source_of_state Dst_mac
      ; byte_source_of_state Src_mac
      ; byte_source_of_state Eth_type
      ; byte_source_of_state Payload
      ; byte_source_of_state Fcs
      ]
    -- "byte_mux"
  in
  { byte_out = byte_mux; s_axis_tready = zero 1; keep = lsb byte_mux }
;;

let hierarchical ?instance ?(ethertype = 0x9999) scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"tx_datapath" (create ~ethertype) i
;;
