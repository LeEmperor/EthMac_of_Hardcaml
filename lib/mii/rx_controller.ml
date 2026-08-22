(*
  Bohdan Purtell
  University of Florida

  Module: Rx_controller
  FSM controller for the receive path of the Hardcaml Ethernet MAC. Walks the
  frame header (preamble/SFD → dst → src → ethertype → payload) and drives the
  datapath's register-enable / payload-select control lines.

  NB: SFD (0xD5) is detected inside the PREAMBLE state rather than a dedicated
  state, and there is no FCS state — the trailing CRC is stripped in the datapath
  pipeline, not the controller. (Full unification with Common_types.States is
  deferred for that reason; see rx_controller_intf.ml.)
*)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits (* in theory things like increm_counter can be thrown in here? *)

let () = Stdio.print_endline "=== Imported MAC RX Controller ==="

module I = struct
  type 'a t = {
    (* spec *)
    clock : 'a;
    reset : 'a;
    en  : 'a;

    (* control lines  *)
    rx_dv : 'a;
    rx_er : 'a;

    (* data line -> rxd presents Preamble/SFD *)
    rx_data       : 'a [@bits 8];
    rx_data_valid : 'a;
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    (* submodule ens *)
    byte_assembler_en : 'a;

    (* reg ens *)
    dst_mac_reg_en    : 'a;
    src_mac_reg_en    : 'a;
    eth_type_reg_en   : 'a;

    (* sels *)
    payload_sel : 'a;

    (* misc *)
    emit_payload  : 'a;
    fcs_present   : 'a;

    (* FSM state indicators — 1 when currently in that state *)
    in_preamble : 'a;
    in_dst_mac  : 'a;
    in_payload  : 'a;

    (* debug lines *)
    keep : 'a;
  } [@@deriving hardcaml]
end

module States = struct
  type t =
    | IDLE
    | PREAMBLE
    | DST_MAC
    | SRC_MAC
    | ETH_TYPE
    | PAYLOAD
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* Registered controls. The datapath reg-enables are pipelined one cycle behind
   the state (a Moore output) so they line up with the byte assembler's
   one-cycle-late byte_valid. mac_byte_count paces the multi-byte header states. *)
module I_Regs = struct
  type 'a t = {
    mac_byte_count  : 'a [@bits 3];
    dst_mac_reg_en  : 'a;
    src_mac_reg_en  : 'a;
    eth_type_reg_en : 'a;
  } [@@deriving hardcaml]
end

(* Combinational (Moore) status/select lines — default 0, raised in-state. *)
module I_Wires = struct
  type 'a t = {
    payload_sel  : 'a;
    is_preamble  : 'a;
    is_dst_mac   : 'a;
    is_payload   : 'a;
    emit_payload : 'a;
  } [@@deriving hardcaml]
end

let create
  (scope : Scope.t)
  (i : _ I.t) : _ O.t
  =
  let open Always in
  let rising_edge : Reg_spec.t = Reg_spec.create ~clock:i.clock ~clear:i.reset () in
  let sm = State_machine.create (module States) ~enable:vdd rising_edge in

  let r = I_Regs.Of_always.reg ~enable:vdd rising_edge in
  I_Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) r;

  let w = I_Wires.Of_always.wire Signal.zero in
  I_Wires.Of_always.apply_names ~prefix:"wire_" ~naming_op:(Scope.naming scope) w;

  (* aliases *)
  let rx_er   = i.rx_er in
  let rx_dv   = i.rx_dv in
  let in_data = i.rx_data in
  let en      = i.en in
  let valid   = i.rx_data_valid in
  let stable  = rx_dv &: ~:rx_er &: en in

  let const_0x55 = of_int_trunc ~width:8 0x55 in
  let const_0xD5 = of_int_trunc ~width:8 0xD5 in

  (* helper functions *)
  let byte_counter_is (n : int) =
    r.mac_byte_count.value ==:. n
  in

  let increm_byte_counter = r.mac_byte_count <-- r.mac_byte_count.value +:. 1 in

  (* rising edge of dv, delayed 1 cycle: the CRC-valid strobe helper for mac_top *)
  let fcs_present = rising_edge_delayed rising_edge ~n_cycles:1 i.rx_dv -- "fcs_present" in

  compile [
    (* registered reg-enable defaults (wires default to 0 via Of_always.wire) *)
    r.dst_mac_reg_en <--. 0;
    r.src_mac_reg_en <--. 0;
    r.eth_type_reg_en <--. 0;

    (* Moore outputs per state *)
    sm.switch ~default:[] [
      PREAMBLE, [ w.is_preamble <--. 1 ];
      DST_MAC,  [ w.is_dst_mac <--. 1; r.dst_mac_reg_en <--. 1 ];
      SRC_MAC,  [ r.src_mac_reg_en <--. 1 ];
      ETH_TYPE, [ r.eth_type_reg_en <--. 1 ];
      PAYLOAD,  [ w.is_payload <--. 1; w.payload_sel <--. 1; w.emit_payload <--. 1 ];
    ];

    (* next-state logic, only on a valid assembled byte *)
    when_ valid [
      sm.switch ~default:[ sm.set_next IDLE ] [
        IDLE, [
          if_ stable [ sm.set_next PREAMBLE ] [ sm.set_next IDLE ];
        ];

        PREAMBLE, [
          if_ stable [
            Always.switch in_data [
              const_0x55, [ sm.set_next PREAMBLE ];
              const_0xD5, [ sm.set_next DST_MAC ];
            ];
          ] [
            sm.set_next IDLE;
          ];
        ];

        DST_MAC, [
          if_ stable [
            (* if_ (r.mac_byte_count.value ==:. 5) [ *)
            if_ (byte_counter_is 5) [
              r.mac_byte_count <--. 0;
              sm.set_next SRC_MAC;
            ] [
              r.mac_byte_count <-- r.mac_byte_count.value +:. 1;
              sm.set_next DST_MAC;
            ];
          ] [
            sm.set_next IDLE;
          ];
        ];

        SRC_MAC, [
          if_ stable [
            if_ (r.mac_byte_count.value ==:. 5) [
              r.mac_byte_count <--. 0;
              sm.set_next ETH_TYPE;
            ] [
              r.mac_byte_count <-- r.mac_byte_count.value +:. 1;
              sm.set_next SRC_MAC;
            ];
          ] [
            sm.set_next IDLE;
          ];
        ];

        ETH_TYPE, [
          if_ stable [
            if_ (r.mac_byte_count.value ==:. 1) [
              r.mac_byte_count <--. 0;
              sm.set_next PAYLOAD;
            ] [
              r.mac_byte_count <-- r.mac_byte_count.value +:. 1;
              sm.set_next ETH_TYPE;
            ];
          ] [
            sm.set_next IDLE;
          ];
        ];

        PAYLOAD, [
          (* priority: error aborts; otherwise stay while dv holds, else frame end *)
          if_ rx_er [
            sm.set_next IDLE;
          ] [
            if_ rx_dv [ sm.set_next PAYLOAD ] [ sm.set_next PREAMBLE ];
          ];
        ];
      ];
    ];
  ];

  let keep =
    reduce ~f:( |: )
      (bits_lsb r.mac_byte_count.value
       @ bits_lsb r.dst_mac_reg_en.value
       @ bits_lsb r.src_mac_reg_en.value
       @ bits_lsb w.emit_payload.value
       @ bits_lsb fcs_present)
  in

  { O.
    byte_assembler_en = i.en &: i.rx_dv;
    dst_mac_reg_en    = r.dst_mac_reg_en.value;
    src_mac_reg_en    = r.src_mac_reg_en.value;
    eth_type_reg_en   = r.eth_type_reg_en.value;
    payload_sel       = w.payload_sel.value;
    emit_payload      = w.emit_payload.value;
    fcs_present;
    in_preamble       = w.is_preamble.value;
    in_dst_mac        = w.is_dst_mac.value;
    in_payload        = w.is_payload.value;
    keep;
  }
;;
