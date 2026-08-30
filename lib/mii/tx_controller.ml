(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_controller.ml" *)
(* FSM controller for the transmit path of the Hardcaml Ethernet MAC. *)

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
    ; start : 'a
    ; fifo_empty : 'a
    ; frame_ready : 'a
        (* store-and-forward gate: 1 once a whole tlast-terminated datagram is buffered in
           the TX FIFO *)
    ; dis_ready : 'a (* tx_byte_disassembler.ready — 1 when it can accept a new byte *)
    ; payload_last : 'a
    (* s_axis tlast travelling with the current FIFO byte — marks the final payload byte *)
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { byte_mux_sel : 'a [@bits 3]
    ; mac_byte_sel : 'a [@bits 3]
    ; crc_en : 'a
        (* 1 while the byte being emitted this cycle is covered by the FCS: the two MACs,
           the ethertype and the payload. Low through the preamble and SFD, which precede
           the CRC, and through Fcs, where the accumulator is being read out rather than
           fed. This is the FSM's own statement of the CRC window - [mac_top] gates
           [Tx_crc.data_valid] with it rather than re-deriving the window from [state]. *)
    ; state : 'a [@bits 3]
    ; tx_busy : 'a (* 1 while a frame is in flight (Preamble..Fcs), 0 in Idle *)
    ; pad : 'a
    (* 1 while zero-padding a sub-minimum payload — datapath emits 0x00 and the FIFO is
       not popped *)
    }
  [@@deriving hardcaml]
end

module I_Regs = struct
  type 'a t =
    { byte_counter : 'a [@bits 11]
    ; busy : 'a
    ; start_pending : 'a
        (* latched start request — lets a streaming source pulse start before its first
           byte lands in the FIFO *)
    ; padding : 'a
    (* 1 once the datagram's real bytes are exhausted but the 46-byte minimum is not yet
       met *)
    }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) i : _ O.t =
  let open Always in
  let open Variable in
  let _scope : Scope.t = Scope.sub_scope scope "tx_controller_scope" in
  let clock = i.I.clock in
  let reset = i.I.reset in
  let clear = i.I.reset in
  let _en = i.I.en in
  let start = i.I.start in
  let fifo_empty = i.I.fifo_empty -- "fifo_empty" in
  let frame_ready = i.I.frame_ready -- "frame_ready" in
  let dis_ready = i.I.dis_ready -- "dis_ready" in
  let rising_edge : Reg_spec.t = Reg_spec.create ~clock ~clear () in
  let sm = State_machine.create (module States) ~enable:vdd rising_edge in
  let i_regs = I_Regs.Of_always.reg ~enable:vdd rising_edge in
  I_Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) i_regs;
  (* local closure helper functions for counter handling *)
  let rst_counter = i_regs.byte_counter <--. 0 in
  let increm_counter = i_regs.byte_counter <-- i_regs.byte_counter.value +:. 1 in
  (* Padding, as the [Payload] state below should read it. The registered [padding] only
     latches on a byte carrying [payload_last], so a zero-length datagram — which reaches
     [Payload] with the FIFO already empty and never presents such a byte — could satisfy
     neither exit of that state and would wait there forever. An empty payload is
     sub-minimum by definition, so "empty on arrival in [Payload]" is padding too, and the
     pad branch then emits the full 46 zero bytes like any other short datagram.

     Combinational rather than a second registered latch on purpose: a cycle spent in
     [Payload] with [pad] low and the FIFO empty would still present a byte to the
     serializer ([mac_top] ties [byte_in_valid] to ~(state == Idle)), putting one garbage
     byte on the wire before the pad run started. Store-and-forward means [fifo_empty]
     cannot rise mid-payload for a datagram that has any bytes at all, so this reads
     identically to [padding] for every length >= 1. *)
  let padding_now =
    (i_regs.padding.value |: (sm.is Payload &: fifo_empty)) -- "padding_now"
  in
  (* The FCS covers the two MACs, the ethertype and the payload - everything between the
     SFD and the FCS itself. A decode of the current state, not a pulse: [Tx_crc]'s
     [data_valid] wants "the byte on the wire this cycle is covered", which is exactly
     what state membership says. *)
  let crc_active =
    (sm.is Dst_mac |: sm.is Src_mac |: sm.is Eth_type |: sm.is Payload) -- "crc_active"
  in
  compile
    [ i_regs.busy <-- i_regs.busy.value
    ; sm.switch
        ~default:[]
        [ ( Idle
          , [ (* Store-and-forward launch. [start] and the buffered-frame gate arrive
                 independently, in either order:
                 - pre-buffered (tx_path_tb): the whole datagram — tlast included — is
                   written first, then [start] pulses. [frame_ready] is already 1, so this
                   fires the same cycle.
                 - streaming (udp_tx): [start] pulses as the first byte is emitted, long
                   before the datagram finishes arriving. We latch it in [start_pending]
                   and hold in Idle until [frame_ready] rises (the tlast byte has landed
                   in the FIFO), then launch. Waiting for the complete frame is what makes
                   the drain race-free: once we leave Idle every payload byte is already
                   resident, so the FIFO read side never chases an in-flight write. *)
              when_ start [ i_regs.start_pending <--. 1 ]
            ; when_
                (start |: i_regs.start_pending.value &: frame_ready)
                [ i_regs.busy <--. 1
                ; i_regs.padding <--. 0
                ; i_regs.start_pending <--. 0
                ; rst_counter
                ; sm.set_next Preamble
                ]
            ] )
        ; ( Preamble
          , [ (* 7 bytes of 0x55 *)
              when_
                dis_ready
                [ if_
                    (i_regs.byte_counter.value ==:. 6)
                    [ rst_counter; sm.set_next Sfd ]
                    [ increm_counter ]
                ]
            ] )
        ; Sfd, [ (* 1 byte: 0xD5 *) when_ dis_ready [ sm.set_next Dst_mac ] ]
        ; ( Dst_mac
          , [ (* 6 bytes *)
              when_
                dis_ready
                [ if_
                    (i_regs.byte_counter.value ==:. 5)
                    [ rst_counter; sm.set_next Src_mac ]
                    [ increm_counter ]
                ]
            ] )
        ; ( Src_mac
          , [ (* 6 bytes *)
              when_
                dis_ready
                [ if_
                    (i_regs.byte_counter.value ==:. 5)
                    [ rst_counter; sm.set_next Eth_type ]
                    [ increm_counter ]
                ]
            ] )
        ; ( Eth_type
          , [ (* 2 bytes *)
              when_
                dis_ready
                [ if_
                    (i_regs.byte_counter.value ==:. 1)
                    [ rst_counter; sm.set_next Payload ]
                    [ increm_counter ]
                ]
            ] )
        ; (* minimum frame 64 bytes (incl FCS, excl preamble/SFD) header: 6+6+2 = 14, FCS:
             4 → minimum payload: 64-14-4 = 46 bytes.

             The payload length is now data-driven: it ends on [payload_last] (s_axis
             tlast), not a fixed count. A short datagram (< 46 payload bytes) is
             zero-padded up to the minimum before FCS; a long one runs until tlast.
             byte_counter is the running payload-byte index (0-based), so index 45 == the
             46th byte == exactly the minimum. *)
          ( Payload
          , [ if_
                padding_now
                [ (* real bytes are exhausted (fifo_empty); emit zeros on serializer ready
                     only, until the frame reaches the 46-byte minimum. Latch [padding] so
                     the pad run holds even if [fifo_empty] does not; the handover to Fcs
                     below clears it again. *)
                  i_regs.padding <--. 1
                ; when_
                    dis_ready
                    [ if_
                        (i_regs.byte_counter.value ==:. 45)
                        [ rst_counter; i_regs.padding <--. 0; sm.set_next Fcs ]
                        [ increm_counter ]
                    ]
                ]
                [ (* consume a real FIFO byte when the serializer can take it *)
                  when_
                    (~:fifo_empty &: dis_ready)
                    [ if_
                        i.I.payload_last
                        [ (* final byte of the datagram *)
                          if_
                            (i_regs.byte_counter.value >=:. 45)
                            [ (* frame already meets the minimum → straight to FCS *)
                              rst_counter
                            ; sm.set_next Fcs
                            ]
                            [ (* sub-minimum → pad the remainder with zeros *)
                              i_regs.padding <--. 1
                            ; increm_counter
                            ]
                        ]
                        [ increm_counter ]
                    ]
                ]
            ] )
        ; ( Fcs
          , [ (* 4 bytes *)
              when_
                dis_ready
                [ if_
                    (i_regs.byte_counter.value ==:. 3)
                    [ (* last FCS byte — frame complete, drop busy exactly on return to
                         Idle *)
                      rst_counter
                    ; i_regs.busy <--. 0
                    ; sm.set_next Idle
                    ]
                    [ increm_counter ]
                ]
            ] )
        ]
    ];
  { byte_mux_sel = sm.current
  ; mac_byte_sel = select i_regs.byte_counter.value ~high:2 ~low:0
  ; crc_en = crc_active
  ; state = sm.current
  ; tx_busy = i_regs.busy.value
  ; pad = padding_now
  }
;;

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"tx_controller" create i
;;
