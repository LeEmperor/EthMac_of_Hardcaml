(* Bohdan Purtell University of Florida

   Module: Tx_byte_disassembler This module serves as the nibble-serializer for my
   Hardcaml Ethernet MAC
*)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits

module I = struct
  type 'a t =
    { (* spec *)
      clock : 'a
    ; reset : 'a
    ; en : 'a
    ; (* data lignes *)
      byte_in : 'a [@bits 8]
    ; byte_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { ready : 'a
    ; tx_d : 'a [@bits 4]
    ; tx_en : 'a
    ; (* debug lines *)
      keep : 'a
    }
  [@@deriving hardcaml]
end

(* internal regs block *)
module I_Regs = struct
  type 'a t =
    { bottom_nibble : 'a [@bits 4]
    ; upper_nibble : 'a [@bits 4]
    ; byte_reg : 'a [@bits 8]
    ; high_sent : 'a
    }
  [@@deriving hardcaml]
end

(* internal wires block *)
module I_Wires = struct
  type 'a t =
    { output_nibble : 'a [@bits 4]
    ; nibble_valid : 'a
    ; ready : 'a
    }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) i : _ O.t =
  (* scope shenanigans *)
  let _scope : Scope.t = Scope.sub_scope scope "tx_disassembler_scope" in
  let open Always in
  let open Variable in
  (* port aliases - these resolve as Signal.t items *)
  let clock = i.I.clock in
  let reset = i.I.reset in
  let clear = i.I.reset in
  let en = i.I.en in
  let byte_in = i.I.byte_in in
  let byte_in_valid = i.I.byte_in_valid in
  let rising_edge = Reg_spec.create ~clock ~clear () in
  (* tagging + register creation *)
  let i_regs = I_Regs.Of_always.reg ~enable:vdd rising_edge in
  I_Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) i_regs;
  (* tagging + wire creation *)
  let i_wires = I_Wires.Of_always.wire Signal.zero in
  I_Wires.Of_always.apply_names ~prefix:"wire_" ~naming_op:(Scope.naming scope) i_wires;
  Always.compile
    [ (* defaults *)
      i_regs.high_sent <-- i_regs.high_sent.value
    ; i_regs.byte_reg <-- i_regs.byte_reg.value
    ; i_wires.output_nibble <--. 0
    ; i_wires.ready <--. 0
    ; i_wires.nibble_valid <--. 0
    ; (* if high has been sent, set it to having not been sent *)
      if_
        i_regs.high_sent.value
        [ i_wires.output_nibble <-- sel_top ~width:4 i_regs.byte_reg.value
        ; i_wires.nibble_valid <--. 1
        ; i_regs.high_sent <--. 0
        ]
        [ (* is the new value valid? *)
          if_
            byte_in_valid
            [ (* yes, latch it *)
              i_regs.byte_reg <-- byte_in
            ; (* send it *)
              i_wires.output_nibble <-- sel_bottom ~width:4 byte_in
            ; i_wires.nibble_valid <--. 1
            ; (* declare send high *)
              i_regs.high_sent <--. 1
            ]
            [ (* no, emit nothing *) ]
        ]
    ];
  { ready = ~:(i_regs.high_sent.value)
  ; keep = zero 1
  ; tx_d = i_wires.output_nibble.value
  ; tx_en = i_wires.nibble_valid.value
  }
;;
