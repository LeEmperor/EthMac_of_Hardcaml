(*
  Bohdan Purtell
  University of Florida

  Module: Rx_byte_assembler
  Pairs the PHY's 4-bit MII nibbles into bytes, lo nibble first. The first nibble
  of a pair lands in the low half of [byte_out], the second in the high half;
  [byte_valid] pulses for one cycle as the second nibble completes a byte.
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t = {
    clock   : 'a;
    reset   : 'a;
    rx_data : 'a [@bits 4];
    en      : 'a;
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    byte_out   : 'a [@bits 8]; (* 1 byte *)
    byte_valid : 'a;
  } [@@deriving hardcaml]
end

(* nibble_lo = first nibble seen (low half of the byte); nibble_hi = second.
   have_lo tracks whether we are mid-pair. byte_valid strobes on completion. *)
module I_Regs = struct
  type 'a t = {
    byte_valid : 'a;
    nibble_lo  : 'a [@bits 4];
    nibble_hi  : 'a [@bits 4];
    have_lo    : 'a;
  } [@@deriving hardcaml]
end

let create
  (scope : Scope.t)
  (i : _ I.t) : _ O.t
  =
    let open Always in
    let spec : Reg_spec.t = Reg_spec.create ~clock:i.clock ~clear:i.reset () in

    let r = I_Regs.Of_always.reg ~enable:vdd spec in
    I_Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) r;

    compile [
      r.byte_valid <--. 0;
      when_ i.en [
        if_ r.have_lo.value [
          r.nibble_hi   <-- i.rx_data;
          r.have_lo     <--. 0;
          r.byte_valid  <--. 1;
        ] [
          r.nibble_lo <-- i.rx_data;
          r.have_lo   <--. 1;
        ];
      ];
    ];

  { O.
    byte_out   = r.nibble_hi.value @: r.nibble_lo.value;
    byte_valid = r.byte_valid.value;
  }
;;
