open! Core
open! Hardcaml
open! Signal
open! Always
open! Variable

module I = struct
  type 'a t =
    { (* spec *)
      clock : 'a
    ; reset : 'a
    ; en : 'a
    ; (* data lignes *)
      rx_data : 'a [@bits 8]
    ; rx_data_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { crc_valid : 'a
    ; crc_out : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

let const_crc_reflect = Signal.of_int_trunc ~width:32 0xEDB88320

(* define crc for a single bit *)
let crc_bit current_crc input_bit : Signal.t =
  let least_sig_bit = current_crc.:[0, 0] in
  let feedback = least_sig_bit ^: input_bit in
  let shifted = srl current_crc ~by:1 in
  let folded = shifted ^: const_crc_reflect in
  mux2 feedback folded shifted
;;

(* define crc for a byte *)
let crc_byte current_crc input_byte : Signal.t =
  List.fold (Signal.bits_lsb input_byte) ~init:current_crc ~f:crc_bit
;;

let create (scope : Scope.t) i : _ O.t =
  (* scope shenanigans *)
  let _scope : Scope.t = Scope.sub_scope scope "rx_crc_scope" in
  (* port aliase *)
  let clock = i.I.clock in
  let reset = i.I.reset in
  let en = i.I.en in
  let rx_data = i.I.rx_data in
  let rx_data_valid = i.I.rx_data_valid in
  (* no ~clear:rst — init value is controlled explicitly in the compile block *)
  let rising_edge = Reg_spec.create ~clock () in
  let crc_reg = reg ~enable:vdd ~width:32 rising_edge in
  let crc_init = Signal.of_int_trunc ~width:32 0xFFFFFFFF in
  (* 0xDEBB20E3 = ~0x2144DF1C: the standard residue without the final output inversion *)
  let crc_residue = Signal.of_int_trunc ~width:32 0xDEBB20E3 in
  compile
    [ if_
        (reset |: ~:en)
        [ crc_reg <-- crc_init ]
        [ when_ rx_data_valid [ crc_reg <-- crc_byte crc_reg.value rx_data ] ]
    ];
  { crc_valid = crc_reg.value ==: crc_residue; crc_out = crc_reg.value }
;;
