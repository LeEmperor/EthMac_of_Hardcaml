(* Bohdan Purtell University of Florida

   Module: Rx_datapath Byte-level RX datapath: reassembles nibbles into bytes, latches the
   Ethernet header fields (dst/src MAC + ethertype), and runs the 4-deep FCS-strip
   pipeline so the emitted payload excludes the trailing 4 CRC bytes.
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset : 'a
    ; en : 'a
    ; (* reg enables (from rx_controller) *)
      dst_mac_reg_en : 'a
    ; src_mac_reg_en : 'a
    ; byte_assembler_en : 'a
    ; eth_type_reg_en : 'a
    ; (* sels *)
      payload_sel : 'a
    ; emit_payload : 'a
    ; fcs_present : 'a
    ; (* branch input: raw MII nibble *)
      rx_data : 'a [@bits 4]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { raw_byte_out : 'a [@bits 8]
    ; raw_byte_out_valid : 'a
    ; payload_out : 'a [@bits 8]
    ; payload_out_valid : 'a
    ; (* latched ethertype — valid once the ETH_TYPE bytes have shifted in. Surfaced for
         downstream protocol filtering (e.g. UDP-rx). *)
      eth_type : 'a [@bits 16]
    ; (* debug lines *)
      keep : 'a
    }
  [@@deriving hardcaml]
end

(* Header accumulators. Each shifts in one byte per enabled cycle (MSB-first), so after
   the header states they hold the full dst/src MAC and ethertype. *)
module I_Regs = struct
  type 'a t =
    { dst_addr : 'a [@bits 48]
    ; src_addr : 'a [@bits 48]
    ; eth_type : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

let create (scope : Scope.t) (i : _ I.t) : _ O.t =
  let open Always in
  let spec = Reg_spec.create ~clock:i.clock ~clear:i.reset () in
  let byte_assembler_inst =
    Rx_byte_assembler.hierarchical
      ~instance:"rx_byte_assembler"
      scope
      { Rx_byte_assembler.I.rx_data = i.rx_data
      ; en = i.byte_assembler_en
      ; clock = i.clock
      ; reset = i.reset
      }
  in
  let raw_byte_out = byte_assembler_inst.byte_out -- "raw_byte_out" in
  let raw_byte_out_valid = byte_assembler_inst.byte_valid -- "raw_byte_out_valid" in
  let r = I_Regs.Of_always.reg ~enable:vdd spec in
  I_Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) r;
  (* shift one byte into [reg] MSB-first *)
  let shift_in reg byte =
    reg <-- (sll reg.value ~by:8 |: uresize byte ~width:(width reg.value))
  in
  compile
    [ when_ (i.dst_mac_reg_en &: raw_byte_out_valid) [ shift_in r.dst_addr raw_byte_out ]
    ; when_ (i.src_mac_reg_en &: raw_byte_out_valid) [ shift_in r.src_addr raw_byte_out ]
    ; when_ (i.eth_type_reg_en &: raw_byte_out_valid) [ shift_in r.eth_type raw_byte_out ]
    ];
  (* FCS-strip pipeline: delay the byte stream (and its emit-valid) by 4 enabled cycles.
     Only bytes older than the 4 in-flight FCS bytes are ever emitted, so the trailing CRC
     never reaches payload_out. Enable = raw_byte_out_valid so the pipeline advances
     exactly once per assembled byte. *)
  let delayed_byte =
    Signal.pipeline spec ~n:4 ~enable:raw_byte_out_valid raw_byte_out -- "delayed_byte"
  in
  let delayed_valid =
    Signal.pipeline spec ~n:4 ~enable:raw_byte_out_valid i.emit_payload -- "delayed_valid"
  in
  let wire_out = mux i.payload_sel [ zero 8; delayed_byte ] -- "payload_out" in
  let payload_out_valid = (i.emit_payload &: delayed_valid) -- "payload_out_valid" in
  let keep =
    reduce
      ~f:( |: )
      (bits_lsb raw_byte_out
       @ bits_lsb raw_byte_out_valid
       @ bits_lsb r.dst_addr.value
       @ bits_lsb r.src_addr.value
       @ bits_lsb r.eth_type.value
       @ bits_lsb wire_out
       @ bits_lsb delayed_byte
       @ bits_lsb i.fcs_present)
  in
  { O.raw_byte_out
  ; raw_byte_out_valid
  ; payload_out = wire_out
  ; payload_out_valid
  ; eth_type = r.eth_type.value
  ; keep
  }
;;

let hierarchical ?instance scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"rx_datapath" create i
;;
