(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_tx_path.ml" *)
(* Transmit-side portion of the MII MAC. This circuit owns the TX payload queue,
   completed-frame gate, controller, datapath, serializer, and CRC. *)

open! Core
open! Hardcaml
open! Signal

module Tx_word = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; last : 'a
    }
  [@@deriving hardcaml]
end

module Tx_fifo = Hardcaml_circuits.Fast_fifo.Make (Tx_word)

module I = struct
  type 'a t =
    { clock_i : 'a
    ; reset_i : 'a
    ; en_i : 'a
    ; s_axis_tdata_i : 'a [@bits 8]
    ; s_axis_tvalid_i : 'a
    ; s_axis_tlast_i : 'a
    ; s_axis_tuser_i : 'a
    ; tx_start_i : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { tx_d_o : 'a [@bits 4]
    ; tx_en_o : 'a
    ; tx_busy_o : 'a
    ; s_axis_tready_o : 'a
    }
  [@@deriving hardcaml]
end

let create ?(ethertype = 0x9999) (scope : Scope.t) (i : _ I.t) : _ O.t =
  let clock = i.I.clock_i in
  let reset = i.I.reset_i in
  let en = i.I.en_i in
  (* Forward wires retain the controller/serializer/CRC combinational contracts. *)
  let fifo_rd_en = wire 1 in
  let serializer_ready = wire 1 in
  let fcs_byte = wire 8 in
  let fifo =
    Tx_fifo.hierarchical
      ~instance:"tx_payload_fifo"
      ~name:"tx_payload_fifo"
      ~cut_through:true
      ~capacity:128
      scope
      { Tx_fifo.I.clock
      ; clear = reset
      ; wr_enable = i.s_axis_tvalid_i
      ; wr_data = { Tx_word.data = i.s_axis_tdata_i; last = i.s_axis_tlast_i }
      ; rd_enable = fifo_rd_en
      }
  in
  (* Store-and-forward: launch only after a complete tlast-terminated frame is resident,
     so the read side cannot overtake a streaming writer. *)
  let spec = Reg_spec.create ~clock ~clear:reset () in
  let frame_wr_last = i.s_axis_tvalid_i &: i.s_axis_tlast_i &: ~:(fifo.full) in
  let frame_rd_last = fifo_rd_en &: fifo.rd_data.last in
  let frames_buffered =
    reg_fb spec ~enable:vdd ~width:4 ~f:(fun count ->
      count +: uresize frame_wr_last ~width:4 -: uresize frame_rd_last ~width:4)
    -- "frames_buffered"
  in
  let frame_ready = (frames_buffered <>:. 0) -- "tx_frame_ready" in
  let controller =
    Tx_controller.hierarchical
      ~instance:"tx_controller"
      scope
      { Tx_controller.I.clock
      ; reset
      ; en
      ; start = i.tx_start_i
      ; fifo_empty = ~:(fifo.rd_valid)
      ; frame_ready
      ; dis_ready = serializer_ready
      ; payload_last = fifo.rd_data.last
      }
  in
  (* State 6 is Payload in [Common_types.States]. This deliberately preserves the existing
     decode until that interface exposes a named payload predicate. *)
  fifo_rd_en <-- (controller.state ==:. 6 &: serializer_ready &: ~:(controller.pad));
  let datapath =
    Tx_datapath.hierarchical
      ~instance:"tx_datapath"
      ~ethertype
      scope
      { Tx_datapath.I.clock
      ; reset
      ; en
      ; s_axis_tdata = fifo.rd_data.data
      ; s_axis_tvalid = fifo.rd_valid
      ; s_axis_tuser = i.s_axis_tuser_i
      ; fcs_byte
      ; byte_mux_sel = controller.byte_mux_sel
      ; mac_byte_sel = controller.mac_byte_sel
      ; pad = controller.pad
      }
  in
  let serializer =
    Tx_byte_disassembler.hierarchical
      ~instance:"tx_byte_disassembler"
      scope
      { Tx_byte_disassembler.I.clock
      ; reset
      ; en
      ; byte_in = datapath.byte_out
      ; byte_in_valid = controller.tx_busy
      }
  in
  serializer_ready <-- serializer.ready;
  (* [tx_busy] doubles as the CRC's inter-frame reset. [crc_en] only marks bytes covered
     by the FCS and therefore must not replace it here. *)
  let crc =
    Tx_crc.hierarchical
      ~instance:"tx_crc"
      scope
      { Tx_crc.I.clock
      ; reset
      ; en = controller.tx_busy
      ; data = datapath.byte_out
      ; data_valid = serializer_ready &: controller.crc_en
      ; byte_sel = select controller.mac_byte_sel ~high:1 ~low:0
      }
  in
  fcs_byte <-- crc.fcs_byte;
  { O.tx_d_o = serializer.tx_d
  ; tx_en_o = serializer.tx_en
  ; tx_busy_o = controller.tx_busy
  ; s_axis_tready_o = ~:(fifo.full)
  }
;;

let hierarchical ?instance ?(ethertype = 0x9999) scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"mac_tx_path" (create ~ethertype) i
;;
