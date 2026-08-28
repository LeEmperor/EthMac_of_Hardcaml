(* Bohdan Purtell University of Florida

   Module: Mac_top Toplevel of the MII MAC.
*)

open! Core
open! Hardcaml
open! Signal

module Rx_word = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; last : 'a
    ; user : 'a
    (* NB: no [keep] field. tkeep is constant 1 on a single-byte MAC, so it was previously
       packed as [Signal.vdd] into the FIFO word. Writing a constant into the async FIFO's
       distributed RAM makes Vivado constant-propagate that RAM bit and orphan the
       read-address pin on the primitive it shares with [user] — the "Driverless net
       ram_reg_0_63_9_10/DPRA0" DRC failure. tkeep is now tied high directly on the read
       side instead of crossing the FIFO. *)
    }
  [@@deriving hardcaml]
end

(* RX FIFO is the RX→TX clock-domain crossing: a Gray-coded asynchronous FIFO, written on
   rx_clock (PHY RX domain) and read on tx_clock (consumer domain). The whole Rx_word
   (data+last+keep+user) is packed into the flat [data_in] bus and unpacked on the read
   side, so tlast/tuser stay attached to their byte across the crossing. Depth 2^6 = 64:
   kept within a single distributed-RAM address range to avoid the Gray-pointer addressing
   glitches Async_fifo warns about above 2^LUT_SIZE. *)
module Rx_async_fifo = Hardcaml.Async_fifo.Make (struct
    let width = Rx_word.sum_of_port_widths
    let log2_depth = 6
    let optimize_for_same_clock_rate_and_always_reading = false
  end)

module Tx_word = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; last : 'a
    (* s_axis tlast, carried alongside its byte so the controller sees it at consume time *)
    }
  [@@deriving hardcaml]
end

module Tx_fifo = Hardcaml_circuits.Fast_fifo.Make (Tx_word)

module I = struct
  type 'a t =
    { (* ── clocks / resets ── Two independent domains, as required by MII:
         - rx_clock (eth_rx_clk): RX data from the PHY is source-synchronous to it.
         - tx_clock (eth_tx_clk): the PHY samples TX data on it. Each domain has its own
           (already-synchronized) reset. *)
      rx_clock : 'a
    ; rx_reset : 'a
    ; tx_clock : 'a
    ; tx_reset : 'a
    ; en : 'a
    ; (* ethernet phy rx lines (rx_clock domain) *)
      rx_dv : 'a (* activity line *)
    ; rx_er : 'a (* phy error line *)
    ; rx_data : 'a [@bits 4]
    ; (* axis exposed out signals *)
      (* Logic -> PHY *)
      m_axis_tready : 'a
    ; (* TX AXI-Stream input *)
      s_axis_tdata : 'a [@bits 8]
    ; s_axis_tvalid : 'a
    ; s_axis_tlast : 'a
        (* marks the final payload byte; drives variable-length TX + zero padding *)
    ; s_axis_tuser : 'a
    ; tx_start : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { (* axis exposed out signals *)
      (* PHY -> MAC -> downstream logic *)
      m_axis_tdata : 'a [@bits 8] (* 1 byte *)
    ; m_axis_tkeep : 'a
    ; m_axis_tlast : 'a
    ; m_axis_tvalid : 'a
    ; m_axis_tuser : 'a
    ; (* 1-transfer pulse on the first payload byte of each RX frame (read/tx_clock
         domain, aligned to m_axis). This is the start-of-frame kick a downstream protocol
         FSM (e.g. UDP-rx) needs to begin parsing. *)
      m_axis_tfirst : 'a
    ; (* FSM state indicators *)
      in_preamble : 'a
    ; in_dst_mac : 'a
    ; in_payload : 'a
    ; (* CRC result — sampled once per frame *)
      frame_crc_ok : 'a (* holds last frame's CRC result; 1 = good *)
    ; frame_done : 'a (* 1-cycle pulse when a frame completes *)
    ; (* Latched RX ethertype (rx_clock domain), surfaced for protocol filtering —
         consistent with the frame_crc_ok/in_payload rx-domain passthroughs already
         re-exported by udp_mac_top. Stable per-frame; a tx-domain consumer samples it at
         m_axis_tfirst. (Future hardening: pack into Rx_word to make it read-side-aligned
         if the CDC caveat bites.) *)
      rx_eth_type : 'a [@bits 16]
    ; (* TX MII output *)
      tx_d : 'a [@bits 4]
    ; tx_en : 'a
    ; (* TX status: 1 while a frame is being transmitted (Preamble..Fcs) *)
      tx_busy : 'a
    ; (* TX AXI-Stream backpressure *)
      s_axis_tready : 'a
    ; (* debug lines *)
      keep : 'a
    }
  [@@deriving hardcaml]
end

let create ?(rx_fifo_for_sim = false) ?(ethertype = 0x9999) (scope : Scope.t) inputs
  : _ O.t
  =
  let open Always in
  let open Variable in
  (* port aliases *)
  let rx_clock = inputs.I.rx_clock in
  let rx_reset = inputs.I.rx_reset in
  let tx_clock = inputs.I.tx_clock in
  let tx_reset = inputs.I.tx_reset in
  let en = inputs.I.en in
  let d_in = inputs.I.rx_data in
  let d_out = Signal.wire 8 in
  (* rx_spec (eth_rx_clk) clocks all top-level RX pipeline registers: byte assembler,
     datapath, CRC, and the RX FIFO write side. RX pins are source-synchronous to
     eth_rx_clk. The TX submodules take tx_clock/tx_reset directly, so there is no
     top-level tx reg spec here. NOTE: at this step the RX FIFO is still a single-clock
     Fast_fifo clocked on rx_clock; the true async CDC bridge (Async_fifo, write=rx /
     read=tx) is the next step. Until then, m_axis is still in the rx_clock domain. *)
  let rx_spec : Reg_spec.t = Reg_spec.create ~clock:rx_clock ~clear:rx_reset () in
  (* internal ties *)
  (* let wire_byte_assembler_en = wire ~default:gnd in (* does this autosize? *) *)
  let wire_byte_assembler_en = Signal.wire 1 in
  let wire_raw_byte_out = Signal.wire 8 in
  let wire_raw_byte_out_valid = Signal.wire 1 in
  let wire_payload_sel = Signal.wire 1 in
  let wire_dst_mac_reg_en = Signal.wire 1 in
  let wire_src_mac_reg_en = Signal.wire 1 in
  let wire_eth_type_reg_en = Signal.wire 1 in
  let wire_emit_payload = Signal.wire 1 in
  let wire_fcs_present = Signal.wire 1 in
  (* NB: no forward-declared [keep] wire here. keep-folding is a pure sink (nothing feeds
     back into it), so it is built at the end of [create] once every submodule instance is
     in scope — see the [reduce] below. Forward wires are only needed to break
     combinational cycles, which this isn't. *)
  let datapath_inst : Signal.t Rx_datapath.O.t =
    Rx_datapath.create
      scope
      { Rx_datapath.I.rx_data = d_in
      ; Rx_datapath.I.byte_assembler_en = wire_byte_assembler_en
      ; Rx_datapath.I.clock = rx_clock
      ; Rx_datapath.I.reset = rx_reset
      ; Rx_datapath.I.en
      ; Rx_datapath.I.payload_sel = wire_payload_sel
      ; Rx_datapath.I.dst_mac_reg_en = wire_dst_mac_reg_en
      ; Rx_datapath.I.src_mac_reg_en = wire_src_mac_reg_en
      ; Rx_datapath.I.eth_type_reg_en = wire_eth_type_reg_en
      ; Rx_datapath.I.emit_payload = wire_emit_payload
      ; Rx_datapath.I.fcs_present =
          wire_fcs_present
          (* perhaps fcs can be moved to purely datapath item instead of needing to cross
             from the controller to the datapath *)
      }
  in
  let controller_inst =
    Rx_controller.create
      scope
      { Rx_controller.I.clock = rx_clock
      ; Rx_controller.I.reset = rx_reset
      ; Rx_controller.I.en
      ; Rx_controller.I.rx_dv = inputs.I.rx_dv
      ; Rx_controller.I.rx_er = inputs.I.rx_er
      ; Rx_controller.I.rx_data_valid = wire_raw_byte_out_valid
      ; Rx_controller.I.rx_data = datapath_inst.raw_byte_out
      }
  in
  (* wire assigns *)
  Signal.(wire_raw_byte_out <-- datapath_inst.raw_byte_out);
  Signal.(wire_raw_byte_out_valid <-- datapath_inst.raw_byte_out_valid);
  Signal.(wire_byte_assembler_en <-- controller_inst.byte_assembler_en);
  Signal.(wire_payload_sel <-- controller_inst.payload_sel);
  Signal.(wire_dst_mac_reg_en <-- controller_inst.dst_mac_reg_en);
  Signal.(wire_src_mac_reg_en <-- controller_inst.src_mac_reg_en);
  Signal.(wire_eth_type_reg_en <-- controller_inst.eth_type_reg_en);
  Signal.(wire_emit_payload <-- controller_inst.emit_payload);
  Signal.(wire_fcs_present <-- controller_inst.fcs_present);
  let frame_end = Helper_circuits.falling_edge_detector rx_spec inputs.I.rx_dv in
  (* rx_dv drops before byte_valid fires for FCS[3] (the last byte). Extend crc_en through
     frame_end so the CRC processes that final byte and settles at the residue on the
     cycle when we sample crc_valid for tuser. *)
  let crc_en = ~:(controller_inst.in_preamble) &: (inputs.I.rx_dv |: frame_end) &: en in
  let crc_inst =
    Rx_crc.create
      scope
      { Rx_crc.I.clock = rx_clock
      ; reset = rx_reset
      ; en = crc_en
      ; rx_data = datapath_inst.raw_byte_out
      ; rx_data_valid = datapath_inst.raw_byte_out_valid
      }
  in
  (* frame_end_d: frame_end delayed by 1 cycle. B(F3) = the extra cycle after rx_dv drops
     (raw_byte_out_valid=1 for FCS[3]). wr_enable_raw=1 at B(F3) → wr_valid_d=1 one cycle
     later (i=0 of drain phase). frame_end fires at B(F3); frame_end_d fires at i=0,
     aligning with wr_valid_d. *)
  let frame_end_d = Signal.reg rx_spec frame_end in
  (* frame_end fires when rx_dv drops; crc_reg updates with FCS[3] at the clock edge
     ending that cycle. frame_end_d fires one cycle later when crc_valid is already
     settled — that is the correct sample point. *)
  let frame_ok = Signal.reg rx_spec ~enable:frame_end_d crc_inst.crc_valid in
  (* gate on raw_byte_out_valid: the datapath holds payload_out_valid high between nibble
     pairs, so without this gate every byte would be written twice *)
  let wr_enable_raw =
    datapath_inst.payload_out_valid &: datapath_inst.raw_byte_out_valid
  in
  (* 1-cycle delay: crc_inst.crc_valid is combinatorial from crc_reg, which is updated at
     the clock edge that processes FCS[3]. the delayed write lands on the following cycle
     when crc_valid is already settled, so we can use it directly for tuser. *)
  let wr_data_d = Signal.reg rx_spec datapath_inst.payload_out in
  let wr_valid_d = Signal.reg rx_spec wr_enable_raw in
  (* tlast: frame_end_d (1-cycle-delayed falling edge) aligns with wr_valid_d for last
     byte *)
  let tlast_wr = frame_end_d &: wr_valid_d in
  (* pack the RX word on the write (rx_clock) side; unpack on the read (tx_clock) side *)
  let rx_wr_word =
    { Rx_word.data = wr_data_d
    ; last = tlast_wr
    ; user = mux2 tlast_wr ~:(crc_inst.crc_valid) Signal.gnd
    }
  in
  (* Async_fifo uses async resets internally, which Cyclesim can't model; the testbenches
     pass [~rx_fifo_for_sim:true] to swap in the sync-clear variant. The eta-expansion
     over [i] keeps both branches at type [I.t -> O.t]. *)
  let rx_fifo_impl (i : Signal.t Rx_async_fifo.I.t) : Signal.t Rx_async_fifo.O.t =
    if rx_fifo_for_sim
    then
      Rx_async_fifo.For_testing
      .create_with_synchronous_clear_semantics_for_simulation_only
        ~scope
        i
    else Rx_async_fifo.create ~scope i
  in
  let rx_fifo =
    rx_fifo_impl
      { Rx_async_fifo.I.clock_write = rx_clock
      ; reset_write = rx_reset
      ; clock_read = tx_clock
      ; reset_read = tx_reset
      ; data_in = Rx_word.Of_signal.pack rx_wr_word
      ; write_enable = wr_valid_d
      ; read_enable = inputs.m_axis_tready
      }
  in
  let rx_rd_word = Rx_word.Of_signal.unpack rx_fifo.data_out in
  (* TX payload FIFO: AXI-S writes in, controller reads out during Payload state.
     wire_tx_fifo_rd_en and wire_dis_ready break instantiation-order dependencies. *)
  let wire_tx_fifo_rd_en = Signal.wire 1 in
  let wire_dis_ready = Signal.wire 1 in
  let wire_fcs_byte = Signal.wire 8 in
  let tx_fifo =
    Tx_fifo.create
      ~cut_through:true
      ~capacity:128
      scope
      { Tx_fifo.I.clock = tx_clock
      ; clear = tx_reset
      ; wr_enable = inputs.I.s_axis_tvalid
      ; wr_data = { Tx_word.data = inputs.I.s_axis_tdata; last = inputs.I.s_axis_tlast }
      ; rd_enable = wire_tx_fifo_rd_en
      }
  in
  (* Store-and-forward gate. The TX FIFO is cut-through, so left alone the controller
     would start draining as soon as the first byte arrives. That is safe for a
     pre-buffered writer (tx_path_tb writes the whole frame, then pulses tx_start) but
     races a streaming writer (udp_tx fills the FIFO while the MAC is already reading):
     the read side overtakes the writer at the header→payload boundary, dropping the first
     payload byte and its tlast.

     Fix: count whole datagrams resident in the FIFO. A frame is "complete" once its
     tlast-bearing word is written; it leaves when that word is popped. [tx_frame_ready]
     holds the controller in Idle until a full frame is buffered (see tx_controller.ml),
     after which the drain reads only settled bytes and is identical to the pre-buffered
     case. The 4-bit counter tolerates a couple of queued frames; it never underflows
     because a frame is always written (inc) before it is drained (dec). *)
  let tx_spec : Reg_spec.t = Reg_spec.create ~clock:tx_clock ~clear:tx_reset () in
  let frame_wr_last =
    inputs.I.s_axis_tvalid &: inputs.I.s_axis_tlast &: ~:(tx_fifo.full)
  in
  let frame_rd_last = wire_tx_fifo_rd_en &: tx_fifo.rd_data.last in
  let frames_buffered =
    Signal.reg_fb tx_spec ~enable:vdd ~width:4 ~f:(fun c ->
      c +: uresize frame_wr_last ~width:4 -: uresize frame_rd_last ~width:4)
    -- "frames_buffered"
  in
  let tx_frame_ready = (frames_buffered <>:. 0) -- "tx_frame_ready" in
  (* TODO(magic-state-numbers): the FIFO read-enable below still compares tx_ctrl.state
     against a raw literal - [state ==:. 6] = Payload in the current Tx_controller
     encoding (0=Idle 1=Preamble 2=Sfd 3=Dst_mac 4=Src_mac 5=Eth_type 6=Payload 7=Fcs) -
     and breaks silently if the FSM states are reordered or renamed. The CRC and
     serializer gating no longer does: the controller states the CRC window itself in
     [crc_en] and "a frame is in flight" in [tx_busy]. When the _intf.ml unification
     lands, a named Payload predicate (or a decoded one-hot) retires this last literal
     too. *)
  let tx_ctrl =
    Tx_controller.create
      scope
      { Tx_controller.I.clock = tx_clock
      ; reset = tx_reset
      ; en
      ; start = inputs.I.tx_start
      ; fifo_empty = ~:(tx_fifo.rd_valid)
      ; frame_ready = tx_frame_ready
      ; dis_ready = wire_dis_ready
      ; payload_last = tx_fifo.rd_data.last
      }
  in
  (* Pop FIFO and advance controller only when the serializer can accept the next byte —
     but NOT while padding: pad bytes are synthesised, not popped, so the tlast-bearing
     word stays consumed exactly once. *)
  (* state 6 = Payload (see magic-state-numbers TODO above) *)
  Signal.(
    wire_tx_fifo_rd_en <-- (tx_ctrl.state ==:. 6 &: wire_dis_ready &: ~:(tx_ctrl.pad)));
  let tx_dp =
    Tx_datapath.create
      ~ethertype
      scope
      { Tx_datapath.I.clock = tx_clock
      ; reset = tx_reset
      ; en
      ; s_axis_tdata = tx_fifo.rd_data.data
      ; s_axis_tvalid = tx_fifo.rd_valid
      ; s_axis_tuser = inputs.I.s_axis_tuser
      ; fcs_byte = wire_fcs_byte
      ; byte_mux_sel = tx_ctrl.byte_mux_sel
      ; mac_byte_sel = tx_ctrl.mac_byte_sel
      ; pad = tx_ctrl.pad
      }
  in
  let tx_ser =
    Tx_byte_disassembler.create
      scope
      { Tx_byte_disassembler.I.clock = tx_clock
      ; reset = tx_reset
      ; en
      ; byte_in = tx_dp.byte_out
      ; byte_in_valid = tx_ctrl.tx_busy
      }
  in
  Signal.(wire_dis_ready <-- tx_ser.ready);
  (* CRC accumulates dst_mac/src_mac/eth_type/payload, which is what tx_ctrl.crc_en marks,
     gated on dis_ready so each byte is counted exactly once. byte_sel drives the FCS byte
     mux in Fcs state. NOTE: the Tx_crc [en] input below is NOT the module's global [en] —
     it is locally tied to tx_busy. That doubles as the CRC's inter-frame reset: Tx_crc
     reloads its accumulator with 0xFFFFFFFF whenever [reset |: ~:en] holds, so en falling
     in Idle is what clears it between frames. Do NOT drive it from crc_en instead: crc_en
     goes low for the whole Fcs state, which would reload the accumulator while its own
     result is being read out. Same reasoning for the serializer's byte_in_valid. *)
  let tx_crc_inst =
    Tx_crc.create
      scope
      { Tx_crc.I.clock = tx_clock
      ; reset = tx_reset
      ; en = tx_ctrl.tx_busy
      ; data = tx_dp.byte_out
      ; data_valid = wire_dis_ready &: tx_ctrl.crc_en
      ; byte_sel = Signal.select tx_ctrl.mac_byte_sel ~high:1 ~low:0
      }
  in
  Signal.(wire_fcs_byte <-- tx_crc_inst.fcs_byte);
  (* can i map this to a function that lets me auto-bind the keep functionality? *)
  let keep =
    reduce ~f:( |: ) (bits_lsb datapath_inst.keep @ bits_lsb controller_inst.keep)
  in
  (* ── RX start-of-frame qualifier (read/tx_clock side, aligned to m_axis) ── A sideband
     like tlast/tuser: asserted alongside the first byte of a frame and sampled by the
     consumer on its transfer (tvalid & tready & tfirst = SOF). frame_active tracks
     "mid-frame" across real AXI transfers — set on any transfer, cleared on the tlast
     transfer — so tfirst is high from the moment the first byte is presented until that
     byte is actually consumed. A 1-byte frame both starts and (via tlast) ends on the
     same transfer, leaving frame_active clear for the next frame. *)
  let rx_transfer = rx_fifo.valid &: inputs.I.m_axis_tready in
  let frame_active =
    Signal.reg_fb tx_spec ~enable:vdd ~width:1 ~f:(fun cur ->
      mux2 rx_transfer (mux2 rx_rd_word.last gnd vdd) cur)
    -- "rx_frame_active"
  in
  let m_axis_tfirst = (rx_fifo.valid &: ~:frame_active) -- "m_axis_tfirst" in
  (* old non-fifo interface -> use param to emulate generate block *)
  (* { *)
  (* m_axis_tdata = datapath_inst.payload_out; *)
  (* m_axis_tuser = Signal.gnd; *)
  (* m_axis_tvalid = datapath_inst.payload_out_valid; *)
  (* m_axis_tlast = Signal.gnd; *)
  (* m_axis_tkeep = Signal.gnd; *)
  (* *)
  (* keep = Signal.zero 1; *)
  (* } *)
  (* *)
  { m_axis_tdata = rx_rd_word.data
  ; m_axis_tvalid = rx_fifo.valid
  ; m_axis_tlast = rx_rd_word.last
  ; m_axis_tkeep = Signal.vdd
  ; (* single-byte MAC: tkeep is always 1, tied off here rather than crossed through the
       FIFO RAM *)
    m_axis_tuser = rx_rd_word.user
  ; m_axis_tfirst
  ; in_preamble = controller_inst.in_preamble
  ; in_dst_mac = controller_inst.in_dst_mac
  ; in_payload = controller_inst.in_payload
  ; frame_crc_ok = frame_ok
  ; frame_done = Signal.reg rx_spec frame_end_d
  ; rx_eth_type = datapath_inst.eth_type
  ; tx_d = tx_ser.tx_d
  ; tx_en = tx_ser.tx_en
  ; tx_busy = tx_ctrl.tx_busy
  ; s_axis_tready = ~:(tx_fifo.full)
  ; keep
  }
;;
