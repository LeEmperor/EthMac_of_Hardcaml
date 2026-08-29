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
   (data+last+user) is packed into the flat [data_in] bus and unpacked on the read side,
   so tlast/tuser stay attached to their byte across the crossing. Depth 2^6 = 64: kept
   within a single distributed-RAM address range to avoid the Gray-pointer addressing
   glitches Async_fifo warns about above 2^LUT_SIZE. *)
module Rx_async_fifo = Hardcaml.Async_fifo.Make (struct
    let width = Rx_word.sum_of_port_widths
    let log2_depth = 6
    let optimize_for_same_clock_rate_and_always_reading = false
  end)

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
  let rx_clock = inputs.I.rx_clock in
  let rx_reset = inputs.I.rx_reset in
  let tx_clock = inputs.I.tx_clock in
  let tx_reset = inputs.I.tx_reset in
  let en = inputs.I.en in
  let rx_path =
    Mac_rx_path.hierarchical
      ~instance:"rx_path"
      scope
      { Mac_rx_path.I.clock_i = rx_clock
      ; reset_i = rx_reset
      ; en_i = en
      ; rx_dv_i = inputs.I.rx_dv
      ; rx_er_i = inputs.I.rx_er
      ; rx_data_i = inputs.I.rx_data
      }
  in
  let rx_wr_word =
    { Rx_word.data = rx_path.stream_data_o
    ; last = rx_path.stream_last_o
    ; user = rx_path.stream_user_o
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
    else
      let module H = Hierarchy.In_scope (Rx_async_fifo.I) (Rx_async_fifo.O) in
      H.hierarchical
        ~instance:"rx_async_fifo"
        ~scope
        ~name:"hardcaml_async_fifo"
        (fun child_scope child_i -> Rx_async_fifo.create ~scope:child_scope child_i)
        i
  in
  let rx_fifo =
    rx_fifo_impl
      { Rx_async_fifo.I.clock_write = rx_clock
      ; reset_write = rx_reset
      ; clock_read = tx_clock
      ; reset_read = tx_reset
      ; data_in = Rx_word.Of_signal.pack rx_wr_word
      ; write_enable = rx_path.stream_valid_o
      ; read_enable = inputs.m_axis_tready
      }
  in
  let rx_rd_word = Rx_word.Of_signal.unpack rx_fifo.data_out in
  let tx_path =
    Mac_tx_path.hierarchical
      ~instance:"tx_path"
      ~ethertype
      scope
      { Mac_tx_path.I.clock_i = tx_clock
      ; reset_i = tx_reset
      ; en_i = en
      ; s_axis_tdata_i = inputs.I.s_axis_tdata
      ; s_axis_tvalid_i = inputs.I.s_axis_tvalid
      ; s_axis_tlast_i = inputs.I.s_axis_tlast
      ; s_axis_tuser_i = inputs.I.s_axis_tuser
      ; tx_start_i = inputs.I.tx_start
      }
  in
  (* RX start-of-frame tracking belongs on the FIFO read side in the consumer clock
     domain, so it intentionally remains in this multi-clock composition circuit. *)
  let tx_spec : Reg_spec.t = Reg_spec.create ~clock:tx_clock ~clear:tx_reset () in
  let rx_transfer = rx_fifo.valid &: inputs.I.m_axis_tready in
  let frame_active =
    Signal.reg_fb tx_spec ~enable:vdd ~width:1 ~f:(fun cur ->
      mux2 rx_transfer (mux2 rx_rd_word.last gnd vdd) cur)
    -- "rx_frame_active"
  in
  let m_axis_tfirst = (rx_fifo.valid &: ~:frame_active) -- "m_axis_tfirst" in
  { m_axis_tdata = rx_rd_word.data
  ; m_axis_tvalid = rx_fifo.valid
  ; m_axis_tlast = rx_rd_word.last
  ; m_axis_tkeep = Signal.vdd
  ; (* single-byte MAC: tkeep is always 1, tied off here rather than crossed through the
       FIFO RAM *)
    m_axis_tuser = rx_rd_word.user
  ; m_axis_tfirst
  ; in_preamble = rx_path.in_preamble_o
  ; in_dst_mac = rx_path.in_dst_mac_o
  ; in_payload = rx_path.in_payload_o
  ; frame_crc_ok = rx_path.frame_crc_ok_o
  ; frame_done = rx_path.frame_done_o
  ; rx_eth_type = rx_path.rx_eth_type_o
  ; tx_d = tx_path.tx_d_o
  ; tx_en = tx_path.tx_en_o
  ; tx_busy = tx_path.tx_busy_o
  ; s_axis_tready = tx_path.s_axis_tready_o
  ; keep = rx_path.keep_o |: inputs.I.s_axis_tuser
  }
;;

let hierarchical ?instance ?(rx_fifo_for_sim = false) ?(ethertype = 0x9999) scope i =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ?instance ~scope ~name:"mac_top" (create ~rx_fifo_for_sim ~ethertype) i
;;
