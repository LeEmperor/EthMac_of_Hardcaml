(*
  Module: Udp_duplex_validation_harness
  Board-level validation harness for the FULL-DUPLEX UDP-over-MAC stack on the
  Arty A7-100T — Harness #1 (adjacent-independent) from
  UDP_FULL_DUPLEX_HARNESS_PLAN.md.

  This is a straight MERGE of the two single-direction harnesses' bodies around the
  single-Mac_top [Udp_duplex_mac_top]:
    - TX side: the btn[3] one-shot [Tx_trigger_states] FSM from
      [Udp_mac_top_validation_harness] — one press emits one UDP datagram
      (fpga -> laptop), validated host-side by `udp_app.py --validate`.
    - RX side: the 1-byte/sec [rx_drain] on [app_tready] from
      [Udp_rx_mac_top_validation_harness] — a host-sent datagram (laptop -> fpga)
      is parsed and its recovered payload walks out led[3:0] one byte/second.
      Send `--pattern alt` (0xAA/0x55) and led[3:0] toggles 0xA <-> 0x5.
  The two directions are INDEPENDENT — there is no RX->TX coupling here (that is
  Harness #2's bridge). All the board plumbing is shared verbatim via
  [Board_scaffolding], and the AXI-Lite reg block is tied off as in both siblings.

  Controls: btn[0] = active-high reset, sw[0] = enable,
            btn[3] = one-shot "emit one UDP datagram" (TX).

  LED map (combined — TX status on led1, RX status on led0/led2/led3):
    led[3:0]  low nibble of last drained recovered-UDP app byte (RX, ~1/sec)
    led0_r    heartbeat toggle              led0_g  saw_valid_datagram (RX SOF, held)
    led1_r    tx_busy (TX frame in air)     led1_g  phy_ready       led1_b  tx_udp_busy
    led2_r    crc_error (RX, held)          led2_g  checksum_ok (RX IPv4)  led2_b  in_payload
    led3_r    crc_error mirror              led3_b  rx_udp_busy (RX parser mid-datagram)
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml   (* Udp_duplex_mac_top *)
open! Signal

let () = Stdio.print_endline "=== Imported UDP Duplex Validation Harness ==="

(* One-shot btn[3] TX trigger — identical to the TX harness:
     Idle    wait for a debounced/edged btn[3] press
     Stream  hold tx_start until the first byte is accepted, then stream
             payload_tdata/tvalid for payload_len bytes, advancing on payload_tready
     Busy    hold until both Udp_tx and the MAC report idle, then re-arm *)
module Tx_trigger_states = struct
  type t = Idle | Stream | Busy
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* Reuse the canonical board pin contract as the harness port interface. *)
module I = Arty_board_top.I
module O = Arty_board_top.O

let create
  (scope : Scope.t)
  (i : _ I.t)
  : _ O.t
  =
  let rst = Signal.bit i.I.btn ~pos:0 -- "rst" in  (* btn[0]: raw async reset button *)
  let en  = Signal.bit i.I.sw  ~pos:0 -- "en"  in  (* sw[0]:  active-high enable      *)

  (* ── Shared board plumbing (identical to both single-direction harnesses) ─── *)
  let sys_rst = Board_scaffolding.reset_sync ~clock:i.I.clk100mhz ~async_rst:rst -- "sys_rst" in
  let rx_rst  = Board_scaffolding.reset_sync ~clock:i.I.eth_rx_clk ~async_rst:rst -- "rx_rst"  in
  let tx_rst  = Board_scaffolding.reset_sync ~clock:i.I.eth_tx_clk ~async_rst:rst -- "tx_rst"  in

  let spec100 = Reg_spec.create ~clock:i.I.clk100mhz ~clear:sys_rst () in
  let spec_tx = Reg_spec.create ~clock:i.I.eth_tx_clk ~clear:tx_rst () in

  let clk_div_inst = Board_scaffolding.eth_ref_clk ~scope ~clk100mhz:i.I.clk100mhz ~sys_rst ~en in
  let phy = Board_scaffolding.phy_hard_reset ~spec100 ~sys_rst in
  let phy_ready = phy.ready in
  let heartbeat = Board_scaffolding.heartbeat ~scope ~clk100mhz:i.I.clk100mhz ~sys_rst ~spec100 in
  let heartbeat_toggle = heartbeat.toggle in

  (* ── TX: one-shot btn[3]-triggered UDP datagram (from the TX harness) ─────── *)
  (* app_payload_len = 18 => Ethernet payload exactly 46 bytes (no MAC padding). *)
  let app_payload_len = 18 in
  let btn3 = Signal.bit i.I.btn ~pos:3 -- "btn3" in

  (* wire-back stubs break the FSM <-> Udp_duplex_mac_top instantiation cycle *)
  let wire_tready   = Signal.wire 1 -- "wire_tready"   in  (* payload_tready backpressure *)
  let wire_udp_busy = Signal.wire 1 -- "wire_udp_busy" in  (* tx_udp_busy *)
  let wire_tx_busy  = Signal.wire 1 -- "wire_tx_busy"  in

  let btn3_tx   = Signal.reg spec_tx (Signal.reg spec_tx btn3) -- "btn3_tx" in
  let btn3_edge = btn3_tx &: ~:(Signal.reg spec_tx btn3_tx) -- "btn3_edge" in
  let phy_ready_tx =
    Signal.reg spec_tx (Signal.reg spec_tx phy_ready) -- "dbg_phy_ready_tx"
  in

  let sm = Always.State_machine.create (module Tx_trigger_states) ~enable:vdd spec_tx in
  let ptr     = Always.Variable.reg ~enable:vdd ~width:11 spec_tx in  (* app-byte index *)
  let started = Always.Variable.reg ~enable:vdd ~width:1  spec_tx in  (* first byte accepted yet? *)
  let tx_tstart  = Always.Variable.wire ~default:gnd () in
  let tx_tvalid  = Always.Variable.wire ~default:gnd () in

  (* payload bytes: incrementing 0x01, 0x02, … keyed off the accepted-byte index *)
  let payload_byte = uresize (ptr.value +:. 1) ~width:8 -- "payload_byte" in

  Always.(compile [
    sm.switch [
      Tx_trigger_states.Idle, [
        when_ (btn3_edge &: phy_ready_tx) [
          ptr <--. 0;
          started <--. 0;
          sm.set_next Tx_trigger_states.Stream;
        ];
      ];
      Tx_trigger_states.Stream, [
        tx_tvalid <-- vdd;
        when_ (~:(started.value)) [ tx_tstart <-- vdd ];
        when_ (wire_tready) [
          started <-- vdd;
          if_ (ptr.value ==:. (app_payload_len - 1)) [
            sm.set_next Tx_trigger_states.Busy;
          ] [
            ptr <-- ptr.value +:. 1;
          ];
        ];
      ];
      Tx_trigger_states.Busy, [
        when_ (~:wire_udp_busy &: ~:wire_tx_busy) [
          sm.set_next Tx_trigger_states.Idle;
        ];
      ];
    ];
  ]);

  (* ── RX drain: pop one recovered UDP byte per second (from the RX harness).
     Gating app_tready backpressures the whole IPv4/UDP RX chain, so the datagram
     buffers in the MAC's async RX FIFO and its bytes walk out onto led[3:0]. *)
  let rx_drain = Board_scaffolding.rx_drain ~scope ~clock:i.I.eth_tx_clk ~reset:tx_rst in

  (* ── The full-duplex UDP-over-MAC stack (one Mac_top, both directions) ────── *)
  let udp_inst =
    Udp_duplex_mac_top.create ~rx_fifo_for_sim:false scope {
      Udp_duplex_mac_top.I.rx_clock = i.I.eth_rx_clk;
      rx_reset       = rx_rst;
      tx_clock       = i.I.eth_tx_clk;
      tx_reset       = tx_rst;
      en;
      rx_dv          = i.I.eth_rx_dv;
      rx_er          = i.I.eth_rxerr;
      rx_data        = i.I.eth_rxd;
      (* TX app interface — btn[3] FSM *)
      tx_start       = tx_tstart.value;
      payload_len    = of_int_trunc ~width:16 app_payload_len;
      payload_tdata  = payload_byte;
      payload_tvalid = tx_tvalid.value;
      (* RX recovered-payload backpressure — slow drain *)
      app_tready     = rx_drain.pulse;
    }
  in

  (* close the TX wire-back loop now that the stack exists *)
  Signal.(wire_tready   <-- udp_inst.payload_tready);
  Signal.(wire_udp_busy <-- udp_inst.tx_udp_busy);
  Signal.(wire_tx_busy  <-- udp_inst.tx_busy);

  (* ── Each drained recovered UDP app byte → led[3:0] (RX) ──────────────────── *)
  let last_byte =
    Signal.reg spec_tx
      ~enable:(rx_drain.pulse &: udp_inst.app_tvalid)
      udp_inst.app_tdata
  in

  (* ── "Saw a valid datagram" — held once any UDP payload SOF is observed (RX) ─ *)
  let saw_valid_datagram =
    Signal.reg_fb spec_tx ~width:1 ~enable:vdd
      ~f:(fun q -> q |: udp_inst.app_start)
    -- "saw_valid_datagram"
  in

  (* ── Consume the RX app-stream sidebands (Route 35-13 DPRA driverless-net fix):
     latch crc_error|app_tlast at rx_frame_done so the recovered-stream read bits
     stay connected to a real pin and can't be pruned. *)
  let bad_frame_latched =
    Signal.reg_fb spec_tx ~width:1
      ~enable:udp_inst.rx_frame_done
      ~f:(fun _q -> udp_inst.crc_error |: udp_inst.app_tlast)
    -- "bad_frame_latched"
  in

  (* ── MAC RX-status CDC into the tx_clk domain (same as both siblings) ─────── *)
  let spec_rx = Reg_spec.create ~clock:i.I.eth_rx_clk ~clear:rx_rst () in
  let sync2_tx x = Board_scaffolding.sync2 ~spec:spec_tx x in
  let pulse_sync_tx p = Board_scaffolding.pulse_sync ~src_spec:spec_rx ~dst_spec:spec_tx p in
  let frame_crc_ok_tx = sync2_tx      udp_inst.frame_crc_ok -- "frame_crc_ok_tx" in
  let in_payload_tx   = sync2_tx      udp_inst.in_payload   -- "in_payload_tx"   in
  let frame_done_tx   = pulse_sync_tx udp_inst.frame_done   -- "frame_done_tx"   in

  (* ── Register block (STUB) — reused verbatim; taps the CDC'd MAC RX status ─── *)
  let regs_inst = Mac_top_validation_harness_regs.create scope {
    Mac_top_validation_harness_regs.I.clock = i.I.eth_tx_clk;
    reset         = tx_rst;
    s_axi_awaddr  = zero 4;
    s_axi_awvalid = gnd;
    s_axi_wdata   = zero 32;
    s_axi_wstrb   = zero 4;
    s_axi_wvalid  = gnd;
    s_axi_bready  = gnd;
    s_axi_araddr  = zero 4;
    s_axi_arvalid = gnd;
    s_axi_rready  = gnd;
    frame_done    = frame_done_tx;
    frame_crc_ok  = frame_crc_ok_tx;
    in_payload    = in_payload_tx;
    tx_en         = udp_inst.tx_en;   (* TX is live on this board *)
    rx_dv         = i.I.eth_rx_dv;
  } in

  (* keep debug OR-reductions alive so synthesis doesn't prune the submodules *)
  ignore heartbeat.keep;
  ignore rx_drain.keep;
  ignore regs_inst.keep;
  ignore bad_frame_latched;

  { O.
    led = Signal.select last_byte ~high:3 ~low:0;

    led0_r = heartbeat_toggle;
    led0_g = saw_valid_datagram;   (* RX: saw a UDP payload SOF *)
    led0_b = gnd;

    led1_r = wire_tx_busy;         (* TX: MAC frame in flight (btn[3]) *)
    led1_g = phy_ready;            (* PHY out of hard reset            *)
    led1_b = wire_udp_busy;        (* TX: Udp_tx emitting a datagram   *)

    led2_r = udp_inst.crc_error;   (* RX: held bad-FCS verdict (padding-safe) *)
    led2_g = udp_inst.checksum_ok; (* RX: IPv4 header checksum verified        *)
    led2_b = in_payload_tx;        (* RX: MAC actively receiving               *)

    led3_r = udp_inst.crc_error;   (* RX: mirror — eye-catching "bad frame"    *)
    led3_g = gnd;
    led3_b = udp_inst.rx_udp_busy; (* RX: Udp_rx parser mid-datagram           *)

    uart_rxd_out = vdd;

    eth_mdc      = gnd;
    eth_rstn     = Signal.msb phy.cnt;
    eth_ref_clk  = clk_div_inst.dst_clk;
    (* full-duplex board: MII TX pins are driven by the TX stack *)
    eth_tx_en    = udp_inst.tx_en;
    eth_txd      = udp_inst.tx_d;
  }
;;
