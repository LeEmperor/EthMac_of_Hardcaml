(*
  Module: Udp_rx_mac_top_validation_harness
  Board-level validation harness for the UDP-over-MAC *receive* stack on the
  Arty A7-100T — the RX mirror of Udp_mac_top_validation_harness.

  Same purpose (throwaway silicon bring-up scaffolding, kept OUTSIDE lib/), same
  Arty_board_top.I/O pin contract, and it shares ALL the board plumbing (clocking,
  PHY reset, per-domain reset sync, heartbeat, CDC) via Board_scaffolding. The
  differences from the TX harness are exactly the ones called out in the RX
  handoff:
    1. it wraps Udp_rx_mac_top (Ipv4_rx + Udp_rx stacked on Mac_top's RX path,
       TX tied off) instead of Udp_mac_top, and
    2. there is NO TX stimulus FSM — nothing is transmitted. The board just
       receives host-sent UDP datagrams, parses them through the whole
       MAC -> IPv4 -> UDP chain, and shows the recovered payload + status on the
       LEDs. The MII TX pins are held idle.

  Controls: btn[0] = active-high reset, sw[0] = enable.

  LED map:
    led[3:0]  low nibble of the currently-drained UDP application byte
              (steps once per second through the recovered payload)
    led0_r    heartbeat toggle (0.5 Hz — "fabric alive")
    led0_g    saw_valid_datagram — held once any UDP payload SOF is seen
    led1_g    phy_ready (PHY out of hard reset)
    led1_b    udp_busy (RX parser mid-datagram)
    led2_r    crc_error  (held bad-FCS verdict, padding-safe)
    led2_g    checksum_ok (IPv4 header checksum verified)
    led2_b    in_payload  (MAC RX actively receiving — CDC'd to tx_clock)
    led3_r    crc_error   (mirror of led2_r for an eye-catching "bad frame")

  The recovered UDP payload is drained ONE BYTE PER SECOND (app_tready gated by a
  slow Second_pulse), the UDP mirror of the MAC harness's FIFO-drain trick:
  gating app_tready backpressures the whole Udp_rx -> Ipv4_rx -> MAC chain, so the
  datagram parks in the MAC's async RX FIFO and its application bytes walk out
  slowly onto led[3:0]. Send an alternating 0xAA/0x55 host payload and led[3:0]
  toggles 0xA <-> 0x5 as each recovered byte is popped — a direct eyeball check
  that every payload byte transits the L2->L3->L4 chain intact.

  crc_error / checksum_ok / rx_frame_done / app_* are all already in the tx_clock
  domain (the async RX FIFO presents m_axis there); only the MAC RX status levels
  (frame_crc_ok / in_payload / frame_done) live in rx_clock and are CDC'd, same as
  the TX harness.
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml   (* Udp_rx_mac_top *)
open! Signal

let () = Stdio.print_endline "=== Imported UDP RX + MAC Validation Harness ==="

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

  (* ── Shared board plumbing (identical to the TX harness) ──────────────────── *)
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

  (* ── RX drain: pop one recovered UDP byte per second (mirrors the MAC harness).
     Gating app_tready with this pulse backpressures the whole IPv4/UDP chain, so
     the datagram buffers in the MAC's async RX FIFO and its application bytes are
     released one-per-second — eye-visible on led[3:0]. *)
  let rx_drain = Board_scaffolding.rx_drain ~scope ~clock:i.I.eth_tx_clk ~reset:tx_rst in

  (* ── The UDP-over-MAC *receive* stack ─────────────────────────────────────── *)
  (* RX-only top: the MAC TX sink is tied off inside Udp_rx_mac_top. app_tready is
     the slow drain pulse, so the recovered payload steps out one byte/second.
     ~rx_fifo_for_sim:false selects the real async RX FIFO for the board. *)
  let udp_inst =
    Udp_rx_mac_top.create ~rx_fifo_for_sim:false scope {
      Udp_rx_mac_top.I.rx_clock = i.I.eth_rx_clk;
      rx_reset   = rx_rst;
      tx_clock   = i.I.eth_tx_clk;
      tx_reset   = tx_rst;
      en;
      rx_dv      = i.I.eth_rx_dv;
      rx_er      = i.I.eth_rxerr;
      rx_data    = i.I.eth_rxd;
      app_tready = rx_drain.pulse;
    }
  in

  (* ── Each drained recovered UDP app byte → led[3:0] ───────────────────────────
     Latch on the drain handshake (pulse & tvalid), exactly as the MAC harness
     latches its FIFO byte — so an alternating 0xAA/0x55 payload shows led[3:0]
     stepping 0xA <-> 0x5. *)
  let last_byte =
    Signal.reg spec_tx
      ~enable:(rx_drain.pulse &: udp_inst.app_tvalid)
      udp_inst.app_tdata
  in

  (* ── "Saw a valid datagram" — held once any UDP payload SOF is observed ────── *)
  let saw_valid_datagram =
    Signal.reg_fb spec_tx ~width:1 ~enable:vdd
      ~f:(fun q -> q |: udp_inst.app_start)
    -- "saw_valid_datagram"
  in

  (* ── Consume the app-stream sidebands (Route 35-13 DPRA driverless-net fix) ──
     Latch the app-visible bad-frame verdict at rx_frame_done so it survives, and
     fold app_tlast in too so synthesis can't prune the recovered-stream bits. *)
  let bad_frame_latched =
    Signal.reg_fb spec_tx ~width:1
      ~enable:udp_inst.rx_frame_done
      ~f:(fun _q -> udp_inst.crc_error |: udp_inst.app_tlast)
    -- "bad_frame_latched"
  in

  (* ── MAC RX-status CDC into the tx_clk domain (same rationale as the TX harness):
     frame_crc_ok / in_payload / frame_done are captured in rx_clock. ──────────── *)
  let spec_rx = Reg_spec.create ~clock:i.I.eth_rx_clk ~clear:rx_rst () in
  let sync2_tx x = Board_scaffolding.sync2 ~spec:spec_tx x in
  let pulse_sync_tx p = Board_scaffolding.pulse_sync ~src_spec:spec_rx ~dst_spec:spec_tx p in
  let frame_crc_ok_tx = sync2_tx      udp_inst.frame_crc_ok -- "frame_crc_ok_tx" in
  let in_payload_tx   = sync2_tx      udp_inst.in_payload   -- "in_payload_tx"   in
  let frame_done_tx   = pulse_sync_tx udp_inst.frame_done   -- "frame_done_tx"   in

  (* ── Register block (STUB) — reused verbatim from the TX harness, AXI-Lite tied
     off. TX is idle here so tx_en = gnd. Consumes the CDC'd MAC RX status. ────── *)
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
    tx_en         = gnd;            (* RX-only board: MII TX is idle *)
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
    led0_g = saw_valid_datagram;
    led0_b = gnd;

    led1_r = gnd;                  (* no TX activity on an RX-only board *)
    led1_g = phy_ready;            (* PHY out of hard reset              *)
    led1_b = udp_inst.udp_busy;    (* RX parser mid-datagram             *)

    led2_r = udp_inst.crc_error;   (* held bad-FCS verdict (padding-safe) *)
    led2_g = udp_inst.checksum_ok; (* IPv4 header checksum verified        *)
    led2_b = in_payload_tx;        (* MAC RX actively receiving            *)

    led3_r = udp_inst.crc_error;   (* mirror: eye-catching "bad frame"     *)
    led3_g = gnd;
    led3_b = gnd;

    uart_rxd_out = vdd;

    eth_mdc      = gnd;
    eth_rstn     = Signal.msb phy.cnt;
    eth_ref_clk  = clk_div_inst.dst_clk;
    (* RX-only board: hold the MII TX pins idle. *)
    eth_tx_en    = gnd;
    eth_txd      = zero 4;
  }
;;
