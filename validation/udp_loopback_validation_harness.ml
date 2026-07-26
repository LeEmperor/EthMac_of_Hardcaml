(*
  Module: Udp_loopback_validation_harness
  Board-level validation harness for the echo/loopback UDP-over-MAC top (Harness #2)
  on the Arty A7-100T.

  Same throwaway bring-up scaffolding as its siblings — same Arty_board_top.I/O pin
  contract, all board plumbing (clocking, PHY reset, per-domain reset sync,
  heartbeat, CDC) shared via Board_scaffolding. What differs:
    1. it wraps Udp_loopback_mac_top (both stacks + the RX->TX bridge FSM on ONE
       Mac_top), and
    2. there is NO TX stimulus FSM — like the RX harness the board just receives,
       but the recovered datagram is fed straight back out through the bridge, so
       the board TRANSMITS the echo on the MII TX pins (unlike the RX harness,
       which held them idle). No app_tready knob either: the bridge drives RX
       backpressure internally.

  This makes RX validation host-asserted: the host sends one datagram and asserts
  on the echo (udp_app.py --echo), instead of eyeballing the slow LED drain.

  Controls: btn[0] = active-high reset, sw[0] = enable.

  LED map:
    led[3:0]  low nibble of the last recovered UDP application byte
    led0_r    heartbeat toggle (0.5 Hz — "fabric alive")
    led0_g    saw_valid_datagram — held once any UDP payload SOF is seen
    led1_r    tx_busy       (echo frame in flight on the MII TX pins)
    led1_g    phy_ready     (PHY out of hard reset)
    led1_b    bridge_active (RX->TX bridge mid-echo)
    led2_r    crc_error     (held bad-FCS verdict, padding-safe)
    led2_g    checksum_ok   (IPv4 header checksum verified)
    led2_b    in_payload    (MAC RX actively receiving — CDC'd to tx_clock)
    led3_r    crc_error     (mirror — eye-catching "bad frame")
    led3_b    rx_udp_busy   (Udp_rx parser mid-datagram)

  crc_error / checksum_ok / rx_frame_done / app_* / tx_* / bridge_active are all in
  the tx_clock domain already; only the MAC RX status levels (frame_crc_ok /
  in_payload / frame_done) live in rx_clock and are CDC'd, same as the siblings.
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml   (* Udp_loopback_mac_top *)
open! Signal

let () = Stdio.print_endline "=== Imported UDP Loopback + MAC Validation Harness ==="

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

  (* ── Shared board plumbing (identical to the sibling harnesses) ───────────── *)
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

  (* ── The echo/loopback UDP-over-MAC top ───────────────────────────────────── *)
  (* No app_tready input: the bridge FSM pulls the recovered stream and drives it
     back into the TX interface internally. ~rx_fifo_for_sim:false selects the real
     async RX FIFO for the board. *)
  let udp_inst =
    Udp_loopback_mac_top.create ~rx_fifo_for_sim:false scope {
      Udp_loopback_mac_top.I.rx_clock = i.I.eth_rx_clk;
      rx_reset = rx_rst;
      tx_clock = i.I.eth_tx_clk;
      tx_reset = tx_rst;
      en;
      rx_dv    = i.I.eth_rx_dv;
      rx_er    = i.I.eth_rxerr;
      rx_data  = i.I.eth_rxd;
    }
  in

  (* ── Last recovered UDP app byte → led[3:0] (latch as bytes flow through) ──── *)
  let last_byte =
    Signal.reg spec_tx ~enable:udp_inst.app_tvalid udp_inst.app_tdata
  in

  (* ── "Saw a valid datagram" — held once any UDP payload SOF is observed ────── *)
  let saw_valid_datagram =
    Signal.reg_fb spec_tx ~width:1 ~enable:vdd
      ~f:(fun q -> q |: udp_inst.app_start)
    -- "saw_valid_datagram"
  in

  (* ── Consume the app-stream sidebands (Route 35-13 DPRA driverless-net fix) ──
     Latch the app-visible bad-frame verdict at rx_frame_done and fold app_tlast in
     so synthesis can't prune the recovered-stream bits. *)
  let bad_frame_latched =
    Signal.reg_fb spec_tx ~width:1
      ~enable:udp_inst.rx_frame_done
      ~f:(fun _q -> udp_inst.crc_error |: udp_inst.app_tlast)
    -- "bad_frame_latched"
  in

  (* ── MAC RX-status CDC into the tx_clk domain (same rationale as the siblings) ─ *)
  let spec_rx = Reg_spec.create ~clock:i.I.eth_rx_clk ~clear:rx_rst () in
  let sync2_tx x = Board_scaffolding.sync2 ~spec:spec_tx x in
  let pulse_sync_tx p = Board_scaffolding.pulse_sync ~src_spec:spec_rx ~dst_spec:spec_tx p in
  let frame_crc_ok_tx = sync2_tx      udp_inst.frame_crc_ok -- "frame_crc_ok_tx" in
  let in_payload_tx   = sync2_tx      udp_inst.in_payload   -- "in_payload_tx"   in
  let frame_done_tx   = pulse_sync_tx udp_inst.frame_done   -- "frame_done_tx"   in

  (* ── Register block (STUB) — reused verbatim from the siblings, AXI-Lite tied
     off. This board DOES transmit (the echo), so tx_en carries the real value. ── *)
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
    tx_en         = udp_inst.tx_en;   (* the echo drives the MII TX pins *)
    rx_dv         = i.I.eth_rx_dv;
  } in

  (* keep debug OR-reductions alive so synthesis doesn't prune the submodules *)
  ignore heartbeat.keep;
  ignore regs_inst.keep;
  ignore bad_frame_latched;

  { O.
    led = Signal.select last_byte ~high:3 ~low:0;

    led0_r = heartbeat_toggle;
    led0_g = saw_valid_datagram;
    led0_b = gnd;

    led1_r = udp_inst.tx_busy;       (* echo frame in flight on the MII pins *)
    led1_g = phy_ready;              (* PHY out of hard reset                *)
    led1_b = udp_inst.bridge_active; (* RX->TX bridge mid-echo               *)

    led2_r = udp_inst.crc_error;     (* held bad-FCS verdict (padding-safe)  *)
    led2_g = udp_inst.checksum_ok;   (* IPv4 header checksum verified         *)
    led2_b = in_payload_tx;          (* MAC RX actively receiving             *)

    led3_r = udp_inst.crc_error;     (* mirror: eye-catching "bad frame"      *)
    led3_g = gnd;
    led3_b = udp_inst.rx_udp_busy;   (* Udp_rx parser mid-datagram            *)

    uart_rxd_out = vdd;

    eth_mdc      = gnd;
    eth_rstn     = Signal.msb phy.cnt;
    eth_ref_clk  = clk_div_inst.dst_clk;
    (* the echo: drive the MII TX pins from the loopback top *)
    eth_tx_en    = udp_inst.tx_en;
    eth_txd      = udp_inst.tx_d;
  }
;;
