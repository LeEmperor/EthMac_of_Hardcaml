(*
  Module: Udp_mac_top_validation_harness
  Board-level validation harness for the UDP-over-MAC TX stack on the Arty A7-100T.

  Sibling of Mac_top_validation_harness — same purpose (throwaway silicon bring-up
  scaffolding, kept OUTSIDE lib/), same Arty_board_top.I/O pin contract, and it
  shares ALL the board plumbing (clocking, PHY reset, per-domain reset sync,
  heartbeat, RX drain, CDC) via Board_scaffolding. The ONLY things that differ
  from the MAC harness are:
    1. the core it wraps — Udp_mac_top (Udp_tx stacked on Mac_top) instead of the
       bare Mac_top, and
    2. the btn[3] stimulus FSM — it drives the UDP *application* interface
       (tx_start + payload_len + payload_tdata/tvalid, honouring payload_tready)
       instead of raw-filling the MAC TX FIFO. Udp_tx synthesizes the IPv4 + UDP
       headers; we only feed application-payload bytes.

  Controls: btn[0] = active-high reset, sw[0] = enable,
            btn[3] = one-shot "emit one UDP datagram".

  LED map (mirrors the MAC harness, plus udp_busy):
    led[3:0]  lower nibble of last drained RX byte
    led0_r    heartbeat toggle
    led1_r    tx_busy (MAC frame in flight)     led1_g  phy_ready
    led1_b    udp_busy (Udp_tx emitting)
    led2_r    RX AXI-S CRC error (tuser@tlast)  led2_g  last-frame CRC ok
    led2_b    in_payload (RX active)
    led3_r    last-frame CRC bad

  ETHERTYPE: Udp_mac_top drives Mac_top with ~ethertype:0x0800, so these are real
  IPv4/UDP frames a host kernel will dissect (Wireshark decodes them as UDP).
  validation/udp_app.py --validate still raw-sniffs and hand-parses the IPv4/UDP
  bytes — that keeps the check independent of kernel routing/socket state and lets
  it assert on every field — but a plain UDP-socket recvfrom would now also work.
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml   (* Udp_mac_top *)
open! Signal

let () = Stdio.print_endline "=== Imported UDP+MAC Validation Harness ==="

(* One-shot btn[3] TX trigger:
     Idle    wait for a debounced/edged btn[3] press
     Stream  drive the UDP app interface: hold tx_start until the first byte is
             accepted, then stream payload_tdata/tvalid for payload_len bytes,
             advancing on payload_tready
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

  (* ── Shared board plumbing (identical to the MAC harness) ─────────────────── *)
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

  (* ── One-shot btn[3]-triggered UDP datagram ───────────────────────────────── *)
  (* A single btn[3] press emits exactly one UDP datagram of [app_payload_len]
     application bytes (incrementing 0x01, 0x02, …). Udp_tx wraps them in the
     IPv4 + UDP headers; the MAC prepends Ethernet framing and appends the FCS
     (zero-padding the Ethernet payload to 46 bytes if the datagram is shorter).

     app_payload_len = 18 makes the Ethernet payload exactly 46 bytes
     (20 IPv4 + 8 UDP + 18 app), i.e. the minimum frame with NO MAC padding —
     the simplest case for first bring-up. Drop it below 18 to exercise the MAC
     zero-pad path (the host validator keys off the IPv4 total-length field, so
     it tolerates the trailing pad either way). *)
  let app_payload_len = 18 in
  let btn3 = Signal.bit i.I.btn ~pos:3 -- "btn3" in

  (* wire-back stubs break the FSM <-> Udp_mac_top instantiation cycle *)
  let wire_tready   = Signal.wire 1 -- "wire_tready"   in  (* payload_tready backpressure *)
  let wire_udp_busy = Signal.wire 1 -- "wire_udp_busy" in
  let wire_tx_busy  = Signal.wire 1 -- "wire_tx_busy"  in

  (* 2-FF sync btn[3] into the tx-clk domain, then take a one-shot rising edge *)
  let btn3_tx   = Signal.reg spec_tx (Signal.reg spec_tx btn3) -- "btn3_tx" in
  let btn3_edge = btn3_tx &: ~:(Signal.reg spec_tx btn3_tx) -- "btn3_edge" in
  (* phy_ready is a quasi-static level from the 100 MHz domain; 2-FF sync it *)
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
        (* present a valid app byte every cycle; hold tx_start high until the
           first byte is accepted (Udp_tx latches start + payload_len there) *)
        tx_tvalid <-- vdd;
        when_ (~:(started.value)) [ tx_tstart <-- vdd ];
        when_ (wire_tready) [
          started <-- vdd;
          if_ (ptr.value ==:. (app_payload_len - 1)) [
            sm.set_next Tx_trigger_states.Busy;   (* last app byte just accepted *)
          ] [
            ptr <-- ptr.value +:. 1;
          ];
        ];
      ];
      Tx_trigger_states.Busy, [
        (* wait for the whole datagram to drain out of Udp_tx AND off the MII pins
           before re-arming. udp_busy asserts at tx_start and holds through the
           header+payload push; tx_busy holds until wire transmission finishes —
           requiring both low avoids a premature re-trigger. *)
        when_ (~:wire_udp_busy &: ~:wire_tx_busy) [
          sm.set_next Tx_trigger_states.Idle;
        ];
      ];
    ];
  ]);

  (* ── RX drain: pop one byte per second, show low nibble on the plain LEDs ─── *)
  let rx_drain = Board_scaffolding.rx_drain ~scope ~clock:i.I.eth_tx_clk ~reset:tx_rst in

  (* ── The UDP-over-MAC stack ───────────────────────────────────────────────── *)
  let udp_inst = Udp_mac_top.create scope {
    Udp_mac_top.I.rx_clock = i.I.eth_rx_clk;
    rx_reset       = rx_rst;
    tx_clock       = i.I.eth_tx_clk;
    tx_reset       = tx_rst;
    en;
    rx_dv          = i.I.eth_rx_dv;
    rx_er          = i.I.eth_rxerr;
    rx_data        = i.I.eth_rxd;
    m_axis_tready  = rx_drain.pulse;
    tx_start       = tx_tstart.value;
    payload_len    = of_int_trunc ~width:16 app_payload_len;
    payload_tdata  = payload_byte;
    payload_tvalid = tx_tvalid.value;
  } in

  (* close the wire-back loop now that the stack exists *)
  Signal.(wire_tready   <-- udp_inst.payload_tready);
  Signal.(wire_udp_busy <-- udp_inst.udp_busy);
  Signal.(wire_tx_busy  <-- udp_inst.tx_busy);

  (* ── RX-status CDC into the tx_clk domain (see the MAC harness for rationale) ─ *)
  let spec_rx = Reg_spec.create ~clock:i.I.eth_rx_clk ~clear:rx_rst () in
  let sync2_tx x = Board_scaffolding.sync2 ~spec:spec_tx x in
  let pulse_sync_tx p = Board_scaffolding.pulse_sync ~src_spec:spec_rx ~dst_spec:spec_tx p in
  let frame_crc_ok_tx = sync2_tx     udp_inst.frame_crc_ok -- "frame_crc_ok_tx" in
  let in_payload_tx   = sync2_tx     udp_inst.in_payload   -- "in_payload_tx"   in
  let frame_done_tx   = pulse_sync_tx udp_inst.frame_done  -- "frame_done_tx"   in

  (* ── Register block (STUB) — reused verbatim from the MAC harness ─────────── *)
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
    tx_en         = udp_inst.tx_en;
    rx_dv         = i.I.eth_rx_dv;
  } in

  (* ── Last drained RX byte → led[3:0] ─────────────────────────────────────── *)
  let last_byte =
    Signal.reg spec_tx
      ~enable:(rx_drain.pulse &: udp_inst.m_axis_tvalid)
      udp_inst.m_axis_tdata
  in

  (* ── Consume the AXI-S sideband (tlast/tuser); anti-prunes the FIFO read bits
     exactly as in the MAC harness (Route 35-13 DPRA driverless-net fix). *)
  let rx_axis_crc_err =
    Signal.reg_fb spec_tx ~width:1
      ~enable:(rx_drain.pulse &: udp_inst.m_axis_tvalid &: udp_inst.m_axis_tlast)
      ~f:(fun _q -> udp_inst.m_axis_tuser)
    -- "rx_axis_crc_err"
  in

  (* keep debug OR-reductions alive so synthesis doesn't prune the submodules *)
  ignore heartbeat.keep;
  ignore rx_drain.keep;
  ignore regs_inst.keep;

  { O.
    led = Signal.select last_byte ~high:3 ~low:0;

    led0_r = heartbeat_toggle;
    led0_g = gnd;
    led0_b = gnd;

    led1_r = wire_tx_busy;   (* MAC TX frame in flight (btn[3]-triggered) *)
    led1_g = phy_ready;      (* PHY out of hard reset                     *)
    led1_b = wire_udp_busy;  (* Udp_tx emitting a datagram                *)

    led2_r = rx_axis_crc_err;  (* CRC error latched from AXI-S tuser@tlast (also anti-prunes FIFO sideband) *)
    led2_g = frame_crc_ok_tx;
    led2_b = in_payload_tx;

    led3_r = frame_done_tx &: ~:(frame_crc_ok_tx);
    led3_g = gnd;
    led3_b = gnd;

    uart_rxd_out = vdd;

    eth_mdc      = gnd;
    eth_rstn     = Signal.msb phy.cnt;
    eth_ref_clk  = clk_div_inst.dst_clk;
    eth_tx_en    = udp_inst.tx_en;
    eth_txd      = udp_inst.tx_d;
  }
;;
