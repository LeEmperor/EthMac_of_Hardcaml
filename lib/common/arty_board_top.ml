(* Module: Arty_board_top Arty A7-100T board toplevel — physical-pin interface only.

   Canonical pin contract for the board. This is the *reference* copy: the validation
   harness reuses Arty_board_top.I/O as its port interface and supplies its own [create].
   Keep the pin map here single-source.

   Every port in I/O maps 1:1 to a real IOB pin and has an XDC constraint. XDC pin
   locations are noted inline for each signal.

   ===== Bidirectional pins ===== Pmods JA-JD, ChipKit digital/analog headers, ChipKit
   I2C, MDIO, and QSPI DQ are all bidirectional. They are NOT included here because
   Hardcaml I/O modules only represent unidirectional ports; inout requires Xilinx IOBUF
   primitives. Add them by instantiating IOBUF via Hardcaml's Instantiation module, or in
   a thin Verilog wrapper around the generated RTL.

   ===== eth_ref_clk ===== Drives the PHY's reference clock input. Must be routed through
   an ODDR primitive clocked by a 25 MHz MMCM output — do not drive directly from a fabric
   register. NOTE: You can drive from fabric clk division, but do so at your own risk.
   Roughly calculated for a clk with 2ns of drift, you should be able to divide down
   200Mhz without too much issue.

   ===== Power-measurement / XADC pins ===== vsnsvu, vsns5v0, isns5v0, isns0v95 connect to
   the on-chip XADC. Instantiate the XADC primitive directly; do not add these as
   fabric-level inputs here.
*)

open! Core
open! Hardcaml
open! Signal

(* ── Inputs: signals driven into the FPGA from the board ──────────────────── *)
module I = struct
  type 'a t =
    { (* Clock *)
      clk100mhz : 'a
        (* E3 100 MHz system clock *)
        (* this is the main system clk *)
    ; (* Slide switches *)
      sw : 'a [@bits 4] (* A8 C11 C10 A10 *)
    ; (* Push buttons *)
      btn : 'a [@bits 4] (* D9 C9 B9 B8 *)
    ; (* USB-UART (host → FPGA) *)
      uart_txd_in : 'a (* A9 *)
    ; (* Ethernet PHY — MII RX + shared *)
      eth_col : 'a (* D17 collision detect *)
    ; eth_crs : 'a (* G14 carrier sense *)
    ; eth_rx_dv : 'a (* G16 RX data valid *)
    ; eth_rxd : 'a [@bits 4] (* D18 E17 E18 G17 *)
    ; eth_rxerr : 'a (* C17 RX error *)
    ; eth_tx_clk : 'a (* H16 TX clock from PHY *)
    ; eth_rx_clk : 'a (* F15 RX clock from PHY *)
    }
  [@@deriving hardcaml]
end

(* ── Outputs: signals driven out of the FPGA to the board ─────────────────── *)
module O = struct
  type 'a t =
    { (* Plain LEDs (led[4..7] on schematic) *)
      led : 'a [@bits 4] (* H5 J5 T9 T10 *)
    ; (* RGB LEDs *)
      led0_r : 'a
    ; led0_g : 'a
    ; led0_b : 'a (* G6 F6 E1 *)
    ; led1_r : 'a
    ; led1_g : 'a
    ; led1_b : 'a (* G3 J4 G4 *)
    ; led2_r : 'a
    ; led2_g : 'a
    ; led2_b : 'a (* J3 J2 H4 *)
    ; led3_r : 'a
    ; led3_g : 'a
    ; led3_b : 'a (* K1 H6 K2 *)
    ; (* USB-UART (FPGA → host) *)
      uart_rxd_out : 'a (* D10 *)
    ; (* Ethernet PHY — MII TX + control *)
      eth_mdc : 'a (* F16 MDC clock output *)
    ; eth_rstn : 'a (* C16 PHY reset, active-low *)
    ; eth_ref_clk : 'a (* G18 ref clock — use ODDR *)
    ; eth_tx_en : 'a (* H15 TX enable *)
    ; eth_txd : 'a [@bits 4] (* H14 J14 J13 H17 *)
    }
  [@@deriving hardcaml]
end

(* Stub: safe defaults for every output. Replace fields with actual logic. *)
let create (_scope : Scope.t) (_i : _ I.t) : _ O.t =
  { O.led = zero 4
  ; led0_r = gnd
  ; led0_g = gnd
  ; led0_b = gnd
  ; led1_r = gnd
  ; led1_g = gnd
  ; led1_b = gnd
  ; led2_r = gnd
  ; led2_g = gnd
  ; led2_b = gnd
  ; led3_r = gnd
  ; led3_g = gnd
  ; led3_b = gnd
  ; uart_rxd_out = vdd
  ; eth_mdc = gnd
  ; eth_rstn = vdd
  ; eth_ref_clk = gnd
  ; eth_tx_en = gnd
  ; eth_txd = zero 4
  }
;;
