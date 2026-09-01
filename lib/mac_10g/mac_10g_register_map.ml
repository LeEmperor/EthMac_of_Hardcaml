(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_10g_register_map.ml" *)
(* Frozen phase-0 constants for the 10G MAC AXI4-Lite register ABI. *)

module Address = struct
  let core_id = 0x000
  let core_version = 0x004
  let control = 0x008
  let status = 0x00c
  let max_frame_length = 0x010
  let irq_status = 0x014
  let irq_enable = 0x018
  let counter_control = 0x01c
  let scratch = 0x020
  let tx_frames_low = 0x100
  let tx_frames_high = 0x104
  let tx_bytes_low = 0x108
  let tx_bytes_high = 0x10c
  let tx_drops_low = 0x110
  let tx_drops_high = 0x114
  let tx_malformed_axi_low = 0x118
  let tx_malformed_axi_high = 0x11c
  let tx_underflow_low = 0x120
  let tx_underflow_high = 0x124
  let rx_good_frames_low = 0x180
  let rx_good_frames_high = 0x184
  let rx_bad_frames_low = 0x188
  let rx_bad_frames_high = 0x18c
  let rx_bytes_low = 0x190
  let rx_bytes_high = 0x194
  let rx_fcs_errors_low = 0x198
  let rx_fcs_errors_high = 0x19c
  let rx_length_errors_low = 0x1a0
  let rx_length_errors_high = 0x1a4
  let rx_xgmii_errors_low = 0x1a8
  let rx_xgmii_errors_high = 0x1ac
  let rx_overflow_low = 0x1b0
  let rx_overflow_high = 0x1b4
end

module Value = struct
  let core_id = 0x4d414331
  let core_version = 0x01000000
  let default_max_frame_length = 1518
end

module Control = struct
  let tx_enable = 0
  let rx_enable = 1
  let drop_bad_rx = 2
  let tx_soft_reset = 8
  let rx_soft_reset = 9
end

module Status = struct
  let tx_active = 0
  let rx_active = 1
  let tx_buffer_nonempty = 2
  let rx_buffer_nonempty = 3
  let tx_underflow = 8
  let rx_overflow = 9
  let local_fault = 16
  let remote_fault = 17
end

module Irq = struct
  let tx_drop = 0
  let tx_malformed_axi = 1
  let tx_underflow = 2
  let rx_bad_frame = 8
  let rx_overflow = 9
  let local_fault = 10
  let remote_fault = 11
end

module Counter_control = struct
  let snapshot = 0
  let clear = 1
end
