open! Core
open! Interface_contract_testbench

let%expect_test "phase-0 contract summary" =
  print_s [%sexp (default_parameters : parameters)];
  print_s [%sexp (word_summary Xgmii_of_hardcaml.Xgmii.idle_word : int list * int)];
  print_s [%sexp (input_ports : (string * int) list)];
  print_s [%sexp (output_ports : (string * int) list)];
  [%expect
    {|
    ((tx_buffer_depth_bytes 8192) (rx_buffer_depth_bytes 8192)
     (descriptor_capacity 4) (max_supported_frame_length 1518))
    ((7 7 7 7 7 7 7 7) 255)
    ((axi_clock_i 1) (axi_reset_i 1) (m_axis_rx_tready_i 1) (rx_clock_i 1)
     (rx_reset_i 1) (s_axi_araddr_i 12) (s_axi_arvalid_i 1) (s_axi_awaddr_i 12)
     (s_axi_awvalid_i 1) (s_axi_bready_i 1) (s_axi_rready_i 1) (s_axi_wdata_i 32)
     (s_axi_wstrb_i 4) (s_axi_wvalid_i 1) (s_axis_tx_tdata_i 64)
     (s_axis_tx_tkeep_i 8) (s_axis_tx_tlast_i 1) (s_axis_tx_tuser_i 1)
     (s_axis_tx_tvalid_i 1) (tx_clock_i 1) (tx_reset_i 1) (xgmii_rxc_i 8)
     (xgmii_rxd_i 64))
    ((irq_o 1) (m_axis_rx_tdata_o 64) (m_axis_rx_tkeep_o 8) (m_axis_rx_tlast_o 1)
     (m_axis_rx_tuser_o 1) (m_axis_rx_tvalid_o 1) (s_axi_arready_o 1)
     (s_axi_awready_o 1) (s_axi_bresp_o 2) (s_axi_bvalid_o 1) (s_axi_rdata_o 32)
     (s_axi_rresp_o 2) (s_axi_rvalid_o 1) (s_axi_wready_o 1)
     (s_axis_tx_tready_o 1) (xgmii_txc_o 8) (xgmii_txd_o 64))
    |}]
;;
