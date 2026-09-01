open! Core
open! Hardcaml
open! Mac_10g_of_hardcaml
open! Xgmii_of_hardcaml
open! Xgmii
open! Interface_contract_testbench

let%test_unit "the proposed top-level port names and widths are frozen" =
  [%test_result: (string * int) list]
    input_ports
    ~expect:
      [ "axi_clock_i", 1
      ; "axi_reset_i", 1
      ; "m_axis_rx_tready_i", 1
      ; "rx_clock_i", 1
      ; "rx_reset_i", 1
      ; "s_axi_araddr_i", 12
      ; "s_axi_arvalid_i", 1
      ; "s_axi_awaddr_i", 12
      ; "s_axi_awvalid_i", 1
      ; "s_axi_bready_i", 1
      ; "s_axi_rready_i", 1
      ; "s_axi_wdata_i", 32
      ; "s_axi_wstrb_i", 4
      ; "s_axi_wvalid_i", 1
      ; "s_axis_tx_tdata_i", 64
      ; "s_axis_tx_tkeep_i", 8
      ; "s_axis_tx_tlast_i", 1
      ; "s_axis_tx_tuser_i", 1
      ; "s_axis_tx_tvalid_i", 1
      ; "tx_clock_i", 1
      ; "tx_reset_i", 1
      ; "xgmii_rxc_i", 8
      ; "xgmii_rxd_i", 64
      ];
  [%test_result: (string * int) list]
    output_ports
    ~expect:
      [ "irq_o", 1
      ; "m_axis_rx_tdata_o", 64
      ; "m_axis_rx_tkeep_o", 8
      ; "m_axis_rx_tlast_o", 1
      ; "m_axis_rx_tuser_o", 1
      ; "m_axis_rx_tvalid_o", 1
      ; "s_axi_arready_o", 1
      ; "s_axi_awready_o", 1
      ; "s_axi_bresp_o", 2
      ; "s_axi_bvalid_o", 1
      ; "s_axi_rdata_o", 32
      ; "s_axi_rresp_o", 2
      ; "s_axi_rvalid_o", 1
      ; "s_axi_wready_o", 1
      ; "s_axis_tx_tready_o", 1
      ; "xgmii_txc_o", 8
      ; "xgmii_txd_o", 64
      ]
;;

let%test_unit "direction-neutral word and stream beat fields are frozen" =
  [%test_result: (string * int) list]
    xgmii_word_fields
    ~expect:[ "control", 8; "data", 64 ];
  [%test_result: (string * int) list]
    axis_beat_fields
    ~expect:[ "data", 64; "keep", 8; "last", 1; "user", 1 ]
;;

let%test_unit "XGMII constants and lane ordering agree with the PCS contract" =
  [%test_result: int list * int]
    (word_summary idle_word)
    ~expect:([ 0x07; 0x07; 0x07; 0x07; 0x07; 0x07; 0x07; 0x07 ], 0xff);
  [%test_result: int list * int]
    (word_summary local_fault_word)
    ~expect:([ 0x9c; 0x00; 0x00; 0x01; 0x9c; 0x00; 0x00; 0x01 ], 0x11);
  [%test_result: int list * int]
    (word_summary remote_fault_word)
    ~expect:([ 0x9c; 0x00; 0x00; 0x02; 0x9c; 0x00; 0x00; 0x02 ], 0x11);
  List.iter (List.range 0 8) ~f:(fun lane ->
    [%test_result: bool] (is_legal_start_lane lane) ~expect:(lane = 0 || lane = 4))
;;

let%test_unit "lane helpers reject invalid indices and preserve wire order" =
  let word = of_lane_bytes [ 0; 1; 2; 3; 4; 5; 6; 7 ] ~control:0x81 in
  [%test_result: int list * int]
    (word_summary word)
    ~expect:([ 0; 1; 2; 3; 4; 5; 6; 7 ], 0x81);
  let changed =
    set_lane word ~lane:4 ~byte:(Signal.of_int_trunc ~width:8 0xaa) ~is_control:Signal.vdd
  in
  [%test_result: int list * int]
    (word_summary changed)
    ~expect:([ 0; 1; 2; 3; 0xaa; 5; 6; 7 ], 0x91);
  List.iter [ -1; 8; 12 ] ~f:(fun lane ->
    assert (Result.is_error (Or_error.try_with (fun () -> lane_byte word lane))))
;;

let%test_unit "legal default and boundary configurations elaborate" =
  [%test_result: unit Or_error.t] (elaborate default_parameters) ~expect:(Ok ());
  [%test_result: unit Or_error.t]
    (elaborate
       { tx_buffer_depth_bytes = 64
       ; rx_buffer_depth_bytes = 64
       ; descriptor_capacity = 2
       ; max_supported_frame_length = 64
       })
    ~expect:(Ok ())
;;

let%test_unit "the Phase-0 scaffold exposes only safe inactive outputs" =
  [%test_result: scaffold_summary]
    (scaffold_summary ())
    ~expect:
      { tx_ready = 0
      ; tx_xgmii = [ 0x07; 0x07; 0x07; 0x07; 0x07; 0x07; 0x07; 0x07 ], 0xff
      ; rx_valid = 0
      ; axi_bvalid = 0
      ; axi_rvalid = 0
      ; irq = 0
      }
;;

let%test_unit "illegal elaboration parameters are rejected at the top boundary" =
  let illegal =
    [ { default_parameters with max_supported_frame_length = 63 }
    ; { default_parameters with max_supported_frame_length = 65536 }
    ; { default_parameters with tx_buffer_depth_bytes = 1024 }
    ; { default_parameters with rx_buffer_depth_bytes = 1518 }
    ; { default_parameters with descriptor_capacity = 1 }
    ; { default_parameters with descriptor_capacity = 3 }
    ]
  in
  List.iter illegal ~f:(fun parameters -> assert (Result.is_error (elaborate parameters)))
;;

let%test_unit "the initial register ABI constants are frozen" =
  [%test_result: int] Mac_10g_register_map.Value.core_id ~expect:0x4d414331;
  [%test_result: int] Mac_10g_register_map.Value.core_version ~expect:0x01000000;
  [%test_result: int] Mac_10g_register_map.Value.default_max_frame_length ~expect:1518;
  let module A = Mac_10g_register_map.Address in
  [%test_result: int list]
    [ A.core_id
    ; A.core_version
    ; A.control
    ; A.status
    ; A.max_frame_length
    ; A.irq_status
    ; A.irq_enable
    ; A.counter_control
    ; A.scratch
    ; A.tx_frames_low
    ; A.tx_frames_high
    ; A.tx_bytes_low
    ; A.tx_bytes_high
    ; A.tx_drops_low
    ; A.tx_drops_high
    ; A.tx_malformed_axi_low
    ; A.tx_malformed_axi_high
    ; A.tx_underflow_low
    ; A.tx_underflow_high
    ; A.rx_good_frames_low
    ; A.rx_good_frames_high
    ; A.rx_bad_frames_low
    ; A.rx_bad_frames_high
    ; A.rx_bytes_low
    ; A.rx_bytes_high
    ; A.rx_fcs_errors_low
    ; A.rx_fcs_errors_high
    ; A.rx_length_errors_low
    ; A.rx_length_errors_high
    ; A.rx_xgmii_errors_low
    ; A.rx_xgmii_errors_high
    ; A.rx_overflow_low
    ; A.rx_overflow_high
    ]
    ~expect:
      [ 0x000
      ; 0x004
      ; 0x008
      ; 0x00c
      ; 0x010
      ; 0x014
      ; 0x018
      ; 0x01c
      ; 0x020
      ; 0x100
      ; 0x104
      ; 0x108
      ; 0x10c
      ; 0x110
      ; 0x114
      ; 0x118
      ; 0x11c
      ; 0x120
      ; 0x124
      ; 0x180
      ; 0x184
      ; 0x188
      ; 0x18c
      ; 0x190
      ; 0x194
      ; 0x198
      ; 0x19c
      ; 0x1a0
      ; 0x1a4
      ; 0x1a8
      ; 0x1ac
      ; 0x1b0
      ; 0x1b4
      ];
  let module C = Mac_10g_register_map.Control in
  let module S = Mac_10g_register_map.Status in
  let module I = Mac_10g_register_map.Irq in
  let module CC = Mac_10g_register_map.Counter_control in
  [%test_result: int list]
    [ C.tx_enable
    ; C.rx_enable
    ; C.drop_bad_rx
    ; C.tx_soft_reset
    ; C.rx_soft_reset
    ; S.tx_active
    ; S.rx_active
    ; S.tx_buffer_nonempty
    ; S.rx_buffer_nonempty
    ; S.tx_underflow
    ; S.rx_overflow
    ; S.local_fault
    ; S.remote_fault
    ; I.tx_drop
    ; I.tx_malformed_axi
    ; I.tx_underflow
    ; I.rx_bad_frame
    ; I.rx_overflow
    ; I.local_fault
    ; I.remote_fault
    ; CC.snapshot
    ; CC.clear
    ]
    ~expect:[ 0; 1; 2; 8; 9; 0; 1; 2; 3; 8; 9; 16; 17; 0; 1; 2; 8; 9; 10; 11; 0; 1 ]
;;
