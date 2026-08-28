(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "mac_top_unit_quickcheck_tests.ml" *)
(* Directed and generated integration tests for both halves of the complete MII MAC. *)

open! Core
open! Hardcaml_verif
open! Mac_top_testbench

let check_tx payload (observation : Tx_observation.t) =
  [%test_result: int list] observation.frame ~expect:(expected_tx_frame payload);
  [%test_result: int] observation.tx_en_rises ~expect:1;
  [%test_result: int] observation.tx_en_falls ~expect:1;
  [%test_result: int] observation.nibble_count ~expect:(2 * List.length observation.frame);
  [%test_result: bool] observation.busy_cleared ~expect:true
;;

let check_rx payload (observation : Rx_observation.t) =
  [%test_result: int list] observation.payload ~expect:payload;
  [%test_result: int list] observation.tfirst_indices ~expect:[ 0 ];
  [%test_result: int list] observation.tlast_indices ~expect:[ List.length payload - 1 ];
  [%test_result: bool option] observation.tuser_on_last ~expect:(Some false);
  [%test_result: bool] observation.frame_crc_ok ~expect:true;
  [%test_result: int] observation.rx_eth_type ~expect:0x0800
;;

let%test_unit "TX pads a short payload and emits a modelled FCS" =
  let payload = [ 0xde; 0xad; 0xbe; 0xef ] in
  check_tx payload (transmit payload)
;;

let%test_unit "RX reports a corrupt FCS on tuser without dropping bytes" =
  let payload = make_payload 12 in
  let observation = receive_payload ~corrupt_fcs:true payload in
  [%test_result: int list] observation.payload ~expect:payload;
  [%test_result: bool option] observation.tuser_on_last ~expect:(Some true);
  [%test_result: bool] observation.frame_crc_ok ~expect:false
;;

let%test_unit "TX-to-RX loopback preserves random unpadded payloads" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "mac-top-loopback")
    (* The simulation-only RX FIFO is 64 words deep and this fixture deliberately buffers
       the whole frame before draining it, so keep generated payloads below the FIFO
       boundary. Longer streaming/backpressure cases live in the composition tbs. *)
    (Generators.byte_list ~min_length:46 ~max_length:60 ())
    ~f:(fun payload ->
      let tx, rx = loopback payload in
      check_tx payload tx;
      check_rx payload rx)
;;
