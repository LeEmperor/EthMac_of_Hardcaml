(* University of Florida *)
(* Author: Bohdan Purtell *)

(* Unit and Quickcheck Test Suite: Rx_controller

   Typed examples and generated properties covering the receive-header state sequence,
   registered datapath enables, valid gaps, reset recovery, and receive errors.
*)

open! Core
open! Rx_controller_testbench

module Generators = struct
  let byte = Int.gen_incl 0x00 0xFF
  let bytes length = List.gen_with_length length byte

  let frame =
    let open Quickcheck.Generator.Let_syntax in
    let%bind preamble_length = Int.gen_incl 1 10 in
    let%bind destination_mac = bytes 6 in
    let%bind source_mac = bytes 6 in
    let%bind eth_type = bytes 2 in
    let%bind payload_length = Int.gen_incl 0 24 in
    let%map payload = bytes payload_length in
    Frame.create ~preamble_length ~destination_mac ~source_mac ~eth_type ~payload ()
  ;;
end

let output
  ?(byte_assembler_en = true)
  ?(dst_mac_reg_en = false)
  ?(src_mac_reg_en = false)
  ?(eth_type_reg_en = false)
  ?(payload_sel = false)
  ?(emit_payload = false)
  ?(fcs_present = false)
  ?(in_preamble = false)
  ?(in_dst_mac = false)
  ?(in_payload = false)
  ()
  : Output_snapshot.t
  =
  { byte_assembler_en
  ; dst_mac_reg_en
  ; src_mac_reg_en
  ; eth_type_reg_en
  ; payload_sel
  ; emit_payload
  ; fcs_present
  ; in_preamble
  ; in_dst_mac
  ; in_payload
  }
;;

let observe phase byte output : Observation.t = { phase; byte; output }

let expected_frame (frame : Frame.t) =
  let preamble =
    List.init frame.preamble_length ~f:(fun index ->
      observe
        (Phase.Preamble index)
        0x55
        (output ~fcs_present:(index = 0) ~in_preamble:true ()))
  in
  let sfd = observe Phase.Sfd 0xD5 (output ~in_dst_mac:true ()) in
  let destination_mac =
    List.mapi frame.destination_mac ~f:(fun index byte ->
      observe
        (Phase.Destination_mac index)
        byte
        (output ~dst_mac_reg_en:true ~in_dst_mac:(index < 5) ()))
  in
  let source_mac =
    List.mapi frame.source_mac ~f:(fun index byte ->
      observe (Phase.Source_mac index) byte (output ~src_mac_reg_en:true ()))
  in
  let eth_type =
    List.mapi frame.eth_type ~f:(fun index byte ->
      observe
        (Phase.Eth_type index)
        byte
        (output
           ~eth_type_reg_en:true
           ~payload_sel:(index = 1)
           ~emit_payload:(index = 1)
           ~in_payload:(index = 1)
           ()))
  in
  let payload =
    List.mapi frame.payload ~f:(fun index byte ->
      observe
        (Phase.Payload index)
        byte
        (output ~payload_sel:true ~emit_payload:true ~in_payload:true ()))
  in
  preamble @ (sfd :: destination_mac) @ source_mac @ eth_type @ payload
;;

let check_frame frame =
  [%test_result: Observation.t list] (Testbench.run_frame frame) ~expect:(expected_frame frame)
;;

let%test_unit "walks a complete receive frame" =
  check_frame (Frame.create ~payload:[ 0xDE; 0xAD; 0xBE; 0xEF ] ())
;;

let%test_unit "accepts the SFD after a single preamble byte" =
  check_frame (Frame.create ~preamble_length:1 ~payload:[ 0x42 ] ())
;;

let%test_unit "byte assembler enable is en and rx_dv" =
  List.iter [ false, false; false, true; true, false; true, true ] ~f:(fun (en, rx_dv) ->
    let actual = (Testbench.run_enable_case ~en ~rx_dv).byte_assembler_en in
    [%test_result: bool] actual ~expect:(en && rx_dv))
;;

let%test_unit "an invalid cycle does not advance the destination count" =
  let actual = Testbench.run_destination_pause () in
  let destination = output ~dst_mac_reg_en:true ~in_dst_mac:true () in
  let expect : Pause_observation.t =
    { before_pause = destination
    ; during_pause = destination
    ; after_sixth_destination_byte = output ~dst_mac_reg_en:true ()
    ; after_first_source_byte = output ~src_mac_reg_en:true ()
    }
  in
  [%test_result: Pause_observation.t] actual ~expect
;;

let%test_unit "reset discards a partial header and accepts a new frame" =
  let actual = Testbench.run_reset_mid_frame () in
  let expect : Reset_observation.t =
    { before_reset = output ~dst_mac_reg_en:true ~in_dst_mac:true ()
    ; after_reset = output ~byte_assembler_en:false ()
    ; after_next_preamble = output ~fcs_present:true ~in_preamble:true ()
    }
  in
  [%test_result: Reset_observation.t] actual ~expect
;;

let%test_unit "rx_er aborts payload processing" =
  let actual = Testbench.run_payload_error () in
  let expect : Error_observation.t =
    { before_error =
        output
          ~eth_type_reg_en:true
          ~payload_sel:true
          ~emit_payload:true
          ~in_payload:true
          ()
    ; after_error = output ()
    ; following_idle_cycle = output ~byte_assembler_en:false ()
    }
  in
  [%test_result: Error_observation.t] actual ~expect
;;

let%test_unit "walks randomized frames" =
  Quickcheck.test
    ~trials:100
    ~seed:(`Deterministic "rx-controller-reference-model")
    ~sexp_of:[%sexp_of: Frame.t]
    ~f:check_frame
    Generators.frame
;;
