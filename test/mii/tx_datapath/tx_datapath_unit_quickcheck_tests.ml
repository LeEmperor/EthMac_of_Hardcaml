(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_datapath_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Tx_datapath

   Typed examples and generated properties covering every leg of the transmit byte mux.

   Net-new verification - this block had no testbench of its own. The oracle is
   [Testbench.expected_byte], the pure-OCaml mux in the testbench, and the headline
   property sweeps the entire selector space against it rather than sampling it: eight
   [byte_mux_sel] values times eight [mac_byte_sel] values is 64 combinations, cheap
   enough to enumerate exhaustively for each of the payload/pad cases.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Tx_datapath_testbench

let byte_out
  ?(mac_byte_sel = 0)
  ?(s_axis_tdata = 0xA5)
  ?(fcs_byte = 0x5A)
  ?(pad = false)
  byte_source
  =
  let selectors = { Selectors.byte_source; mac_byte_sel; s_axis_tdata; fcs_byte; pad } in
  match Testbench.run_selectors [ selectors ] with
  | [ observation ] -> observation.output.byte_out
  | observations ->
    raise_s [%message "expected one observation" (List.length observations : int)]
;;

let%test_unit "the constant legs emit their constants" =
  [%test_result: int] (byte_out Byte_source.Idle) ~expect:0x00;
  [%test_result: int] (byte_out Byte_source.Preamble) ~expect:0x55;
  [%test_result: int] (byte_out Byte_source.Sfd) ~expect:0xD5
;;

let%test_unit "the destination MAC is broadcast" =
  List.iter (List.init 6 ~f:Fn.id) ~f:(fun mac_byte_sel ->
    [%test_result: int] (byte_out ~mac_byte_sel Byte_source.Dst_mac) ~expect:0xFF)
;;

let%test_unit "the source MAC is the Arty's locally-administered address" =
  [%test_result: int list]
    (List.init 6 ~f:(fun mac_byte_sel -> byte_out ~mac_byte_sel Byte_source.Src_mac))
    ~expect:[ 0x02; 0x00; 0x00; 0x00; 0x00; 0x01 ]
;;

let%test_unit "the payload leg passes s_axis_tdata through, and zeroes it while padding" =
  [%test_result: int] (byte_out ~s_axis_tdata:0x3C Byte_source.Payload) ~expect:0x3C;
  [%test_result: int]
    (byte_out ~s_axis_tdata:0x3C ~pad:true Byte_source.Payload)
    ~expect:0x00
;;

let%test_unit "the FCS leg passes fcs_byte through" =
  [%test_result: int] (byte_out ~fcs_byte:0x26 Byte_source.Fcs) ~expect:0x26
;;

(* [mac_byte_sel] is three bits but the MAC lists hold six entries. Hardcaml's [mux]
   saturates at the last element, so 6 and 7 repeat index 5. The controller never drives
   those - it resets its counter per header field - but freeze the behavior anyway, since
   a wrapping mux would silently emit the wrong header byte if it ever did. *)
let%test_unit "an out-of-range mac_byte_sel saturates at the last MAC byte" =
  List.iter [ 6; 7 ] ~f:(fun mac_byte_sel ->
    [%test_result: int] (byte_out ~mac_byte_sel Byte_source.Src_mac) ~expect:0x01;
    [%test_result: int] (byte_out ~mac_byte_sel Byte_source.Dst_mac) ~expect:0xFF)
;;

let%test_unit "s_axis_tready is tied low on every leg" =
  (* The RTL wires it to zero unconditionally; real backpressure comes from [mac_top]. *)
  let observations =
    Testbench.run_full_sweep ~s_axis_tdata:0xA5 ~fcs_byte:0x5A ~pad:false
  in
  List.iter observations ~f:(fun (observation : Observation.t) ->
    [%test_result: bool] observation.output.s_axis_tready ~expect:false)
;;

let%test_unit "the block is combinational" =
  (* Both sides of the clock edge must agree; a registered stage sneaking in here would
     desynchronize the datapath from the controller that drives its selectors. *)
  let trials = Testbench.selector_space ~s_axis_tdata:0xA5 ~fcs_byte:0x5A ~pad:false in
  List.iter
    (Testbench.run_selectors_across_edge trials)
    ~f:(fun (selectors, before, after) ->
      [%test_result: Output_snapshot.t] before ~expect:after;
      [%test_result: Output_snapshot.t]
        after
        ~expect:(Testbench.expected_output selectors))
;;

(* Build the expected list from the selector space itself rather than from what came back,
   so a dropped or reordered observation fails too. *)
let expected_sweep ~s_axis_tdata ~fcs_byte ~pad ~expected_output =
  List.map (Testbench.selector_space ~s_axis_tdata ~fcs_byte ~pad) ~f:(fun selectors ->
    { Observation.selectors; output = expected_output selectors })
;;

let check_sweep ~s_axis_tdata ~fcs_byte ~pad =
  [%test_result: Observation.t list]
    (Testbench.run_full_sweep ~s_axis_tdata ~fcs_byte ~pad)
    ~expect:
      (expected_sweep
         ~s_axis_tdata
         ~fcs_byte
         ~pad
         ~expected_output:Testbench.expected_output)
;;

let%test_unit "the full selector space matches the model" =
  check_sweep ~s_axis_tdata:0xA5 ~fcs_byte:0x5A ~pad:false;
  check_sweep ~s_axis_tdata:0xA5 ~fcs_byte:0x5A ~pad:true
;;

let%test_unit "a distinguishable ethertype is emitted high byte first" =
  (* 0x9999 hides a byte swap; 0x0800 does not. *)
  let observations =
    Ipv4_testbench.run_selectors
      (List.init 2 ~f:(fun mac_byte_sel ->
         { Selectors.byte_source = Byte_source.Eth_type
         ; mac_byte_sel
         ; s_axis_tdata = 0xA5
         ; fcs_byte = 0x5A
         ; pad = false
         }))
  in
  [%test_result: int list]
    (List.map observations ~f:(fun (observation : Observation.t) ->
       observation.output.byte_out))
    ~expect:[ 0x08; 0x00 ]
;;

let%test_unit "the full selector space matches the model for an IPv4 ethertype" =
  [%test_result: Observation.t list]
    (Ipv4_testbench.run_full_sweep ~s_axis_tdata:0xA5 ~fcs_byte:0x5A ~pad:false)
    ~expect:
      (expected_sweep
         ~s_axis_tdata:0xA5
         ~fcs_byte:0x5A
         ~pad:false
         ~expected_output:Ipv4_testbench.expected_output)
;;

(* Random selector combinations, so the payload and FCS legs are checked against values
   the fixed sweeps never try. *)
module Trial = struct
  type t = Selectors.t [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind byte_source = Quickcheck.Generator.of_list Byte_source.all in
    let%bind mac_byte_sel = Int.gen_incl 0 7 in
    let%bind s_axis_tdata = Generators.byte in
    let%bind fcs_byte = Generators.byte in
    let%map pad = Bool.quickcheck_generator in
    { Selectors.byte_source; mac_byte_sel; s_axis_tdata; fcs_byte; pad }
  ;;
end

let%test_unit "random selector combinations match the model" =
  Quickcheck.test
    ~trials:200
    ~seed:(`Deterministic "tx-datapath-selectors")
    ~sexp_of:[%sexp_of: Trial.t]
    ~f:(fun selectors ->
      match Testbench.run_selectors [ selectors ] with
      | [ observation ] ->
        [%test_result: Output_snapshot.t]
          observation.output
          ~expect:(Testbench.expected_output selectors)
      | _ -> failwith "expected one observation")
    Trial.quickcheck_generator
;;
