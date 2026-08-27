(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Unit and Quickcheck Test Suite: Pcs_tx_encoder

   Exact known-answer cases plus generated data, control, termination, and ordered-set
   payloads checked against the independent software model.
*)

open! Core
open! Hardcaml
open! Pcs_tx_encoder_testbench

let testbench = lazy (Testbench.create ())
let check lanes = Testbench.check (force testbench) lanes

let check_exact lanes ~payload ~header ~bad_xgmii =
  let actual = Testbench.encode (force testbench) lanes in
  let expected : Encoded.t =
    { payload = Bits.of_hex ~width:64 payload; header; bad_xgmii }
  in
  if not (Encoded.equal actual expected)
  then
    raise_s
      [%message
        "unexpected known-answer encoding"
          (lanes : Lane.t list)
          (Encoded.printable actual : Sexp.t)
          (Encoded.printable expected : Sexp.t)]
;;

let%test_unit "all-data preserves lane order and uses the data header" =
  check_exact
    (List.map [ 0x00; 0x11; 0x22; 0x33; 0x44; 0x55; 0x66; 0x77 ] ~f:Lane.data)
    ~payload:"7766554433221100"
    ~header:0b10
    ~bad_xgmii:false
;;

let%test_unit "canonical idle is a control block" =
  check_exact
    (List.init 8 ~f:(fun _ -> Lane.idle))
    ~payload:"000000000000001e"
    ~header:0b01
    ~bad_xgmii:false
;;

let%test_unit "start in lane zero" =
  check_exact
    (Lane.start :: List.map [ 0x11; 0x22; 0x33; 0x44; 0x55; 0x66; 0x77 ] ~f:Lane.data)
    ~payload:"7766554433221178"
    ~header:0b01
    ~bad_xgmii:false
;;

let%test_unit "start in lane four" =
  check_exact
    ([ Lane.idle; Lane.idle; Lane.idle; Lane.idle; Lane.start ]
     @ List.map [ 0xaa; 0xbb; 0xcc ] ~f:Lane.data)
    ~payload:"ccbbaa0000000033"
    ~header:0b01
    ~bad_xgmii:false
;;

let%test_unit "terminate in every lane" =
  List.iter (List.range 0 8) ~f:(fun terminate_lane ->
    check
      (List.init 8 ~f:(fun lane ->
         if lane < terminate_lane
         then Lane.data ((0x31 + lane) land 0xff)
         else if lane = terminate_lane
         then Lane.terminate
         else if lane land 1 = 0
         then Lane.error
         else Lane.idle)))
;;

let%test_unit "all error characters use the Clause 49 error control code" =
  check (List.init 8 ~f:(fun _ -> Lane.error))
;;

let%test_unit "ordered sets in lanes zero and four" =
  check_exact
    [ Lane.ordered_set
    ; Lane.data 0x00
    ; Lane.data 0x00
    ; Lane.data 0x01
    ; Lane.ordered_set
    ; Lane.data 0x00
    ; Lane.data 0x00
    ; Lane.data 0x01
    ]
    ~payload:"0100000001000055"
    ~header:0b01
    ~bad_xgmii:false
;;

let%test_unit "all supported mixed ordered-set block types" =
  check
    [ Lane.ordered_set
    ; Lane.data 0x11
    ; Lane.data 0x22
    ; Lane.data 0x33
    ; Lane.idle
    ; Lane.error
    ; Lane.idle
    ; Lane.error
    ];
  check
    [ Lane.error
    ; Lane.idle
    ; Lane.error
    ; Lane.idle
    ; Lane.ordered_set
    ; Lane.data 0x55
    ; Lane.data 0x66
    ; Lane.data 0x77
    ];
  check
    [ Lane.ordered_set
    ; Lane.data 0x11
    ; Lane.data 0x22
    ; Lane.data 0x33
    ; Lane.start
    ; Lane.data 0x55
    ; Lane.data 0x66
    ; Lane.data 0x77
    ]
;;

let%test_unit "illegal XGMII is replaced with an all-error control block" =
  let unsupported = Lane.control 0x1c :: List.init 7 ~f:(fun _ -> Lane.idle) in
  let actual = Testbench.encode (force testbench) unsupported in
  let expected : Encoded.t =
    { payload = Reference.error_payload; header = 0b01; bad_xgmii = true }
  in
  if not (Encoded.equal actual expected)
  then
    raise_s
      [%message
        "unexpected illegal-XGMII fallback"
          (Encoded.printable actual : Sexp.t)
          (Encoded.printable expected : Sexp.t)];
  check (List.init 3 ~f:(fun _ -> Lane.idle) @ [ Lane.start ] @ List.init 4 ~f:Lane.data);
  check (Lane.terminate :: List.init 7 ~f:(fun _ -> Lane.data 0x00))
;;

module Generators = struct
  let byte = Int.gen_incl 0x00 0xff
  let bytes count = List.gen_with_length count byte
  let regular_control = Quickcheck.Generator.of_list [ Lane.idle; Lane.error ]
end

let%test_unit "random all-data blocks match the reference model" =
  Quickcheck.test
    ~trials:200
    ~seed:(`Deterministic "pcs-tx-encoder-data")
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun bytes -> check (List.map bytes ~f:Lane.data))
    (Generators.bytes 8)
;;

let%test_unit "random all-control blocks match the reference model" =
  Quickcheck.test
    ~trials:200
    ~seed:(`Deterministic "pcs-tx-encoder-control")
    ~sexp_of:[%sexp_of: Lane.t list]
    ~f:check
    (List.gen_with_length 8 Generators.regular_control)
;;

let%test_unit "random termination blocks match the reference model" =
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind terminate_lane = Int.gen_incl 0 7 in
    let%bind leading_data = Generators.bytes terminate_lane in
    let%map trailing_controls =
      List.gen_with_length (7 - terminate_lane) Generators.regular_control
    in
    List.map leading_data ~f:Lane.data @ [ Lane.terminate ] @ trailing_controls
  in
  Quickcheck.test
    ~trials:400
    ~seed:(`Deterministic "pcs-tx-encoder-terminate")
    ~sexp_of:[%sexp_of: Lane.t list]
    ~f:check
    generator
;;

let%test_unit "random ordered-set blocks match the reference model" =
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind format = Int.gen_incl 0 3 in
    let%bind data = Generators.bytes 6 in
    let%map controls = List.gen_with_length 4 Generators.regular_control in
    let d index = Lane.data (List.nth_exn data index) in
    match format with
    | 0 -> [ Lane.ordered_set; d 0; d 1; d 2 ] @ controls
    | 1 -> controls @ [ Lane.ordered_set; d 3; d 4; d 5 ]
    | 2 -> [ Lane.ordered_set; d 0; d 1; d 2; Lane.ordered_set; d 3; d 4; d 5 ]
    | 3 -> [ Lane.ordered_set; d 0; d 1; d 2; Lane.start; d 3; d 4; d 5 ]
    | _ -> assert false
  in
  Quickcheck.test
    ~trials:400
    ~seed:(`Deterministic "pcs-tx-encoder-ordered-set")
    ~sexp_of:[%sexp_of: Lane.t list]
    ~f:check
    generator
;;
