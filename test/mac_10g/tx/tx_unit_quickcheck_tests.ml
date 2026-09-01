(* University of Florida *)
(* Author: Bohdan Purtell *)

open! Core
open! Hardcaml_verif
open! Tx_testbench

let bytes length = List.init length ~f:(fun n -> ((n * 37) + length) land 0xff)

let%test_unit "all final keep widths, padding boundary, and termination lanes decode" =
  let lengths = List.range 14 74 in
  let frames = List.map lengths ~f:bytes in
  let observations = frames |> List.concat_map ~f:Beat.of_frame |> Testbench.run in
  [%test_result: int list list]
    (decode_frames observations)
    ~expect:(List.map frames ~f:expected_wire_frame);
  [%test_result: int list]
    (termination_lanes observations |> List.dedup_and_sort ~compare:Int.compare)
    ~expect:(List.range 0 8);
  assert (List.for_all (interframe_idle_counts observations) ~f:(fun count -> count >= 12));
  let final = List.last_exn observations in
  [%test_result: int] final.frames ~expect:(List.length frames);
  [%test_result: int]
    final.bytes
    ~expect:
      (List.sum (module Int) frames ~f:(fun frame -> Int.max 60 (List.length frame) + 4));
  [%test_result: int] final.underflows ~expect:0
;;

let%test_unit "arbitrary pre-commit AXI gaps cannot cause wire underflow" =
  let frame_generator = Generators.byte_list ~min_length:14 ~max_length:180 () in
  Quickcheck.test
    ~trials:60
    ~seed:(`Deterministic "mac-10g-functional-tx")
    ~sexp_of:[%sexp_of: int list]
    frame_generator
    ~f:(fun frame ->
      let observations =
        Testbench.run
          ~gaps:(fun cycle -> cycle mod 5 = 1 || cycle mod 11 = 3)
          (Beat.of_frame frame)
      in
      [%test_result: int list list]
        (decode_frames observations)
        ~expect:[ expected_wire_frame frame ];
      [%test_result: int] (List.last_exn observations).underflows ~expect:0)
;;

let%test_unit "tuser, illegal keeps, and illegal lengths roll back whole frames" =
  let good = bytes 32 in
  let bad_keep =
    [ { Beat.bytes = bytes 8; keep = 0xff; last = false; user = false }
    ; { Beat.bytes = bytes 3; keep = 0x05; last = true; user = false }
    ]
  in
  let too_short = Beat.of_frame (bytes 13) in
  let observations =
    Testbench.run
      (Beat.of_frame ~user:true (bytes 20) @ bad_keep @ too_short @ Beat.of_frame good)
  in
  [%test_result: int list list]
    (decode_frames observations)
    ~expect:[ expected_wire_frame good ];
  let final = List.last_exn observations in
  [%test_result: int] final.frames ~expect:1;
  [%test_result: int] final.drops ~expect:3;
  [%test_result: int] final.malformed ~expect:2
;;

let%test_unit "TX counter clear resets every owning-domain statistic and sticky event" =
  let final =
    Testbench.run ~clear_counters:true (Beat.of_frame (bytes 80)) |> List.last_exn
  in
  [%test_result: int] final.frames ~expect:0;
  [%test_result: int] final.bytes ~expect:0;
  [%test_result: int] final.drops ~expect:0;
  [%test_result: int] final.malformed ~expect:0;
  [%test_result: int] final.underflows ~expect:0;
  [%test_result: bool] final.underflow_sticky ~expect:false
;;
