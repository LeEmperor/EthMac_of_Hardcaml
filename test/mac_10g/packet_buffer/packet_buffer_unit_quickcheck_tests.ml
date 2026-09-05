(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "packet_buffer_unit_quickcheck_tests.ml" *)
(* Commit/rollback, descriptor pressure, wraparound, and generated frame properties. *)

open! Core
open! Hardcaml_verif
open! Packet_buffer_testbench

let bytes start length = List.init length ~f:(fun offset -> (start + offset) land 0xff)

let%test_unit "a committed partial beat is visible and stable until accepted" =
  let frame = bytes 0x10 5 in
  let observations =
    Testbench.run
      [ Action.write ~commit:true ~error:0xa frame
      ; Action.idle
      ; Action.idle
      ; Action.read
      ]
  in
  let after_commit = (List.nth_exn observations 0).after_edge in
  [%test_result: bool] after_commit.read_valid ~expect:true;
  [%test_result: int list] after_commit.read_bytes ~expect:frame;
  [%test_result: int] after_commit.read_keep ~expect:0x1f;
  [%test_result: bool] after_commit.read_last ~expect:true;
  [%test_result: int] after_commit.read_error ~expect:0xa;
  [%test_result: Output_snapshot.t]
    (List.nth_exn observations 1).before_edge
    ~expect:(List.nth_exn observations 2).before_edge;
  [%test_result: int list] (Testbench.accepted_read_bytes observations) ~expect:frame;
  [%test_result: bool] (List.last_exn observations).after_edge.read_valid ~expect:false
;;

let%test_unit "rollback releases only speculative bytes" =
  let good = [ 0xaa; 0xbb; 0xcc ] in
  let observations =
    Testbench.run
      [ Action.write (bytes 0 8)
      ; Action.write (bytes 8 5)
      ; Action.rollback
      ; Action.write ~commit:true good
      ; Action.read
      ]
  in
  let after_rollback = (List.nth_exn observations 2).after_edge in
  [%test_result: int] after_rollback.current_frame_length ~expect:0;
  [%test_result: int] after_rollback.bytes_used ~expect:0;
  [%test_result: int list] (Testbench.accepted_read_bytes observations) ~expect:good
;;

let%test_unit "descriptor full blocks commit without losing the speculative frame" =
  let first_four = List.init 4 ~f:(fun n -> [ 0x20 + n ]) in
  let actions =
    List.concat_map first_four ~f:Testbench.writes_for_frame
    @ [ Action.write ~commit:true [ 0x99 ]
      ; Action.read
      ; { Action.idle with commit = true }
      ]
    @ List.init 4 ~f:(Fn.const Action.read)
  in
  let observations = Testbench.run actions in
  let blocked = List.nth_exn observations 4 in
  [%test_result: bool] blocked.before_edge.commit_ready ~expect:false;
  [%test_result: int] blocked.after_edge.current_frame_length ~expect:1;
  [%test_result: int] blocked.after_edge.descriptors_used ~expect:4;
  [%test_result: int list]
    (Testbench.accepted_read_bytes observations)
    ~expect:(List.concat first_four @ [ 0x99 ])
;;

let%test_unit "byte-ring full backpressures writes and rollback preserves committed data" =
  let committed = bytes 0x30 24 in
  let observations =
    Testbench.run
      (Testbench.writes_for_frame committed
       @ [ Action.write (bytes 0x80 8)
         ; Action.write [ 0xff ]
         ; Action.read
         ; Action.write [ 0xff ]
         ; Action.rollback
         ; Action.read
         ; Action.read
         ])
  in
  let blocked = List.nth_exn observations 4 in
  [%test_result: bool] blocked.before_edge.write_ready ~expect:false;
  [%test_result: int] blocked.after_edge.current_frame_length ~expect:8;
  [%test_result: int] blocked.after_edge.bytes_used ~expect:32;
  let after_rollback = (List.nth_exn observations 7).after_edge in
  [%test_result: int] after_rollback.current_frame_length ~expect:0;
  [%test_result: int] after_rollback.bytes_used ~expect:(List.length committed - 8);
  [%test_result: int list] (Testbench.accepted_read_bytes observations) ~expect:committed
;;

let%test_unit "byte and descriptor pointers wrap repeatedly" =
  let frames = [ bytes 0x00 7; bytes 0x20 13; bytes 0x40 5; bytes 0x60 18 ] in
  let actions =
    List.concat_map frames ~f:(fun frame ->
      Testbench.writes_for_frame frame @ Testbench.reads_for_frame frame)
  in
  let observations = Testbench.run actions in
  [%test_result: int list]
    (Testbench.accepted_read_bytes observations)
    ~expect:(List.concat frames);
  let final = (List.last_exn observations).after_edge in
  [%test_result: int] final.bytes_used ~expect:0;
  [%test_result: int] final.descriptors_used ~expect:0
;;

let%test_unit "simultaneous read/write and commit/pop preserve frame order" =
  let first = bytes 0x10 13 in
  let second = bytes 0x80 10 in
  let #(first_head, first_tail) = List.split_n first 8 in
  let #(second_head, second_tail) = List.split_n second 8 in
  let observations =
    Testbench.run
      [ Action.write first_head
      ; Action.write ~commit:true first_tail
      ; { (Action.write second_head) with read_ready = true }
      ; { (Action.write ~commit:true second_tail) with read_ready = true }
      ; Action.read
      ; Action.read
      ]
  in
  [%test_result: int list]
    (Testbench.accepted_read_bytes observations)
    ~expect:(first @ second);
  let simultaneous_commit_and_pop = (List.nth_exn observations 3).after_edge in
  [%test_result: int] simultaneous_commit_and_pop.descriptors_used ~expect:1;
  [%test_result: int] simultaneous_commit_and_pop.bytes_used ~expect:(List.length second)
;;

let%test_unit "generated frames survive bank and ring rotations" =
  let frame_generator = Generators.byte_list ~min_length:1 ~max_length:24 () in
  Quickcheck.test
    ~trials:50
    ~seed:(`Deterministic "mac-10g-packet-buffer-wrap")
    ~sexp_of:[%sexp_of: int list list]
    ~f:(fun frames ->
      let actions =
        List.concat_map frames ~f:(fun frame ->
          Testbench.writes_for_frame frame
          @ [ Action.idle ]
          @ Testbench.reads_for_frame frame)
      in
      let observations = Testbench.run actions in
      [%test_result: int list]
        (Testbench.accepted_read_bytes observations)
        ~expect:(List.concat frames))
    (List.quickcheck_generator frame_generator
     |> Quickcheck.Generator.filter ~f:(fun xs -> List.length xs <= 6))
;;

let%test_unit "all short commit/rollback choices agree with the frame reference model" =
  let choices =
    [ Action.write [ 1 ]
    ; Action.write [ 2; 3 ]
    ; { Action.idle with commit = true; error = 5 }
    ; Action.rollback
    ]
  in
  let rec sequences length =
    if length = 0
    then [ [] ]
    else
      List.concat_map choices ~f:(fun action ->
        List.map (sequences (length - 1)) ~f:(fun rest -> action :: rest))
  in
  List.iter (sequences 4) ~f:(fun actions ->
    let model = List.fold actions ~init:Reference.empty ~f:Reference.apply in
    let drain =
      List.concat_map model.committed ~f:(fun descriptor ->
        Testbench.reads_for_frame descriptor.bytes)
    in
    let observations = Testbench.run (actions @ drain) in
    [%test_result: int list]
      (Testbench.accepted_read_bytes observations)
      ~expect:(List.concat_map model.committed ~f:(fun descriptor -> descriptor.bytes)))
;;
