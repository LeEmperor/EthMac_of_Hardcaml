(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "packet_buffer_expect_tests.ml" *)
(* Compact golden trace for speculative write, rollback, commit, stall, and drain. *)

open! Core
open! Packet_buffer_testbench

let summary ({ action; after_edge = o; _ } : Cycle_observation.t) =
  print_s
    [%sexp
      { write = (action.write_bytes : int list)
      ; commit = (action.commit : bool)
      ; rollback = (action.rollback : bool)
      ; ready = (action.read_ready : bool)
      ; frame_length = (o.current_frame_length : int)
      ; used = (o.bytes_used : int)
      ; descriptors = (o.descriptors_used : int)
      ; read = (o.read_bytes : int list)
      ; valid = (o.read_valid : bool)
      ; last = (o.read_last : bool)
      }]
;;

let%expect_test "transactional packet-buffer trace" =
  Testbench.run
    [ Action.write [ 1; 2; 3 ]
    ; Action.rollback
    ; Action.write ~commit:true ~error:2 [ 10; 11; 12; 13; 14 ]
    ; Action.idle
    ; Action.read
    ]
  |> List.iter ~f:summary;
  [%expect
    {|
    ((write (1 2 3)) (commit false) (rollback false) (ready false)
     (frame_length 3) (used 3) (descriptors 0) (read ()) (valid false)
     (last false))
    ((write ()) (commit false) (rollback true) (ready false) (frame_length 0)
     (used 0) (descriptors 0) (read ()) (valid false) (last false))
    ((write (10 11 12 13 14)) (commit true) (rollback false) (ready false)
     (frame_length 0) (used 5) (descriptors 1) (read (10 11 12 13 14))
     (valid true) (last true))
    ((write ()) (commit false) (rollback false) (ready false) (frame_length 0)
     (used 5) (descriptors 1) (read (10 11 12 13 14)) (valid true) (last true))
    ((write ()) (commit false) (rollback false) (ready true) (frame_length 0)
     (used 0) (descriptors 0) (read ()) (valid false) (last false))
    |}]
;;
