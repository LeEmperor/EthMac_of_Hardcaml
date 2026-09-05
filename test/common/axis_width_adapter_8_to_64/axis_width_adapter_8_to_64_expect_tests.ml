open! Core
open! Axis_width_adapter_8_to_64_testbench

let%expect_test "full and partial output words show the 10G-style lane order" =
  let frame = List.range 1 12 in
  let observation = Testbench.run [ frame ] in
  List.iter observation.transfers ~f:(fun beat -> print_endline (compact_beat beat));
  [%expect
    {|
    data=0x0807060504030201 keep=0xff first
    data=0x00000000000b0a09 keep=0x07 last
    |}]
;;

let%expect_test "short back-to-back frames remain separate" =
  let frames = [ [ 0xDE; 0xAD; 0xBE; 0xEF ]; [ 0x11; 0x22 ] ] in
  let observation = Testbench.run frames in
  List.iter observation.transfers ~f:(fun beat -> print_endline (compact_beat beat));
  [%expect
    {|
    data=0x00000000efbeadde keep=0x0f first last
    data=0x0000000000002211 keep=0x03 first last
    |}]
;;
