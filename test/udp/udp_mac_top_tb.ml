(*
  End-to-end UDP transmit integration test.

  The source side drives application bytes through Udp_tx and Ipv4_tx into the
  store-and-forward MAC.  The sink reconstructs bytes from the MII nibbles and
  checks the complete wire frame against an independent software model.

  This test deliberately concentrates on cross-layer behavior.  Detailed UDP,
  IPv4, and MAC state-machine behavior belongs in their unit testbenches.
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Helper_tb_functions

let () = print_endline "=== Running UDP+MAC Top End-to-End Testbench ==="

module Sim = Cyclesim.With_interface (Udp_mac_top.I) (Udp_mac_top.O)

let src_ip = [ 192; 168; 1; 10 ]
let dst_ip = [ 192; 168; 1; 1 ]
let src_port = 0x1234
let dst_port = 0x1235

let w16 hi lo = (hi lsl 8) lor lo
let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF

let ip_checksum ~total_length =
  let words =
    [ 0x4500
    ; total_length
    ; 0x0000
    ; 0x4000
    ; 0x4011
    ; 0x0000
    ; w16 (List.nth_exn src_ip 0) (List.nth_exn src_ip 1)
    ; w16 (List.nth_exn src_ip 2) (List.nth_exn src_ip 3)
    ; w16 (List.nth_exn dst_ip 0) (List.nth_exn dst_ip 1)
    ; w16 (List.nth_exn dst_ip 2) (List.nth_exn dst_ip 3)
    ]
  in
  let sum = List.fold words ~init:0 ~f:( + ) in
  let rec fold s =
    if s > 0xFFFF then fold ((s land 0xFFFF) + (s lsr 16)) else s
  in
  lnot (fold sum) land 0xFFFF
;;

let golden_datagram ~app =
  let n = List.length app in
  let total_length = 28 + n in
  let udp_length = 8 + n in
  let checksum = ip_checksum ~total_length in
  [ 0x45
  ; 0x00
  ; hi8 total_length
  ; lo8 total_length
  ; 0x00
  ; 0x00
  ; 0x40
  ; 0x00
  ; 0x40
  ; 0x11
  ; hi8 checksum
  ; lo8 checksum
  ]
  @ src_ip
  @ dst_ip
  @ [ hi8 src_port
    ; lo8 src_port
    ; hi8 dst_port
    ; lo8 dst_port
    ; hi8 udp_length
    ; lo8 udp_length
    ; 0x00
    ; 0x00
    ]
  @ app
;;

(* Reflected Ethernet CRC-32, independently calculated from the RTL. *)
let sw_crc_bit crc bit =
  let feedback = ((crc land 1) lxor bit) land 1 in
  let shifted = crc lsr 1 in
  if feedback = 1 then shifted lxor 0xEDB88320 else shifted
;;

let sw_crc_byte crc byte =
  let crc = ref crc in
  for bit = 0 to 7 do
    crc := sw_crc_bit !crc ((byte lsr bit) land 1)
  done;
  !crc
;;

let sw_crc bytes =
  List.fold bytes ~init:0xFFFFFFFF ~f:sw_crc_byte lxor 0xFFFFFFFF
;;

let bytes_of_int ~n x =
  List.init n ~f:(fun byte -> (x lsr (8 * byte)) land 0xFF)
;;

let exp_preamble = List.init 7 ~f:(fun _ -> 0x55)
let exp_dst_mac = [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]
let exp_src_mac = [ 0x02; 0x00; 0x00; 0x00; 0x00; 0x01 ]
let exp_eth_type = [ 0x08; 0x00 ]
let min_eth_payload = 46

let make_payload ~salt n =
  List.init n ~f:(fun index -> ((index * 37) + salt) land 0xFF)
;;

type observation =
  { accepted_app : int list
  ; frame : int list
  ; nibble_count : int
  ; tx_en_rises : int
  ; tx_en_falls : int
  ; first_tx_cycle : int
  ; last_tx_cycle : int
  ; saw_udp_busy : bool
  ; udp_busy_cleared : bool
  ; saw_tx_busy : bool
  ; tx_busy_cleared : bool
  ; saw_source_bubble : bool
  ; saw_source_backpressure : bool
  ; source_stable_while_waiting : bool
  }

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Udp_mac_top.create ~rx_fifo_for_sim:true scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let global_cycle = ref 0 in
  let cycle () =
    Cyclesim.cycle sim;
    incr global_cycle
  in
  let bit signal = Bits.to_bool !signal in

  let reset () =
    i.Udp_mac_top.I.rx_reset <-- 1;
    i.tx_reset <-- 1;
    i.en <-- 0;
    i.tx_start <-- 0;
    i.payload_len <-- 0;
    i.payload_tdata <-- 0;
    i.payload_tvalid <-- 0;
    i.rx_dv <-- 0;
    i.rx_er <-- 0;
    i.rx_data <-- 0;
    i.m_axis_tready <-- 0;
    cycle ();
    i.rx_reset <-- 0;
    i.tx_reset <-- 0;
    i.en <-- 1
  in

  (* [bubble_before index] is the number of invalid source cycles inserted
     before offering that application byte.  Once valid is raised for a byte,
     it and the data are held until payload_tready accepts the transfer. *)
  let transmit ?(bubble_before = fun _ -> 0) app =
    if bit o.udp_busy || bit o.tx_busy
    then failwith "attempted to start a datagram while the transmitter was busy";
    let data = Array.of_list app in
    let length = Array.length data in
    let pointer = ref 0 in
    let accepted_app = ref [] in
    let bubbles_left = ref (if length = 0 then 0 else bubble_before 0) in
    let nibbles = ref [] in
    let tx_en_previous = ref false in
    let tx_en_rises = ref 0 in
    let tx_en_falls = ref 0 in
    let first_tx_cycle = ref None in
    let last_tx_cycle = ref None in
    let saw_udp_busy = ref false in
    let saw_tx_busy = ref false in
    let saw_source_bubble = ref false in
    let saw_source_backpressure = ref false in
    let source_stable_while_waiting = ref true in
    let previous_waiting_beat = ref None in
    let idle_after_frame = ref 0 in
    let started = ref false in
    let cycles = ref 0 in
    let timeout = 512 + (length * 12) in
    while !idle_after_frame < 4 && !cycles < timeout do
      i.tx_start <-- (if !started then 0 else 1);
      i.payload_len <-- length;

      let has_data = !pointer < length in
      let source_valid = has_data && !bubbles_left = 0 in
      let source_data = if has_data then data.(!pointer) else 0 in
      i.payload_tvalid <-- (if source_valid then 1 else 0);
      i.payload_tdata <-- source_data;

      (match !previous_waiting_beat with
       | Some previous_data ->
         if (not source_valid) || source_data <> previous_data
         then source_stable_while_waiting := false
       | None -> ());

      let ready = bit o.Udp_mac_top.O.payload_tready in
      let accepted = source_valid && ready in
      if source_valid && not ready then saw_source_backpressure := true;
      if has_data && not source_valid then saw_source_bubble := true;
      previous_waiting_beat :=
        (if source_valid && not ready then Some source_data else None);

      cycle ();
      started := true;
      if accepted
      then (
        accepted_app := source_data :: !accepted_app;
        incr pointer;
        bubbles_left :=
          (if !pointer < length then bubble_before !pointer else 0))
      else if !bubbles_left > 0
      then decr bubbles_left;

      let tx_en = bit o.tx_en in
      if tx_en && not !tx_en_previous
      then (
        incr tx_en_rises;
        if Option.is_none !first_tx_cycle then first_tx_cycle := Some !global_cycle);
      if (not tx_en) && !tx_en_previous
      then (
        incr tx_en_falls;
        last_tx_cycle := Some (!global_cycle - 1));
      if tx_en
      then (
        idle_after_frame := 0;
        nibbles := Bits.to_int_trunc !(o.tx_d) :: !nibbles)
      else if !tx_en_falls > 0
      then incr idle_after_frame;
      tx_en_previous := tx_en;
      if bit o.udp_busy then saw_udp_busy := true;
      if bit o.tx_busy then saw_tx_busy := true;
      incr cycles
    done;
    i.tx_start <-- 0;
    i.payload_tvalid <-- 0;
    if !cycles >= timeout
    then
      failwithf
        "UDP TX integration timeout: app=%d accepted=%d tx_en_rises=%d"
        length
        !pointer
        !tx_en_rises
        ();
    if !pointer <> length
    then
      failwithf
        "UDP TX source incomplete: accepted %d of %d application bytes"
        !pointer
        length
        ();
    let nibbles = List.rev !nibbles in
    let rec bytes_of_nibbles = function
      | low :: high :: rest -> ((high lsl 4) lor low) :: bytes_of_nibbles rest
      | _ -> []
    in
    { accepted_app = List.rev !accepted_app
    ; frame = bytes_of_nibbles nibbles
    ; nibble_count = List.length nibbles
    ; tx_en_rises = !tx_en_rises
    ; tx_en_falls = !tx_en_falls
    ; first_tx_cycle = Option.value_exn !first_tx_cycle
    ; last_tx_cycle = Option.value_exn !last_tx_cycle
    ; saw_udp_busy = !saw_udp_busy
    ; udp_busy_cleared = not (bit o.udp_busy)
    ; saw_tx_busy = !saw_tx_busy
    ; tx_busy_cleared = not (bit o.tx_busy)
    ; saw_source_bubble = !saw_source_bubble
    ; saw_source_backpressure = !saw_source_backpressure
    ; source_stable_while_waiting = !source_stable_while_waiting
    }
  in

  let pass_count = ref 0 in
  let check_count = ref 0 in
  let expect label condition =
    incr check_count;
    if condition then incr pass_count;
    printf "  %-54s %s\n" label (if condition then "PASS" else "FAIL")
  in
  let slice list ~pos ~len =
    if pos + len <= List.length list then List.sub list ~pos ~len else []
  in
  let check_frame ~label ~app observation =
    printf "\n-- %s: %d application bytes --\n" label (List.length app);
    let datagram = golden_datagram ~app in
    let pad_length = Int.max 0 (min_eth_payload - List.length datagram) in
    let padding = List.init pad_length ~f:(fun _ -> 0) in
    let wire_payload = datagram @ padding in
    let crc_input = exp_dst_mac @ exp_src_mac @ exp_eth_type @ wire_payload in
    let fcs = bytes_of_int ~n:4 (sw_crc crc_input) in
    let expected =
      exp_preamble
      @ [ 0xD5 ]
      @ exp_dst_mac
      @ exp_src_mac
      @ exp_eth_type
      @ wire_payload
      @ fcs
    in
    let frame = observation.frame in
    let ip_offset = 22 in
    let udp_offset = ip_offset + 20 in
    let padding_offset = ip_offset + List.length datagram in
    expect
      (label ^ ": application transfers exactly once")
      (List.equal Int.equal observation.accepted_app app);
    expect
      (label ^ ": source stable while backpressured")
      observation.source_stable_while_waiting;
    expect
      (label ^ ": udp_busy asserts and clears")
      (observation.saw_udp_busy && observation.udp_busy_cleared);
    expect
      (label ^ ": tx_busy asserts and clears")
      (observation.saw_tx_busy && observation.tx_busy_cleared);
    expect
      (label ^ ": exactly one contiguous tx_en interval")
      (observation.tx_en_rises = 1 && observation.tx_en_falls = 1);
    expect
      (label ^ ": complete low-nibble/high-nibble pairs")
      (observation.nibble_count = 2 * List.length frame);
    expect
      (label ^ ": exact complete Ethernet frame")
      (List.equal Int.equal frame expected);
    expect
      (label ^ ": IPv4 version/IHL and UDP protocol")
      (List.equal Int.equal (slice frame ~pos:ip_offset ~len:2) [ 0x45; 0x00 ]
       && List.equal Int.equal (slice frame ~pos:(ip_offset + 9) ~len:1) [ 17 ]);
    let total_length = 28 + List.length app in
    expect
      (label ^ ": IPv4 total length")
      (List.equal
         Int.equal
         (slice frame ~pos:(ip_offset + 2) ~len:2)
         [ hi8 total_length; lo8 total_length ]);
    let checksum = ip_checksum ~total_length in
    expect
      (label ^ ": IPv4 checksum")
      (List.equal
         Int.equal
         (slice frame ~pos:(ip_offset + 10) ~len:2)
         [ hi8 checksum; lo8 checksum ]);
    expect
      (label ^ ": IPv4 addresses")
      (List.equal Int.equal (slice frame ~pos:(ip_offset + 12) ~len:8) (src_ip @ dst_ip));
    let udp_length = 8 + List.length app in
    expect
      (label ^ ": UDP ports, length, and zero checksum")
      (List.equal
         Int.equal
         (slice frame ~pos:udp_offset ~len:8)
         [ hi8 src_port
         ; lo8 src_port
         ; hi8 dst_port
         ; lo8 dst_port
         ; hi8 udp_length
         ; lo8 udp_length
         ; 0
         ; 0
         ]);
    expect
      (label ^ ": padding starts after IPv4 total length")
      (List.equal Int.equal (slice frame ~pos:padding_offset ~len:pad_length) padding);
    expect
      (label ^ ": FCS covers header and padded payload")
      (List.equal Int.equal (slice frame ~pos:(22 + List.length wire_payload) ~len:4) fcs)
  in

  (* The TX FIFO holds 128 IPv4-payload bytes.  Since the composition is
     store-and-forward and contributes 20 IPv4 + 8 UDP header bytes, 100 bytes
     is the largest application payload this integration can buffer. *)
  let directed_lengths = [ 0; 1; 17; 18; 19; 40; 99; 100 ] in
  List.iteri directed_lengths ~f:(fun case_index length ->
    reset ();
    let app = make_payload ~salt:(0x31 + case_index) length in
    let observation = transmit app in
    check_frame ~label:(sprintf "length boundary %dB" length) ~app observation;
    if length > 0
    then
      expect
        (sprintf "length boundary %dB: application backpressure exercised" length)
        observation.saw_source_backpressure);

  reset ();
  let bubbled_app = make_payload ~salt:0xA7 23 in
  let bubble_before index =
    if index = 0
    then 3
    else if index = List.length bubbled_app - 1
    then 3
    else if index % 5 = 2
    then 2
    else 0
  in
  let bubbled = transmit ~bubble_before bubbled_app in
  check_frame ~label:"source-valid bubbles" ~app:bubbled_app bubbled;
  expect "source-valid bubbles: bubbles exercised" bubbled.saw_source_bubble;
  expect
    "source-valid bubbles: backpressure exercised"
    bubbled.saw_source_backpressure;

  printf "\n-- no-reset multi-frame sequence --\n";
  reset ();
  let sequence = [ 4; 40; 0; 18 ] in
  let observations =
    List.mapi sequence ~f:(fun index length ->
      let app = make_payload ~salt:(0xD0 + index) length in
      let observation = transmit app in
      check_frame
        ~label:(sprintf "sequence frame %d (%dB)" (index + 1) length)
        ~app
        observation;
      observation)
  in
  List.iter2_exn
    (List.drop_last_exn observations)
    (List.tl_exn observations)
    ~f:(fun previous next ->
    let idle_cycles = next.first_tx_cycle - previous.last_tx_cycle - 1 in
    expect
      (sprintf
         "sequence: observed MII inter-frame gap is at least 24 cycles (%d)"
         idle_cycles)
      (idle_cycles >= 24));

  printf
    "\n==== SUMMARY: %d/%d checks passed ====\n"
    !pass_count
    !check_count;
  print_endline "\n=== SIMULATION COMPLETE ===";
  if !pass_count <> !check_count then failwith "UDP+MAC integration test failures"
;;
