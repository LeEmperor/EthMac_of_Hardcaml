(*
  Echo / loopback UDP-over-MAC integration testbench (Harness #2 top).

  Exercises [Udp_loopback_mac_top] — one [Mac_top], both composition stacks, and an
  RX->TX **bridge FSM** that feeds the recovered application stream straight back
  into the UDP TX interface. Unlike the duplex top, there is NO external TX app
  interface and NO app_tready input: the echo is entirely RX-triggered, and the
  bridge drives RX backpressure internally.

      MII RX nibbles ─→ Mac_top ─→ Ipv4_rx ─→ Udp_rx ─→ bridge ─→ Udp_tx ─→ Ipv4_tx
                                                                    ─→ Mac_top ─→ MII TX

  The end-to-end proof is the echo: drive a full IPv4/UDP frame onto the MII RX
  pins, capture the re-emitted MII TX nibbles, reassemble them, and assert the
  echoed frame == the golden IPv4/UDP frame carrying the SAME application payload
  (which also pins the regenerated IPv4 header checksum + Ethernet FCS, since the
  golden builder computes both). RX metadata (ports/ips/length) is checked off the
  held app_start pulse.

  Four tests:
    1. normal 18-byte datagram (no MAC pad)              — echo == golden.
    2. alternating 0xAA/0x55 payload                      — echo == golden.
    3. short (4-byte) datagram, MAC-padded to 46 bytes    — echo re-pads correctly.
    4. bad-FCS-in                                          — forward-everything policy:
       payload survives, so the echo still matches golden, and crc_error is flagged.

  Single-clock Cyclesim with [rx_fifo_for_sim:true]. Builders match udp_duplex_tb.
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Helper_tb_functions

let () = print_endline "=== Running UDP Loopback (echo) Integration Testbench ==="

module Sim = Cyclesim.With_interface (Udp_loopback_mac_top.I) (Udp_loopback_mac_top.O)

(* ── shared endpoints (match the top's constants + udp_app.py golden values) ─── *)
let src_ip = [ 192; 168; 1; 10 ]
let dst_ip = [ 192; 168; 1; 1 ]
let src_port = 0x1234
let dst_port = 0x1235

(* ── byte builders (network order) ──────────────────────────────────────────── *)
let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF
let w16 hi lo = ((hi land 0xFF) lsl 8) lor (lo land 0xFF)
let ip32 bytes = List.fold bytes ~init:0 ~f:(fun acc b -> (acc lsl 8) lor (b land 0xFF))

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
  let rec fold s = if s > 0xFFFF then fold ((s land 0xFFFF) + (s lsr 16)) else s in
  lnot (fold sum) land 0xFFFF
;;

(* IPv4 header ++ UDP datagram, optionally MAC-padded to the 46-byte minimum. *)
let ipv4_udp_eth_payload ?(ethernet_padding = false) ~payload () =
  let n = List.length payload in
  let udp_length = 8 + n in
  let total_length = 20 + udp_length in
  let checksum = ip_checksum ~total_length in
  let header =
    [ 0x45; 0x00; hi8 total_length; lo8 total_length
    ; 0x00; 0x00; 0x40; 0x00
    ; 0x40; 0x11; hi8 checksum; lo8 checksum
    ]
    @ src_ip @ dst_ip
  in
  let udp =
    [ hi8 src_port; lo8 src_port
    ; hi8 dst_port; lo8 dst_port
    ; hi8 udp_length; lo8 udp_length
    ; 0x00; 0x00
    ]
    @ payload
  in
  let datagram = header @ udp in
  if ethernet_padding
  then datagram @ List.init (Int.max 0 (46 - List.length datagram)) ~f:(fun _ -> 0)
  else datagram
;;

(* ── reflected Ethernet CRC-32 / FCS ────────────────────────────────────────── *)
let sw_crc_bit crc bit =
  let feedback = ((crc land 1) lxor bit) land 1 in
  let shifted = crc lsr 1 in
  if feedback = 1 then shifted lxor 0xEDB88320 else shifted
;;

let sw_crc_byte crc byte =
  let crc = ref crc in
  for i = 0 to 7 do crc := sw_crc_bit !crc ((byte lsr i) land 1) done;
  !crc
;;

let sw_crc bytes = List.fold bytes ~init:0xFFFFFFFF ~f:sw_crc_byte
let bytes_of_int ~n x = List.init n ~f:(fun i -> (x lsr (8 * i)) land 0xFF)
let compute_fcs frame_bytes = bytes_of_int ~n:4 (sw_crc frame_bytes lxor 0xFFFF_FFFF)

(* arbitrary RX MACs (Ipv4_rx ignores them); the MAC's own TX MACs are fixed in RTL *)
let rx_dst_mac = [ 0x36; 0x12; 0x73; 0x36; 0x24; 0x85 ]
let rx_src_mac = [ 0x37; 0x52; 0x33; 0x76; 0x94; 0x05 ]
let tx_dst_mac = [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]
let tx_src_mac = [ 0x02; 0x00; 0x00; 0x00; 0x00; 0x01 ]
let eth_type = [ 0x08; 0x00 ]

(* Expected full MII TX frame (preamble..FCS) for a given application payload. The
   golden builder recomputes the IPv4 checksum and Ethernet FCS, so an exact match
   validates the echo end-to-end (recovered payload + regenerated headers/FCS). *)
let expected_tx_frame ~app =
  let datagram = ipv4_udp_eth_payload ~payload:app () in
  let pad = List.init (Int.max 0 (46 - List.length datagram)) ~f:(fun _ -> 0) in
  let wire_payload = datagram @ pad in
  let crc_input = tx_dst_mac @ tx_src_mac @ eth_type @ wire_payload in
  let fcs = compute_fcs crc_input in
  List.init 7 ~f:(fun _ -> 0x55)
  @ [ 0xD5 ]
  @ tx_dst_mac @ tx_src_mac @ eth_type @ wire_payload @ fcs
;;

type meta =
  { src_port : int
  ; dst_port : int
  ; udp_length : int
  ; payload_length : int
  ; src_ip : int
  ; dst_ip : int
  }

type result =
  { tx_frame : int list       (* reassembled MII TX bytes (empty if no echo) *)
  ; rx_meta : meta option
  ; rx_crc_error : bool
  }

(* ── assertion helpers ──────────────────────────────────────────────────────── *)
let all_ok = ref true

let check name cond =
  if not cond then all_ok := false;
  printf "  %-56s %s\n" name (if cond then "PASS" else "FAIL")
;;

let rec bytes_of_nibbles = function
  | low :: high :: rest -> ((high lsl 4) lor low) :: bytes_of_nibbles rest
  | _ -> []
;;

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Udp_loopback_mac_top.create ~rx_fifo_for_sim:true scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let ( <-- ) r v = r := Bits.of_int_trunc ~width:(Bits.width !r) v in

  (* app_start is HELD only until the bridge accepts the first recovered byte —
     which, being cut-through, happens while the frame is still clocking in. So
     sample RX metadata + crc_error on EVERY cycle (frame clock-in included), not
     just during the echo-capture window. *)
  let g_rx_meta = ref None in
  let g_rx_crc_error = ref false in
  let bit s = Bits.to_bool !s in
  let cycle () =
    if bit o.Udp_loopback_mac_top.O.app_start
    then
      g_rx_meta :=
        Some
          { src_port = Bits.to_int_trunc !(o.src_port)
          ; dst_port = Bits.to_int_trunc !(o.dst_port)
          ; udp_length = 8 + Bits.to_int_trunc !(o.payload_length)
          ; payload_length = Bits.to_int_trunc !(o.payload_length)
          ; src_ip = Bits.to_int_trunc !(o.src_ip)
          ; dst_ip = Bits.to_int_trunc !(o.dst_ip)
          };
    if bit o.crc_error then g_rx_crc_error := true;
    Cyclesim.cycle sim
  in

  let quiet_rx () =
    i.Udp_loopback_mac_top.I.rx_dv <-- 0;
    i.rx_er <-- 0;
    i.rx_data <-- 0
  in
  let reset () =
    g_rx_meta := None;
    g_rx_crc_error := false;
    i.en <-- 0;
    i.rx_reset <-- 1;
    i.tx_reset <-- 1;
    quiet_rx ();
    cycle ();
    i.en <-- 1;
    i.rx_reset <-- 0;
    i.tx_reset <-- 0;
    cycle ()
  in

  (* Clock a whole Ethernet frame onto the MII RX pins. The bridge drives RX
     backpressure internally, so no draining knob is needed here. *)
  let clock_in_rx_frame ?(corrupt_fcs = false) eth_payload =
    let body = rx_dst_mac @ rx_src_mac @ eth_type @ eth_payload in
    let fcs =
      let f = compute_fcs body in
      if corrupt_fcs then List.map f ~f:(fun b -> b lxor 0xFF) else f
    in
    i.rx_dv <-- 1;
    let send_byte b = send_byte ~cycle ~t_in:i.rx_data b in
    for _ = 1 to 7 do send_byte 0x55 done;
    send_byte 0xD5;
    List.iter body ~f:send_byte;
    List.iter fcs ~f:send_byte;
    i.rx_dv <-- 0;
    cycle ()
  in

  (* Free-run, capturing the MII TX echo (nibbles while tx_en), the RX metadata
     (latched on the held app_start pulse), and any RX CRC error. Stops once the
     store-and-forward TX burst has risen, fallen, and the wire has been idle a
     while (the MAC legitimately holds tx_en LOW while the echo fills the TX FIFO). *)
  let capture ~settle () =
    let nibbles = ref [] in
    let tx_en_prev = ref false in
    let saw_tx_fall = ref false in
    let idle_after = ref 0 in
    let settle_left = ref settle in
    let continue = ref true in
    let cycles = ref 0 in
    let timeout = 4096 in
    while !continue && !cycles < timeout do
      let tx_en = bit o.Udp_loopback_mac_top.O.tx_en in
      if tx_en then nibbles := Bits.to_int_trunc !(o.tx_d) :: !nibbles;
      cycle ();
      if !tx_en_prev && not tx_en then saw_tx_fall := true;
      tx_en_prev := tx_en;
      if tx_en then idle_after := 0 else if !saw_tx_fall then incr idle_after;
      let tx_done = !saw_tx_fall && !idle_after >= 6 in
      if tx_done then decr settle_left;
      if tx_done && !settle_left <= 0 then continue := false;
      incr cycles
    done;
    { tx_frame = bytes_of_nibbles (List.rev !nibbles)
    ; rx_meta = !g_rx_meta
    ; rx_crc_error = !g_rx_crc_error
    }
  in

  let expect_echo ~label ~result ~app =
    check (label ^ ": echo frame == golden IPv4/UDP frame")
      (List.equal Int.equal result.tx_frame (expected_tx_frame ~app));
    (match result.rx_meta with
     | None -> check (label ^ ": RX metadata captured") false
     | Some m ->
       check (label ^ ": RX src/dst port") (m.src_port = src_port && m.dst_port = dst_port);
       check (label ^ ": RX payload length") (m.payload_length = List.length app);
       check (label ^ ": RX udp length") (m.udp_length = 8 + List.length app);
       check (label ^ ": RX src/dst ip") (m.src_ip = ip32 src_ip && m.dst_ip = ip32 dst_ip))
  in

  (* ── test 1: normal 18-byte datagram (no MAC pad) ────────────────────────── *)
  printf "\n-- test 1: normal 18-byte datagram echo --\n";
  reset ();
  let app1 = List.init 18 ~f:(fun k -> k + 1) in
  clock_in_rx_frame (ipv4_udp_eth_payload ~payload:app1 ());
  let r1 = capture ~settle:8 () in
  expect_echo ~label:"test 1" ~result:r1 ~app:app1;
  check "test 1: no RX CRC error on a good frame" (not r1.rx_crc_error);

  (* ── test 2: alternating 0xAA/0x55 payload ───────────────────────────────── *)
  printf "\n-- test 2: alternating 0xAA/0x55 payload echo --\n";
  reset ();
  let app2 = List.init 18 ~f:(fun k -> if k % 2 = 0 then 0xAA else 0x55) in
  clock_in_rx_frame (ipv4_udp_eth_payload ~payload:app2 ());
  let r2 = capture ~settle:8 () in
  expect_echo ~label:"test 2" ~result:r2 ~app:app2;
  check "test 2: no RX CRC error on a good frame" (not r2.rx_crc_error);

  (* ── test 3: short 4-byte datagram, MAC-padded in, re-padded on echo ──────── *)
  printf "\n-- test 3: short (4-byte) MAC-padded datagram echo --\n";
  reset ();
  let app3 = [ 0xDE; 0xAD; 0xBE; 0xEF ] in
  clock_in_rx_frame (ipv4_udp_eth_payload ~ethernet_padding:true ~payload:app3 ());
  let r3 = capture ~settle:8 () in
  expect_echo ~label:"test 3" ~result:r3 ~app:app3;
  check "test 3: no RX CRC error on a good frame" (not r3.rx_crc_error);

  (* ── test 4: bad-FCS-in — forward-everything policy ──────────────────────── *)
  printf "\n-- test 4: bad-FCS input (echoed-but-flagged policy) --\n";
  reset ();
  let app4 = List.init 18 ~f:(fun k -> (k * 13 + 7) land 0xFF) in
  clock_in_rx_frame ~corrupt_fcs:true (ipv4_udp_eth_payload ~payload:app4 ());
  let r4 = capture ~settle:8 () in
  (* payload bytes are intact (only the trailing FCS was corrupted), so the echo
     still matches golden; the difference is the flagged CRC error. *)
  expect_echo ~label:"test 4" ~result:r4 ~app:app4;
  check "test 4: RX CRC error flagged on the bad-FCS frame" r4.rx_crc_error;

  printf "\n==== SUMMARY: %s ====\n" (if !all_ok then "ALL PASS" else "FAILURES");
  print_endline "\n=== SIMULATION COMPLETE ===";
  if not !all_ok then exit 1
;;
