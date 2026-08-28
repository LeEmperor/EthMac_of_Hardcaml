(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_duplex_mac_top_legacy_assertion_test.ml" *)
(* Superseded full-duplex UDP/MAC harness. Tags: [{ "DEPRECATED" ; "TEST" }] *)

(*
 * Full-duplex UDP-over-MAC integration testbench (Harness #1 top).
 *
 * Exercises [Udp_duplex_mac_top] — the union of the TX and RX composition stacks
 * around ONE [Mac_top], with the two directions INDEPENDENT (no bridge). It proves
 * the merge: both stacks work, and they work *simultaneously* on the single MAC.
 *
 *     RX:  MII RX nibbles ─→ Mac_top ─→ Ipv4_rx ─→ Udp_rx ─→ app        (recover)
 *     TX:  app bytes ─→ Udp_tx ─→ Ipv4_tx ─→ Mac_top ─→ MII TX nibbles  (emit)
 *
 * Three tests:
 *   1. RX only — recover a host-shaped datagram's payload + metadata.
 *   2. TX only — the emitted MII frame decodes to the golden IPv4/UDP frame.
 *   3. Concurrent — an RX frame buffered in the MAC FIFO drains WHILE a TX datagram
 *      is streamed out; both are checked, confirming the two stacks coexist.
 *
 * Single-clock Cyclesim with [rx_fifo_for_sim:true] (sync RX FIFO; rx/tx advance
 * together). Frame/CRC builders are the same shape as udp_mac_rx_tb / udp_mac_top_tb.
 *)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Helper_tb_functions

let () = print_endline "=== Running UDP Duplex (full-duplex) Integration Testbench ==="

module Sim = Cyclesim.With_interface (Udp_duplex_mac_top.I) (Udp_duplex_mac_top.O)

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
  in
  let udp =
    [ hi8 src_port
    ; lo8 src_port
    ; hi8 dst_port
    ; lo8 dst_port
    ; hi8 udp_length
    ; lo8 udp_length
    ; 0x00
    ; 0x00
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
  let feedback = crc land 1 lxor bit land 1 in
  let shifted = crc lsr 1 in
  if feedback = 1 then shifted lxor 0xEDB88320 else shifted
;;

let sw_crc_byte crc byte =
  let crc = ref crc in
  for i = 0 to 7 do
    crc := sw_crc_bit !crc ((byte lsr i) land 1)
  done;
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

(* Expected full MII TX frame (preamble..FCS) for a given application payload. *)
let expected_tx_frame ~app =
  let datagram = ipv4_udp_eth_payload ~payload:app () in
  let pad = List.init (Int.max 0 (46 - List.length datagram)) ~f:(fun _ -> 0) in
  let wire_payload = datagram @ pad in
  let crc_input = tx_dst_mac @ tx_src_mac @ eth_type @ wire_payload in
  let fcs = compute_fcs crc_input in
  List.init 7 ~f:(fun _ -> 0x55)
  @ [ 0xD5 ]
  @ tx_dst_mac
  @ tx_src_mac
  @ eth_type
  @ wire_payload
  @ fcs
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
  { tx_frame : int list (* reassembled MII TX bytes (empty if none emitted) *)
  ; rx_payload : int list (* recovered UDP application bytes *)
  ; rx_meta : meta option
  ; rx_crc_error : bool
  }

(* ── assertion helpers ──────────────────────────────────────────────────────── *)
let all_ok = ref true

let check name cond =
  if not cond then all_ok := false;
  printf "  %-56s %s\n" name (if cond then "PASS" else "FAIL")
;;

let bit s = Bits.to_bool !s

let rec bytes_of_nibbles = function
  | low :: high :: rest -> ((high lsl 4) lor low) :: bytes_of_nibbles rest
  | _ -> []
;;

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Udp_duplex_mac_top.create ~rx_fifo_for_sim:true scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  let ( <-- ) r v = r := Bits.of_int_trunc ~width:(Bits.width !r) v in
  let quiet_tx () =
    i.Udp_duplex_mac_top.I.tx_start <-- 0;
    i.payload_len <-- 0;
    i.payload_tdata <-- 0;
    i.payload_tvalid <-- 0
  in
  let quiet_rx () =
    i.rx_dv <-- 0;
    i.rx_er <-- 0;
    i.rx_data <-- 0
  in
  let reset () =
    i.en <-- 0;
    i.rx_reset <-- 1;
    i.tx_reset <-- 1;
    i.app_tready <-- 0;
    quiet_tx ();
    quiet_rx ();
    cycle ();
    i.en <-- 1;
    i.rx_reset <-- 0;
    i.tx_reset <-- 0;
    cycle ()
  in
  (* Clock a whole Ethernet frame onto the MII RX pins. app_tready stays low so the
     recovered payload buffers in the MAC RX FIFO for later draining. *)
  let clock_in_rx_frame ?(corrupt_fcs = false) eth_payload =
    let body = rx_dst_mac @ rx_src_mac @ eth_type @ eth_payload in
    let fcs =
      let f = compute_fcs body in
      if corrupt_fcs then List.map f ~f:(fun b -> b lxor 0xFF) else f
    in
    i.rx_dv <-- 1;
    let send_byte b = send_byte ~cycle ~t_in:i.rx_data b in
    for _ = 1 to 7 do
      send_byte 0x55
    done;
    send_byte 0xD5;
    List.iter body ~f:send_byte;
    List.iter fcs ~f:send_byte;
    i.rx_dv <-- 0;
    cycle ()
  in
  (* Run [n_cycles], draining the recovered RX stream (app_tready high) while also driving
     the TX application interface for [tx_app] (or nothing when empty). Captures the MII
     TX nibbles and the recovered RX bytes + metadata + status. *)
  let run ?(tx_app = []) ~settle () =
    let data = Array.of_list tx_app in
    let length = Array.length data in
    let ptr = ref 0 in
    let started = ref (length = 0) in
    (* nothing to start if no TX payload *)
    let nibbles = ref [] in
    let rx_got = ref [] in
    let rx_meta = ref None in
    let rx_crc_error = ref false in
    (* idle counting must only begin AFTER a transmit burst ends: the MAC is
       store-and-forward, so tx_en is legitimately LOW while the datagram fills the TX
       FIFO. Count idle only once tx_en has risen and fallen at least once. *)
    let tx_en_prev = ref false in
    let saw_tx_fall = ref false in
    let idle_after = ref 0 in
    let settle_left = ref settle in
    let continue = ref true in
    let cycles = ref 0 in
    let timeout = 1024 + (length * 12) in
    i.app_tready <-- 1;
    while !continue && !cycles < timeout do
      (* drive TX *)
      if length > 0
      then (
        i.tx_start <-- if !started then 0 else 1;
        i.payload_len <-- length;
        let has_data = !ptr < length in
        i.payload_tvalid <-- if has_data then 1 else 0;
        i.payload_tdata <-- if has_data then data.(!ptr) else 0)
      else quiet_tx ();
      let ready = bit o.Udp_duplex_mac_top.O.payload_tready in
      let accepted = length > 0 && !ptr < length && ready in
      (* sample RX drain (combinational outputs valid this cycle) *)
      if bit o.app_start
      then
        rx_meta
        := Some
             { src_port = Bits.to_int_trunc !(o.src_port)
             ; dst_port = Bits.to_int_trunc !(o.dst_port)
             ; udp_length = Bits.to_int_trunc !(o.udp_length)
             ; payload_length = Bits.to_int_trunc !(o.payload_length)
             ; src_ip = Bits.to_int_trunc !(o.src_ip)
             ; dst_ip = Bits.to_int_trunc !(o.dst_ip)
             };
      if bit o.app_tvalid then rx_got := Bits.to_int_trunc !(o.app_tdata) :: !rx_got;
      if bit o.crc_error then rx_crc_error := true;
      let tx_en = bit o.tx_en in
      if tx_en then nibbles := Bits.to_int_trunc !(o.tx_d) :: !nibbles;
      cycle ();
      started := true;
      if accepted then incr ptr;
      if !tx_en_prev && not tx_en then saw_tx_fall := true;
      tx_en_prev := tx_en;
      if tx_en then idle_after := 0 else if !saw_tx_fall then incr idle_after;
      (* TX done = no TX requested, or a burst completed and the wire went idle *)
      let tx_done = length = 0 || (!saw_tx_fall && !idle_after >= 6) in
      if tx_done then decr settle_left;
      if tx_done && !settle_left <= 0 then continue := false;
      incr cycles
    done;
    quiet_tx ();
    if length > 0 && !ptr <> length
    then failwithf "TX incomplete: accepted %d of %d app bytes" !ptr length ();
    { tx_frame = bytes_of_nibbles (List.rev !nibbles)
    ; rx_payload = List.rev !rx_got
    ; rx_meta = !rx_meta
    ; rx_crc_error = !rx_crc_error
    }
  in
  let expect_rx ~label ~result ~payload =
    check
      (label ^ ": RX payload recovered")
      (List.equal Int.equal result.rx_payload payload);
    match result.rx_meta with
    | None -> check (label ^ ": RX metadata captured") false
    | Some m ->
      check (label ^ ": RX src/dst port") (m.src_port = src_port && m.dst_port = dst_port);
      check (label ^ ": RX udp length") (m.udp_length = 8 + List.length payload);
      check (label ^ ": RX payload length") (m.payload_length = List.length payload);
      check (label ^ ": RX src/dst ip") (m.src_ip = ip32 src_ip && m.dst_ip = ip32 dst_ip)
  in
  let expect_tx ~label ~result ~app =
    check
      (label ^ ": TX frame == golden IPv4/UDP frame")
      (List.equal Int.equal result.tx_frame (expected_tx_frame ~app))
  in
  (* ── test 1: RX only ─────────────────────────────────────────────────────── *)
  printf "\n-- test 1: RX-only datagram recovery --\n";
  reset ();
  let rx_payload1 = List.init 18 ~f:(fun k -> k + 1) in
  clock_in_rx_frame (ipv4_udp_eth_payload ~payload:rx_payload1 ());
  let r1 = run ~settle:80 () in
  expect_rx ~label:"test 1" ~result:r1 ~payload:rx_payload1;
  check "test 1: no RX CRC error on a good frame" (not r1.rx_crc_error);
  check "test 1: TX stayed idle" (List.is_empty r1.tx_frame);
  (* ── test 2: TX only ─────────────────────────────────────────────────────── *)
  printf "\n-- test 2: TX-only datagram emission --\n";
  reset ();
  let tx_app2 = List.init 18 ~f:(fun k -> ((k * 37) + 5) land 0xFF) in
  let r2 = run ~tx_app:tx_app2 ~settle:8 () in
  expect_tx ~label:"test 2" ~result:r2 ~app:tx_app2;
  check "test 2: no RX payload emitted" (List.is_empty r2.rx_payload);
  (* ── test 3: concurrent RX drain + TX emit on the one MAC ────────────────── *)
  printf "\n-- test 3: concurrent full-duplex (RX drains while TX emits) --\n";
  reset ();
  let rx_payload3 = List.init 12 ~f:(fun k -> (0xA0 + k) land 0xFF) in
  let tx_app3 = List.init 20 ~f:(fun k -> (0x80 + (k * 11)) land 0xFF) in
  clock_in_rx_frame (ipv4_udp_eth_payload ~payload:rx_payload3 ());
  let r3 = run ~tx_app:tx_app3 ~settle:80 () in
  expect_rx ~label:"test 3" ~result:r3 ~payload:rx_payload3;
  expect_tx ~label:"test 3" ~result:r3 ~app:tx_app3;
  check "test 3: no RX CRC error while transmitting" (not r3.rx_crc_error);
  printf "\n==== SUMMARY: %s ====\n" (if !all_ok then "ALL PASS" else "FAILURES");
  print_endline "\n=== SIMULATION COMPLETE ===";
  if not !all_ok then exit 1
;;

(* End of the legacy duplex harness. *)
