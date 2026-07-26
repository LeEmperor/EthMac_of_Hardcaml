(*
  Top-level UDP receive integration testbench.

  Drives real MII RX nibbles (preamble / SFD / dst_mac / src_mac / ethertype /
  IPv4 header / UDP header / app payload / FCS) into [Udp_rx_mac_top] and checks
  the recovered application datagram + metadata out of the UDP layer:

      MII nibbles ─→ Mac_top ─→ Ipv4_rx ─→ Udp_rx ─→ app

  This is the RX mirror of udp_mac_top_tb.ml, and the piece that
  udp_ipv4_rx_tb.ml could not cover: the real MAC-RX → m_axis → Ipv4_rx handoff
  (SOF + rx_eth_type sidebands, cut-through RX FIFO), exercised from the wire.

  Single-clock Cyclesim with [rx_fifo_for_sim:true] (the async RX FIFO is swapped
  for a sync one, so rx_clock and tx_clock advance together).
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Helper_tb_functions

let () = print_endline "=== Running UDP-over-MAC RX Integration Testbench ==="

module Sim = Cyclesim.With_interface (Udp_rx_mac_top.I) (Udp_rx_mac_top.O)

(* ── frame-byte builders (network order), shared shape with udp_ipv4_rx_tb ──── *)
let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF
let w16 hi lo = ((hi land 0xFF) lsl 8) lor (lo land 0xFF)
let ip32 bytes = List.fold bytes ~init:0 ~f:(fun acc b -> (acc lsl 8) lor (b land 0xFF))

let ip_checksum ~total_length ~protocol ~src_ip ~dst_ip =
  let words =
    [ 0x4500
    ; total_length
    ; 0x0000
    ; 0x4000
    ; w16 0x40 protocol
    ; 0x0000
    ; w16 (List.nth_exn src_ip 0) (List.nth_exn src_ip 1)
    ; w16 (List.nth_exn src_ip 2) (List.nth_exn src_ip 3)
    ; w16 (List.nth_exn dst_ip 0) (List.nth_exn dst_ip 1)
    ; w16 (List.nth_exn dst_ip 2) (List.nth_exn dst_ip 3)
    ]
  in
  let sum = List.fold words ~init:0 ~f:( + ) in
  let rec fold sum = if sum > 0xFFFF then fold ((sum land 0xFFFF) + (sum lsr 16)) else sum in
  lnot (fold sum) land 0xFFFF
;;

let udp_datagram ~src_port ~dst_port ~checksum ~payload =
  let length = 8 + List.length payload in
  [ hi8 src_port; lo8 src_port
  ; hi8 dst_port; lo8 dst_port
  ; hi8 length;   lo8 length
  ; hi8 checksum; lo8 checksum
  ]
  @ payload
;;

(* Full Ethernet *payload* (IPv4 header ++ UDP datagram), optionally MAC-padded to
   the 46-byte minimum so the RX pad-strip path is exercised. *)
let ipv4_udp_eth_payload ?(ethernet_padding = false) ~src_ip ~dst_ip ~src_port ~dst_port
  ~udp_checksum ~payload () =
  let udp = udp_datagram ~src_port ~dst_port ~checksum:udp_checksum ~payload in
  let total_length = 20 + List.length udp in
  let checksum = ip_checksum ~total_length ~protocol:17 ~src_ip ~dst_ip in
  let header =
    [ 0x45; 0x00; hi8 total_length; lo8 total_length
    ; 0x00; 0x00; 0x40; 0x00
    ; 0x40; 0x11; hi8 checksum; lo8 checksum
    ]
    @ src_ip @ dst_ip
  in
  let datagram = header @ udp in
  if ethernet_padding
  then datagram @ List.init (Int.max 0 (46 - List.length datagram)) ~f:(fun _ -> 0)
  else datagram
;;

(* ── SW CRC-32 / FCS (identical convention to test/mii/rx_path_tb.ml) ───────── *)
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
let compute_fcs frame_bytes = bytes_of_int ~n:4 ((sw_crc frame_bytes) lxor 0xFFFF_FFFF)

(* arbitrary MACs — Ipv4_rx ignores them, they just precede the ethertype *)
let dst_mac = [ 0x36; 0x12; 0x73; 0x36; 0x24; 0x85 ]
let src_mac = [ 0x37; 0x52; 0x33; 0x76; 0x94; 0x05 ]

type metadata =
  { src_port : int
  ; dst_port : int
  ; udp_length : int
  ; payload_length : int
  ; src_ip : int
  ; dst_ip : int
  }

type run_result =
  { payload : int list
  ; metadata : metadata option
  ; crc_error : bool
  ; checksum_ok : bool
  }

let bit s = Bits.to_bool !s

(* Drive one whole Ethernet frame on the MII RX pins, then drain the recovered
   UDP application stream. app_tready is held low while the frame is clocked in so
   the MAC FIFO buffers it, then raised to drain — mirroring rx_path_tb. *)
let run ?(eth_type = [ 0x08; 0x00 ]) ?(corrupt_fcs = false) eth_payload =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Udp_rx_mac_top.create ~rx_fifo_for_sim:true scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  let ( <-- ) r v = r := Bits.of_int_trunc ~width:(Bits.width !r) v in
  (* reset *)
  i.Udp_rx_mac_top.I.app_tready <-- 0;
  i.en <-- 0;
  i.rx_reset <-- 1;
  i.tx_reset <-- 1;
  i.rx_dv <-- 0;
  i.rx_er <-- 0;
  i.rx_data <-- 0;
  cycle ();
  i.en <-- 1;
  i.rx_reset <-- 0;
  i.tx_reset <-- 0;
  cycle ();
  (* body over which the FCS is computed: everything from dst_mac to end of payload *)
  let body = dst_mac @ src_mac @ eth_type @ eth_payload in
  let fcs =
    let f = compute_fcs body in
    if corrupt_fcs then List.map f ~f:(fun b -> b lxor 0xFF) else f
  in
  (* clock the frame in with rx_dv asserted *)
  i.rx_dv <-- 1;
  let send_byte b = send_byte ~cycle ~t_in:i.rx_data b in
  for _ = 1 to 7 do send_byte 0x55 done;  (* preamble *)
  send_byte 0xD5;                          (* SFD *)
  List.iter body ~f:send_byte;
  List.iter fcs ~f:send_byte;
  i.rx_dv <-- 0;
  cycle ();
  (* drain: raise app_tready and collect the recovered application stream *)
  i.app_tready <-- 1;
  let got = ref [] in
  let meta = ref None in
  let crc_error = ref false in
  let checksum_ok = ref false in
  (* crc_error / checksum_ok are registered in Udp_rx (updated the cycle after its
     own rx_tlast), so they settle one cycle behind app_tlast. OR them across the
     whole drain window (after the frame starts) rather than sampling at tlast. *)
  for _ = 0 to 79 do
    if bit o.app_start
    then
      meta :=
        Some
          { src_port = Bits.to_int_trunc !(o.src_port)
          ; dst_port = Bits.to_int_trunc !(o.dst_port)
          ; udp_length = Bits.to_int_trunc !(o.udp_length)
          ; payload_length = Bits.to_int_trunc !(o.payload_length)
          ; src_ip = Bits.to_int_trunc !(o.src_ip)
          ; dst_ip = Bits.to_int_trunc !(o.dst_ip)
          };
    if bit o.app_tvalid
    then got := !got @ [ Bits.to_int_trunc !(o.app_tdata) ];
    if bit o.crc_error then crc_error := true;
    if bit o.app_tvalid && bit o.app_tlast then checksum_ok := bit o.checksum_ok;
    cycle ()
  done;
  { payload = !got; metadata = !meta; crc_error = !crc_error; checksum_ok = !checksum_ok }
;;

(* ── assertion helpers ─────────────────────────────────────────────────────── *)
let all_ok = ref true
let check name cond =
  if not cond then all_ok := false;
  printf "  %-52s %s\n" name (if cond then "PASS" else "FAIL")
;;

let src_ip = [ 192; 168; 1; 10 ]
let dst_ip = [ 192; 168; 1; 1 ]
let src_port = 0x1234
let dst_port = 0x1235

let expect_datagram ~label ~result ~payload =
  printf "\n-- %s --\n" label;
  check "application payload recovered" (List.equal Int.equal result.payload payload);
  (match result.metadata with
   | None -> check "metadata captured" false
   | Some m ->
     check "source port" (m.src_port = src_port);
     check "destination port" (m.dst_port = dst_port);
     check "udp length" (m.udp_length = 8 + List.length payload);
     check "payload length" (m.payload_length = List.length payload);
     check "source ip" (m.src_ip = ip32 src_ip);
     check "destination ip" (m.dst_ip = ip32 dst_ip))
;;

let () =
  (* test 1: nominal datagram, no MAC padding (18B app => 46B eth payload) *)
  let payload1 = List.init 18 ~f:(fun k -> k + 1) in
  let r1 =
    run
      (ipv4_udp_eth_payload ~src_ip ~dst_ip ~src_port ~dst_port ~udp_checksum:0
         ~payload:payload1 ())
  in
  expect_datagram ~label:"test 1: nominal UDP datagram over MII" ~result:r1 ~payload:payload1;
  check "no FCS error on a good frame" (not r1.crc_error);
  check "IPv4 checksum verified" r1.checksum_ok;

  (* test 2: short datagram => MAC zero-pads to 46B; padding must be stripped *)
  let payload2 = [ 0xDE; 0xAD; 0xBE; 0xEF ] in
  let r2 =
    run
      (ipv4_udp_eth_payload ~ethernet_padding:true ~src_ip ~dst_ip ~src_port ~dst_port
         ~udp_checksum:0 ~payload:payload2 ())
  in
  expect_datagram
    ~label:"test 2: short datagram, MAC padding stripped" ~result:r2 ~payload:payload2;
  check "padding did not leak into payload" (List.length r2.payload = 4);

  (* test 3: non-IPv4 ethertype => nothing reaches the application *)
  let r3 =
    run ~eth_type:[ 0x99; 0x99 ]
      (ipv4_udp_eth_payload ~src_ip ~dst_ip ~src_port ~dst_port ~udp_checksum:0
         ~payload:payload1 ())
  in
  printf "\n-- test 3: non-IPv4 ethertype rejected --\n";
  check "no application payload emitted" (List.is_empty r3.payload);
  check "no metadata captured" (Option.is_none r3.metadata);

  (* test 4: bad Ethernet FCS. The payload is still forwarded (FCS is a status
     sideband, not a gate), AND the bad-frame verdict now reaches the application
     via the frame-level late-status channel (Ipv4_rx/Udp_rx frame_done/frame_error,
     latched-and-held in Udp_rx_mac_top). This exercises the exact-fit (non-padded)
     path where the MAC's tlast+tuser coincide with the last L4 byte. *)
  let r4 =
    run ~corrupt_fcs:true
      (ipv4_udp_eth_payload ~src_ip ~dst_ip ~src_port ~dst_port ~udp_checksum:0
         ~payload:payload1 ())
  in
  printf "\n-- test 4: bad Ethernet FCS reported to the app --\n";
  (* payload path is robust to a bad FCS: bytes still delivered verbatim *)
  check "payload still delivered on a bad-FCS frame"
    (List.equal Int.equal r4.payload payload1);
  check "app-layer crc_error asserted on bad FCS" r4.crc_error;

  (* test 5: bad Ethernet FCS on a MAC-padded frame — the padding pushes the FCS
     verdict PAST L4's tlast, the case a tlast-aligned flag cannot carry. The
     frame-level channel still delivers it. *)
  let payload5 = [ 0xDE; 0xAD; 0xBE; 0xEF ] in
  let r5 =
    run ~corrupt_fcs:true
      (ipv4_udp_eth_payload ~ethernet_padding:true ~src_ip ~dst_ip ~src_port ~dst_port
         ~udp_checksum:0 ~payload:payload5 ())
  in
  printf "\n-- test 5: bad FCS survives MAC padding (late verdict) --\n";
  check "padded payload still delivered on a bad-FCS frame"
    (List.equal Int.equal r5.payload payload5);
  check "app-layer crc_error asserted despite padding delay" r5.crc_error;

  printf "\n==== SUMMARY: %s ====\n" (if !all_ok then "ALL PASS" else "FAILURES");
  print_endline "\n=== SIMULATION COMPLETE ===";
  if not !all_ok then exit 1
;;
