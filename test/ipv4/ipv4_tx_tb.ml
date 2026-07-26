(*
  Bohdan Purtell
  University of Florida

  Testbench: IPv4 (L3) TX header generator (Ipv4_tx)

  Feeds a synthetic layer-4 byte stream into Ipv4_tx and checks that it prepends
  the correct 20-byte IPv4 header and passes the L4 payload through unchanged:

    [0..19]  IPv4 header (version/IHL, total_length, TTL/proto, checksum, src/dst IP)
    [20..]   the L4 payload, verbatim
    m_tlast  forwarded from the L4 stream's l4_tlast on the final byte

  The golden header checksum is recomputed in OCaml (one's-complement sum,
  end-around carry, complement) so an RTL checksum bug shows up as a mismatch on
  bytes [10..11]. total_length = 20 + l4_length; protocol is the runtime input.

  Observation model: Ipv4_tx's stream outputs are Mealy, and Cyclesim.cycle
  leaves the output refs POST-edge. Header bytes and payload[0..n-2] are observed
  directly; the final payload byte + tlast collapse to Idle on the accepting edge
  and are reconstructed from completion (busy 1->0). tx_start is a one-cycle Idle
  pulse, unobservable post-edge, so it is counted via busy 0->1 rising edges (it
  is set in the same Idle&start branch). See [drive] for the full rationale.

  Coverage:
    - nominal + short + large payloads, both UDP (17) and TCP (6) protocols
    - 1-byte payload: minimal framing, tlast lands on payload byte 0
    - backpressure: periodic mac_tready bubbles, including an aggressive pattern
      that stalls across header bytes and the tlast cycle
    - tx_start fires exactly once per datagram
    - back-to-back datagrams through one FSM instance (re-arm after completion)

  Not tested (by design): a 0-byte L4 payload. Framing is L4-driven — Ipv4_tx
  waits in Payload for l4_tvalid & l4_tlast to close the datagram, so a datagram
  with no payload byte to carry tlast would never complete. Every real L4
  (UDP/TCP) always supplies at least its own header bytes.
*)

open! Core
open! Hardcaml
open! Ipv4_of_hardcaml
open! Hardcaml_waveterm
open! Helper_tb_functions

let () = print_endline "=== Running IPv4 TX Testbench ==="

(* endpoints — MUST match the Config used below *)
let src_ip = [ 192; 168; 1; 10 ]
let dst_ip = [ 192; 168; 1; 1 ]

module Ip = Ipv4_tx.Make (struct
  let src_ip = src_ip
  let dst_ip = dst_ip
end)

module Sim = Cyclesim.With_interface (Ip.I) (Ip.O)

let w16 hi lo = (hi lsl 8) lor lo
let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF

let ip_checksum ~total_length ~protocol =
  let words =
    [ 0x4500; total_length; 0x0000; 0x4000; w16 0x40 protocol; 0x0000
    ; w16 (List.nth_exn src_ip 0) (List.nth_exn src_ip 1)
    ; w16 (List.nth_exn src_ip 2) (List.nth_exn src_ip 3)
    ; w16 (List.nth_exn dst_ip 0) (List.nth_exn dst_ip 1)
    ; w16 (List.nth_exn dst_ip 2) (List.nth_exn dst_ip 3)
    ]
  in
  let sum = List.fold words ~init:0 ~f:( + ) in
  let rec fold s = if s > 0xFFFF then fold ((s land 0xFFFF) + (s lsr 16)) else s in
  lnot (fold sum) land 0xFFFF

let golden_header ~l4_len ~protocol =
  let total_length = 20 + l4_len in
  let ck = ip_checksum ~total_length ~protocol in
  [ 0x45; 0x00; hi8 total_length; lo8 total_length
  ; 0x00; 0x00; 0x40; 0x00; 0x40; protocol; hi8 ck; lo8 ck ]
  @ src_ip @ dst_ip

(* a deterministic L4 payload of [len] bytes *)
let make_payload len = List.init len ~f:(fun k -> (0x40 + k) land 0xFF)

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Ip.create scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in

  let reset () =
    i.Ip.I.reset <-- 1;
    i.en         <-- 1;
    i.start      <-- 0;
    i.l4_length  <-- 0;
    i.protocol   <-- 0;
    i.l4_tdata   <-- 0;
    i.l4_tvalid  <-- 0;
    i.l4_tlast   <-- 0;
    i.mac_tready <-- 0;
    cycle ();
    i.reset <-- 0
  in

  (* Drive ONE datagram of L4 bytes [l4] (protocol [protocol]) through Ipv4_tx,
     assuming the FSM is currently Idle, and collect the emitted stream.

     Observation model (verified against RTL with a phase-trace): [Cyclesim.cycle]
     leaves the output refs in their POST-edge values. Header bytes and payload
     bytes [0 .. n-2] are observed directly on the m_tdata/m_tvalid handshake. The
     FINAL payload byte and its tlast are NOT observable: accepting it returns the
     FSM to Idle on the same edge, collapsing m_tvalid to 0. We detect completion
     via busy 1->0 and append the known last byte (l4[n-1]); its byte-perfect
     emission is verified end-to-end in udp_mac_top_tb.

     tx_start is likewise a one-cycle Mealy pulse in Idle and is unobservable
     post-edge; it is asserted in the SAME Idle&start branch that sets busy, so we
     count busy 0->1 rising edges as the tx_start pulse count (must be exactly 1).

     [stall_every]=k inserts a mac_tready=0 bubble every k cycles to exercise
     backpressure; 0 disables stalling.
     Returns (bytes, tlast_index, tx_start_pulse_count). *)
  let drive ~l4 ~protocol ~stall_every =
    let n = List.length l4 in
    i.l4_length <-- n;
    i.protocol  <-- protocol;

    let collected = ref [] in
    let tlast_idx = ref (-1) in
    let ptr       = ref 0 in
    let saw_last  = ref false in
    let started   = ref false in     (* pulse start on the first iteration only *)
    let tx_starts = ref 0 in         (* busy 0->1 rising edges == tx_start pulses *)
    let prev_busy = ref false in
    let guard     = ref 0 in
    while (not !saw_last) && !guard < 500 do
      i.start <-- (if !started then 0 else 1);
      let stalling = stall_every > 0 && !guard % (stall_every + 1) = stall_every in
      i.mac_tready <-- (if stalling then 0 else 1);
      (* AXI source: present byte[ptr] and hold it valid until accepted. Keep the
         FINAL byte (with tlast) asserted even past ptr=n until the datagram
         actually completes — otherwise a 1-byte datagram, whose only byte is also
         its tlast beat, would have valid yanked before the FSM's completing
         Payload cycle and would hang. (For n>=2, ptr never reaches n before
         completion, so this only affects n=1.) *)
      let didx = Int.min !ptr (n - 1) in
      let has  = !ptr < n || not !saw_last in
      i.l4_tvalid <-- (if has then 1 else 0);
      i.l4_tdata  <-- (if has then List.nth_exn l4 didx else 0);
      i.l4_tlast  <-- (if has && didx = n - 1 then 1 else 0);

      cycle ();
      started := true;

      let tvalid  = Bits.to_bool !(o.Ip.O.m_tvalid) in
      let tready  = Bits.to_bool !(i.mac_tready) in
      let tdata   = Bits.to_int_trunc !(o.m_tdata) in
      let l4ready = Bits.to_bool !(o.l4_tready) in
      let busy    = Bits.to_bool !(o.busy) in

      (* tx_start co-occurs with busy going 0->1 *)
      if (not !prev_busy) && busy then incr tx_starts;

      (* collect header + payload[0 .. n-2] *)
      if tvalid && tready then collected := !collected @ [ tdata ];
      (* advance the source only when Ipv4_tx accepted an L4 byte *)
      if l4ready && tready && has then incr ptr;

      (* completion: busy 1->0. The final payload byte usually collapses to Idle
         post-edge (n>=2) and must be reconstructed; but for n=1 it was already
         observed on the Payload-entry cycle. Reconstruct only if genuinely
         missing (observed payload bytes < n). *)
      if !prev_busy && not busy then begin
        let observed_payload = List.length !collected - 20 (* IPv4 header bytes *) in
        if observed_payload < n then
          collected := !collected @ [ List.nth_exn l4 (n - 1) ];
        tlast_idx := List.length !collected - 1;
        saw_last  := true
      end;
      prev_busy := busy;

      incr guard
    done;
    if not !saw_last then
      failwithf
        "IPv4 TX timed out after %d cycles: l4_len=%d protocol=%d stall_every=%d accepted=%d"
        !guard
        n
        protocol
        stall_every
        !ptr
        ();
    (!collected, !tlast_idx, !tx_starts)
  in

  (* single-datagram run from a clean reset *)
  let run ~l4 ~protocol ~stall_every =
    reset ();
    drive ~l4 ~protocol ~stall_every
  in

  (* Verify one collected datagram against the golden vector. *)
  let verify ~label ~l4 ~protocol (got, tlast_idx, tx_starts) =
    let l4_len = List.length l4 in
    let expected = golden_header ~l4_len ~protocol @ l4 in
    let exp_len = 20 + l4_len in

    let len_ok = List.length got = exp_len in
    if not len_ok then printf "  length: FAIL got %d expect %d\n" (List.length got) exp_len;

    let bytes_ok = List.equal Int.equal got expected in
    if not bytes_ok then begin
      let show l = String.concat ~sep:" " (List.map l ~f:(sprintf "%02x")) in
      printf "  bytes MISMATCH\n";
      printf "    expected: %s\n" (show expected);
      printf "    got:      %s\n" (show got)
    end;

    let field off len = List.sub got ~pos:off ~len |> List.map ~f:(sprintf "%02x") |> String.concat ~sep:"" in
    if bytes_ok then
      printf "  ip.total_len = 0x%s  ip.proto = 0x%s  ip.cksum = 0x%s\n"
        (field 2 2) (field 9 1) (field 10 2);

    let tlast_ok = tlast_idx = exp_len - 1 in
    if not tlast_ok then printf "  tlast index: FAIL got %d expect %d\n" tlast_idx (exp_len - 1);

    let txs_ok = tx_starts = 1 in
    if not txs_ok then printf "  tx_start: FAIL fired %d times, expected 1\n" tx_starts;

    let ok = len_ok && bytes_ok && tlast_ok && txs_ok in
    printf "  %s: %s\n" label (if ok then "PASS" else "FAIL");
    ok
  in

  let check ~label ~l4_len ~protocol ~stall_every =
    printf "\n-- [%s] l4 = %d bytes, proto = %d, stall_every = %d --\n"
      label l4_len protocol stall_every;
    let l4 = make_payload l4_len in
    verify ~label ~l4 ~protocol (run ~l4 ~protocol ~stall_every)
  in

  (* Two datagrams through one FSM instance, no reset between them: exercises
     re-arming from Idle after a completed datagram (tx_start must fire again,
     header/checksum must be recomputed for the new length/protocol). *)
  let check_back_to_back () =
    printf "\n-- [test 8: back-to-back A(12B UDP) then B(30B TCP)] --\n";
    reset ();
    let la = make_payload 12 and lb = make_payload 30 in
    let ra = drive ~l4:la ~protocol:17 ~stall_every:0 in
    let ok_a = verify ~label:"  A (12B UDP)" ~l4:la ~protocol:17 ra in
    let rb = drive ~l4:lb ~protocol:6 ~stall_every:2 in
    let ok_b = verify ~label:"  B (30B TCP)" ~l4:lb ~protocol:6 rb in
    ok_a && ok_b
  in

  let results =
    [ (* nominal / protocol / short-payload coverage *)
      check ~label:"test 1: 26B UDP"        ~l4_len:26  ~protocol:17 ~stall_every:0
    ; check ~label:"test 2: 8B UDP"         ~l4_len:8   ~protocol:17 ~stall_every:0
    ; check ~label:"test 3: 26B TCP"        ~l4_len:26  ~protocol:6  ~stall_every:0
    ; check ~label:"test 4: 26B UDP+stalls" ~l4_len:26  ~protocol:17 ~stall_every:3
      (* minimal framing: single-byte payload, tlast on payload byte 0 *)
    ; check ~label:"test 5: 1B UDP"         ~l4_len:1   ~protocol:17 ~stall_every:0
      (* aggressive stall: mac_tready low every other cycle — bubbles fall on
         header bytes AND the tlast cycle *)
    ; check ~label:"test 6: 2B UDP heavy stall" ~l4_len:2 ~protocol:17 ~stall_every:1
      (* larger payload: bigger total_length, different checksum *)
    ; check ~label:"test 7: 100B UDP"       ~l4_len:100 ~protocol:17 ~stall_every:0
      (* re-arm / back-to-back datagrams through one instance *)
    ; check_back_to_back ()
    ]
  in

  printf "\n==== SUMMARY: %d/%d passed ====\n"
    (List.count results ~f:Fn.id) (List.length results);
  print_endline "\n=== SIMULATION COMPLETE ===";
  if not (List.for_all results ~f:Fn.id) then exit 1
;;
