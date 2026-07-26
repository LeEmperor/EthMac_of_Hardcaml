(*
  Bohdan Purtell
  University of Florida

  Testbench: IPv4 (L3) RX header parser (Ipv4_rx)

  Drives a synthetic Ethernet-payload byte stream — exactly what Mac_top.m_axis
  presents (rx_tdata/tvalid/tlast/tuser + the rx_tfirst SOF pulse and latched
  rx_eth_type) — and checks that Ipv4_rx:

    - strips the 20-byte IPv4 header and re-emits the L4 payload verbatim,
    - drives m_tlast off the IP total_length (so trailing Ethernet zero-padding
      on short frames is dropped, not leaked to L4),
    - surfaces the right metadata (protocol, payload_length, src/dst IP),
    - verifies the header checksum (checksum_ok), and honours the
      drop_on_bad_checksum policy,
    - rejects non-IPv4 ethertypes,
    - forwards the MAC's FCS-error flag (rx_tuser) as crc_error.

  The golden IPv4 header (incl. checksum) is built in OCaml, mirroring
  ipv4_tx_tb, so TX and RX agree bit-for-bit on the wire format.
*)

open! Core
open! Hardcaml
open! Ipv4_of_hardcaml
open! Helper_tb_functions

let () = print_endline "=== Running IPv4 RX Testbench ==="

module Rx = Ipv4_rx.Make (struct
  let drop_on_bad_checksum = true
  let debug = true
end)

module Sim = Cyclesim.With_interface (Rx.I) (Rx.O)

let w16 hi lo = ((hi land 0xFF) lsl 8) lor (lo land 0xFF)
let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF

(* one's-complement header checksum, matching ipv4_tx / ipv4_tx_tb *)
let ip_checksum ~total_length ~protocol ~src_ip ~dst_ip =
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

(* build the 20-byte header; [corrupt_cksum] flips the checksum so verification fails *)
let ip_header ~l4_len ~protocol ~src_ip ~dst_ip ~corrupt_cksum =
  let total_length = 20 + l4_len in
  let ck = ip_checksum ~total_length ~protocol ~src_ip ~dst_ip in
  let ck = if corrupt_cksum then ck lxor 0xFFFF else ck in
  [ 0x45; 0x00; hi8 total_length; lo8 total_length
  ; 0x00; 0x00; 0x40; 0x00; 0x40; protocol; hi8 ck; lo8 ck ]
  @ src_ip @ dst_ip

let ip32 bytes =
  List.fold bytes ~init:0 ~f:(fun a b -> (a lsl 8) lor (b land 0xFF))

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Rx.create scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in

  let reset () =
    i.Rx.I.reset <-- 1;
    i.en          <-- 1;
    i.rx_tdata    <-- 0;
    i.rx_tvalid   <-- 0;
    i.rx_tlast    <-- 0;
    i.rx_tuser    <-- 0;
    i.rx_tfirst   <-- 0;
    i.rx_eth_type <-- 0;
    i.l4_tready   <-- 1;
    cycle ();
    i.reset <-- 0
  in

  (* Present [frame] (the full Ethernet payload: IP header ++ L4 ++ padding) to
     the DUT the way the MAC would, one byte per accepted cycle, with tfirst on
     byte 0 and tlast+tuser on the final byte. [stall_every]=k inserts an
     l4_tready=0 bubble every k transfers to exercise backpressure. Collects the
     stripped L4 stream the DUT emits, plus the metadata sampled at l4_start.

     Cyclesim phase note: this DUT's payload/SOF outputs are Mealy (combinational
     from state + inputs). After [Cyclesim.cycle], the output ports hold
     comb(next_state, current_input) — the valid/last/first *qualifier* belongs
     to the NEXT cycle's state while tdata belongs to the CURRENT input. So we
     read the qualifiers one iteration late: the qualifier for the byte on the
     bus during cycle N is the value we read at the END of iteration N-1
     (= comb(S_N, ..)). We hold those in [q_*] and pair them with the byte we're
     presenting this iteration. tdata itself is just the input byte we drive, so
     we collect that directly rather than reading it back. *)
  let run ~label ~expect_output ~frame ~eth_type ~fcs_bad ~stall_every =
    reset ();
    let n = List.length frame in
    let arr = Array.of_list frame in
    i.rx_eth_type <-- eth_type;

    let ptr        = ref 0 in
    let collected  = ref [] in
    let tlast_idx  = ref (-1) in
    let meta        = ref None in         (* (protocol, payload_length, src, dst) at l4_start *)
    let saw_ip_last = ref false in
    let guard       = ref 0 in
    let guard_limit = (n * 2) + 64 in
    (* qualifiers for the CURRENT cycle's state S_N, read at the end of the prior
       iteration. Initialised to the post-reset Idle state (nothing valid). *)
    let q_valid = ref false in
    let q_last  = ref false in
    let q_first = ref false in
    let transfer = ref false in
    let accepted = ref false in
    while (!ptr < n || (expect_output && not !saw_ip_last)) && !guard < guard_limit do
      let has = !ptr < n in
      let is_last_frame_byte = has && !ptr = n - 1 in
      let stalling = stall_every > 0 && !guard % (stall_every + 1) = stall_every in
      let l4rdy = not stalling in
      let present = if has then arr.(!ptr) else 0 in
      i.l4_tready <-- (if l4rdy then 1 else 0);
      i.rx_tvalid <-- (if has then 1 else 0);
      i.rx_tdata  <-- present;
      i.rx_tfirst <-- (if has && !ptr = 0 then 1 else 0);
      i.rx_tlast  <-- (if is_last_frame_byte then 1 else 0);
      i.rx_tuser  <-- (if is_last_frame_byte && fcs_bad then 1 else 0);

      (* Transfer of the byte on the bus THIS cycle, qualified by the S_N reads:
         an L4 byte moves when the DUT is emitting valid and we grant ready. *)
      transfer := !q_valid && l4rdy && has;
      if !transfer then begin
        collected := !collected @ [ present ];
        if !q_last then begin
          tlast_idx := List.length !collected - 1;
          saw_ip_last := true
        end
      end;
      (* SOF of this cycle -> latch metadata (metadata regs are stable by here) *)
      if !q_first then
        meta := Some
          ( Bits.to_int_trunc !(o.Rx.O.protocol),
            Bits.to_int_trunc !(o.payload_length),
            Bits.to_int_trunc !(o.src_ip),
            Bits.to_int_trunc !(o.dst_ip) );

      (* The DUT consumes a byte whenever it is ready: always in Idle/Header/Flush
         (m_ready=1), and in Payload only when l4_tready. q_valid marks Payload. *)
      accepted := has && ((not !q_valid) || l4rdy);

      cycle ();

      (* read O_N = comb(S_{N+1}, ..) into the qualifiers for the next iteration *)
      q_valid := Bits.to_bool !(o.m_tvalid);
      q_last  := Bits.to_bool !(o.m_tlast);
      q_first := Bits.to_bool !(o.l4_start);
      if !accepted then incr ptr;
      incr guard
    done;
    if !ptr < n then
      failwithf
        "%s: input drain timed out after %d cycles at byte %d of %d"
        label
        guard_limit
        !ptr
        n
        ();
    if expect_output && not !saw_ip_last then
      failwithf
        "%s: output drain timed out after %d cycles (received %d bytes)"
        label
        guard_limit
        (List.length !collected)
        ();

    (* drain a couple cycles so busy/crc_error settle *)
    i.rx_tvalid <-- 0; i.rx_tfirst <-- 0; i.rx_tlast <-- 0;
    let crc_err = Bits.to_bool !(o.Rx.O.crc_error) in
    let csum_ok = Bits.to_bool !(o.checksum_ok) in
    (!collected, !tlast_idx, !meta, csum_ok, crc_err)
  in

  let src_ip = [ 192; 168; 1; 10 ] in
  let dst_ip = [ 10; 0; 0; 7 ] in

  let pass_count = ref 0 in
  let test_count = ref 0 in
  let expect ~label cond =
    incr test_count;
    if cond then incr pass_count;
    printf "  %-40s %s\n" label (if cond then "PASS" else "FAIL")
  in

  (* ---- Test 1: 26B UDP payload, no padding, clean FCS ---- *)
  printf "\n-- test 1: 26B UDP, no stalls --\n";
  let l4 = List.init 26 ~f:(fun k -> (0x40 + k) land 0xFF) in
  let frame = ip_header ~l4_len:26 ~protocol:17 ~src_ip ~dst_ip ~corrupt_cksum:false @ l4 in
  let got, tlast_idx, meta, csum_ok, crc_err = run ~label:"test 1: 26B UDP" ~expect_output:true ~frame ~eth_type:0x0800 ~fcs_bad:false ~stall_every:0 in
  expect ~label:"payload bytes match" (List.equal Int.equal got l4);
  expect ~label:"tlast on final payload byte" (tlast_idx = List.length l4 - 1);
  expect ~label:"checksum_ok" csum_ok;
  expect ~label:"no crc_error" (not crc_err);
  (match meta with
   | Some (proto, plen, s, d) ->
     expect ~label:"protocol = 17" (proto = 17);
     expect ~label:"payload_length = 26" (plen = 26);
     expect ~label:"src_ip" (s = ip32 src_ip);
     expect ~label:"dst_ip" (d = ip32 dst_ip)
   | None -> expect ~label:"metadata captured" false);

  (* ---- Test 2: short payload with Ethernet zero-padding to 46 bytes ---- *)
  printf "\n-- test 2: 10B UDP + zero-pad to 46 (padding must be dropped) --\n";
  let l4 = List.init 10 ~f:(fun k -> (0x80 + k) land 0xFF) in
  let hdr = ip_header ~l4_len:10 ~protocol:17 ~src_ip ~dst_ip ~corrupt_cksum:false in
  let pad = List.init (46 - (20 + 10)) ~f:(fun _ -> 0x00) in
  let frame = hdr @ l4 @ pad in
  let got, tlast_idx, meta, csum_ok, _ = run ~label:"test 2: padded UDP" ~expect_output:true ~frame ~eth_type:0x0800 ~fcs_bad:false ~stall_every:0 in
  expect ~label:"payload = 10 bytes (pad stripped)" (List.equal Int.equal got l4);
  expect ~label:"tlast at index 9" (tlast_idx = 9);
  expect ~label:"checksum_ok" csum_ok;
  (match meta with
   | Some (_, plen, _, _) -> expect ~label:"payload_length = 10" (plen = 10)
   | None -> expect ~label:"metadata captured" false);

  (* ---- Test 3: TCP protocol + backpressure stalls ---- *)
  printf "\n-- test 3: 20B TCP, l4_tready stalls every 3 --\n";
  let l4 = List.init 20 ~f:(fun k -> (0x10 + k) land 0xFF) in
  let frame = ip_header ~l4_len:20 ~protocol:6 ~src_ip ~dst_ip ~corrupt_cksum:false @ l4 in
  let got, tlast_idx, meta, csum_ok, _ = run ~label:"test 3: stalled TCP" ~expect_output:true ~frame ~eth_type:0x0800 ~fcs_bad:false ~stall_every:3 in
  expect ~label:"payload bytes match under stall" (List.equal Int.equal got l4);
  expect ~label:"tlast on final byte" (tlast_idx = List.length l4 - 1);
  expect ~label:"checksum_ok" csum_ok;
  (match meta with
   | Some (proto, plen, _, _) ->
     expect ~label:"protocol = 6" (proto = 6);
     expect ~label:"payload_length = 20" (plen = 20)
   | None -> expect ~label:"metadata captured" false);

  (* ---- Test 4: corrupt checksum -> dropped (drop_on_bad_checksum = true) ---- *)
  printf "\n-- test 4: bad checksum must be dropped (no L4 output) --\n";
  let l4 = List.init 16 ~f:(fun k -> (0x50 + k) land 0xFF) in
  let frame = ip_header ~l4_len:16 ~protocol:17 ~src_ip ~dst_ip ~corrupt_cksum:true @ l4 in
  let got, _, _, csum_ok, _ = run ~label:"test 4: bad checksum" ~expect_output:false ~frame ~eth_type:0x0800 ~fcs_bad:false ~stall_every:0 in
  expect ~label:"no payload emitted" (List.is_empty got);
  expect ~label:"checksum_ok deasserted" (not csum_ok);

  (* ---- Test 5: non-IPv4 ethertype (ARP 0x0806) -> flushed ---- *)
  printf "\n-- test 5: non-IPv4 ethertype must be flushed --\n";
  let frame = List.init 30 ~f:(fun k -> (0xA0 + k) land 0xFF) in
  let got, _, _, _, _ = run ~label:"test 5: non-IPv4" ~expect_output:false ~frame ~eth_type:0x0806 ~fcs_bad:false ~stall_every:0 in
  expect ~label:"no payload emitted" (List.is_empty got);

  (* ---- Test 6: good frame but MAC signals bad FCS (rx_tuser) -> crc_error ---- *)
  printf "\n-- test 6: clean header, MAC FCS error flag forwarded --\n";
  let l4 = List.init 24 ~f:(fun k -> (0x30 + k) land 0xFF) in
  let frame = ip_header ~l4_len:24 ~protocol:17 ~src_ip ~dst_ip ~corrupt_cksum:false @ l4 in
  let got, tlast_idx, _, csum_ok, crc_err = run ~label:"test 6: bad FCS" ~expect_output:true ~frame ~eth_type:0x0800 ~fcs_bad:true ~stall_every:0 in
  expect ~label:"payload still forwarded" (List.equal Int.equal got l4);
  expect ~label:"tlast on final byte" (tlast_idx = List.length l4 - 1);
  expect ~label:"checksum_ok (header intact)" csum_ok;
  expect ~label:"crc_error asserted" crc_err;

  printf "\n==== SUMMARY: %d/%d checks passed ====\n" !pass_count !test_count;
  print_endline "\n=== SIMULATION COMPLETE ===";
  if !pass_count <> !test_count then exit 1
;;
