(*
  Bohdan Purtell
  University of Florida

  Testbench: TX MAC top-level integration test

  Verification strategy (mirror of the RX testbench):
    RX TB: push MII nibbles from the PHY side → read bytes on AXI-S output
    TX TB: write bytes into the TX FIFO via AXI-S → read MII nibbles from tx_d/tx_en
           → reassemble nibbles back into bytes → parse + check frame structure

  Expected frame layout:
    [0..6]   preamble:  7 × 0x55
    [7]      SFD:       0xD5
    [8..13]  dst_mac:   hardcoded in tx_datapath (broadcast: ff ff ff ff ff ff)
    [14..19] src_mac:   hardcoded in tx_datapath (locally-admin: 02 00 00 00 00 01)
    [20..21] eth_type:  hardcoded in tx_datapath (0x9999 custom)
    [22..67] payload:   46 bytes from FIFO
    [68..71] FCS:       CRC-32 over [8..67]

  NOTE: the payload length is now data-driven — the Payload state ends on
  s_axis_tlast (asserted with the final payload byte), not a fixed count. The
  standard 46-byte minimum Ethernet payload (min frame = 14 header + 46 payload
  + 4 FCS = 64 bytes on the wire, excluding preamble/SFD) is still enforced:
  tx_controller zero-pads a sub-minimum datagram up to 46 bytes before FCS. This
  test drives exactly 46 payload bytes, so no padding is exercised (that path is
  covered separately); it does exercise the tlast-terminated Payload state.
*)

open! Core
open! Hardcaml
open! Mii_of_hardcaml
open! Hardcaml_waveterm
open! Helper_tb_functions

let waveform_enabled =
  Array.exists (Sys.get_argv ()) ~f:(String.equal "--waveform")

let () = print_endline "=== Running MAC TX Top Testbench ==="

module Sim = Cyclesim.With_interface(Mac_top.I)(Mac_top.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim   = Sim.create ~config:Cyclesim.Config.trace_all (Mac_top.create ~rx_fifo_for_sim:true scope) in
  let waves, sim = Waveform.create sim in
  let inputs  : _ Mac_top.I.t = Cyclesim.inputs  sim in
  let outputs : _ Mac_top.O.t = Cyclesim.outputs sim in
  (sim, waves, inputs, outputs)

(* ── SW CRC-32 reference (reflected poly 0xEDB88320, init/finalXOR 0xFFFFFFFF) ── *)
let sw_crc_bit crc bit =
  let feedback = ((crc land 1) lxor bit) land 1 in
  let shifted  = crc lsr 1 in
  if feedback = 1 then shifted lxor 0xEDB88320 else shifted

let sw_crc_byte crc byte =
  let crc = ref crc in
  for i = 0 to 7 do crc := sw_crc_bit !crc ((byte lsr i) land 1) done;
  !crc

let sw_crc bytes =
  List.fold bytes ~init:0xFFFFFFFF ~f:sw_crc_byte
  |> fun raw -> raw lxor 0xFFFFFFFF

let bytes_of_int ~n x = 
  List.init n ~f:(fun i -> (x lsr (8 * i)) land 0xFF)

(* ── nibble collector + byte reassembler ── *)

(* 
   Read all nibbles that come out of tx_d while tx_en is 1
   Waits up to [max_wait] cycles for tx_en to first assert, then collects until
   it deasserts

   Returns the reassembled byte list (lo nibble first per byte)
*)

let collect_frame ~label ~cycle ~outputs ~max_wait =
  let tx_en () = Bits.to_bool    !(outputs.Mac_top.O.tx_en) in (* ! grabs value from a container ref *)
  let tx_d  () = Bits.to_int_trunc !(outputs.Mac_top.O.tx_d) in
  let nibbles = ref [] in
  (* wait for first tx_en *)
  let waiting = ref true in
  let i = ref 0 in
  while !waiting && !i < max_wait do
    if tx_en ()
    then waiting := false
    else (cycle (); incr i)
  done;
  if !waiting then
    failwithf "%s: tx_en did not assert within %d cycles" label max_wait ();
  (* collect while tx_en is high, bounded so an underflow (FSM stall) fails
     loudly instead of hanging the sim forever *)
  let guard = ref 0 in
  while tx_en () && !guard < max_wait do
    nibbles := !nibbles @ [tx_d ()];
    cycle ();
    incr guard
  done;
  if tx_en () then
    failwithf
      "%s: tx_en did not deassert within %d cycles (possible payload underflow)"
      label
      max_wait
      ();
  (* reassemble pairs [lo; hi] → byte *)
  let rec pair = function
    | lo :: hi :: rest -> ((hi lsl 4) lor lo) :: pair rest
    | _ -> []
  in
  pair !nibbles

(* ── tb ── *)
let () =
  let open Bits in
  let sim, waves, inputs, outputs = create_sim () in

  let run sim =

  let cycle () = Cyclesim.cycle sim in

  let reset () =
    inputs.rx_reset     <-- 1;
    inputs.tx_reset     <-- 1;
    inputs.en           <-- 0;
    inputs.tx_start     <-- 0;
    inputs.s_axis_tvalid <-- 0;
    inputs.s_axis_tlast  <-- 0;
    inputs.rx_dv        <-- 0;
    cycle ();
    inputs.rx_reset <-- 0;
    inputs.tx_reset <-- 0;
    inputs.en       <-- 1;
  in

  (* hardcoded frame header constants — must match tx_datapath.ml:
       dst = broadcast, src = locally-administered (02:..), ethertype 0x9999 *)
  let exp_dst_mac  = [0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF] in
  let exp_src_mac  = [0x02; 0x00; 0x00; 0x00; 0x00; 0x01] in
  let exp_eth_type = [0x99; 0x99] in

  let exp_preamble = List.init 7 ~f:(fun _ -> 0x55) in
  let min_payload  = 46 in

  (* ── parametrized frame test ──
     Drives [data] (the application payload) into the TX FIFO with tlast on the
     final byte, then checks the reassembled MII frame. The controller is
     expected to:
       - transmit exactly [data] when |data| >= 46, or
       - zero-pad up to 46 bytes when |data| < 46,
     and to append an FCS computed over dst/src/ethertype + the (padded) payload.
     Returns true on full PASS. *)
  let run_frame_test ~label ~data =
    printf "\n-- [%s] payload = %d bytes --\n" label (List.length data);
    reset ();

    (* what the payload region should look like on the wire *)
    let n_data      = List.length data in
    let pad_len     = Int.max 0 (min_payload - n_data) in
    let wire_payload = data @ List.init pad_len ~f:(fun _ -> 0x00) in
    let plen        = List.length wire_payload in

    (* SW reference FCS covers dst/src/ethertype + padded payload *)
    let frame_for_crc = exp_dst_mac @ exp_src_mac @ exp_eth_type @ wire_payload in
    let expected_fcs  = bytes_of_int ~n:4 (sw_crc frame_for_crc) in

    (* stage payload into the FIFO, tlast with the last byte *)
    List.iteri data ~f:(fun i b ->
      inputs.s_axis_tdata  <-- b;
      inputs.s_axis_tvalid <-- 1;
      inputs.s_axis_tlast  <-- (if i = n_data - 1 then 1 else 0);
      cycle ()
    );
    inputs.s_axis_tvalid <-- 0;
    inputs.s_axis_tlast  <-- 0;

    (* pulse tx_start for one cycle *)
    inputs.tx_start <-- 1;
    cycle ();
    inputs.tx_start <-- 0;

    let frame = collect_frame ~label ~cycle ~outputs ~max_wait:500 in
    let exp_frame_len = 22 + plen + 4 in
    printf "frame length: %d bytes (expect %d)\n" (List.length frame) exp_frame_len;

    let check_region name offset expected =
      let n = List.length expected in
      let got =
        if offset + n <= List.length frame
        then List.sub frame ~pos:offset ~len:n
        else List.sub frame ~pos:(min offset (List.length frame))
               ~len:(max 0 (List.length frame - offset))
      in
      let ok = List.equal Int.equal got expected in
      printf "%s @ [%d..%d]: %s\n" name offset (offset + n - 1) (if ok then "PASS" else "FAIL");
      if not ok then begin
        printf "  expected: %s\n" (String.concat ~sep:" " (List.map expected ~f:(sprintf "%02x")));
        printf "  got:      %s\n" (String.concat ~sep:" " (List.map got      ~f:(sprintf "%02x")))
      end;
      ok
    in

    let ok_len      = List.length frame = exp_frame_len in
    if not ok_len then printf "frame length: FAIL (got %d, expect %d)\n" (List.length frame) exp_frame_len;
    let ok_preamble = check_region "preamble " 0  exp_preamble in
    let ok_sfd      = check_region "sfd      " 7  [0xD5]       in
    let ok_dst      = check_region "dst_mac  " 8  exp_dst_mac  in
    let ok_src      = check_region "src_mac  " 14 exp_src_mac  in
    let ok_eth      = check_region "eth_type " 20 exp_eth_type in
    let ok_payload  = check_region "payload  " 22 wire_payload in
    let ok_fcs      = check_region "fcs      " (22 + plen) expected_fcs in
    let ok = ok_len && ok_preamble && ok_sfd && ok_dst && ok_src && ok_eth && ok_payload && ok_fcs in
    printf "%s: %s\n" label (if ok then "PASS" else "FAIL");
    ok
  in

  (* sequential lets (not a list literal) so tests run top-to-bottom — OCaml
     leaves list-element evaluation order unspecified (right-to-left in practice) *)
  (* exactly the minimum — tlast-terminated, no padding (baseline) *)
  let r1 = run_frame_test ~label:"test 1: 46B min"   ~data:(List.init 46 ~f:(fun i -> i + 1)) in
  (* sub-minimum — heavy padding *)
  let r2 = run_frame_test ~label:"test 2: 18B pad"   ~data:(List.init 18 ~f:(fun i -> 0x80 + i)) in
  (* one byte — maximal padding (45 zero bytes) *)
  let r3 = run_frame_test ~label:"test 3: 1B pad"    ~data:[0xAB] in
  (* one short of the minimum — single zero pad byte (boundary) *)
  let r4 = run_frame_test ~label:"test 4: 45B pad"   ~data:(List.init 45 ~f:(fun i -> i + 1)) in
  (* one over the minimum — no padding, variable length above 46 *)
  let r5 = run_frame_test ~label:"test 5: 47B nopad" ~data:(List.init 47 ~f:(fun i -> i + 1)) in
  (* comfortably above minimum *)
  let r6 = run_frame_test ~label:"test 6: 64B nopad" ~data:(List.init 64 ~f:(fun i -> (i * 3) land 0xFF)) in
  let results = [ r1; r2; r3; r4; r5; r6 ] in
  printf "\n==== SUMMARY: %d/%d passed ====\n"
    (List.count results ~f:Fn.id) (List.length results);

  print_endline "\n=== SIMULATION COMPLETE ===";
  if waveform_enabled then Waveform.print ~display_width:100 waves;
  if not (List.for_all results ~f:Fn.id) then exit 1
  in
  if waveform_enabled
  then
    Out_channel.with_file "waves_tx_top.vcd" ~f:(fun oc -> run (Vcd.wrap oc sim))
  else run sim
