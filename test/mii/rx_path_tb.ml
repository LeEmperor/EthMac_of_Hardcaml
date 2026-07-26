(*
  Jane Street
  Author: Bohdan Purtell

  Executable: "rx_path_tb.ml"



  design notes:
    obviously as an executable this generates the waveforms that we might want

    but is it possible for us to have this exist as a test layer item?
    do test-layer items in the alcotest framework have to adhere to being executables in the first place? can tests have system-level sideeffects such as generated log files or other things like waveform files?


    is it possible to make it a cli arg for generation of the waveforms? what about the ascii waveforms? i dont really want to see those, but i dont want to go and write new dune meta-language to define the proper requirements for new test stanzas

*)

open! Core
open! Hardcaml
open! Mii_of_hardcaml
open! Hardcaml_waveterm
open! Helper_tb_functions

let waveform_enabled =
  Array.exists (Sys.get_argv ()) ~f:(String.equal "--waveform")

let () = print_endline "=== Running MAC RX Top Testbench ==="

module Waveform = Hardcaml_waveterm.Waveform
module Sim = Cyclesim.With_interface(Mac_top.I)(Mac_top.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim   = Sim.create ~config:Cyclesim.Config.trace_all (Mac_top.create ~rx_fifo_for_sim:true scope) in
  let waves, sim = Waveform.create sim in
  let inputs  : _ Mac_top.I.t = Cyclesim.inputs  sim in
  let outputs : _ Mac_top.O.t = Cyclesim.outputs sim in
  (sim, waves, inputs, outputs)

(* SW CRC-32 reference (reflected poly 0xEDB88320, init 0xFFFFFFFF, no final XOR) *)
let sw_crc_bit crc bit =
  let feedback = ((crc land 1) lxor bit) land 1 in
  let shifted   = crc lsr 1 in
  if feedback = 1 then shifted lxor 0xEDB88320 else shifted

let sw_crc_byte crc byte =
  let crc = ref crc in
  for i = 0 to 7 do crc := sw_crc_bit !crc ((byte lsr i) land 1) done;
  !crc

let sw_crc bytes = List.fold bytes ~init:0xFFFFFFFF ~f:sw_crc_byte

(* extract n bytes from an OCaml int, LSB first — matches send_bytes ordering *)
let bytes_of_int ~n x = List.init n ~f:(fun i -> (x lsr (8 * i)) land 0xFF)

(* CRC-32 of frame_bytes in little-endian, ready to feed as FCS *)
let compute_fcs frame_bytes =
  let crc32 = (sw_crc frame_bytes) lxor 0xFFFF_FFFF in
  bytes_of_int ~n:4 crc32

let () =
  let open Bits in
  let sim, waves, inputs, outputs = create_sim () in

  let run sim =

  let t_rst_rx = inputs.rx_reset in
  let t_rst_tx = inputs.tx_reset in
  let t_en     = inputs.en in
  let t_in     = inputs.rx_data in
  let t_rx_dv  = inputs.rx_dv in
  let t_tready = inputs.m_axis_tready in

  let cycle () = Cyclesim.cycle sim in

  let reset () =
    t_tready <-- 0;
    t_en     <-- 0;
    t_rst_rx <-- 1;
    t_rst_tx <-- 1;
    t_rx_dv  <-- 0;
    cycle ();
    t_en     <-- 1;
    t_rst_rx <-- 0;
    t_rst_tx <-- 0;
  in

  let dst_mac = 0x36_12_73_36_24_85 in
  let src_mac = 0x37_52_33_76_94_05 in

  let good_fcs ~eth_type ~payload ~payload_len =
    compute_fcs
      (bytes_of_int ~n:6 dst_mac
       @ bytes_of_int ~n:6 src_mac
       @ bytes_of_int ~n:2 eth_type
       @ bytes_of_int ~n:payload_len payload)
  in

  let send_frame_with_fcs ~eth_type ~payload ~payload_len fcs =
    t_rx_dv <-- 1;
    send_frame ~cycle ~t_in "frame"
      ~dst_mac ~src_mac ~eth_type
      ~payload_length:payload_len ~payload;
    List.iter fcs ~f:(send_byte ~cycle ~t_in);
    t_rx_dv <-- 0;
    cycle ()   (* tlast_wr fires on this cycle *)
  in

  let collect_output () =
    let bytes_out = ref [] in
    let tlast_byte = ref None in
    let tuser_on_last = ref false in
    let tfirst_at = ref [] in  (* byte indices where m_axis_tfirst pulsed *)
    (* Sample BEFORE cycling: in cut-through FIFO the head word is already at
       the output.  Read while tvalid&tready, then cycle to advance to next word. *)
    for i = 0 to 30 do
      let v = Bits.to_bool    !(outputs.m_axis_tvalid) in
      let b = Bits.to_int_trunc !(outputs.m_axis_tdata) in
      let l = Bits.to_bool    !(outputs.m_axis_tlast) in
      let u = Bits.to_bool    !(outputs.m_axis_tuser) in
      let f = Bits.to_bool    !(outputs.m_axis_tfirst) in
      if waveform_enabled then
        printf "  [%2d] valid=%d data=0x%02x last=%d user=%d first=%d\n"
          i (if v then 1 else 0) b (if l then 1 else 0) (if u then 1 else 0)
          (if f then 1 else 0);
      if v then begin
        if f then tfirst_at := !tfirst_at @ [ List.length !bytes_out ];
        bytes_out := !bytes_out @ [b];
        if l then begin
          tlast_byte    := Some b;
          tuser_on_last := u
        end
      end;
      cycle ()
    done;
    (!bytes_out, !tlast_byte, !tuser_on_last, !tfirst_at)
  in

  (* Send a valid frame and assert the RX contract end-to-end: exact payload
     bytes, tlast present, tuser=0, SOF on the first byte only, latched ethertype
     (byteswapped — see note below). *)
  let run_valid ~label ~eth_type ~payload ~payload_len =
    printf "\n-- [%s] valid FCS, %dB payload, eth_type=0x%04x --\n"
      label payload_len eth_type;
    reset ();
    (* tready stays 0 (from reset) so the FIFO buffers the whole frame; then
       tready=1 drains it and we observe tlast/tuser/tfirst *)
    send_frame_with_fcs ~eth_type ~payload ~payload_len
      (good_fcs ~eth_type ~payload ~payload_len);
    t_tready <-- 1;
    let got, last, user, tfirst_at = collect_output () in
    let expected = bytes_of_int ~n:payload_len payload in
    (* This TB emits its 16-bit fields LSB-first (send_bytes), so eth_type 0x4521
       hits the wire as [0x21,0x45] and the datapath — accumulating MSB-first —
       latches 0x2145, the byteswap. (A real big-endian wire ethertype yields the
       correct numeric value.) *)
    let eth_swapped = ((eth_type land 0xFF) lsl 8) lor ((eth_type lsr 8) land 0xFF) in
    let bytes_ok = List.equal Int.equal got expected in
    let sof_ok = List.equal Int.equal tfirst_at [ 0 ] in
    let eth_ok = Bits.to_int_trunc !(outputs.rx_eth_type) = eth_swapped in
    let ok = bytes_ok && Option.is_some last && not user && sof_ok && eth_ok in
    printf
      "%s: %s  (bytes=%b  tlast=%b  tuser0=%b  sof@[0]=%b  eth=0x%04x ok=%b)\n"
      label (if ok then "PASS" else "FAIL") bytes_ok (Option.is_some last)
      (not user) sof_ok (Bits.to_int_trunc !(outputs.rx_eth_type)) eth_ok;
    ok
  in

  (* ── test 1: valid frame (5B payload) ── *)
  let t1_ok = run_valid ~label:"test 1" ~eth_type:0x45_21
                ~payload:0x12_34_56_78_90 ~payload_len:5 in

  (* ── test 2: valid frame, longer (7B) payload + different ethertype ── *)
  let t2_ok = run_valid ~label:"test 2" ~eth_type:0x08_00
                ~payload:0xA1_B2_C3_D4_E5_F6_07 ~payload_len:7 in

  (* ── test 3: bad FCS — expect tuser=1 on tlast ── *)
  printf "\n-- [test 3] bad FCS (all bytes inverted) --\n";
  let eth_type = 0x45_21 and payload = 0x12_34_56_78_90 and payload_len = 5 in
  reset ();
  let bad_fcs =
    List.map (good_fcs ~eth_type ~payload ~payload_len) ~f:(fun b -> b lxor 0xFF)
  in
  send_frame_with_fcs ~eth_type ~payload ~payload_len bad_fcs;
  t_tready <-- 1;
  let _got3, last3, user3, _tfirst3 = collect_output () in
  let t3_ok = Option.is_some last3 && user3 in
  printf "test 3: %s  (got_tlast=%b  tuser=1=%b)\n"
    (if t3_ok then "PASS" else "FAIL") (Option.is_some last3) user3;

  let all_ok = t1_ok && t2_ok && t3_ok in
  printf "\n==== SUMMARY: %s ====\n" (if all_ok then "ALL PASS" else "FAILURES");
  print_endline "\n=== SIMULATION COMPLETE ===";
  if waveform_enabled then Waveform.print ~display_width:96 waves;
  if not all_ok then exit 1
  in
  if waveform_enabled
  then Out_channel.with_file "waves_top.vcd" ~f:(fun oc -> run (Vcd.wrap oc sim))
  else run sim
