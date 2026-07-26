open! Core
open! Hardcaml
open! Mii_of_hardcaml
open! Hardcaml_waveterm
open! Helper_tb_functions

let waveform_enabled =
  Array.exists (Sys.get_argv ()) ~f:(String.equal "--waveform")

let () =
  print_endline "=== Running MAC TX CRC Testbench ==="

module Waveform = Hardcaml_waveterm.Waveform
module Sim = Cyclesim.With_interface(Tx_crc.I)(Tx_crc.O)

let create_sim () =
  let scope  = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim    = Sim.create ~config:Cyclesim.Config.trace_all (Tx_crc.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs  : _ Tx_crc.I.t = Cyclesim.inputs  sim in
  let outputs : _ Tx_crc.O.t = Cyclesim.outputs sim in
  (sim, waves, inputs, outputs)
;;

(* software reference CRC-32 — reflected polynomial, init 0xFFFFFFFF, final XOR 0xFFFFFFFF *)
let sw_crc_bit crc bit =
  let feedback = ((crc land 1) lxor bit) land 1 in
  let shifted  = crc lsr 1 in
  if feedback = 1 then shifted lxor 0xEDB88320 else shifted

let sw_crc_byte crc byte =
  let crc = ref crc in
  for i = 0 to 7 do
    crc := sw_crc_bit !crc ((byte lsr i) land 1)
  done;
  !crc

(* returns the 32-bit FCS value (after the final XOR) *)
let sw_crc bytes =
  List.fold bytes ~init:0xFFFFFFFF ~f:sw_crc_byte
  |> fun raw -> raw lxor 0xFFFFFFFF

let () =
  let open Bits in
  let sim, waves, inputs, outputs = create_sim () in

  let run sim =
  let checks = ref 0 in
  let failures = ref 0 in
  let check_hex ~case ~expected ~observed =
    Int.incr checks;
    if expected <> observed then begin
      Int.incr failures;
      eprintf "%s: expected 0x%08x, observed 0x%08x\n" case expected observed
    end
  in

  let t_rst      = inputs.reset in
  let t_en       = inputs.en in
  let t_data     = inputs.data in
  let t_valid    = inputs.data_valid in
  let t_byte_sel = inputs.byte_sel in

  let cycle () =
    Cyclesim.cycle sim;
    if waveform_enabled then
      printf "  data=0x%02x valid=%d  crc_reg=0x%08x  fcs_byte(sel=%d)=0x%02x\n"
        (Bits.to_int_trunc !(t_data))
        (Bits.to_int_trunc !(t_valid))
        (Bits.to_int_trunc !(outputs.crc_out))
        (Bits.to_int_trunc !(t_byte_sel))
        (Bits.to_int_trunc !(outputs.fcs_byte))
  in

  let reset () =
    t_en    <-- 0;
    t_rst   <-- 1;
    t_valid <-- 0;
    t_data  <-- 0;
    cycle ();
    t_rst <-- 0;
    t_en  <-- 1;
  in

  let feed_byte byte =
    t_data  <-- byte;
    t_valid <-- 1;
    cycle ();
  in

  let idle () =
    t_valid <-- 0;
    t_data  <-- 0;
    cycle ();
  in

  (* -- test 1: standard CRC-32 test vector --                        *)
  (* "123456789" (0x31..0x39) → FCS = 0xCBF43926                     *)
  (* bytes LE: 0x26 0x39 0xF4 0xCB                                   *)
  let data_bytes = [0x31;0x32;0x33;0x34;0x35;0x36;0x37;0x38;0x39] in
  let expected_fcs = sw_crc data_bytes in
  printf "\n-- [test 1] standard CRC-32 test vector (\"123456789\") --\n";
  printf "  sw reference FCS = 0x%08x  (expect 0xcbf43926)\n" expected_fcs;
  reset ();

  List.iter data_bytes ~f:feed_byte;
  idle ();

  let hw_raw = Bits.to_int_trunc !(outputs.crc_out) in
  let hw_fcs = hw_raw lxor 0xFFFFFFFF in
  printf "  hw crc_out = 0x%08x  hw FCS = 0x%08x  %s\n"
    hw_raw hw_fcs (if hw_fcs = expected_fcs then "PASS" else "FAIL");
  check_hex ~case:"standard vector FCS" ~expected:expected_fcs ~observed:hw_fcs;

  (* verify each FCS byte individually via byte_sel *)
  t_valid <-- 0;
  List.iteri [0x26; 0x39; 0xF4; 0xCB] ~f:(fun sel expected ->
    t_byte_sel <-- sel;
    Cyclesim.cycle sim;
    let got = Bits.to_int_trunc !(outputs.fcs_byte) in
    printf "  byte_sel=%d: fcs_byte=0x%02x  (expect 0x%02x)  %s\n"
      sel got expected (if got = expected then "PASS" else "FAIL");
    check_hex ~case:(sprintf "FCS byte %d" sel) ~expected ~observed:got
  );

  (* -- test 2: en drop mid-frame resets accumulator -- *)
  printf "\n-- [test 2] mid-frame en drop resets CRC, same data → same FCS --\n";
  reset ();
  feed_byte 0xDE;
  feed_byte 0xAD;
  t_en <-- 0;
  idle ();        (* en=0 resets accumulator back to 0xFFFFFFFF *)
  t_en <-- 1;
  List.iter data_bytes ~f:feed_byte;
  idle ();
  let hw_fcs2 = Bits.to_int_trunc !(outputs.crc_out) lxor 0xFFFFFFFF in
  printf "  hw FCS after reset+rerun = 0x%08x  (expect 0x%08x)  %s\n"
    hw_fcs2 expected_fcs (if hw_fcs2 = expected_fcs then "PASS" else "FAIL");
  check_hex ~case:"enable-drop recovery" ~expected:expected_fcs ~observed:hw_fcs2;

  (* -- test 3: different payload cross-checks against sw reference -- *)
  printf "\n-- [test 3] arbitrary payload matches sw reference CRC --\n";
  let payload = [0xDE; 0xAD; 0xBE; 0xEF; 0xCA; 0xFE] in
  let sw_fcs3 = sw_crc payload in
  reset ();
  List.iter payload ~f:feed_byte;
  idle ();
  let hw_fcs3 = Bits.to_int_trunc !(outputs.crc_out) lxor 0xFFFFFFFF in
  printf "  sw FCS = 0x%08x  hw FCS = 0x%08x  %s\n"
    sw_fcs3 hw_fcs3 (if hw_fcs3 = sw_fcs3 then "PASS" else "FAIL");
  check_hex ~case:"arbitrary payload FCS" ~expected:sw_fcs3 ~observed:hw_fcs3;

  printf "\n==== SUMMARY: %d checks, %d failures ====\n" !checks !failures;
  if waveform_enabled then Waveform.print ~display_width:96 waves;
  if !failures <> 0 then exit 1
  in
  if waveform_enabled
  then
    Out_channel.with_file "waves_tx_crc.vcd" ~f:(fun oc -> run (Vcd.wrap oc sim))
  else run sim
;;
