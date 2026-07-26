open! Core
open! Hardcaml
open! Mii_of_hardcaml
open! Hardcaml_waveterm
open! Helper_tb_functions

let () = print_endline "=== Running MAC TX Byte Disassembler Testbench ==="

let waveform_enabled =
  Array.exists (Sys.get_argv ()) ~f:(String.equal "--waveform")

module Waveform = Hardcaml_waveterm.Waveform
module Sim = Cyclesim.With_interface (Tx_byte_disassembler.I) (Tx_byte_disassembler.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim =
    Sim.create ~config:Cyclesim.Config.trace_all (Tx_byte_disassembler.create scope)
  in
  let waves, sim = Waveform.create sim in
  sim, waves
;;

let run sim waves =
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs ~clock_edge:Side.Before sim in
  let cycle_index = ref 0 in
  let checks = ref 0 in
  let failures = ref 0 in
  let check_int ~case ~signal ~expected observed =
    Int.incr checks;
    if observed <> expected
    then (
      Int.incr failures;
      eprintf
        "%s: %s mismatch at cycle %d: expected 0x%x, observed 0x%x\n"
        case
        signal
        !cycle_index
        expected
        observed)
  in
  let sample ~case ~tx_d ~tx_en ~ready =
    Cyclesim.cycle_check sim;
    Cyclesim.cycle_before_clock_edge sim;
    check_int
      ~case
      ~signal:"tx_d"
      ~expected:tx_d
      (Bits.to_int_trunc !(o.Tx_byte_disassembler.O.tx_d));
    check_int
      ~case
      ~signal:"tx_en"
      ~expected:tx_en
      (Bits.to_int_trunc !(o.tx_en));
    check_int
      ~case
      ~signal:"ready"
      ~expected:ready
      (Bits.to_int_trunc !(o.ready));
    Cyclesim.cycle_at_clock_edge sim;
    Cyclesim.cycle_after_clock_edge sim;
    Int.incr cycle_index
  in

  i.Tx_byte_disassembler.I.reset <-- 1;
  i.en <-- 1;
  i.byte_in <-- 0;
  i.byte_in_valid <-- 0;
  sample ~case:"reset" ~tx_d:0 ~tx_en:0 ~ready:1;
  i.reset <-- 0;

  (* A byte is accepted only while [ready] is high. MII sends the low nibble
     first, then holds [ready] low for the high-nibble cycle. *)
  i.byte_in <-- 0xAB;
  i.byte_in_valid <-- 1;
  sample ~case:"0xab low nibble" ~tx_d:0xB ~tx_en:1 ~ready:1;
  i.byte_in_valid <-- 0;
  sample ~case:"0xab high nibble" ~tx_d:0xA ~tx_en:1 ~ready:0;
  sample ~case:"idle after 0xab" ~tx_d:0 ~tx_en:0 ~ready:1;

  (* Repeat with values that make ordering mistakes obvious at both extremes. *)
  List.iter [ 0x12; 0xF0; 0x0F ] ~f:(fun byte ->
    i.byte_in <-- byte;
    i.byte_in_valid <-- 1;
    sample
      ~case:(sprintf "0x%02x low nibble" byte)
      ~tx_d:(byte land 0xF)
      ~tx_en:1
      ~ready:1;
    i.byte_in_valid <-- 0;
    sample
      ~case:(sprintf "0x%02x high nibble" byte)
      ~tx_d:((byte lsr 4) land 0xF)
      ~tx_en:1
      ~ready:0);
  sample ~case:"final idle" ~tx_d:0 ~tx_en:0 ~ready:1;

  printf "==== SUMMARY: %d checks, %d failures ====\n" !checks !failures;
  if waveform_enabled then Waveform.print ~display_width:120 waves;
  if !failures <> 0 then exit 1
;;

let () =
  let sim, waves = create_sim () in
  if waveform_enabled
  then
    Out_channel.with_file "waves_byte_disassembler.vcd" ~f:(fun oc ->
      run (Vcd.wrap oc sim) waves)
  else run sim waves
;;
