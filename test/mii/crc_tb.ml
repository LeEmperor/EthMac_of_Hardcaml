open! Core
open! Hardcaml
open! Mii_of_hardcaml
open! Hardcaml_waveterm
open! Helper_tb_functions

let waveform_enabled =
  Array.exists (Sys.get_argv ()) ~f:(String.equal "--waveform")

let () =
  print_endline "=== Running MAC CRC Testbench ==="

module Waveform = Hardcaml_waveterm.Waveform
module Sim = Cyclesim.With_interface(Rx_crc.I)(Rx_crc.O)

let create_sim () =
  let scope  = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim    = Sim.create ~config:Cyclesim.Config.trace_all (Rx_crc.create scope) in
  let waves, sim = Waveform.create sim in
  let inputs  : _ Rx_crc.I.t = Cyclesim.inputs sim in
  let outputs : _ Rx_crc.O.t = Cyclesim.outputs sim in
  (sim, waves, inputs, outputs)
;;

(* software reference CRC-32 to cross-check the hardware *)
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

let sw_crc bytes =
  List.fold bytes ~init:0xFFFFFFFF ~f:sw_crc_byte

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

  let t_rst   = inputs.reset in
  let t_en    = inputs.en in
  let t_data  = inputs.rx_data in
  let t_valid = inputs.rx_data_valid in

  let cycle () =
    Cyclesim.cycle sim;
    if waveform_enabled then
      printf "  data=0x%02x valid=%d  hw_crc=0x%08x  crc_valid=%d\n"
        (Bits.to_int_trunc !(t_data))
        (Bits.to_int_trunc !(t_valid))
        (Bits.to_int_trunc !(outputs.crc_out))
        (Bits.to_int_trunc !(outputs.crc_valid))
  in

  let reset () =
    t_en    <-- 0;
    t_rst   <-- 1;
    t_valid <-- 0;
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
    t_en    <-- 0;
    t_valid <-- 0;
    cycle ();
  in

  (* -- test 1: standard CRC-32 test vector --                    *)
  (* data : "123456789" (0x31..0x39)                              *)
  (* CRC-32 of that data = 0xCBF43926                             *)
  (* FCS bytes (little-endian): 0x26 0x39 0xF4 0xCB              *)
  (* feeding data+FCS through running CRC yields residue 0x2144DF1C *)
  let data_bytes = [0x31;0x32;0x33;0x34;0x35;0x36;0x37;0x38;0x39] in
  let fcs_bytes  = [0x26;0x39;0xF4;0xCB] in
  let sw_result  = sw_crc (data_bytes @ fcs_bytes) in
  printf "\n-- [test 1] standard CRC-32 test vector (123456789) --\n";
  printf "  sw reference CRC after data+FCS = 0x%08x  (expect 0xdebb20e3)\n" sw_result;
  check_hex ~case:"software standard residue" ~expected:0xDEBB20E3 ~observed:sw_result;
  reset ();

  (* List.iter is pretty cool, almost like that one Rust function that does a function on each thing in an iterable *)
  List.iter data_bytes ~f:feed_byte;
  List.iter fcs_bytes  ~f:feed_byte;

  t_valid <-- 0;
  let hw_residue = Bits.to_int_trunc !(outputs.crc_out) in
  let hw_valid = Bits.to_int_trunc !(outputs.crc_valid) in
  check_hex ~case:"hardware standard residue" ~expected:0xDEBB20E3 ~observed:hw_residue;
  check_hex ~case:"standard FCS crc_valid" ~expected:1 ~observed:hw_valid;

  (* -- test 2: wrong FCS should not assert crc_valid -- *)
  printf "\n-- [test 2] wrong FCS (all zeros) --\n";
  reset ();

  List.iter data_bytes ~f:feed_byte;
  List.iter [0x00;0x00;0x00;0x00] ~f:feed_byte;

  t_valid <-- 0;
  check_hex
    ~case:"wrong FCS rejection"
    ~expected:0
    ~observed:(Bits.to_int_trunc !(outputs.crc_valid));

  (* -- test 3: en drop mid-frame resets accumulator --           *)
  printf "\n-- [test 3] mid-frame reset via en drop, then full sequence --\n";
  reset ();
  feed_byte 0xDE;
  feed_byte 0xAD;
  idle ();
  t_en <-- 1;

  List.iter data_bytes ~f:feed_byte;
  List.iter fcs_bytes  ~f:feed_byte;

  t_valid <-- 0;
  check_hex
    ~case:"reset recovery residue"
    ~expected:0xDEBB20E3
    ~observed:(Bits.to_int_trunc !(outputs.crc_out));
  check_hex
    ~case:"reset recovery crc_valid"
    ~expected:1
    ~observed:(Bits.to_int_trunc !(outputs.crc_valid));

  printf "\n==== SUMMARY: %d checks, %d failures ====\n" !checks !failures;
  if waveform_enabled then Waveform.print ~display_width:96 waves;
  if !failures <> 0 then exit 1
  in
  if waveform_enabled
  then Out_channel.with_file "waves_crc.vcd" ~f:(fun oc -> run (Vcd.wrap oc sim))
  else run sim
;;
