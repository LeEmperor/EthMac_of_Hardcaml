open! Core
open! Hardcaml
open! Mii_of_hardcaml

let () = print_endline "=== Running MAC RX Datapath Testbench ==="

module Sim = Cyclesim.With_interface (Rx_datapath.I) (Rx_datapath.O)

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Rx_datapath.create scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let ( <-- ) r v = r := Bits.of_int_trunc ~width:(Bits.width !r) v in
  let cycle () = Cyclesim.cycle sim in

  let all_ok = ref true in
  let check name cond =
    if not cond then all_ok := false;
    printf "  %-46s: %s\n" name (if cond then "PASS" else "FAIL")
  in

  let reset () =
    i.Rx_datapath.I.reset <-- 1;
    i.en <-- 0;
    i.byte_assembler_en <-- 0;
    i.dst_mac_reg_en <-- 0;
    i.src_mac_reg_en <-- 0;
    i.eth_type_reg_en <-- 0;
    i.payload_sel <-- 0;
    i.emit_payload <-- 0;
    i.fcs_present <-- 0;
    i.rx_data <-- 0;
    cycle ();
    i.reset <-- 0;
    i.en <-- 1;
    i.byte_assembler_en <-- 1
  in

  (* Drive one MII nibble and, matching mac_top's write gating
     (payload_out_valid & raw_byte_out_valid), collect one payload byte per
     assembled byte. *)
  let collected = ref [] in
  let step nib =
    i.rx_data <-- nib;
    cycle ();
    if Bits.to_bool !(o.Rx_datapath.O.payload_out_valid)
       && Bits.to_bool !(o.raw_byte_out_valid)
    then collected := !collected @ [ Bits.to_int_trunc !(o.payload_out) ]
  in
  let send_byte b =
    step (b land 0xF);
    step ((b lsr 4) land 0xF)
  in

  (* ── test 1: ethertype latch ── *)
  printf "\n[test 1] ethertype latch (0x45, 0x21 -> 0x4521)\n";
  reset ();
  i.eth_type_reg_en <-- 1;
  send_byte 0x45;
  send_byte 0x21;
  cycle ();  (* flush: last byte's raw_byte_out_valid shifts 0x21 in while reg_en high *)
  i.eth_type_reg_en <-- 0;
  printf "  eth_type = 0x%04x\n" (Bits.to_int_trunc !(o.eth_type));
  check "eth_type == 0x4521" (Bits.to_int_trunc !(o.eth_type) = 0x4521);

  (* ── test 2: FCS strip — payload emitted, trailing 4 CRC bytes dropped ── *)
  printf "\n[test 2] FCS strip\n";
  reset ();
  collected := [];
  let payload = [ 0x11; 0x22; 0x33; 0x44; 0x55 ] in
  let fcs = [ 0xAA; 0xBB; 0xCC; 0xDD ] in
  (* emit_payload / payload_sel high across payload+FCS, exactly as the controller
     holds them while rx_dv is asserted; both drop when the frame ends. *)
  i.emit_payload <-- 1;
  i.payload_sel <-- 1;
  List.iter (payload @ fcs) ~f:send_byte;
  i.emit_payload <-- 0;
  i.payload_sel <-- 0;
  (* drain the 4-deep pipeline *)
  for _ = 0 to 12 do
    step 0
  done;
  printf "  collected: %s\n"
    (String.concat ~sep:" " (List.map !collected ~f:(sprintf "%02x")));
  check "payload bytes emitted in order, FCS stripped"
    (List.equal Int.equal !collected payload);

  printf "\n=== %s ===\n" (if !all_ok then "ALL PASS" else "FAILURES PRESENT");
  print_endline "=== SIMULATION COMPLETE ===";
  if not !all_ok then exit 1
;;
