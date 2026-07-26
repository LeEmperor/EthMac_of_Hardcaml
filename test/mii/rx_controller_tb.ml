(*
  Jane Street
  Author: Bohdan Purtell

  Testbench: "rx_controller_tb.ml"

  Alcotest Stanza Inclusions: 


 *)

open! Core
open! Hardcaml
open! Mii_of_hardcaml

let () = print_endline "=== Running MAC RX Controller Testbench ==="

module Sim = Cyclesim.With_interface (Rx_controller.I) (Rx_controller.O)

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Rx_controller.create scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let ( <-- ) r v = r := Bits.of_int_trunc ~width:(Bits.width !r) v in

  let all_ok = ref true in
  let check name cond =
    if not cond then all_ok := false;
    printf "  %-46s: %s\n" name (if cond then "PASS" else "FAIL")
  in

  (* clock + first-high timestamps for each observable output *)
  let t = ref 0 in
  let first = Hashtbl.create (module String) in
  let bit r = Bits.to_bool !r in
  let record () =
    let mark name v =
      if v && not (Hashtbl.mem first name) then Hashtbl.set first ~key:name ~data:!t
    in
    mark "in_preamble" (bit o.Rx_controller.O.in_preamble);
    mark "in_dst_mac" (bit o.in_dst_mac);
    mark "dst_mac_reg_en" (bit o.dst_mac_reg_en);
    mark "src_mac_reg_en" (bit o.src_mac_reg_en);
    mark "eth_type_reg_en" (bit o.eth_type_reg_en);
    mark "in_payload" (bit o.in_payload)
  in
  let cycle () =
    Cyclesim.cycle sim;
    record ();
    incr t
  in

  (* one assembled byte per cycle (rx_data_valid pulses like the assembler does) *)
  (* leaves rx_er untouched so the caller controls it (abort test) *)
  let send_byte b =
    i.en <-- 1;
    i.rx_dv <-- 1;
    i.rx_data_valid <-- 1;
    i.rx_data <-- b;
    cycle ()
  in

  (* reset *)
  i.reset <-- 1;
  i.en <-- 0;
  i.rx_dv <-- 0;
  i.rx_er <-- 0;
  i.rx_data_valid <-- 0;
  i.rx_data <-- 0;
  cycle ();
  i.reset <-- 0;

  (* byte_assembler_en tracks en & rx_dv, combinationally *)
  i.en <-- 1;
  i.rx_dv <-- 1;
  Cyclesim.cycle sim;
  check "byte_assembler_en = en & rx_dv" (bit o.byte_assembler_en);

  (* drive a full header + payload *)
  for _ = 1 to 6 do send_byte 0x55 done;   (* preamble *)
  send_byte 0xD5;                           (* SFD *)
  for _ = 1 to 6 do send_byte 0x11 done;   (* dst mac *)
  for _ = 1 to 6 do send_byte 0x22 done;   (* src mac *)
  send_byte 0x45; send_byte 0x21;           (* ethertype *)
  let payload_emit = ref true in
  let payload_sel  = ref true in
  for _ = 1 to 8 do
    send_byte 0x77;                          (* payload *)
    if bit o.in_payload then begin
      if not (bit o.emit_payload) then payload_emit := false;
      if not (bit o.payload_sel)  then payload_sel  := false
    end
  done;

  (* error mid-payload aborts back to idle (in_payload drops next state) *)
  i.rx_er <-- 1;
  send_byte 0x77;
  send_byte 0x77;
  i.rx_er <-- 0;
  check "rx_er aborts payload (in_payload low after error)" (not (bit o.in_payload));

  let seen name = Hashtbl.mem first name in
  let at name = Hashtbl.find_exn first name in
  printf "\n  first-high cycles: %s\n"
    (String.concat ~sep:"  "
       (List.map
          [ "in_preamble"; "in_dst_mac"; "dst_mac_reg_en"; "src_mac_reg_en"
          ; "eth_type_reg_en"; "in_payload" ]
          ~f:(fun n -> sprintf "%s=%s" n (if seen n then Int.to_string (at n) else "-"))));

  printf "\n[assertions]\n";
  check "in_preamble asserted" (seen "in_preamble");
  check "in_dst_mac asserted" (seen "in_dst_mac");
  check "dst_mac_reg_en asserted" (seen "dst_mac_reg_en");
  check "src_mac_reg_en asserted" (seen "src_mac_reg_en");
  check "eth_type_reg_en asserted" (seen "eth_type_reg_en");
  check "in_payload asserted" (seen "in_payload");
  check "order: preamble < dst < payload"
    (seen "in_preamble" && seen "in_dst_mac" && seen "in_payload"
     && at "in_preamble" < at "in_dst_mac"
     && at "in_dst_mac" < at "in_payload");
  check "order: dst_en < src_en < eth_en"
    (at "dst_mac_reg_en" < at "src_mac_reg_en"
     && at "src_mac_reg_en" < at "eth_type_reg_en");
  check "payload: emit_payload high throughout" !payload_emit;
  check "payload: payload_sel high throughout" !payload_sel;

  printf "\n=== %s ===\n" (if !all_ok then "ALL PASS" else "FAILURES PRESENT");
  print_endline "=== SIMULATION COMPLETE ===";
  if not !all_ok then exit 1
;;
