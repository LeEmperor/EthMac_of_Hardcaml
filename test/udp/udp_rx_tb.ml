(*
  Bohdan Purtell
  University of Florida

  Testbench: UDP (L4) RX header parser (Udp_rx)

  Drives the UDP datagram byte stream exactly as Ipv4_rx presents it and checks
  header stripping, metadata, filtering, backpressure, malformed datagrams, and
  error forwarding.
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Helper_tb_functions

let () = print_endline "=== Running UDP RX Testbench ==="

let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF
let ip32 bytes = List.fold bytes ~init:0 ~f:(fun a b -> (a lsl 8) lor (b land 0xFF))

let udp_datagram ?udp_length ?(checksum = 0) ~src_port ~dst_port ~payload () =
  let length = Option.value udp_length ~default:(8 + List.length payload) in
  [ hi8 src_port
  ; lo8 src_port
  ; hi8 dst_port
  ; lo8 dst_port
  ; hi8 length
  ; lo8 length
  ; hi8 checksum
  ; lo8 checksum
  ]
  @ payload
;;

type metadata =
  { src_port : int
  ; dst_port : int
  ; udp_length : int
  ; payload_length : int
  ; udp_checksum : int
  ; src_ip : int
  ; dst_ip : int
  }

type run_result =
  { payload : int list
  ; metadata : metadata option
  ; app_start_count : int
  ; tfirst_count : int
  ; tlast_indices : int list
  ; crc_error : bool
  ; port_match : bool
  ; busy : bool
  ; saw_payload_stall : bool
  ; ready_low_on_all_stalls : bool
  }

module Testbench (C : Udp_rx.Config) = struct
  module Rx = Udp_rx.Make (C)
  module Sim = Cyclesim.With_interface (Rx.I) (Rx.O)

  let bit x = Bits.to_bool !x

  let run
    ?(protocol = 17)
    ?(fcs_bad = false)
    ?(stall_every = 0)
    ?(src_ip = [ 192; 168; 1; 1 ])
    ?(dst_ip = [ 192; 168; 1; 10 ])
    datagram
    =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let sim = Sim.create (Rx.create scope) in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    let cycle () = Cyclesim.cycle sim in
    i.Rx.I.reset <-- 1;
    i.en <-- 1;
    i.rx_tdata <-- 0;
    i.rx_tvalid <-- 0;
    i.rx_tlast <-- 0;
    i.rx_tuser <-- 0;
    i.rx_tfirst <-- 0;
    i.ip_protocol <-- protocol;
    i.ip_src_ip <-- ip32 src_ip;
    i.ip_dst_ip <-- ip32 dst_ip;
    i.app_tready <-- 1;
    cycle ();
    i.reset <-- 0;

    let bytes = Array.of_list datagram in
    let length = Array.length bytes in
    let ptr = ref 0 in
    let drain_cycles = ref 0 in
    let guard = ref 0 in
    let payload_phase = ref 0 in
    let collected = ref [] in
    let transfer_count = ref 0 in
    let metadata = ref None in
    let app_start_count = ref 0 in
    let tfirst_count = ref 0 in
    let tlast_indices = ref [] in
    let saw_payload_stall = ref false in
    let ready_low_on_all_stalls = ref true in

    (* Cyclesim reports the Mealy stream qualifiers after the state register
       update. Retain each post-cycle qualifier and pair it with the byte driven
       in the following iteration. This is the same phase handling used by the
       IPv4 RX testbench. *)
    let q_valid = ref (bit o.Rx.O.m_tvalid) in
    let q_first = ref (bit o.m_tfirst) in
    let q_last = ref (bit o.m_tlast) in
    let q_app_start = ref (bit o.app_start) in
    let guard_limit = (length * 4) + 32 in
    while (!ptr < length || !drain_cycles < 3) && !guard < guard_limit do
      let has_byte = !ptr < length in
      let present = if has_byte then bytes.(!ptr) else 0 in
      let stalling =
        !q_valid
        && stall_every > 0
        && !payload_phase mod (stall_every + 1) = stall_every
      in
      let app_ready = not stalling in
      let transfer = has_byte && !q_valid && app_ready in
      let start_event = !q_app_start && ((not !q_valid) || app_ready) in

      if start_event
      then (
        incr app_start_count;
        metadata
        := Some
             { src_port = Bits.to_int_trunc !(o.Rx.O.src_port)
             ; dst_port = Bits.to_int_trunc !(o.dst_port)
             ; udp_length = Bits.to_int_trunc !(o.udp_length)
             ; payload_length = Bits.to_int_trunc !(o.payload_length)
             ; udp_checksum = Bits.to_int_trunc !(o.udp_checksum)
             ; src_ip = Bits.to_int_trunc !(o.src_ip)
             ; dst_ip = Bits.to_int_trunc !(o.dst_ip)
             });
      if transfer
      then (
        collected := present :: !collected;
        if !q_first then incr tfirst_count;
        if !q_last then tlast_indices := !transfer_count :: !tlast_indices;
        incr transfer_count);

      i.app_tready <-- (if app_ready then 1 else 0);
      i.rx_tvalid <-- (if has_byte then 1 else 0);
      i.rx_tdata <-- present;
      i.rx_tfirst <-- (if has_byte && !ptr = 0 then 1 else 0);
      i.rx_tlast <-- (if has_byte && !ptr = length - 1 then 1 else 0);
      i.rx_tuser <-- (if has_byte && !ptr = length - 1 && fcs_bad then 1 else 0);

      (* Header/Idle/Flush always accept. Payload acceptance follows app_tready. *)
      let accepted = has_byte && ((not !q_valid) || app_ready) in
      cycle ();

      if stalling && bit o.m_tvalid
      then (
        saw_payload_stall := true;
        if bit o.m_axis_tready then ready_low_on_all_stalls := false);
      q_valid := bit o.m_tvalid;
      q_first := bit o.m_tfirst;
      q_last := bit o.m_tlast;
      q_app_start := bit o.app_start;
      if !q_valid then incr payload_phase;
      if accepted then incr ptr;
      if !ptr >= length then incr drain_cycles else drain_cycles := 0;
      incr guard
    done;
    if !guard >= guard_limit then failwith "UDP RX testbench driver timed out";
    { payload = List.rev !collected
    ; metadata = !metadata
    ; app_start_count = !app_start_count
    ; tfirst_count = !tfirst_count
    ; tlast_indices = List.rev !tlast_indices
    ; crc_error = bit o.crc_error
    ; port_match = bit o.port_match
    ; busy = bit o.busy
    ; saw_payload_stall = !saw_payload_stall
    ; ready_low_on_all_stalls = !ready_low_on_all_stalls
    }
  ;;
end

module Accept_all = Testbench (struct
    let drop_on_port_mismatch = false
    let expected_dst_port = 0x1235
    let debug = true
  end)

module Filter_port = Testbench (struct
    let drop_on_port_mismatch = true
    let expected_dst_port = 0x1235
    let debug = true
  end)

let () =
  let pass_count = ref 0 in
  let test_count = ref 0 in
  let expect label condition =
    incr test_count;
    if condition then incr pass_count;
    printf "  %-44s %s\n" label (if condition then "PASS" else "FAIL")
  in
  let expect_metadata
    result
    ~src_port
    ~dst_port
    ~payload_length
    ~checksum
    ~src_ip
    ~dst_ip
    =
    match result.metadata with
    | None -> expect "metadata captured" false
    | Some meta ->
      expect "src_port" (meta.src_port = src_port);
      expect "dst_port" (meta.dst_port = dst_port);
      expect "udp_length" (meta.udp_length = payload_length + 8);
      expect "payload_length" (meta.payload_length = payload_length);
      expect "udp_checksum" (meta.udp_checksum = checksum);
      expect "src_ip passthrough" (meta.src_ip = ip32 src_ip);
      expect "dst_ip passthrough" (meta.dst_ip = ip32 dst_ip)
  in
  let src_ip = [ 192; 168; 1; 1 ] in
  let dst_ip = [ 192; 168; 1; 10 ] in

  printf "\n-- test 1: basic 4-byte payload and metadata --\n";
  let payload = [ 0xDE; 0xAD; 0xBE; 0xEF ] in
  let checksum = 0xBEEF in
  let result =
    Accept_all.run
      (udp_datagram ~src_port:0x1234 ~dst_port:0x1235 ~checksum ~payload ())
  in
  expect "payload bytes" (List.equal Int.equal result.payload payload);
  expect "one app_start" (result.app_start_count = 1);
  expect "one payload tfirst" (result.tfirst_count = 1);
  expect "tlast on final payload byte" (List.equal Int.equal result.tlast_indices [ 3 ]);
  expect "destination port matches" result.port_match;
  expect "no crc_error" (not result.crc_error);
  expect "busy clears" (not result.busy);
  expect_metadata
    result
    ~src_port:0x1234
    ~dst_port:0x1235
    ~payload_length:4
    ~checksum
    ~src_ip
    ~dst_ip;

  printf "\n-- test 2: one-byte payload --\n";
  let payload = [ 0x5A ] in
  let result =
    Accept_all.run (udp_datagram ~src_port:0xABCD ~dst_port:0x1235 ~payload ())
  in
  expect "payload bytes" (List.equal Int.equal result.payload payload);
  expect
    "tfirst and tlast share the byte"
    (result.tfirst_count = 1 && List.equal Int.equal result.tlast_indices [ 0 ]);
  expect "one app_start" (result.app_start_count = 1);

  printf "\n-- test 3: zero-length application datagram --\n";
  let result =
    Accept_all.run (udp_datagram ~src_port:0x2222 ~dst_port:0x1235 ~payload:[] ())
  in
  expect "no payload emitted" (List.is_empty result.payload);
  expect
    "no payload qualifiers"
    (result.tfirst_count = 0 && List.is_empty result.tlast_indices);
  expect "app_start still reports the datagram" (result.app_start_count = 1);
  expect "busy clears" (not result.busy);
  expect_metadata
    result
    ~src_port:0x2222
    ~dst_port:0x1235
    ~payload_length:0
    ~checksum:0
    ~src_ip
    ~dst_ip;

  printf "\n-- test 4: accept-all mode reports a mismatched port --\n";
  let payload = [ 1; 2; 3 ] in
  let result =
    Accept_all.run (udp_datagram ~src_port:0x1234 ~dst_port:0x9999 ~payload ())
  in
  expect
    "mismatched payload still forwarded"
    (List.equal Int.equal result.payload payload);
  expect "port_match deasserted" (not result.port_match);

  printf "\n-- test 5: bound-port filtering --\n";
  let payload = [ 0x10; 0x20; 0x30; 0x40 ] in
  let dropped =
    Filter_port.run (udp_datagram ~src_port:0x4321 ~dst_port:0x9999 ~payload ())
  in
  expect "wrong-port payload dropped" (List.is_empty dropped.payload);
  expect "wrong-port app_start suppressed" (dropped.app_start_count = 0);
  expect "wrong-port port_match deasserted" (not dropped.port_match);
  expect "wrong-port busy clears" (not dropped.busy);
  let accepted =
    Filter_port.run (udp_datagram ~src_port:0x4321 ~dst_port:0x1235 ~payload ())
  in
  expect
    "right-port payload forwarded"
    (List.equal Int.equal accepted.payload payload);
  expect "right-port port_match asserted" accepted.port_match;
  expect "right-port app_start" (accepted.app_start_count = 1);

  printf "\n-- test 6: non-UDP IP protocol --\n";
  let result =
    Accept_all.run
      ~protocol:6
      (udp_datagram ~src_port:0x1234 ~dst_port:0x1235 ~payload:[ 1; 2; 3 ] ())
  in
  expect "TCP datagram emits no payload" (List.is_empty result.payload);
  expect "TCP datagram emits no app_start" (result.app_start_count = 0);
  expect "TCP flush clears busy" (not result.busy);

  printf "\n-- test 7: application backpressure --\n";
  let payload = List.init 23 ~f:(fun k -> (0x40 + k) land 0xFF) in
  let result =
    Accept_all.run
      ~stall_every:3
      (udp_datagram ~src_port:0x1234 ~dst_port:0x1235 ~payload ())
  in
  expect
    "payload preserved under stalls"
    (List.equal Int.equal result.payload payload);
  expect "payload stall exercised" result.saw_payload_stall;
  expect "upstream ready low on every app stall" result.ready_low_on_all_stalls;
  expect "single tfirst transfer" (result.tfirst_count = 1);
  expect
    "tlast remains on final payload byte"
    (List.equal Int.equal result.tlast_indices [ List.length payload - 1 ]);
  expect "busy clears after stalled datagram" (not result.busy);

  printf "\n-- test 8: frame truncated inside UDP header --\n";
  let full = udp_datagram ~src_port:0x1234 ~dst_port:0x1235 ~payload:[ 1; 2; 3 ] () in
  let result = Accept_all.run (List.take full 5) in
  expect "header truncation emits no payload" (List.is_empty result.payload);
  expect "header truncation emits no app_start" (result.app_start_count = 0);
  expect "header truncation clears busy" (not result.busy);

  printf "\n-- test 9: frame truncated inside UDP payload --\n";
  let partial_payload = [ 0xA1; 0xA2; 0xA3 ] in
  let result =
    Accept_all.run
      (udp_datagram
         ~udp_length:14
         ~src_port:0x1234
         ~dst_port:0x1235
         ~payload:partial_payload
         ())
  in
  expect
    "available payload bytes forwarded"
    (List.equal Int.equal result.payload partial_payload);
  expect "truncated payload has no UDP tlast" (List.is_empty result.tlast_indices);
  expect "truncated payload latches crc_error" result.crc_error;
  expect "truncated payload clears busy" (not result.busy);

  printf "\n-- test 10: lower-layer error flag forwarding --\n";
  let payload = [ 0xC0; 0xFF; 0xEE ] in
  let result =
    Accept_all.run
      ~fcs_bad:true
      (udp_datagram ~src_port:0x1234 ~dst_port:0x1235 ~payload ())
  in
  expect
    "bad-FCS payload still forwarded"
    (List.equal Int.equal result.payload payload);
  expect "bad-FCS crc_error asserted" result.crc_error;
  expect "bad-FCS tlast correct" (List.equal Int.equal result.tlast_indices [ 2 ]);

  printf "\n-- test 11: 96-byte payload --\n";
  let payload = List.init 96 ~f:(fun k -> ((k * 37) + 11) land 0xFF) in
  let result =
    Accept_all.run (udp_datagram ~src_port:0x0001 ~dst_port:0x1235 ~payload ())
  in
  expect "large payload bytes" (List.equal Int.equal result.payload payload);
  expect "large payload tlast" (List.equal Int.equal result.tlast_indices [ 95 ]);
  expect "large payload busy clears" (not result.busy);
  (match result.metadata with
   | Some meta -> expect "large payload length metadata" (meta.payload_length = 96)
   | None -> expect "large payload metadata captured" false);

  printf "\n==== SUMMARY: %d/%d checks passed ====\n" !pass_count !test_count;
  print_endline "\n=== SIMULATION COMPLETE ===";
  if !pass_count <> !test_count then failwith "UDP RX testbench failures"
;;
