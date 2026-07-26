(*
  Bohdan Purtell
  University of Florida

  Testbench: UDP (L4) TX header generator (Udp_tx)

  Since the IPv4/UDP split, Udp_tx emits ONLY the UDP datagram — 8-byte UDP
  header ++ application payload — down to the IPv4 layer. This tb checks that
  byte stream (m_tdata/m_tvalid/m_tlast) against a software golden:

    [0..7]   UDP header (src_port, dst_port, udp_length, checksum=0)
    [8..]    application payload, verbatim
    m_tlast  asserted on the final payload byte

  The IPv4 header (and its checksum) is now Ipv4_tx's job — see ipv4_tx_tb.ml.
  Expected output length is exactly 8 + payload_len (Udp_tx does not pad).

  Coverage:
    - UDP header fields, length metadata, protocol, start, busy, and framing
    - zero-, one-, nominal-, and larger-payload datagrams
    - downstream backpressure, including stalls on both forms of final beat
    - application-source valid bubbles
    - payload length latching and back-to-back datagrams without reset
    - reset recovery while a datagram is in flight
    - AXI-stream output stability and payload-ready propagation
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Hardcaml_waveterm
open! Helper_tb_functions  (* provides the (<--) : Bits.t ref -> int -> unit driver *)

let () = print_endline "=== Running UDP TX Testbench ==="

(* endpoints — MUST match the Config used below *)
let src_port = 0x1234
let dst_port = 0x1235

module U = Udp_tx.Make (struct
  let src_port = src_port
  let dst_port = dst_port
end)

module Sim = Cyclesim.With_interface (U.I) (U.O)

let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF

let golden_header ~n =
  let udp_length = 8 + n in
  [ hi8 src_port; lo8 src_port; hi8 dst_port; lo8 dst_port
  ; hi8 udp_length; lo8 udp_length; 0x00; 0x00 ]

let make_payload n = List.init n ~f:(fun k -> ((k * 37) + 0x40) land 0xFF)

type run_result =
  { bytes : int list
  ; tlast_indices : int list
  ; ip_start_count : int
  ; start_metadata_ok : bool
  ; protocol_constant : bool
  ; saw_busy : bool
  ; busy_cleared : bool
  ; saw_downstream_stall : bool
  ; stream_stable_while_stalled : bool
  ; saw_source_bubble : bool
  ; payload_ready_ok : bool
  }

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (U.create scope) in
  let i = Cyclesim.inputs sim in
  let o_before = Cyclesim.outputs ~clock_edge:Side.Before sim in
  let o_after = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  let bit x = Bits.to_bool !x in

  let reset () =
    i.U.I.reset <-- 1;
    i.en <-- 1;
    i.start <-- 0;
    i.payload_len <-- 0;
    i.payload_tdata <-- 0;
    i.payload_tvalid <-- 0;
    i.l4_tready <-- 0;
    cycle ();
    i.reset <-- 0
  in

  (* Drive one datagram from Idle. Sampling the Before-edge output interface
     makes every scoreboard event correspond to the edge that actually accepts
     it; this avoids the post-edge phase ambiguity of the original smoke test. *)
  let drive
    ?(stall_every = 0)
    ?(bubble_every = 0)
    ?(final_stall_cycles = 0)
    ?mutate_len_after_start
    payload
    =
    let data = Array.of_list payload in
    let n = Array.length data in
    i.payload_len <-- n;

    let collected = ref [] in
    let transfer_count = ref 0 in
    let tlast_indices = ref [] in
    let ptr = ref 0 in
    let done_ = ref false in
    let started = ref false in
    let guard = ref 0 in
    let payload_phase = ref 0 in
    let final_stalls_used = ref 0 in
    let ip_start_count = ref 0 in
    let start_metadata_ok = ref true in
    let protocol_constant = ref true in
    let saw_busy = ref false in
    let saw_downstream_stall = ref false in
    let stream_stable_while_stalled = ref true in
    let saw_source_bubble = ref false in
    let payload_ready_ok = ref true in
    let previous_stalled_beat = ref None in
    let guard_limit = ((n + 8) * 8) + 128 in

    while (not !done_) && !guard < guard_limit do
      i.start <-- (if !started then 0 else 1);
      (match mutate_len_after_start with
       | Some changed when !started -> i.payload_len <-- changed
       | _ -> ());

      let header_complete = !transfer_count >= 8 in
      let has_payload = !ptr < n in
      let source_bubble =
        header_complete
        && has_payload
        && bubble_every > 0
        && !payload_phase % (bubble_every + 1) = bubble_every
      in
      let source_valid = has_payload && not source_bubble in
      i.payload_tvalid <-- (if source_valid then 1 else 0);
      i.payload_tdata <-- (if has_payload then data.(!ptr) else 0);
      if source_bubble then saw_source_bubble := true;

      let targeting_final_beat =
        if n = 0
        then !transfer_count = 7
        else header_complete && source_valid && !ptr = n - 1
      in
      let final_stall =
        targeting_final_beat && !final_stalls_used < final_stall_cycles
      in
      if final_stall then incr final_stalls_used;
      let periodic_stall =
        stall_every > 0 && !guard % (stall_every + 1) = stall_every
      in
      let ready = not (final_stall || periodic_stall) in
      i.l4_tready <-- (if ready then 1 else 0);

      Cyclesim.cycle_check sim;
      Cyclesim.cycle_before_clock_edge sim;

      let tvalid = bit o_before.U.O.m_tvalid in
      let tdata = Bits.to_int_trunc !(o_before.m_tdata) in
      let tlast = bit o_before.m_tlast in
      let payload_ready = bit o_before.payload_tready in
      let busy = bit o_before.busy in
      let payload_accepted = source_valid && payload_ready in

      if bit o_before.ip_start
      then (
        incr ip_start_count;
        if Bits.to_int_trunc !(o_before.l4_length) <> n + 8
           || Bits.to_int_trunc !(o_before.protocol) <> 17
        then start_metadata_ok := false);
      if Bits.to_int_trunc !(o_before.protocol) <> 17
      then protocol_constant := false;
      if busy then saw_busy := true;

      (match !previous_stalled_beat with
       | Some (previous_data, previous_last) ->
         if (not tvalid) || tdata <> previous_data || Bool.(tlast <> previous_last)
         then stream_stable_while_stalled := false
       | None -> ());
      previous_stalled_beat
        := if tvalid && not ready then Some (tdata, tlast) else None;
      if tvalid && not ready then saw_downstream_stall := true;

      let expected_payload_ready = header_complete && n > 0 && ready in
      if Bool.(payload_ready <> expected_payload_ready)
      then payload_ready_ok := false;

      if tvalid && ready
      then (
        collected := tdata :: !collected;
        if tlast then tlast_indices := !transfer_count :: !tlast_indices;
        incr transfer_count;
        if tlast then done_ := true);

      Cyclesim.cycle_at_clock_edge sim;
      Cyclesim.cycle_after_clock_edge sim;
      if payload_accepted then incr ptr;
      if header_complete then incr payload_phase;
      started := true;
      incr guard
    done;
    if !guard >= guard_limit then failwith "UDP TX testbench timed out waiting for tlast";
    { bytes = List.rev !collected
    ; tlast_indices = List.rev !tlast_indices
    ; ip_start_count = !ip_start_count
    ; start_metadata_ok = !start_metadata_ok
    ; protocol_constant = !protocol_constant
    ; saw_busy = !saw_busy
    ; busy_cleared = not (bit o_after.busy)
    ; saw_downstream_stall = !saw_downstream_stall
    ; stream_stable_while_stalled = !stream_stable_while_stalled
    ; saw_source_bubble = !saw_source_bubble
    ; payload_ready_ok = !payload_ready_ok
    }
  in

  let pass_count = ref 0 in
  let test_count = ref 0 in
  let expect label condition =
    incr test_count;
    if condition then incr pass_count;
    printf "  %-52s %s\n" label (if condition then "PASS" else "FAIL")
  in
  let expect_datagram label payload result =
    let expected = golden_header ~n:(List.length payload) @ payload in
    expect (label ^ ": bytes") (List.equal Int.equal result.bytes expected);
    expect
      (label ^ ": one tlast on final byte")
      (List.equal
         Int.equal
         result.tlast_indices
         [ List.length expected - 1 ]);
    expect (label ^ ": one ip_start") (result.ip_start_count = 1);
    expect (label ^ ": start metadata") result.start_metadata_ok;
    expect (label ^ ": protocol remains UDP") result.protocol_constant;
    expect (label ^ ": busy asserted") result.saw_busy;
    expect (label ^ ": busy clears") result.busy_cleared;
    expect (label ^ ": payload ready propagation") result.payload_ready_ok;
    expect
      (label ^ ": output stable while stalled")
      result.stream_stable_while_stalled
  in

  let run_clean ?stall_every ?bubble_every ?final_stall_cycles ?mutate_len payload =
    reset ();
    drive ?stall_every ?bubble_every ?final_stall_cycles
      ?mutate_len_after_start:mutate_len payload
  in

  printf "\n-- test 1: basic 18-byte payload --\n";
  let payload = make_payload 18 in
  expect_datagram "basic" payload (run_clean payload);

  printf "\n-- test 2: one-byte payload --\n";
  let payload = [ 0x5A ] in
  expect_datagram "one byte" payload (run_clean payload);

  printf "\n-- test 3: zero-length application datagram --\n";
  let payload = [] in
  expect_datagram "zero length" payload (run_clean payload);

  printf "\n-- test 4: periodic downstream backpressure --\n";
  let payload = make_payload 23 in
  let result = run_clean ~stall_every:3 payload in
  expect_datagram "downstream stalls" payload result;
  expect "downstream stalls: stall exercised" result.saw_downstream_stall;

  printf "\n-- test 5: application-source valid bubbles --\n";
  let payload = make_payload 17 in
  let result = run_clean ~bubble_every:2 payload in
  expect_datagram "source bubbles" payload result;
  expect "source bubbles: bubble exercised" result.saw_source_bubble;

  printf "\n-- test 6: final payload beat held under backpressure --\n";
  let payload = make_payload 5 in
  let result = run_clean ~final_stall_cycles:3 payload in
  expect_datagram "final payload stall" payload result;
  expect "final payload stall: stall exercised" result.saw_downstream_stall;

  printf "\n-- test 7: zero-length final header held under backpressure --\n";
  let payload = [] in
  let result = run_clean ~final_stall_cycles:2 payload in
  expect_datagram "final header stall" payload result;
  expect "final header stall: stall exercised" result.saw_downstream_stall;

  printf "\n-- test 8: payload length is latched at start --\n";
  let payload = make_payload 12 in
  let result = run_clean ~mutate_len:0x3456 payload in
  expect_datagram "latched length" payload result;

  printf "\n-- test 9: 300-byte payload --\n";
  let payload = make_payload 300 in
  expect_datagram "large payload" payload (run_clean payload);

  printf "\n-- test 10: back-to-back datagrams without reset --\n";
  reset ();
  let payload_a = make_payload 3 in
  let result_a = drive payload_a in
  expect_datagram "back-to-back A" payload_a result_a;
  let payload_b = make_payload 31 in
  let result_b = drive ~stall_every:2 payload_b in
  expect_datagram "back-to-back B" payload_b result_b;
  expect "back-to-back B: stall exercised" result_b.saw_downstream_stall;

  printf "\n-- test 11: reset recovery during a datagram --\n";
  reset ();
  i.payload_len <-- 20;
  i.payload_tvalid <-- 1;
  i.payload_tdata <-- 0xA5;
  i.l4_tready <-- 1;
  i.start <-- 1;
  cycle ();
  i.start <-- 0;
  Cyclesim.cycle ~n:3 sim;
  expect "reset recovery: busy before reset" (bit o_after.busy);
  i.reset <-- 1;
  cycle ();
  expect "reset recovery: busy clears on reset" (not (bit o_after.busy));
  expect "reset recovery: valid clears on reset" (not (bit o_after.m_tvalid));
  expect
    "reset recovery: payload ready clears on reset"
    (not (bit o_after.payload_tready));
  i.reset <-- 0;
  let payload = make_payload 9 in
  expect_datagram "reset recovery packet" payload (drive payload);

  printf "\n==== SUMMARY: %d/%d checks passed ====\n" !pass_count !test_count;
  print_endline "\n=== SIMULATION COMPLETE ===";
  if !pass_count <> !test_count then failwith "UDP TX testbench failures"
;;
