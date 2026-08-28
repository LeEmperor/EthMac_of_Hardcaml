(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_controller_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Tx_controller

   Typed examples and generated properties covering the transmit state walk, the
   store-and-forward launch handshake, the serializer stall, the zero-pad path for
   sub-minimum datagrams, and the CRC enable.

   Net-new verification - this block had no testbench of its own. The oracle is the frame
   layout itself: a datagram of [n] payload bytes puts 7 + 1 + 6 + 6 + 2 + max(n, 46) + 4
   bytes on the wire, one per cycle while [dis_ready] holds, and the per-state cycle
   counts are the per-field byte counts of that frame.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Tx_controller_testbench

let frame_states =
  [ State.Preamble
  ; State.Sfd
  ; State.Dst_mac
  ; State.Src_mac
  ; State.Eth_type
  ; State.Payload
  ; State.Fcs
  ]
;;

(* The frame starts and ends in Idle, with each header field visited exactly once. *)
let expected_state_sequence = (State.Idle :: frame_states) @ [ State.Idle ]

let expected_cycles_per_state payload_length =
  [ State.Idle, 2
  ; State.Preamble, 7
  ; State.Sfd, 1
  ; State.Dst_mac, 6
  ; State.Src_mac, 6
  ; State.Eth_type, 2
  ; State.Payload, Int.max payload_length Testbench.minimum_payload_length
  ; State.Fcs, 4
  ]
;;

let%test_unit "a minimum-length datagram walks the whole frame" =
  let observation = Testbench.run_frame ~payload_length:46 in
  [%test_result: State.t list]
    (Testbench.state_sequence observation)
    ~expect:expected_state_sequence;
  [%test_result: (State.t * int) list]
    (Testbench.cycles_per_state observation)
    ~expect:(expected_cycles_per_state 46);
  [%test_result: int]
    (Testbench.emitted_byte_count observation)
    ~expect:(Testbench.expected_byte_count 46)
;;

let%test_unit "a long datagram runs the payload phase to its own length" =
  let payload_length = 100 in
  let observation = Testbench.run_frame ~payload_length in
  [%test_result: (State.t * int) list]
    (Testbench.cycles_per_state observation)
    ~expect:(expected_cycles_per_state payload_length);
  [%test_result: int]
    (Testbench.emitted_byte_count observation)
    ~expect:(Testbench.expected_byte_count payload_length)
;;

let%test_unit "a sub-minimum datagram is zero-padded to 46 payload bytes" =
  let payload_length = 10 in
  let observation = Testbench.run_frame ~payload_length in
  let padding_cycles =
    List.count observation.trace ~f:(fun (item : Observation.t) -> item.output.pad)
  in
  (* [padding] latches on the last real byte and clears on the transition to Fcs, so it is
     asserted for exactly the cycles that emit zeros. *)
  [%test_result: int]
    padding_cycles
    ~expect:(Testbench.minimum_payload_length - payload_length);
  [%test_result: int]
    (Testbench.emitted_byte_count observation)
    ~expect:(Testbench.expected_byte_count payload_length)
;;

let%test_unit "a zero-length datagram pads the whole payload phase" =
  (* The degenerate case of the sub-minimum path: with no real byte to carry
     [payload_last], the RTL has to recognise an empty FIFO on arrival in Payload as
     padding. It then emits all 46 pad bytes and the frame is a normal minimum-length
     one - so the same byte-count and state-walk oracles apply with no special case. *)
  let observation = Testbench.run_frame ~payload_length:0 in
  [%test_result: State.t list]
    (Testbench.state_sequence observation)
    ~expect:expected_state_sequence;
  [%test_result: (State.t * int) list]
    (Testbench.cycles_per_state observation)
    ~expect:(expected_cycles_per_state 0);
  [%test_result: int]
    (Testbench.emitted_byte_count observation)
    ~expect:(Testbench.expected_byte_count 0);
  let padding_cycles =
    List.count observation.trace ~f:(fun (item : Observation.t) -> item.output.pad)
  in
  [%test_result: int] padding_cycles ~expect:Testbench.minimum_payload_length;
  (* [pad] must stay inside Payload: it gates the FIFO pop in [mac_top], and an assertion
     in Idle would read as a pad byte with no frame in flight. *)
  List.iter observation.trace ~f:(fun (item : Observation.t) ->
    if item.output.pad
    then [%test_result: State.t] item.output.state ~expect:State.Payload)
;;

let%test_unit "pad is never asserted for a datagram that already meets the minimum" =
  List.iter [ 46; 47; 64 ] ~f:(fun payload_length ->
    let observation = Testbench.run_frame ~payload_length in
    [%test_result: bool]
      (List.exists observation.trace ~f:(fun (item : Observation.t) -> item.output.pad))
      ~expect:false)
;;

let%test_unit "pad is only ever asserted inside the payload phase" =
  let observation = Testbench.run_frame ~payload_length:5 in
  List.iter observation.trace ~f:(fun (item : Observation.t) ->
    if item.output.pad
    then [%test_result: State.t] item.output.state ~expect:State.Payload)
;;

let%test_unit "tx_busy is high exactly while a frame is in flight" =
  let observation = Testbench.run_frame ~payload_length:46 in
  List.iter observation.trace ~f:(fun (item : Observation.t) ->
    [%test_result: bool]
      item.output.tx_busy
      ~expect:(not (State.equal item.output.state State.Idle)))
;;

let%test_unit "byte_mux_sel is the current state" =
  (* [tx_datapath] muxes its byte source directly off this, so the two must never
     disagree - a lag between them would emit the previous field's byte. *)
  let observation = Testbench.run_frame ~payload_length:20 in
  List.iter observation.trace ~f:(fun (item : Observation.t) ->
    [%test_result: State.t] item.output.byte_mux_sel ~expect:item.output.state)
;;

let%test_unit "mac_byte_sel indexes each header field from zero" =
  let observation = Testbench.run_frame ~payload_length:46 in
  let selectors_in state =
    List.filter_map observation.trace ~f:(fun (item : Observation.t) ->
      Option.some_if (State.equal item.output.state state) item.output.mac_byte_sel)
  in
  [%test_result: int list] (selectors_in State.Dst_mac) ~expect:[ 0; 1; 2; 3; 4; 5 ];
  [%test_result: int list] (selectors_in State.Src_mac) ~expect:[ 0; 1; 2; 3; 4; 5 ];
  [%test_result: int list] (selectors_in State.Eth_type) ~expect:[ 0; 1 ]
;;

let%test_unit "crc_en is asserted on the last payload byte and the first three FCS bytes" =
  (* [crc_en] is a wire, not a register, so it reads as the enable for the cycle it
     appears on. The final FCS cycle leaves it low: there is nothing left to accumulate
     once the last FCS byte is on the wire.

     This output is dead in the current integration - [mac_top] gates [Tx_crc] off [state]
     directly and never reads [crc_en] - so this is a behavioral freeze, not a check
     against a consumer. See the testbench header. *)
  let observation = Testbench.run_frame ~payload_length:46 in
  let enabled_states =
    List.filter_map observation.trace ~f:(fun (item : Observation.t) ->
      Option.some_if item.output.crc_en item.output.state)
  in
  [%test_result: State.t list]
    enabled_states
    ~expect:[ State.Payload; State.Fcs; State.Fcs; State.Fcs ]
;;

let%test_unit "a start pulse without a buffered frame does not launch" =
  let snapshots = Testbench.run_start_without_frame_ready ~num_cycles:6 in
  List.iter snapshots ~f:(fun (snapshot : Output_snapshot.t) ->
    [%test_result: State.t] snapshot.state ~expect:State.Idle;
    [%test_result: bool] snapshot.tx_busy ~expect:false)
;;

let%test_unit "a latched start launches when frame_ready rises" =
  let after_start, while_waiting, at_frame_ready, after_launch =
    Testbench.run_deferred_launch ~cycles_before_frame_ready:4
  in
  (* The start pulse alone leaves the FSM in Idle... *)
  [%test_result: State.t] after_start.state ~expect:State.Idle;
  List.iter while_waiting ~f:(fun (snapshot : Output_snapshot.t) ->
    [%test_result: State.t] snapshot.state ~expect:State.Idle);
  (* ...and the launch cycle is still Idle, since byte_mux_sel is the current state. *)
  [%test_result: State.t] at_frame_ready.state ~expect:State.Idle;
  (* The latched request fires with no fresh start pulse. *)
  [%test_result: State.t] after_launch.state ~expect:State.Preamble;
  [%test_result: bool] after_launch.tx_busy ~expect:true
;;

let%test_unit "dropping dis_ready stalls the FSM in place" =
  let before_stall, during_stall, after_stall =
    Testbench.run_serializer_stall ~stall_cycles:5
  in
  (* Nothing moves while the serializer cannot take a byte: the state and the byte counter
     are identical on every stalled cycle and on the ready cycle that follows. *)
  List.iter during_stall ~f:(fun (snapshot : Output_snapshot.t) ->
    [%test_result: Output_snapshot.t] snapshot ~expect:after_stall);
  [%test_result: State.t] after_stall.state ~expect:State.Preamble;
  (* The ready cycle before the stall did advance the counter, which is what makes the
     stall's stillness meaningful rather than vacuous. *)
  [%test_result: int] (before_stall.mac_byte_sel + 1) ~expect:after_stall.mac_byte_sel
;;

let%test_unit "random payload lengths produce the right byte count" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "tx-controller-byte-count")
    ~sexp_of:[%sexp_of: int]
    ~shrinker:Int.quickcheck_shrinker
    ~shrink_attempts:(`Limit 100)
    ~f:(fun payload_length ->
      let observation = Testbench.run_frame ~payload_length in
      [%test_result: int]
        (Testbench.emitted_byte_count observation)
        ~expect:(Testbench.expected_byte_count payload_length);
      [%test_result: State.t list]
        (Testbench.state_sequence observation)
        ~expect:expected_state_sequence;
      [%test_result: (State.t * int) list]
        (Testbench.cycles_per_state observation)
        ~expect:(expected_cycles_per_state payload_length))
    (Int.gen_incl 0 120)
;;

let%test_unit "random frames' payloads produce the right byte count" =
  Quickcheck.test
    ~trials:20
    ~seed:(`Deterministic "tx-controller-frames")
    ~sexp_of:[%sexp_of: Eth_frame.t]
    ~f:(fun frame ->
      let payload_length = List.length frame.payload in
      let observation = Testbench.run_frame ~payload_length in
      [%test_result: int]
        (Testbench.emitted_byte_count observation)
        ~expect:(Testbench.expected_byte_count payload_length))
    (Generators.eth_frame ~min_payload_length:0 ~max_payload_length:60 ())
;;
