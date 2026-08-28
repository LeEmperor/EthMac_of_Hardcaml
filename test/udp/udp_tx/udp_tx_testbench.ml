(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_tx_testbench.ml" *)

(* Testbench Support: Udp_tx

   Shared DUT fixture, AXI-Stream drivers, observations, and simulation scenarios used by
   the unit, Quickcheck, and expect test suites.

   Since the IPv4/UDP split this block emits only the UDP datagram - the 8-byte UDP header
   followed by the application payload - down to [Ipv4_tx], plus the metadata
   ([l4_length], [protocol], [ip_start]) that IPv4 needs to build its own header. So a
   transaction here is one datagram, and the oracle is [Hardcaml_verif.Ip_udp.Udp.header]
   followed by the payload verbatim. There is no padding: the stream is exactly
   [8 + payload_length] bytes.

   Sampling. [before_edge]. Every stream output is an [Always.Variable.wire] driven off
   the current state, or a mux off it. [ip_start] and [protocol] are combinational;
   [l4_length] bypasses its latch while [start] is high and reads the latch afterward.
   Each sample is therefore what the block was driving during the cycle. The superseded
   [udp_tx_legacy_assertion_test.ml] already sampled the before-edge interface by hand
   ([Cyclesim.outputs ~clock_edge:Side.Before] plus a manual [cycle_before_clock_edge] /
   [cycle_at_clock_edge] split); [Step.O_data.before_edge] is the same sample without the
   manual phase management.

   Ready and valid schedules take the cycle index, the index of the datagram byte waiting
   to be accepted, and how many cycles have already been spent on that byte. The last of
   those is what lets a scenario stall precisely on the *final* beat - the one carrying
   [m_tlast] - which is a different beat for a zero-length datagram (header byte 7) than
   for any other (the last payload byte). Both forms are exercised.

   Zero-length datagrams are legal here, unlike in [Ipv4_tx]: the block owns its own
   framing and asserts [m_tlast] on header byte 7 when [payload_rem] is zero.

   Length latching. [payload_len] is latched into [len_latch] at [start], and the header's
   udp_length is built from the latch - so changing the input mid-datagram must not move
   the header. [l4_length] uses the live input on that start cycle (the new latch value is
   not visible until the edge) and the same latch thereafter, so it remains the datagram's
   length even if the input moves (resolved findings RTL-10). Both the bypass and the hold
   are asserted rather than assumed.

   Ports are elaboration-time constants, so the suite is a [Make_testbench] functor over
   them and [runners] carries both instantiations. [Primary] is the pair the legacy
   harness used; [Wide_ports] has ports whose two bytes differ from each other and from
   the other port's, so a byte-order or src/dst swap in the header mux cannot hide.

   Coverage (carried over from the legacy harness, plus the additions marked NEW):
   - UDP header fields, length metadata, protocol, start, busy, and framing
   - zero-, one-, nominal-, and larger-payload datagrams
   - downstream backpressure, including stalls on both forms of final beat
   - application-source valid bubbles
   - payload length latching and back-to-back datagrams without reset
   - reset recovery while a datagram is in flight
   - AXI-stream output stability and payload-ready propagation
   - NEW two port configurations
   - NEW random payloads, and random ready/valid schedules
   - NEW [en] pause/resume mid-datagram, with both stream directions quiescent while low
     (findings RTL-7)

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Udp_tx

let header_length = Ip_udp.Udp.header_length
let ip_protocol_udp = 17

(* [keep] is the module's synthesis anti-pruning OR-reduce and carries no verification
   meaning, so it is deliberately absent here. *)
module Output_snapshot = struct
  type t =
    { ip_start : bool
    ; l4_length : int
    ; protocol : int
    ; m_tdata : int
    ; m_tvalid : bool
    ; m_tlast : bool
    ; payload_tready : bool
    ; busy : bool
    }
  [@@deriving sexp, equal, compare]
end

module Beat = struct
  type t =
    | Idle
    | Header of int
    | Payload of int
  [@@deriving sexp, equal, compare]
end

module Observation = struct
  type t =
    { cycle : int
    ; en : bool
    ; beat : Beat.t
    ; byte_index : int (* datagram byte waiting to be accepted *)
    ; l4_tready : bool
    ; payload_tvalid : bool
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Compact_observation = struct
  type t =
    { beat : Beat.t
    ; m_tdata : string
    ; l4_tready : bool
    ; active_outputs : string list
    }
  [@@deriving sexp, equal, compare]
end

module Datagram_observation = struct
  type t =
    { bytes : int list
    ; tlast_indices : int list
    ; ip_start_pulses : int
    ; l4_length_at_start : int option
    ; protocol_values : int list
    ; cycles : int
    ; settled : Output_snapshot.t
    ; trace : Observation.t list
    }
  [@@deriving sexp, equal, compare]
end

module Reset_recovery_observation = struct
  type t =
    { in_flight : Output_snapshot.t (* mid-datagram, before the reset is applied *)
    ; during_reset : Output_snapshot.t (* the cycle the reset is asserted *)
    ; after_reset : Output_snapshot.t (* the cycle after it takes effect *)
    ; datagram : Datagram_observation.t (* a fresh datagram once reset is released *)
    }
  [@@deriving sexp, equal, compare]
end

(* Schedules see the cycle, the datagram byte index waiting to be accepted, and how many
   cycles have been spent waiting on it. *)
let always ~cycle:_ ~beat:_ ~beat_cycles:_ = true

let stall_every stride =
  if stride <= 0
  then always
  else fun ~cycle ~beat:_ ~beat_cycles:_ -> cycle % (stride + 1) <> stride
;;

(* Hold the sink un-ready for the first [count] cycles of the datagram's final beat, which
   is the beat carrying [m_tlast]. *)
let stall_final_beat ~count ~total ~cycle:_ ~beat ~beat_cycles =
  not (beat = total - 1 && beat_cycles < count)
;;

let pattern_schedule pattern ~cycle ~beat:_ ~beat_cycles:_ =
  List.nth_exn pattern (cycle % List.length pattern)
;;

let make_payload length =
  List.init length ~f:(fun index -> ((index * 37) + 0x40) land 0xFF)
;;

module type Ports = sig
  val src_port : int
  val dst_port : int
end

module Make_testbench (Ports : Ports) = struct
  module Udp = Udp_tx.Make (Ports)

  let src_port = Ports.src_port
  let dst_port = Ports.dst_port

  module Fixture = Sim_fixture.Make (struct
      include Udp

      let name = "Udp_tx"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let golden_header ~payload_length =
    Ip_udp.Udp.header ~src_port ~dst_port ~payload_length
  ;;

  let golden_datagram ~payload =
    golden_header ~payload_length:(List.length payload) @ payload
  ;;

  let inputs ~reset ~en ~start ~payload_len ~payload_tdata ~payload_tvalid ~l4_tready =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; start = bit start
    ; payload_len = Bits.of_int_trunc ~width:16 payload_len
    ; payload_tdata = Bits.of_int_trunc ~width:8 payload_tdata
    ; payload_tvalid = bit payload_tvalid
    ; l4_tready = bit l4_tready
    }
  ;;

  let snapshot (output : Bits.t Udp.O.t) : Output_snapshot.t =
    { ip_start = Bits.to_bool output.ip_start
    ; l4_length = Bits.to_int_trunc output.l4_length
    ; protocol = Bits.to_int_trunc output.protocol
    ; m_tdata = Bits.to_int_trunc output.m_tdata
    ; m_tvalid = Bits.to_bool output.m_tvalid
    ; m_tlast = Bits.to_bool output.m_tlast
    ; payload_tready = Bits.to_bool output.payload_tready
    ; busy = Bits.to_bool output.busy
    }
  ;;

  let active_outputs (output : Output_snapshot.t) =
    List.filter_opt
      [ Option.some_if output.ip_start "ip_start"
      ; Option.some_if output.m_tvalid "m_tvalid"
      ; Option.some_if output.m_tlast "m_tlast"
      ; Option.some_if output.payload_tready "payload_tready"
      ; Option.some_if output.busy "busy"
      ]
  ;;

  let compact ({ beat; l4_tready; output; _ } : Observation.t) : Compact_observation.t =
    { beat
    ; m_tdata = sprintf "0x%02x" output.m_tdata
    ; l4_tready
    ; active_outputs = active_outputs output
    }
  ;;

  let idle_inputs =
    inputs
      ~reset:false
      ~en:true
      ~start:false
      ~payload_len:0
      ~payload_tdata:0
      ~payload_tvalid:false
      ~l4_tready:true
  ;;

  let reset_inputs =
    inputs
      ~reset:true
      ~en:false
      ~start:false
      ~payload_len:0
      ~payload_tdata:0
      ~payload_tvalid:false
      ~l4_tready:false
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay ~num_cycles handler reset_inputs
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  let budget payloads =
    64
    + (64
       * List.sum (module Int) payloads ~f:(fun payload ->
         header_length + List.length payload))
  ;;

  let drain_cycles = 2

  (* Drive one datagram from Idle and collect what the block put on the wire.
     [payload_len_after_start] overrides the [payload_len] input once the start pulse has
     been taken, which is how the latch is tested. *)
  let drive_datagram
    (handler : Step.Handler.t @ local)
    ~payload
    ~ready
    ~source_valid
    ~en
    ~payload_len_after_start
    ~budget
    ~cycle_offset
    =
    let length = List.length payload in
    let rec loop
      (handler : Step.Handler.t @ local)
      ~cycle_index
      ~accepted
      ~beat_cycles
      ~source_index
      ~launched
      =
      if cycle_index - cycle_offset >= budget
      then failwith "Udp_tx did not complete a datagram within its cycle budget"
      else (
        let l4_tready = ready ~cycle:cycle_index ~beat:accepted ~beat_cycles in
        let has_payload_byte = source_index < length in
        let payload_tvalid =
          has_payload_byte && source_valid ~cycle:cycle_index ~beat:accepted ~beat_cycles
        in
        let payload_len =
          match payload_len_after_start with
          | Some override when launched -> override
          | _ -> length
        in
        let en = en ~cycle:cycle_index ~beat:accepted ~beat_cycles in
        let data =
          Step.cycle
            handler
            (inputs
               ~reset:false
               ~en
               ~start:(not launched)
               ~payload_len
               ~payload_tdata:
                 (if has_payload_byte then List.nth_exn payload source_index else 0)
               ~payload_tvalid
               ~l4_tready)
        in
        let output = Step.O_data.before_edge data |> snapshot in
        let moved = output.m_tvalid && l4_tready in
        let beat =
          if not moved
          then Beat.Idle
          else if accepted < header_length
          then Beat.Header accepted
          else Beat.Payload (accepted - header_length)
        in
        let observation =
          { Observation.cycle = cycle_index
          ; en
          ; beat
          ; byte_index = accepted
          ; l4_tready
          ; payload_tvalid
          ; output
          }
        in
        if moved && output.m_tlast
        then [ observation ], cycle_index + 1
        else (
          let remaining, next_cycle =
            loop
              handler
              ~cycle_index:(cycle_index + 1)
              ~accepted:(if moved then accepted + 1 else accepted)
              ~beat_cycles:(if moved then 0 else beat_cycles + 1)
              ~source_index:
                (if payload_tvalid && output.payload_tready
                 then source_index + 1
                 else source_index)
              ~launched:true
          in
          observation :: remaining, next_cycle))
    in
    loop
      handler
      ~cycle_index:cycle_offset
      ~accepted:0
      ~beat_cycles:0
      ~source_index:0
      ~launched:false
  ;;

  let rec drain (handler : Step.Handler.t @ local) ~remaining ~last =
    if remaining = 0
    then last
    else (
      let output =
        Step.cycle handler idle_inputs |> Step.O_data.before_edge |> snapshot
      in
      drain handler ~remaining:(remaining - 1) ~last:output)
  ;;

  let summarize trace ~settled =
    let moved =
      List.filter trace ~f:(fun (observation : Observation.t) ->
        observation.output.m_tvalid && observation.l4_tready)
    in
    { Datagram_observation.bytes =
        List.map moved ~f:(fun (observation : Observation.t) ->
          observation.output.m_tdata)
    ; tlast_indices =
        List.filter_mapi moved ~f:(fun index (observation : Observation.t) ->
          Option.some_if observation.output.m_tlast index)
    ; ip_start_pulses =
        List.count trace ~f:(fun (observation : Observation.t) ->
          observation.output.ip_start)
    ; l4_length_at_start =
        List.find trace ~f:(fun (observation : Observation.t) ->
          observation.output.ip_start)
        |> Option.map ~f:(fun (observation : Observation.t) ->
          observation.output.l4_length)
    ; protocol_values =
        List.map trace ~f:(fun (observation : Observation.t) ->
          observation.output.protocol)
        |> List.dedup_and_sort ~compare:Int.compare
    ; cycles = List.length trace
    ; settled
    ; trace
    }
  ;;

  (* One or more datagrams through a single instance, with no reset between them. *)
  let run_sequence ~payloads ~ready ~source_valid ~en ~payload_len_after_start =
    let budget = budget payloads in
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec drive_all (handler : Step.Handler.t @ local) ~cycle_offset = function
        | [] -> []
        | payload :: remaining_payloads ->
          let trace, next_cycle =
            drive_datagram
              handler
              ~payload
              ~ready
              ~source_valid
              ~en
              ~payload_len_after_start
              ~budget
              ~cycle_offset
          in
          let settled =
            drain
              handler
              ~remaining:drain_cycles
              ~last:(List.last_exn trace).Observation.output
          in
          summarize trace ~settled
          :: drive_all
               handler
               ~cycle_offset:(next_cycle + drain_cycles)
               remaining_payloads
      in
      drive_all handler ~cycle_offset:0 payloads
    in
    run_with_timeout
      ~timeout:(budget + (List.length payloads * (drain_cycles + 8)) + 8)
      ~testbench
  ;;

  (* Assert reset in the middle of a datagram, then check the block accepts a fresh one.
     The clear is synchronous, so the cycle that applies it still reports the old register
     contents at [before_edge] - [after_reset] is the cycle where it has taken effect. *)
  let run_reset_recovery ~payload =
    let budget = budget [ payload ] + 64 in
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let launch =
        inputs
          ~reset:false
          ~en:true
          ~start:true
          ~payload_len:20
          ~payload_tdata:0xA5
          ~payload_tvalid:true
          ~l4_tready:true
      in
      let running =
        inputs
          ~reset:false
          ~en:true
          ~start:false
          ~payload_len:20
          ~payload_tdata:0xA5
          ~payload_tvalid:true
          ~l4_tready:true
      in
      ignore (Step.cycle handler launch : Step.O_data.t);
      Step.delay ~num_cycles:2 handler running;
      let in_flight = Step.cycle handler running |> Step.O_data.before_edge |> snapshot in
      let during_reset =
        Step.cycle handler reset_inputs |> Step.O_data.before_edge |> snapshot
      in
      let after_reset =
        Step.cycle handler idle_inputs |> Step.O_data.before_edge |> snapshot
      in
      let trace, _ =
        drive_datagram
          handler
          ~payload
          ~ready:always
          ~source_valid:always
          ~en:always
          ~payload_len_after_start:None
          ~budget
          ~cycle_offset:0
      in
      let settled =
        drain
          handler
          ~remaining:drain_cycles
          ~last:(List.last_exn trace).Observation.output
      in
      { Reset_recovery_observation.in_flight
      ; during_reset
      ; after_reset
      ; datagram = summarize trace ~settled
      }
    in
    run_with_timeout ~timeout:(budget + 32) ~testbench
  ;;
end

(* The functor's result type mentions its parameter, so each argument has to be a named
   module rather than an inline struct. *)
module Primary_ports = struct
  let src_port = 0x1234
  let dst_port = 0x1235
end

(* Ports whose four bytes are all distinct, so a hi/lo swap inside a port or a src/dst
   swap between them shows up as a wrong byte rather than a plausible one. *)
module Wide_ports = struct
  let src_port = 0x00A1
  let dst_port = 0xFF07
end

module Primary = Make_testbench (Primary_ports)
module Wide = Make_testbench (Wide_ports)

type schedule = cycle:int -> beat:int -> beat_cycles:int -> bool

type runner =
  { name : string
  ; src_port : int
  ; dst_port : int
  ; golden_datagram : payload:int list -> int list
  ; run_sequence :
      payloads:int list list
      -> ready:schedule
      -> source_valid:schedule
      -> en:schedule
      -> payload_len_after_start:int option
      -> Datagram_observation.t list
  ; run_reset_recovery : payload:int list -> Reset_recovery_observation.t
  ; compact : Observation.t -> Compact_observation.t
  }

let runners =
  [ { name = "Primary"
    ; src_port = Primary.src_port
    ; dst_port = Primary.dst_port
    ; golden_datagram = Primary.golden_datagram
    ; run_sequence = Primary.run_sequence
    ; run_reset_recovery = Primary.run_reset_recovery
    ; compact = Primary.compact
    }
  ; { name = "Wide"
    ; src_port = Wide.src_port
    ; dst_port = Wide.dst_port
    ; golden_datagram = Wide.golden_datagram
    ; run_sequence = Wide.run_sequence
    ; run_reset_recovery = Wide.run_reset_recovery
    ; compact = Wide.compact
    }
  ]
;;

let primary = List.hd_exn runners

let run_datagrams
  ?(ready = always)
  ?(source_valid = always)
  ?(en = always)
  ?payload_len_after_start
  runner
  ~payloads
  =
  runner.run_sequence ~payloads ~ready ~source_valid ~en ~payload_len_after_start
;;

let run_datagram ?ready ?source_valid ?en ?payload_len_after_start runner ~payload =
  match
    run_datagrams
      ?ready
      ?source_valid
      ?en
      ?payload_len_after_start
      runner
      ~payloads:[ payload ]
  with
  | [ observation ] -> observation
  | observations ->
    raise_s [%message "expected one datagram" (List.length observations : int)]
;;

(* AXI-Stream's rule for the source: once a beat is offered it must stay offered, byte and
   [tlast] unchanged, until the sink takes it. Only meaningful where the application
   source is not itself bubbling - in Payload [m_tvalid] is [payload_tvalid], so a bubble
   legitimately withdraws the beat. *)
let stalled_beat_violations (observation : Datagram_observation.t) =
  let rows = Array.of_list observation.trace in
  List.filter_mapi observation.trace ~f:(fun index (item : Observation.t) ->
    if (not item.output.m_tvalid) || item.l4_tready || index + 1 >= Array.length rows
    then None
    else (
      let next = rows.(index + 1) in
      (* A header beat is offered by the block itself; a payload beat is only still on
         offer if the application source is still presenting it. *)
      let still_offered = item.byte_index < header_length || next.payload_tvalid in
      if still_offered
         && ((not next.output.m_tvalid)
             || next.output.m_tdata <> item.output.m_tdata
             || Bool.( <> ) next.output.m_tlast item.output.m_tlast)
      then Some (item.cycle, item.output, next.output)
      else None))
;;

(* [payload_tready] is [l4_tready] in Payload and low everywhere else, which is what the
   application source above has to see. The expected value is computed from the driver's
   own byte accounting rather than from the DUT's state. *)
let payload_ready_violations (observation : Datagram_observation.t) ~payload_length =
  List.filter_map observation.trace ~f:(fun (item : Observation.t) ->
    let in_payload = payload_length > 0 && item.byte_index >= Ip_udp.Udp.header_length in
    let expected = in_payload && item.l4_tready && item.en in
    Option.some_if
      (Bool.( <> ) item.output.payload_tready expected)
      (item.cycle, item.output.payload_tready, expected))
;;
