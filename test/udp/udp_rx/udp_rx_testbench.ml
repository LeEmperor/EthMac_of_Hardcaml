(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_rx_testbench.ml" *)

(* Testbench Support: Udp_rx

   Shared DUT fixture, AXI-Stream drivers, observations, and simulation scenarios used by
   the unit, Quickcheck, and expect test suites.

   The stimulus is what [Ipv4_rx] presents: the UDP *datagram* - 8-byte UDP header
   followed by application data - as a byte stream, plus the [rx_tfirst] SOF pulse and the
   IP metadata ([ip_protocol], [ip_src_ip], [ip_dst_ip]) held stable for the frame, and
   the frame-level late-status channel ([ip_frame_done], [ip_frame_error]) that the block
   forwards straight up. So a transaction here is one datagram and the DUT's job is to
   hand back the application payload alone, with the metadata beside it.

   Sampling. [before_edge]. [m_tdata] is [rx_tdata] wired through, the qualifiers around
   it are [Always.Variable.wire]s off the current state, and [frame_done] / [frame_error]
   / [src_ip] / [dst_ip] are straight passthroughs of inputs - all of which describe the
   cycle in progress. The header fields latch at the header-end edge, so they are settled
   by the first payload cycle; [crc_error] latches at [rx_tlast] and is therefore read
   from the drain cycles rather than from the trace.

   The superseded [udp_rx_legacy_assertion_test.ml] carried [q_valid] / [q_first] /
   [q_last] / [q_app_start] forward by hand to undo [Cyclesim]'s post-edge phase; at
   [before_edge] there is nothing to carry.

   SOF is a qualifier, not a pulse. [m_tfirst] is [rx_tvalid &: first_pend] and
   [first_pend] only clears when the beat is accepted, so while the application stalls the
   first payload byte, [m_tfirst] - and [app_start] with it - stays high for every stalled
   cycle. A consumer must qualify it with the handshake, which is why [app_start_events]
   counts only the cycles where the beat moved (or where there is no beat at all, which is
   the empty-datagram case: [empty_start] is a registered one-cycle pulse and is the only
   genuinely pulse-shaped half of [app_start]).

   Empty datagrams. A UDP length of exactly 8 is a legal datagram with no application
   data: the block reports it through [app_start] and emits no payload byte. Note the
   contrast with [Ipv4_rx], whose corresponding test is [>=] rather than [>] and which has
   no such path - see findings RTL-9.

   Configuration axis. [drop_on_port_mismatch] is elaboration-time policy, so the suite is
   a [Make_testbench] functor over it and [runners] carries both instantiations.
   [Accept_all] forwards every datagram and reports [port_match] informationally;
   [Filter_port] behaves like a bound socket. Both use the legacy harness's
   [expected_dst_port] so its printed output reads against these goldens.

   Coverage (carried over from the legacy harness, plus the additions marked NEW):
   - header stripping, payload pass-through, and the full metadata record
   - one-byte, zero-length, nominal and large payloads
   - accept-all reporting a mismatched port, and bound-port filtering
   - a non-UDP ip_protocol is flushed
   - application backpressure, including upstream ready staying low through it
   - truncation inside the header and inside the payload
   - the lower layer's error flag reaching crc_error
   - NEW a UDP length shorter than the bytes present: the surplus is flushed, not
     forwarded
   - NEW source-side bubbles as well as sink-side backpressure
   - NEW frame_done / frame_error are unconditional passthroughs
   - NEW checksum_ok is a stub tied high (findings RTL-11)
   - NEW [en] pause/resume mid-datagram, including upstream backpressure (findings RTL-7)

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Udp_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Udp_rx

let header_length = Ip_udp.Udp.header_length
let ip_protocol_udp = 17
let ip_protocol_tcp = 6
let expected_dst_port = 0x1235

(* The addresses the legacy harness passed down from IPv4. They are stimulus, not a
   parameter: this block only forwards them. *)
let src_ip = [ 192; 168; 1; 1 ]
let dst_ip = [ 192; 168; 1; 10 ]

let ip32 bytes =
  List.fold bytes ~init:0 ~f:(fun accumulator byte ->
    (accumulator lsl 8) lor (byte land 0xFF))
;;

(* [udp_length] overrides the header's length field so a datagram can promise more or
   fewer bytes than it carries. *)
let udp_datagram ?udp_length ?(checksum = 0) ~src_port ~dst_port ~payload () =
  let length = Option.value udp_length ~default:(header_length + List.length payload) in
  [ Ip_udp.hi8 src_port
  ; Ip_udp.lo8 src_port
  ; Ip_udp.hi8 dst_port
  ; Ip_udp.lo8 dst_port
  ; Ip_udp.hi8 length
  ; Ip_udp.lo8 length
  ; Ip_udp.hi8 checksum
  ; Ip_udp.lo8 checksum
  ]
  @ payload
;;

let make_payload ?(first = 0x40) length =
  List.init length ~f:(fun index -> (first + index) land 0xFF)
;;

module Output_snapshot = struct
  type t =
    { m_axis_tready : bool
    ; m_tdata : int
    ; m_tvalid : bool
    ; m_tlast : bool
    ; m_tfirst : bool
    ; app_start : bool
    ; src_port : int
    ; dst_port : int
    ; udp_length : int
    ; payload_length : int
    ; udp_checksum : int
    ; src_ip : int
    ; dst_ip : int
    ; port_match : bool
    ; checksum_ok : bool
    ; crc_error : bool
    ; busy : bool
    ; frame_done : bool
    ; frame_error : bool
    }
  [@@deriving sexp, equal, compare]
end

module Metadata = struct
  type t =
    { src_port : int
    ; dst_port : int
    ; udp_length : int
    ; payload_length : int
    ; udp_checksum : int
    ; src_ip : int
    ; dst_ip : int
    }
  [@@deriving sexp, equal, compare]

  let of_snapshot (output : Output_snapshot.t) =
    { src_port = output.src_port
    ; dst_port = output.dst_port
    ; udp_length = output.udp_length
    ; payload_length = output.payload_length
    ; udp_checksum = output.udp_checksum
    ; src_ip = output.src_ip
    ; dst_ip = output.dst_ip
    }
  ;;
end

module Beat = struct
  type t =
    | Idle
    | Payload of int
  [@@deriving sexp, equal, compare]
end

module Observation = struct
  type t =
    { cycle : int
    ; en : bool
    ; rx_index : int option
    ; beat : Beat.t
    ; app_tready : bool
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Compact_observation = struct
  type t =
    { beat : Beat.t
    ; rx_byte : string
    ; app_tready : bool
    ; active_outputs : string list
    }
  [@@deriving sexp, equal, compare]
end

module Datagram_observation = struct
  type t =
    { payload : int list
    ; tlast_indices : int list
    ; tfirst_indices : int list
    ; app_start_events : int
    ; metadata : Metadata.t option
    ; upstream_ready_held_low : bool (* m_axis_tready followed app_tready in Payload *)
    ; settled : Output_snapshot.t
    ; cycles : int
    ; trace : Observation.t list
    }
  [@@deriving sexp, equal, compare]
end

let always _ = true

let stall_every stride =
  if stride <= 0 then always else fun cycle -> cycle % (stride + 1) <> stride
;;

module Make_testbench (Config : Udp_rx.Config) = struct
  module Rx = Udp_rx.Make (Config)

  module Fixture = Sim_fixture.Make (struct
      include Rx

      let name = "Udp_rx"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs
    ~reset
    ~en
    ~rx_tdata
    ~rx_tvalid
    ~rx_tlast
    ~rx_tuser
    ~rx_tfirst
    ~ip_protocol
    ~ip_frame_done
    ~ip_frame_error
    ~app_tready
    =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; rx_tdata = Bits.of_int_trunc ~width:8 rx_tdata
    ; rx_tvalid = bit rx_tvalid
    ; rx_tlast = bit rx_tlast
    ; rx_tuser = bit rx_tuser
    ; rx_tfirst = bit rx_tfirst
    ; ip_protocol = Bits.of_int_trunc ~width:8 ip_protocol
    ; ip_src_ip = Bits.of_int_trunc ~width:32 (ip32 src_ip)
    ; ip_dst_ip = Bits.of_int_trunc ~width:32 (ip32 dst_ip)
    ; ip_frame_done = bit ip_frame_done
    ; ip_frame_error = bit ip_frame_error
    ; app_tready = bit app_tready
    }
  ;;

  let snapshot (output : Bits.t Rx.O.t) : Output_snapshot.t =
    { m_axis_tready = Bits.to_bool output.m_axis_tready
    ; m_tdata = Bits.to_int_trunc output.m_tdata
    ; m_tvalid = Bits.to_bool output.m_tvalid
    ; m_tlast = Bits.to_bool output.m_tlast
    ; m_tfirst = Bits.to_bool output.m_tfirst
    ; app_start = Bits.to_bool output.app_start
    ; src_port = Bits.to_int_trunc output.src_port
    ; dst_port = Bits.to_int_trunc output.dst_port
    ; udp_length = Bits.to_int_trunc output.udp_length
    ; payload_length = Bits.to_int_trunc output.payload_length
    ; udp_checksum = Bits.to_int_trunc output.udp_checksum
    ; src_ip = Bits.to_int_trunc output.src_ip
    ; dst_ip = Bits.to_int_trunc output.dst_ip
    ; port_match = Bits.to_bool output.port_match
    ; checksum_ok = Bits.to_bool output.checksum_ok
    ; crc_error = Bits.to_bool output.crc_error
    ; busy = Bits.to_bool output.busy
    ; frame_done = Bits.to_bool output.frame_done
    ; frame_error = Bits.to_bool output.frame_error
    }
  ;;

  let active_outputs (output : Output_snapshot.t) =
    List.filter_opt
      [ Option.some_if output.m_axis_tready "m_axis_tready"
      ; Option.some_if output.m_tvalid "m_tvalid"
      ; Option.some_if output.m_tlast "m_tlast"
      ; Option.some_if output.m_tfirst "m_tfirst"
      ; Option.some_if output.app_start "app_start"
      ; Option.some_if output.port_match "port_match"
      ; Option.some_if output.crc_error "crc_error"
      ; Option.some_if output.busy "busy"
      ; Option.some_if output.frame_done "frame_done"
      ; Option.some_if output.frame_error "frame_error"
      ]
  ;;

  let compact ({ beat; rx_index; app_tready; output; _ } : Observation.t)
    : Compact_observation.t
    =
    { beat
    ; rx_byte =
        (match rx_index with
         | None -> "--"
         | Some _ -> sprintf "0x%02x" output.m_tdata)
    ; app_tready
    ; active_outputs = active_outputs output
    }
  ;;

  let idle_inputs ~ip_protocol =
    inputs
      ~reset:false
      ~en:true
      ~rx_tdata:0
      ~rx_tvalid:false
      ~rx_tlast:false
      ~rx_tuser:false
      ~rx_tfirst:false
      ~ip_protocol
      ~ip_frame_done:false
      ~ip_frame_error:false
      ~app_tready:true
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs
         ~reset:true
         ~en:false
         ~rx_tdata:0
         ~rx_tvalid:false
         ~rx_tlast:false
         ~rx_tuser:false
         ~rx_tfirst:false
         ~ip_protocol:0
         ~ip_frame_done:false
         ~ip_frame_error:false
         ~app_tready:false)
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout
  let budget datagram = 64 + (64 * List.length datagram)
  let drain_cycles = 4

  (* [frame_status] decides the late-status channel per cycle; by default it mirrors what
     [Ipv4_rx] drives - [frame_done] on the final byte, carrying the FCS verdict. *)
  let run_datagram ~datagram ~ip_protocol ~fcs_bad ~ready ~source_valid ~en ~frame_status =
    let length = List.length datagram in
    if length = 0 then failwith "Udp_rx needs at least one datagram byte";
    let budget = budget datagram in
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec drive (handler : Step.Handler.t @ local) ~cycle_index ~rx_index ~emitted =
        if cycle_index >= budget
        then failwith "Udp_rx did not drain the datagram within its cycle budget"
        else if rx_index >= length
        then []
        else (
          let rx_tvalid = source_valid cycle_index in
          let app_tready = ready cycle_index in
          let last_byte = rx_tvalid && rx_index = length - 1 in
          let ip_frame_done, ip_frame_error =
            frame_status ~cycle:cycle_index ~last_byte
          in
          let en = en cycle_index in
          let data =
            Step.cycle
              handler
              (inputs
                 ~reset:false
                 ~en
                 ~rx_tdata:(List.nth_exn datagram rx_index)
                 ~rx_tvalid
                 ~rx_tlast:last_byte
                 ~rx_tuser:(last_byte && fcs_bad)
                 ~rx_tfirst:(rx_tvalid && rx_index = 0)
                 ~ip_protocol
                 ~ip_frame_done
                 ~ip_frame_error
                 ~app_tready)
          in
          let output = Step.O_data.before_edge data |> snapshot in
          let moved = output.m_tvalid && app_tready in
          let observation =
            { Observation.cycle = cycle_index
            ; en
            ; rx_index = Option.some_if rx_tvalid rx_index
            ; beat = (if moved then Beat.Payload emitted else Beat.Idle)
            ; app_tready
            ; output
            }
          in
          observation
          :: drive
               handler
               ~cycle_index:(cycle_index + 1)
               ~rx_index:
                 (if rx_tvalid && output.m_axis_tready then rx_index + 1 else rx_index)
               ~emitted:(if moved then emitted + 1 else emitted))
      in
      (* The drain cycles are part of the trace, not a separate reading. An empty
         datagram's [app_start] is [empty_start], a *registered* pulse set at the
         header-end edge, so it lands on the cycle after the last input byte - after the
         driver has run out of bytes to present. A trace that stopped at the last input
         byte would miss the only report that datagram ever makes. *)
      let rec drain (handler : Step.Handler.t @ local) ~cycle_index ~remaining =
        if remaining = 0
        then []
        else (
          let output =
            Step.cycle handler (idle_inputs ~ip_protocol)
            |> Step.O_data.before_edge
            |> snapshot
          in
          { Observation.cycle = cycle_index
          ; en = true
          ; rx_index = None
          ; beat = Beat.Idle
          ; app_tready = true
          ; output
          }
          :: drain handler ~cycle_index:(cycle_index + 1) ~remaining:(remaining - 1))
      in
      let driven = drive handler ~cycle_index:0 ~rx_index:0 ~emitted:0 in
      let trace =
        driven @ drain handler ~cycle_index:(List.length driven) ~remaining:drain_cycles
      in
      let settled = (List.last_exn trace).Observation.output in
      let moved =
        List.filter trace ~f:(fun (observation : Observation.t) ->
          observation.output.m_tvalid && observation.app_tready)
      in
      { Datagram_observation.payload =
          List.map moved ~f:(fun (observation : Observation.t) ->
            observation.output.m_tdata)
      ; tlast_indices =
          List.filter_mapi moved ~f:(fun index (observation : Observation.t) ->
            Option.some_if observation.output.m_tlast index)
      ; tfirst_indices =
          List.filter_mapi moved ~f:(fun index (observation : Observation.t) ->
            Option.some_if observation.output.m_tfirst index)
      ; (* A SOF that is never taken is not an event: count the cycles where the beat
           moved, plus the beat-less pulse an empty datagram reports. *)
        app_start_events =
          List.count trace ~f:(fun (observation : Observation.t) ->
            observation.output.app_start
            && ((not observation.output.m_tvalid) || observation.app_tready))
      ; metadata =
          List.find trace ~f:(fun (observation : Observation.t) ->
            observation.output.app_start)
          |> Option.map ~f:(fun (observation : Observation.t) ->
            Metadata.of_snapshot observation.output)
      ; upstream_ready_held_low =
          List.for_all trace ~f:(fun (observation : Observation.t) ->
            (not observation.output.m_tvalid)
            || observation.app_tready
            || not observation.output.m_axis_tready)
      ; settled
      ; cycles = List.length trace
      ; trace
      }
    in
    run_with_timeout ~timeout:(budget + drain_cycles + 8) ~testbench
  ;;
end

module Accept_all_config = struct
  let drop_on_port_mismatch = false
  let expected_dst_port = expected_dst_port
  let debug = true
end

module Filter_port_config = struct
  let drop_on_port_mismatch = true
  let expected_dst_port = expected_dst_port
  let debug = true
end

module Accept_all = Make_testbench (Accept_all_config)
module Filter_port = Make_testbench (Filter_port_config)

type runner =
  { name : string
  ; drop_on_port_mismatch : bool
  ; run_datagram :
      datagram:int list
      -> ip_protocol:int
      -> fcs_bad:bool
      -> ready:(int -> bool)
      -> source_valid:(int -> bool)
      -> en:(int -> bool)
      -> frame_status:(cycle:int -> last_byte:bool -> bool * bool)
      -> Datagram_observation.t
  ; compact : Observation.t -> Compact_observation.t
  }

let runners =
  [ { name = "Accept_all"
    ; drop_on_port_mismatch = Accept_all_config.drop_on_port_mismatch
    ; run_datagram = Accept_all.run_datagram
    ; compact = Accept_all.compact
    }
  ; { name = "Filter_port"
    ; drop_on_port_mismatch = Filter_port_config.drop_on_port_mismatch
    ; run_datagram = Filter_port.run_datagram
    ; compact = Filter_port.compact
    }
  ]
;;

let accept_all = List.hd_exn runners
let filter_port = List.nth_exn runners 1
let default_frame_status ~fcs_bad ~cycle:_ ~last_byte = last_byte, last_byte && fcs_bad

let run_datagram
  ?(ip_protocol = ip_protocol_udp)
  ?(fcs_bad = false)
  ?(ready = always)
  ?(source_valid = always)
  ?(en = always)
  ?frame_status
  runner
  ~datagram
  =
  runner.run_datagram
    ~datagram
    ~ip_protocol
    ~fcs_bad
    ~ready
    ~source_valid
    ~en
    ~frame_status:(Option.value frame_status ~default:(default_frame_status ~fcs_bad))
;;

(* The [debug] knob only decides whether [keep] folds the internal registers or is tied
   off, so it is checked on the elaborated circuit rather than in simulation. *)
let keep_is_constant ~drop_on_port_mismatch ~debug =
  let module Config = struct
    let drop_on_port_mismatch = drop_on_port_mismatch
    let expected_dst_port = expected_dst_port
    let debug = debug
  end
  in
  let module Rx = Udp_rx.Make (Config) in
  let scope = Scope.create ~flatten_design:true () in
  let outputs = Rx.create scope (Rx.I.Of_signal.inputs ()) in
  Signal.Type.is_const outputs.keep
;;
