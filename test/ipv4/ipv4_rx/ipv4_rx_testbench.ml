(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "ipv4_rx_testbench.ml" *)

(* Testbench Support: Ipv4_rx

   Shared DUT fixture, AXI-Stream drivers, observations, and simulation scenarios used by
   the unit, Quickcheck, and expect test suites.

   The stimulus is exactly what [Mac_top.m_axis] presents: the Ethernet *payload* as a
   byte stream ([rx_tdata]/[rx_tvalid]/[rx_tlast]/[rx_tuser]) plus the [rx_tfirst] SOF
   pulse and the latched [rx_eth_type]. So a transaction here is a whole received frame -
   IPv4 header, L4 payload, and any Ethernet zero-padding the MAC left on the end - and
   the DUT's job is to hand back the L4 payload alone.

   Sampling. [before_edge]. [m_tdata] is [rx_tdata] wired straight through and the
   qualifiers around it ([m_tvalid], [m_tlast], [m_tfirst], [l4_start], [m_axis_tready])
   are [Always.Variable.wire]s driven off the current state, so the byte and the qualifier
   that describes it belong to the same cycle. The metadata registers ([protocol],
   [payload_length], [src_ip], [dst_ip], [checksum_ok]) latch at the header-end edge,
   which is before the first payload cycle, so reading them at [before_edge] of the
   [l4_start] cycle is reading settled values. [crc_error] latches from [rx_tuser] at
   [rx_tlast] and is therefore only settled *after* the frame: it is read from the drain
   cycles, not from the trace.

   This is what the superseded [ipv4_rx_legacy_assertion_test.ml] spent its longest
   comment on. Sampling [Cyclesim]'s post-edge refs put the qualifier one cycle ahead of
   the byte, so that harness carried [q_valid] / [q_last] / [q_first] forward by hand and
   paired each with the byte it had presented the iteration before. At [before_edge] there
   is nothing to carry.

   Driving model. An ordinary AXI source and sink: a byte is consumed when
   [rx_tvalid &: m_axis_tready] and an L4 byte moves out when [m_tvalid &: l4_tready].
   [ready] and [source_valid] are schedules over the cycle index. After the last input
   byte the driver runs [drain_cycles] idle cycles so the late status ([crc_error],
   [busy]) is read settled.

   Configuration axis. [drop_on_bad_checksum] is elaboration-time policy, so the suite is
   a [Make_testbench] functor over it and [runners] carries both instantiations. [Strict]
   is what the legacy harness ran and drops a frame whose header checksum fails;
   [Permissive] forwards it anyway and reports the verdict on [checksum_ok] alone, which
   is the bring-up configuration and had no coverage at all.

   Coverage (carried over from the legacy harness, plus the additions marked NEW):
   - strips the 20-byte header and re-emits the L4 payload verbatim
   - m_tlast is driven off IP total_length, so Ethernet zero-padding is dropped
   - metadata: protocol, payload_length, src_ip, dst_ip, sampled at l4_start
   - the header checksum is verified, and drop_on_bad_checksum is honoured
   - non-IPv4 ethertypes are flushed
   - the MAC's FCS-error flag (rx_tuser) is forwarded as crc_error
   - NEW drop_on_bad_checksum = false: the payload is forwarded and only checksum_ok
     reports the failure
   - NEW IHL > 5 (a header with options) is flushed even under ethertype 0x0800
   - NEW truncated frames: rx_tlast inside the header, and a payload shorter than
     total_length, both abort rather than hang
   - NEW an empty datagram (total_length = 20) emits no L4 byte
   - NEW source-side bubbles as well as sink-side backpressure
   - NEW frame_done is combinational and handshake-qualified (findings RTL-8)
   - NEW [en] pause/resume mid-frame, including upstream backpressure (findings RTL-7)

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Ipv4_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Ipv4_rx

let ipv4_ethertype = 0x0800
let arp_ethertype = 0x0806
let protocol_udp = 17
let protocol_tcp = 6
let header_length = Ip_udp.Ipv4.header_length

(* The endpoints the legacy harness used, so its printed metadata reads against these
   goldens. They are stimulus here, not a parameter: an RX parser accepts whatever
   addresses arrive. *)
let src_ip = [ 192; 168; 1; 10 ]
let dst_ip = [ 10; 0; 0; 7 ]

let ip32 bytes =
  List.fold bytes ~init:0 ~f:(fun accumulator byte ->
    (accumulator lsl 8) lor (byte land 0xFF))
;;

(* Flip both checksum bytes so verification fails while every other field stays
   well-formed - the failure has to come from the checksum and nothing else. *)
let corrupt_checksum header =
  List.mapi header ~f:(fun index byte ->
    if index = 10 || index = 11 then byte lxor 0xFF else byte)
;;

let ip_header ?(corrupt = false) ?(version_ihl = 0x45) ~protocol ~payload_length () =
  let header = Ip_udp.Ipv4.header ~src_ip ~dst_ip ~protocol ~payload_length in
  let header = if corrupt then corrupt_checksum header else header in
  match header with
  | _ :: rest -> version_ihl :: rest
  | [] -> header
;;

(* The Ethernet payload the MAC hands up: header, L4 payload, then whatever zero-padding
   the MAC added to reach the 46-byte minimum. *)
let ethernet_payload ?corrupt ?version_ihl ?(pad_to = 0) ~protocol ~payload () =
  let datagram =
    ip_header ?corrupt ?version_ihl ~protocol ~payload_length:(List.length payload) ()
    @ payload
  in
  datagram @ List.init (Int.max 0 (pad_to - List.length datagram)) ~f:(fun _ -> 0x00)
;;

let make_payload ?(first = 0x40) length =
  List.init length ~f:(fun index -> (first + index) land 0xFF)
;;

(* [keep] is the module's synthesis anti-pruning OR-reduce and carries no verification
   meaning as a value; the [debug] knob that produces it is checked structurally instead,
   by [keep_is_constant]. *)
module Output_snapshot = struct
  type t =
    { m_axis_tready : bool
    ; m_tdata : int
    ; m_tvalid : bool
    ; m_tlast : bool
    ; m_tfirst : bool
    ; l4_start : bool
    ; protocol : int
    ; payload_length : int
    ; src_ip : int
    ; dst_ip : int
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
    { protocol : int
    ; payload_length : int
    ; src_ip : int
    ; dst_ip : int
    }
  [@@deriving sexp, equal, compare]

  let of_snapshot (output : Output_snapshot.t) =
    { protocol = output.protocol
    ; payload_length = output.payload_length
    ; src_ip = output.src_ip
    ; dst_ip = output.dst_ip
    }
  ;;
end

(* What the cycle did to the L4 stream, in the driver's own accounting: an L4 byte left at
   some index, or none did. The header, the flush and every stalled cycle are all [Idle]
   as far as layer 4 is concerned. *)
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
    ; rx_index : int option (* index of the frame byte presented, if any *)
    ; beat : Beat.t
    ; l4_tready : bool
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Compact_observation = struct
  type t =
    { beat : Beat.t
    ; rx_byte : string
    ; active_outputs : string list
    }
  [@@deriving sexp, equal, compare]
end

module Frame_observation = struct
  type t =
    { payload : int list
    ; tlast_index : int
    ; tfirst_index : int
    ; metadata : Metadata.t option
    ; frame_done_pulses : int
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

module type Policy = sig
  val drop_on_bad_checksum : bool
  val debug : bool
end

module Make_testbench (Policy : Policy) = struct
  module Rx = Ipv4_rx.Make (Policy)

  module Fixture = Sim_fixture.Make (struct
      include Rx

      let name = "Ipv4_rx"
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
    ~rx_eth_type
    ~l4_tready
    =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; rx_tdata = Bits.of_int_trunc ~width:8 rx_tdata
    ; rx_tvalid = bit rx_tvalid
    ; rx_tlast = bit rx_tlast
    ; rx_tuser = bit rx_tuser
    ; rx_tfirst = bit rx_tfirst
    ; rx_eth_type = Bits.of_int_trunc ~width:16 rx_eth_type
    ; l4_tready = bit l4_tready
    }
  ;;

  let snapshot (output : Bits.t Rx.O.t) : Output_snapshot.t =
    { m_axis_tready = Bits.to_bool output.m_axis_tready
    ; m_tdata = Bits.to_int_trunc output.m_tdata
    ; m_tvalid = Bits.to_bool output.m_tvalid
    ; m_tlast = Bits.to_bool output.m_tlast
    ; m_tfirst = Bits.to_bool output.m_tfirst
    ; l4_start = Bits.to_bool output.l4_start
    ; protocol = Bits.to_int_trunc output.protocol
    ; payload_length = Bits.to_int_trunc output.payload_length
    ; src_ip = Bits.to_int_trunc output.src_ip
    ; dst_ip = Bits.to_int_trunc output.dst_ip
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
      ; Option.some_if output.checksum_ok "checksum_ok"
      ; Option.some_if output.crc_error "crc_error"
      ; Option.some_if output.busy "busy"
      ; Option.some_if output.frame_done "frame_done"
      ; Option.some_if output.frame_error "frame_error"
      ]
  ;;

  let compact ({ beat; rx_index; output; _ } : Observation.t) : Compact_observation.t =
    { beat
    ; rx_byte =
        (match rx_index with
         | None -> "--"
         | Some _ -> sprintf "0x%02x" output.m_tdata)
    ; active_outputs = active_outputs output
    }
  ;;

  let idle_inputs ~eth_type =
    inputs
      ~reset:false
      ~en:true
      ~rx_tdata:0
      ~rx_tvalid:false
      ~rx_tlast:false
      ~rx_tuser:false
      ~rx_tfirst:false
      ~rx_eth_type:eth_type
      ~l4_tready:true
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
         ~rx_eth_type:0
         ~l4_tready:false)
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* Generous by design: its job is to turn a genuine hang into a message, not to be a
     tight bound on a schedule the caller chose. *)
  let budget frame = 64 + (64 * List.length frame)
  let drain_cycles = 4

  (* Present [frame] - the whole Ethernet payload - the way the MAC would, and collect the
     L4 stream the block emits. [eth_type] is held for the frame, as the MAC holds it. *)
  let run_frame ~frame ~eth_type ~fcs_bad ~ready ~source_valid ~en =
    let length = List.length frame in
    if length = 0 then failwith "Ipv4_rx needs at least one frame byte";
    let budget = budget frame in
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec drive (handler : Step.Handler.t @ local) ~cycle_index ~rx_index ~emitted =
        if cycle_index >= budget
        then failwith "Ipv4_rx did not drain the frame within its cycle budget"
        else if rx_index >= length
        then []
        else (
          let rx_tvalid = source_valid cycle_index in
          let l4_tready = ready cycle_index in
          let en = en cycle_index in
          let data =
            Step.cycle
              handler
              (inputs
                 ~reset:false
                 ~en
                 ~rx_tdata:(List.nth_exn frame rx_index)
                 ~rx_tvalid
                 ~rx_tlast:(rx_tvalid && rx_index = length - 1)
                 ~rx_tuser:(rx_tvalid && rx_index = length - 1 && fcs_bad)
                 ~rx_tfirst:(rx_tvalid && rx_index = 0)
                 ~rx_eth_type:eth_type
                 ~l4_tready)
          in
          let output = Step.O_data.before_edge data |> snapshot in
          let moved = output.m_tvalid && l4_tready in
          let observation =
            { Observation.cycle = cycle_index
            ; en
            ; rx_index = Option.some_if rx_tvalid rx_index
            ; beat = (if moved then Beat.Payload emitted else Beat.Idle)
            ; l4_tready
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
      (* The late status - [crc_error], [busy] - only settles after the edge that consumed
         the final byte, so it is read from idle cycles rather than from the trace. *)
      let rec drain (handler : Step.Handler.t @ local) ~remaining ~last =
        if remaining = 0
        then last
        else (
          let output =
            Step.cycle handler (idle_inputs ~eth_type)
            |> Step.O_data.before_edge
            |> snapshot
          in
          drain handler ~remaining:(remaining - 1) ~last:output)
      in
      let trace = drive handler ~cycle_index:0 ~rx_index:0 ~emitted:0 in
      let settled =
        drain
          handler
          ~remaining:drain_cycles
          ~last:(List.last_exn trace).Observation.output
      in
      let moved =
        List.filter trace ~f:(fun (observation : Observation.t) ->
          observation.output.m_tvalid && observation.l4_tready)
      in
      { Frame_observation.payload =
          List.map moved ~f:(fun (observation : Observation.t) ->
            observation.output.m_tdata)
      ; tlast_index =
          List.findi moved ~f:(fun _ (observation : Observation.t) ->
            observation.output.m_tlast)
          |> Option.value_map ~default:(-1) ~f:fst
      ; tfirst_index =
          List.findi moved ~f:(fun _ (observation : Observation.t) ->
            observation.output.m_tfirst)
          |> Option.value_map ~default:(-1) ~f:fst
      ; metadata =
          List.find trace ~f:(fun (observation : Observation.t) ->
            observation.output.l4_start)
          |> Option.map ~f:(fun (observation : Observation.t) ->
            Metadata.of_snapshot observation.output)
      ; frame_done_pulses =
          List.count trace ~f:(fun (observation : Observation.t) ->
            observation.output.frame_done)
      ; settled
      ; cycles = List.length trace
      ; trace
      }
    in
    run_with_timeout ~timeout:(budget + drain_cycles + 8) ~testbench
  ;;
end

(* The functor's result type mentions its parameter, so each argument has to be a named
   module rather than an inline struct. *)
module Strict_policy = struct
  let drop_on_bad_checksum = true
  let debug = true
end

module Permissive_policy = struct
  let drop_on_bad_checksum = false
  let debug = true
end

module Strict = Make_testbench (Strict_policy)
module Permissive = Make_testbench (Permissive_policy)

type runner =
  { name : string
  ; drop_on_bad_checksum : bool
  ; run_frame :
      frame:int list
      -> eth_type:int
      -> fcs_bad:bool
      -> ready:(int -> bool)
      -> source_valid:(int -> bool)
      -> en:(int -> bool)
      -> Frame_observation.t
  ; compact : Observation.t -> Compact_observation.t
  }

let runners =
  [ { name = "Strict"
    ; drop_on_bad_checksum = Strict_policy.drop_on_bad_checksum
    ; run_frame = Strict.run_frame
    ; compact = Strict.compact
    }
  ; { name = "Permissive"
    ; drop_on_bad_checksum = Permissive_policy.drop_on_bad_checksum
    ; run_frame = Permissive.run_frame
    ; compact = Permissive.compact
    }
  ]
;;

let strict = List.hd_exn runners
let permissive = List.nth_exn runners 1

let run_frame
  ?(eth_type = ipv4_ethertype)
  ?(fcs_bad = false)
  ?(ready = always)
  ?(source_valid = always)
  ?(en = always)
  runner
  ~frame
  =
  runner.run_frame ~frame ~eth_type ~fcs_bad ~ready ~source_valid ~en
;;

(* The [debug] knob only decides whether [keep] folds the internal registers or is tied
   off, so it is checked on the elaborated circuit rather than in simulation: with debug
   off [keep] is a constant, with it on it is not. *)
let keep_is_constant ~drop_on_bad_checksum ~debug =
  let module Policy = struct
    let drop_on_bad_checksum = drop_on_bad_checksum
    let debug = debug
  end
  in
  let module Rx = Ipv4_rx.Make (Policy) in
  let scope = Scope.create ~flatten_design:true () in
  let outputs = Rx.create scope (Rx.I.Of_signal.inputs ()) in
  Signal.Type.is_const outputs.keep
;;
