(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "ipv4_tx_testbench.ml" *)

(* Testbench Support: Ipv4_tx

   Shared DUT fixture, AXI-Stream drivers, observations, and simulation scenarios used by
   the unit, Quickcheck, and expect test suites.

   The DUT prepends a 20-byte IPv4 header - with a correct header checksum - to whatever
   byte stream layer 4 hands down, then passes that payload through unchanged. So the
   transaction here is a whole datagram, and the oracle is
   [Hardcaml_verif.Ip_udp.Ipv4.header] followed by the payload verbatim.

   Sampling. [before_edge]. Every stream output is an [Always.Variable.wire] driven off
   the current state ([m_tvalid], [m_tlast], [tx_start], [l4_tready]) or a mux off it
   ([m_tdata]), so the byte that goes on the wire during a cycle is the one this cycle's
   outputs carry. [busy] is the one register in the snapshot and reads as the value the
   FSM held entering the cycle, which is what a downstream consumer would see.

   This is the whole reason the driver here is far shorter than the superseded
   [ipv4_tx_legacy_assertion_test.ml]. That harness sampled [Cyclesim]'s post-edge refs,
   where the accepting edge has already collapsed the FSM back to Idle: it could not see
   the final payload byte, its [m_tlast], or the [tx_start] pulse at all, and
   reconstructed all three from [busy] transitions. At [before_edge] each is observed
   directly, and [tx_start] is counted rather than inferred.

   Driving model. An ordinary AXI source: present byte [source_index], hold it valid until
   the block accepts it ([l4_tvalid &: l4_tready]), advance. [ready] and [source_valid]
   are schedules over the *global* cycle index, so a stall pattern does not re-align with
   the start of each datagram in a multi-datagram run. A datagram ends on the cycle its
   [m_tlast] byte is accepted - the block returns to Idle at that edge - which is also
   where the next datagram's drive picks up.

   Endpoints are elaboration-time constants, so the suite is a [Make_testbench] functor
   over them and [runners] carries every instantiation behind one record. [Primary] is the
   pair the legacy harness used, so its printed trace can be read against these goldens;
   [Broadcast_carry] is chosen so the header words sum past 0xFFFF and the checksum's
   end-around carry is actually exercised - under [Primary] alone a fold that dropped the
   carry would still pass.

   Zero-length L4 payloads are out of scope, as they were for the legacy harness: framing
   is layer-4 driven, so a datagram with no payload byte to carry [l4_tlast] never
   completes. [drive] rejects one outright rather than timing out.

   Coverage (carried over from the legacy harness, plus the additions marked NEW):
   - nominal + short + large payloads, both UDP (17) and TCP (6) protocols
   - 1-byte payload: minimal framing, tlast lands on payload byte 0
   - backpressure: periodic mac_tready bubbles, including an aggressive pattern that
     stalls across header bytes and the tlast cycle
   - tx_start fires exactly once per datagram
   - back-to-back datagrams through one FSM instance (re-arm after completion)
   - NEW source-side bubbles: l4_tvalid deasserted mid-payload must produce an idle output
     beat, not a repeated byte
   - NEW two endpoint configurations, one of them checksum-carry-forcing
   - NEW [en] pause/resume mid-datagram, with no handshake or event escaping while low
     (findings RTL-7)

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Ipv4_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Ipv4_tx

(* IP protocol numbers, the two the block is ever handed. *)
let protocol_udp = 17
let protocol_tcp = 6

(* [keep] is the module's synthesis anti-pruning OR-reduce and carries no verification
   meaning, so it is deliberately absent here. *)
module Output_snapshot = struct
  type t =
    { m_tdata : int
    ; m_tvalid : bool
    ; m_tlast : bool
    ; tx_start : bool
    ; l4_tready : bool
    ; busy : bool
    }
  [@@deriving sexp, equal, compare]
end

(* What the cycle did to the output stream, in the driver's own accounting rather than the
   DUT's state: a byte was accepted at some index of the datagram, or none was. *)
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
    ; mac_tready : bool
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

(* Expect tests use this compact form: which beat the cycle was, the byte on the wire, and
   the control lines that were up - not a wall of [false] fields. *)
module Compact_observation = struct
  type t =
    { beat : Beat.t
    ; m_tdata : string
    ; active_outputs : string list
    }
  [@@deriving sexp, equal, compare]
end

module Datagram_observation = struct
  type t =
    { bytes : int list
    ; tlast_index : int
    ; tx_start_pulses : int
    ; cycles : int
    ; trace : Observation.t list
    }
  [@@deriving sexp, equal, compare]

  let accepted trace =
    List.filter trace ~f:(fun (observation : Observation.t) ->
      observation.output.m_tvalid && observation.mac_tready)
  ;;

  (* Everything a caller checks is derived from the trace, so a scenario cannot report a
     byte count that disagrees with the cycles it printed. *)
  let of_trace trace =
    let accepted = accepted trace in
    { bytes =
        List.map accepted ~f:(fun (observation : Observation.t) ->
          observation.output.m_tdata)
    ; tlast_index =
        List.findi accepted ~f:(fun _ (observation : Observation.t) ->
          observation.output.m_tlast)
        |> Option.value_map ~default:(-1) ~f:fst
    ; tx_start_pulses =
        List.count trace ~f:(fun (observation : Observation.t) ->
          observation.output.tx_start)
    ; cycles = List.length trace
    ; trace
    }
  ;;
end

(* Schedules over the global cycle index. [stall_every k] drops [mac_tready] on every k-th
   cycle; 0 disables stalling, which is the free-running case. *)
let always _ = true

let stall_every stride =
  if stride <= 0 then always else fun cycle -> cycle % (stride + 1) <> stride
;;

module Make_testbench (Endpoints : Ipv4_tx.Config) = struct
  module Ip = Ipv4_tx.Make (Endpoints)

  let src_ip = Endpoints.src_ip
  let dst_ip = Endpoints.dst_ip

  module Fixture = Sim_fixture.Make (struct
      include Ip

      let name = "Ipv4_tx"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  (* The oracle: the 20 header bytes this configuration must emit, then the payload. *)
  let golden_header ~protocol ~payload_length =
    Ip_udp.Ipv4.header ~src_ip ~dst_ip ~protocol ~payload_length
  ;;

  let golden_datagram ~protocol ~payload =
    golden_header ~protocol ~payload_length:(List.length payload) @ payload
  ;;

  let inputs
    ~reset
    ~en
    ~start
    ~l4_length
    ~protocol
    ~l4_tdata
    ~l4_tvalid
    ~l4_tlast
    ~mac_tready
    =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; start = bit start
    ; l4_length = Bits.of_int_trunc ~width:16 l4_length
    ; protocol = Bits.of_int_trunc ~width:8 protocol
    ; l4_tdata = Bits.of_int_trunc ~width:8 l4_tdata
    ; l4_tvalid = bit l4_tvalid
    ; l4_tlast = bit l4_tlast
    ; mac_tready = bit mac_tready
    }
  ;;

  let snapshot (output : Bits.t Ip.O.t) : Output_snapshot.t =
    { m_tdata = Bits.to_int_trunc output.m_tdata
    ; m_tvalid = Bits.to_bool output.m_tvalid
    ; m_tlast = Bits.to_bool output.m_tlast
    ; tx_start = Bits.to_bool output.tx_start
    ; l4_tready = Bits.to_bool output.l4_tready
    ; busy = Bits.to_bool output.busy
    }
  ;;

  let active_outputs (output : Output_snapshot.t) =
    List.filter_opt
      [ Option.some_if output.m_tvalid "m_tvalid"
      ; Option.some_if output.m_tlast "m_tlast"
      ; Option.some_if output.tx_start "tx_start"
      ; Option.some_if output.l4_tready "l4_tready"
      ; Option.some_if output.busy "busy"
      ]
  ;;

  let compact ({ beat; output; _ } : Observation.t) : Compact_observation.t =
    { beat
    ; m_tdata = sprintf "0x%02x" output.m_tdata
    ; active_outputs = active_outputs output
    }
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs
         ~reset:true
         ~en:false
         ~start:false
         ~l4_length:0
         ~protocol:0
         ~l4_tdata:0
         ~l4_tvalid:false
         ~l4_tlast:false
         ~mac_tready:false)
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* One cycle per header byte and per payload byte at best; the multiplier is headroom
     for whatever [ready] and [source_valid] do to that. A payload byte needs both
     schedules high at once, so with two independent periodic schedules the wait can be
     their least common multiple - the cap is deliberately far above any schedule the
     suites use, because its job is to turn a genuine deadlock into a message rather than
     to be a tight bound. *)
  let budget datagrams =
    64
    + (64
       * List.sum (module Int) datagrams ~f:(fun (payload, _) ->
         Ip_udp.Ipv4.header_length + List.length payload))
  ;;

  (* Drive one datagram to completion from Idle. [en] is a cycle schedule so the suite can
     pause and resume the block at field boundaries as well as run it continuously. *)
  let run_sequence ~datagrams ~ready ~source_valid ~en =
    List.iter datagrams ~f:(fun (payload, _) ->
      if List.is_empty payload
      then
        failwith
          "Ipv4_tx never completes a datagram with no L4 payload byte to carry l4_tlast");
    let budget = budget datagrams in
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec drive
        (handler : Step.Handler.t @ local)
        ~payload
        ~protocol
        ~cycle_index
        ~accepted_out
        ~source_index
        ~launched
        =
        if cycle_index >= budget
        then failwith "Ipv4_tx did not complete a datagram within its cycle budget"
        else (
          let length = List.length payload in
          let mac_tready = ready cycle_index in
          let l4_tvalid = source_valid cycle_index in
          (* Hold the final byte presented rather than running off the end: a one-byte
             payload's only beat is also its [l4_tlast] beat, and yanking valid before the
             block's completing cycle would hang it. *)
          let index = Int.min source_index (length - 1) in
          let en = en cycle_index in
          let data =
            Step.cycle
              handler
              (inputs
                 ~reset:false
                 ~en
                 ~start:(not launched)
                 ~l4_length:length
                 ~protocol
                 ~l4_tdata:(List.nth_exn payload index)
                 ~l4_tvalid
                 ~l4_tlast:(index = length - 1)
                 ~mac_tready)
          in
          let output = Step.O_data.before_edge data |> snapshot in
          let accepted = output.m_tvalid && mac_tready in
          let beat =
            if not accepted
            then Beat.Idle
            else if accepted_out < Ip_udp.Ipv4.header_length
            then Beat.Header accepted_out
            else Beat.Payload (accepted_out - Ip_udp.Ipv4.header_length)
          in
          let observation =
            { Observation.cycle = cycle_index; en; beat; mac_tready; output }
          in
          if accepted && output.m_tlast
          then [ observation ], cycle_index + 1
          else (
            let remaining, next_cycle =
              drive
                handler
                ~payload
                ~protocol
                ~cycle_index:(cycle_index + 1)
                ~accepted_out:(if accepted then accepted_out + 1 else accepted_out)
                ~source_index:
                  (if l4_tvalid && output.l4_tready
                   then source_index + 1
                   else source_index)
                ~launched:true
            in
            observation :: remaining, next_cycle))
      in
      let rec drive_all (handler : Step.Handler.t @ local) ~cycle_index = function
        | [] -> []
        | (payload, protocol) :: remaining_datagrams ->
          let trace, next_cycle =
            drive
              handler
              ~payload
              ~protocol
              ~cycle_index
              ~accepted_out:0
              ~source_index:0
              ~launched:false
          in
          Datagram_observation.of_trace trace
          :: drive_all handler ~cycle_index:next_cycle remaining_datagrams
      in
      drive_all handler ~cycle_index:0 datagrams
    in
    run_with_timeout ~timeout:(budget + 8) ~testbench
  ;;
end

(* The functor's result type mentions its parameter (the [I] / [O] records come from
   [Ipv4_tx.Make]), so each argument has to be a named module rather than an inline
   struct. *)
module Primary_endpoints = struct
  let src_ip = [ 192; 168; 1; 10 ]
  let dst_ip = [ 192; 168; 1; 1 ]
end

module Primary = Make_testbench (Primary_endpoints)

(* Endpoints whose header words sum well past 0xFFFF, so the checksum's end-around carry
   has to happen for the golden to match. Broadcast destination, and a source with a high
   third octet, are the cheapest way to get there. *)
module Broadcast_carry_endpoints = struct
  let src_ip = [ 172; 16; 254; 1 ]
  let dst_ip = [ 255; 255; 255; 255 ]
end

module Broadcast_carry = Make_testbench (Broadcast_carry_endpoints)

(* Every instantiation behind one record, so a property runs across the whole set rather
   than being written out per configuration. The observation types live outside the
   functor precisely so these are all the same type. *)
type runner =
  { name : string
  ; src_ip : int list
  ; dst_ip : int list
  ; golden_datagram : protocol:int -> payload:int list -> int list
  ; run_sequence :
      datagrams:(int list * int) list
      -> ready:(int -> bool)
      -> source_valid:(int -> bool)
      -> en:(int -> bool)
      -> Datagram_observation.t list
  ; compact : Observation.t -> Compact_observation.t
  }

let runners =
  [ { name = "Primary"
    ; src_ip = Primary.src_ip
    ; dst_ip = Primary.dst_ip
    ; golden_datagram = Primary.golden_datagram
    ; run_sequence = Primary.run_sequence
    ; compact = Primary.compact
    }
  ; { name = "Broadcast_carry"
    ; src_ip = Broadcast_carry.src_ip
    ; dst_ip = Broadcast_carry.dst_ip
    ; golden_datagram = Broadcast_carry.golden_datagram
    ; run_sequence = Broadcast_carry.run_sequence
    ; compact = Broadcast_carry.compact
    }
  ]
;;

let primary = List.hd_exn runners

(* One datagram, free-running unless a caller says otherwise. *)
let run_datagram
  ?(ready = always)
  ?(source_valid = always)
  ?(en = always)
  runner
  ~payload
  ~protocol
  =
  match runner.run_sequence ~datagrams:[ payload, protocol ] ~ready ~source_valid ~en with
  | [ observation ] -> observation
  | observations ->
    raise_s [%message "expected one datagram" (List.length observations : int)]
;;

let run_datagrams
  ?(ready = always)
  ?(source_valid = always)
  ?(en = always)
  runner
  ~datagrams
  =
  runner.run_sequence ~datagrams ~ready ~source_valid ~en
;;

(* A deterministic payload, the one the legacy harness used, so its printed bytes can be
   read against these goldens. *)
let make_payload length = List.init length ~f:(fun index -> (0x40 + index) land 0xFF)

(* Elaboration only: [Ipv4_tx.Make] asserts both endpoints are four bytes, and the assert
   fires when the functor is applied rather than when the circuit is built. Returns the
   exception so a suite can pin it rather than only the fact that something was raised. *)
let elaborate ~src_ip ~dst_ip =
  Or_error.try_with (fun () ->
    let module Endpoints = struct
      let src_ip = src_ip
      let dst_ip = dst_ip
    end
    in
    let module Ip = Ipv4_tx.Make (Endpoints) in
    let scope = Scope.create ~flatten_design:true () in
    let (_ : Signal.t Ip.O.t) = Ip.create scope (Ip.I.Of_signal.inputs ()) in
    ())
;;
