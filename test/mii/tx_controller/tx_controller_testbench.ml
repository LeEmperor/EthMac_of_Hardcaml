(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_controller_testbench.ml" *)

(* Testbench Support: Tx_controller

   Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit,
   Quickcheck, and expect test suites. This is the transmit dual of [rx_controller]'s
   suite and mirrors its [Phase] / [Compact_observation] shape.

   Net-new verification: this block had no testbench of its own and was only exercised
   indirectly through [tx_path_tb.ml].

   Sampling point. Unlike the receive suites, observations here come from
   [Step.O_data.before_edge], not [after_edge]. Every output is the controller's
   instruction to [tx_datapath] and [tx_byte_disassembler] *for the cycle now in
   progress*: [byte_mux_sel] is [sm.current], and [tx_datapath] muxes off it
   combinationally. So the byte emitted during a cycle is the one selected by that cycle's
   [before_edge] outputs. [after_edge] carries the *next* state, which the driver uses to
   decide the following cycle's stimulus - which is the whole reason the driver is a
   state-following loop rather than a fixed script.

   Driving model. [dis_ready] is held high throughout, so exactly one byte is emitted per
   non-Idle cycle and the emitted byte count is simply the number of such cycles.
   [fifo_empty] is driven from the driver's own payload accounting - low while real bytes
   remain, high once they are exhausted - and [payload_last] is asserted on the cycle that
   consumes the final real byte.

   Padding. A datagram shorter than the 46-byte Ethernet minimum is zero-padded: the RTL
   latches [padding] on the last real byte and emits zeros until the payload counter
   reaches index 45. So the payload phase always occupies max(length, 46) cycles, and the
   whole frame is 7 + 1 + 6 + 6 + 2 + max(length, 46) + 4 bytes.

   [crc_en] is currently dead. [mac_top] does not consume it: it derives its own
   [crc_active] from [state] (3..6) and ties [Tx_crc.en] to ~(state == Idle). The
   controller's [crc_en] is still an output of this block, so the suite freezes what it
   does - assert on the last payload cycle and the first three FCS cycles - rather than
   leaving it unobserved. Anything that starts consuming it should be checked against
   [Tx_crc]'s enable semantics first, since a low [en] there reloads the accumulator
   rather than stalling it.

   Known limitation, deliberately not exercised. A zero-length payload deadlocks: the
   Payload state only leaves via [~fifo_empty &: dis_ready], and [padding] is only latched
   on a [payload_last] byte, so with no real byte to carry [payload_last] the FSM waits in
   Payload forever. The store-and-forward gate upstream never launches an empty datagram,
   so the generators below start at length 1.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Tx_controller

(* The FSM's states, in the declaration order of [Common_types.States]. Both
   [byte_mux_sel] and [state] carry this encoding. *)
module State = struct
  type t =
    | Idle
    | Preamble
    | Sfd
    | Dst_mac
    | Src_mac
    | Eth_type
    | Payload
    | Fcs
  [@@deriving sexp, equal, compare, enumerate]

  let of_int_exn value =
    match List.nth all value with
    | Some t -> t
    | None -> raise_s [%message "state encoding out of range" (value : int)]
  ;;
end

module Output_snapshot = struct
  type t =
    { byte_mux_sel : State.t
    ; mac_byte_sel : int
    ; crc_en : bool
    ; state : State.t
    ; tx_busy : bool
    ; pad : bool
    }
  [@@deriving sexp, equal, compare]
end

(* Where the driver believes it is, independent of what the DUT reports. Keeping the two
   apart is what lets a test assert the DUT's state sequence rather than assume it. *)
module Phase = struct
  type t =
    | Launch
    | In_frame of
        int (* byte index within the frame, counting from the first preamble byte *)
    | Idle_after_frame
  [@@deriving sexp, equal, compare]
end

module Observation = struct
  type t =
    { phase : Phase.t
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

(* Expect tests use this compact form so a golden emphasizes the state walk and the
   asserted control lines instead of printing a wall of [false] fields. *)
module Compact_observation = struct
  type t =
    { phase : Phase.t
    ; state : State.t
    ; mac_byte_sel : int
    ; active_outputs : string list
    }
  [@@deriving sexp, equal, compare]
end

module Frame_observation = struct
  type t =
    { payload_length : int
    ; trace : Observation.t list
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Tx_controller"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs ~reset ~en ~start ~fifo_empty ~frame_ready ~dis_ready ~payload_last =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; start = bit start
    ; fifo_empty = bit fifo_empty
    ; frame_ready = bit frame_ready
    ; dis_ready = bit dis_ready
    ; payload_last = bit payload_last
    }
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { byte_mux_sel = State.of_int_exn (Bits.to_int_trunc output.byte_mux_sel)
    ; mac_byte_sel = Bits.to_int_trunc output.mac_byte_sel
    ; crc_en = Bits.to_bool output.crc_en
    ; state = State.of_int_exn (Bits.to_int_trunc output.state)
    ; tx_busy = Bits.to_bool output.tx_busy
    ; pad = Bits.to_bool output.pad
    }
  ;;

  let active_outputs (output : Output_snapshot.t) =
    List.filter_opt
      [ Option.some_if output.crc_en "crc_en"
      ; Option.some_if output.tx_busy "tx_busy"
      ; Option.some_if output.pad "pad"
      ]
  ;;

  let compact ({ phase; output } : Observation.t) : Compact_observation.t =
    { phase
    ; state = output.state
    ; mac_byte_sel = output.mac_byte_sel
    ; active_outputs = active_outputs output
    }
  ;;

  (* A cycle reports both sides of the edge: [during] is what the datapath acted on,
     [next_state] is what the driver needs to pick the following cycle's stimulus. *)
  let cycle
    (handler : Step.Handler.t @ local)
    ~reset
    ~en
    ~start
    ~fifo_empty
    ~frame_ready
    ~dis_ready
    ~payload_last
    =
    let data =
      Step.cycle
        handler
        (inputs ~reset ~en ~start ~fifo_empty ~frame_ready ~dis_ready ~payload_last)
    in
    let during = Step.O_data.before_edge data |> snapshot in
    let next = Step.O_data.after_edge data |> snapshot in
    during, next.state
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs
         ~reset:true
         ~en:false
         ~start:false
         ~fifo_empty:true
         ~frame_ready:false
         ~dis_ready:false
         ~payload_last:false)
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* The minimum payload the RTL pads up to: 64-byte minimum frame less the 14-byte header
     and the 4-byte FCS. *)
  let minimum_payload_length = 46

  (* Bytes on the wire for a datagram of [payload_length]: preamble, SFD, the two MACs,
     the ethertype, the padded payload, and the FCS. *)
  let expected_byte_count payload_length =
    7 + 1 + 6 + 6 + 2 + Int.max payload_length minimum_payload_length + 4
  ;;

  (* Walk one whole frame, following the DUT's own state out of each cycle rather than
     running a fixed script. [payload_index] counts real bytes consumed, which is what
     decides [fifo_empty] and [payload_last] for the next cycle. *)
  let run_frame ~payload_length =
    if payload_length < 1
    then invalid_arg "payload_length must be at least 1: an empty datagram deadlocks";
    let limit = expected_byte_count payload_length + 8 in
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop
        (handler : Step.Handler.t @ local)
        ~state
        ~cycle_index
        ~payload_index
        ~launched
        =
        if cycle_index > limit
        then failwith "tx_controller did not return to Idle within the frame budget"
        else (
          (* Idle before the launch pulse has been accepted; the payload phase consumes a
             real byte per cycle until they run out, then pads. *)
          let payload_exhausted = payload_index >= payload_length in
          let start = not launched in
          let fifo_empty =
            match (state : State.t) with
            | Payload -> payload_exhausted
            | _ -> false
          in
          let payload_last =
            match (state : State.t) with
            | Payload -> payload_index = payload_length - 1
            | _ -> false
          in
          let phase =
            match (state : State.t), launched with
            | Idle, false -> Phase.Launch
            | Idle, true -> Phase.Idle_after_frame
            | _, _ -> Phase.In_frame (cycle_index - 1)
          in
          let during, next_state =
            cycle
              handler
              ~reset:false
              ~en:true
              ~start
              ~fifo_empty
              ~frame_ready:true
              ~dis_ready:true
              ~payload_last
          in
          let observation = { Observation.phase; output = during } in
          match (state : State.t), launched with
          | Idle, true -> [ observation ]
          | _, _ ->
            let payload_index =
              match (state : State.t) with
              | Payload when not payload_exhausted -> payload_index + 1
              | _ -> payload_index
            in
            observation
            :: loop
                 handler
                 ~state:next_state
                 ~cycle_index:(cycle_index + 1)
                 ~payload_index
                 ~launched:(launched || not (State.equal next_state Idle)))
      in
      let trace =
        loop handler ~state:State.Idle ~cycle_index:0 ~payload_index:0 ~launched:false
      in
      { Frame_observation.payload_length; trace }
    in
    run_with_timeout ~timeout:(limit + 8) ~testbench
  ;;

  (* The cycles that actually put a byte on the wire: every non-Idle cycle, since
     [dis_ready] is held high. *)
  let emitting_cycles ({ trace; _ } : Frame_observation.t) =
    List.filter trace ~f:(fun (observation : Observation.t) ->
      not (State.equal observation.output.state State.Idle))
  ;;

  let emitted_byte_count observation = List.length (emitting_cycles observation)

  (* The state walk with consecutive repeats collapsed, so a caller can assert the
     sequence of states without counting cycles. *)
  let state_sequence ({ trace; _ } : Frame_observation.t) =
    List.fold trace ~init:[] ~f:(fun accumulated (observation : Observation.t) ->
      match accumulated with
      | previous :: _ when State.equal previous observation.output.state -> accumulated
      | _ -> observation.output.state :: accumulated)
    |> List.rev
  ;;

  (* How many cycles the FSM spent in each state, in the order they were first entered.
     One cycle is one byte, so this is also the per-field byte count of the frame. Idle is
     visited twice - the launch cycle and the return - and appears once here, with both
     cycles counted. *)
  let cycles_per_state (observation : Frame_observation.t) =
    List.fold (state_sequence observation) ~init:[] ~f:(fun accumulated state ->
      if List.exists accumulated ~f:(fun (seen, _) -> State.equal seen state)
      then accumulated
      else (
        let count =
          List.count observation.trace ~f:(fun (item : Observation.t) ->
            State.equal item.output.state state)
        in
        (state, count) :: accumulated))
    |> List.rev
  ;;

  (* The launch handshake: [start] and [frame_ready] arrive independently, so a [start]
     pulse ahead of a buffered frame must be latched and honored when [frame_ready] rises. *)
  let run_deferred_launch ~cycles_before_frame_ready =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let pulse (handler : Step.Handler.t @ local) ~start ~frame_ready =
        cycle
          handler
          ~reset:false
          ~en:true
          ~start
          ~fifo_empty:false
          ~frame_ready
          ~dis_ready:true
          ~payload_last:false
      in
      (* One cycle of [start] with no buffered frame, then a gap with neither. *)
      let after_start, _ = pulse handler ~start:true ~frame_ready:false in
      let rec wait (handler : Step.Handler.t @ local) remaining accumulated =
        if remaining = 0
        then List.rev accumulated
        else (
          let during, _ = pulse handler ~start:false ~frame_ready:false in
          wait handler (remaining - 1) (during :: accumulated))
      in
      let while_waiting = wait handler cycles_before_frame_ready [] in
      (* [frame_ready] rises with no fresh [start]: the latched request must launch. *)
      let at_frame_ready, _ = pulse handler ~start:false ~frame_ready:true in
      let after_launch, _ = pulse handler ~start:false ~frame_ready:true in
      after_start, while_waiting, at_frame_ready, after_launch
    in
    run_with_timeout ~timeout:(8 + cycles_before_frame_ready) ~testbench
  ;;

  (* Idle must stay Idle while the store-and-forward gate is closed, however long [start]
     is held. *)
  let run_start_without_frame_ready ~num_cycles =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) remaining accumulated =
        if remaining = 0
        then List.rev accumulated
        else (
          let during, _ =
            cycle
              handler
              ~reset:false
              ~en:true
              ~start:true
              ~fifo_empty:false
              ~frame_ready:false
              ~dis_ready:true
              ~payload_last:false
          in
          loop handler (remaining - 1) (during :: accumulated))
      in
      loop handler num_cycles []
    in
    run_with_timeout ~timeout:(4 + num_cycles) ~testbench
  ;;

  (* Dropping [dis_ready] must stall the FSM in place: the serializer cannot take a byte,
     so no state advances and no counter moves. *)
  let run_serializer_stall ~stall_cycles =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let step (handler : Step.Handler.t @ local) ~start ~dis_ready =
        cycle
          handler
          ~reset:false
          ~en:true
          ~start
          ~fifo_empty:false
          ~frame_ready:true
          ~dis_ready
          ~payload_last:false
      in
      ignore (step handler ~start:true ~dis_ready:true : Output_snapshot.t * State.t);
      (* Two preamble bytes, so the counter is off zero when the stall lands. *)
      ignore (step handler ~start:false ~dis_ready:true : Output_snapshot.t * State.t);
      let before_stall, _ = step handler ~start:false ~dis_ready:true in
      let rec stall (handler : Step.Handler.t @ local) remaining accumulated =
        if remaining = 0
        then List.rev accumulated
        else (
          let during, _ = step handler ~start:false ~dis_ready:false in
          stall handler (remaining - 1) (during :: accumulated))
      in
      let during_stall = stall handler stall_cycles [] in
      let after_stall, _ = step handler ~start:false ~dis_ready:true in
      before_stall, during_stall, after_stall
    in
    run_with_timeout ~timeout:(8 + stall_cycles) ~testbench
  ;;
end
