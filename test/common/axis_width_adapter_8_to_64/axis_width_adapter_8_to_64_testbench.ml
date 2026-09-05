open! Core
open! Hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Axis_width_adapter_8_to_64

module Source_beat = struct
  type t =
    { data : int
    ; first : bool
    ; last : bool
    }
  [@@deriving sexp, equal, compare]
end

module Wide_beat = struct
  type t =
    { data : int64
    ; keep : int
    ; first : bool
    ; last : bool
    }
  [@@deriving sexp, equal, compare]
end

module Cycle_observation = struct
  type t =
    { cycle : int
    ; source_data : int option
    ; source_ready : bool
    ; output : Wide_beat.t option
    ; output_ready : bool
    }
  [@@deriving sexp]
end

module Observation = struct
  type t =
    { transfers : Wide_beat.t list
    ; stalled_outputs : Wide_beat.t list
    ; trace : Cycle_observation.t list
    }
  [@@deriving sexp]
end

let source_beats frames =
  List.concat_map frames ~f:(fun frame ->
    List.mapi frame ~f:(fun index data ->
      { Source_beat.data; first = index = 0; last = index = List.length frame - 1 }))
;;

let wide_beat (o : Bits.t Dut.O.t) =
  { Wide_beat.data = Bits.to_int64_trunc o.m_tdata_o
  ; keep = Bits.to_int_trunc o.m_tkeep_o
  ; first = Bits.to_bool o.m_tfirst_o
  ; last = Bits.to_bool o.m_tlast_o
  }
;;

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Axis_width_adapter_8_to_64"
    end)

  module Step = Fixture.Step

  let inputs ~reset ~en ~source ~output_ready =
    let source_data, source_valid, source_first, source_last =
      match source with
      | None -> 0, false, false, false
      | Some beat -> beat.Source_beat.data, true, beat.first, beat.last
    in
    { Step.input_hold with
      reset_i = Bits_conv.bit reset
    ; en_i = Bits_conv.bit en
    ; s_tdata_i = Bits.of_int_trunc ~width:8 source_data
    ; s_tvalid_i = Bits_conv.bit source_valid
    ; s_tlast_i = Bits_conv.bit source_last
    ; s_tfirst_i = Bits_conv.bit source_first
    ; m_tready_i = Bits_conv.bit output_ready
    }
  ;;

  let run ?(ready = fun _ -> true) frames =
    if List.exists frames ~f:List.is_empty
    then invalid_arg "the byte-stream adapter cannot represent an empty frame";
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      Step.delay
        ~num_cycles:2
        handler
        (inputs ~reset:true ~en:true ~source:None ~output_ready:false);
      let rec loop
        (handler : Step.Handler.t @ local)
        cycle
        remaining
        transfers
        stalled_outputs
        trace
        =
        let source = List.hd remaining in
        let output_ready = ready cycle in
        let edge =
          Step.cycle handler (inputs ~reset:false ~en:true ~source ~output_ready)
        in
        let before = Step.O_data.before_edge edge in
        let source_ready = Bits.to_bool before.s_tready_o in
        let output_valid = Bits.to_bool before.m_tvalid_o in
        let output = if output_valid then Some (wide_beat before) else None in
        let trace =
          { Cycle_observation.cycle
          ; source_data = Option.map source ~f:(fun beat -> beat.Source_beat.data)
          ; source_ready
          ; output
          ; output_ready
          }
          :: trace
        in
        let remaining =
          if Option.is_some source && source_ready
          then List.tl_exn remaining
          else remaining
        in
        let transfers =
          match output with
          | Some beat when output_ready -> beat :: transfers
          | _ -> transfers
        in
        let stalled_outputs =
          match output with
          | Some beat when not output_ready -> beat :: stalled_outputs
          | _ -> stalled_outputs
        in
        if Option.is_none source && not output_valid
        then
          { Observation.transfers = List.rev transfers
          ; stalled_outputs = List.rev stalled_outputs
          ; trace = List.rev trace
          }
        else if cycle > 512
        then failwith "adapter scenario timed out"
        else loop handler (cycle + 1) remaining transfers stalled_outputs trace
      in
      loop handler 0 (source_beats frames) [] [] []
    in
    let input_bytes = List.sum (module Int) frames ~f:List.length in
    Fixture.run_with_timeout ~timeout:(32 + (4 * input_bytes)) ~testbench
  ;;
end

let expected_frame frame =
  List.chunks_of frame ~length:8
  |> List.mapi ~f:(fun word_index bytes ->
    let data =
      List.foldi bytes ~init:0L ~f:(fun lane acc byte ->
        Int64.bit_or acc (Int64.shift_left (Int64.of_int byte) (lane * 8)))
    in
    { Wide_beat.data
    ; keep = (1 lsl List.length bytes) - 1
    ; first = word_index = 0
    ; last = (word_index + 1) * 8 >= List.length frame
    })
;;

let expected frames = List.concat_map frames ~f:expected_frame

let compact_beat (beat : Wide_beat.t) =
  sprintf
    "data=0x%016Lx keep=0x%02x%s%s"
    beat.data
    beat.keep
    (if beat.first then " first" else "")
    (if beat.last then " last" else "")
;;
