(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "uart_tx_testbench.ml" *)

(* Testbench Support: Uart_tx

   Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit,
   Quickcheck, and expect test suites.

   The oracle is a software UART receiver, not a cycle model. [Uart_receiver.decode] takes
   the symbols the fixture sampled off the line and reconstructs a byte the way a real
   receiver would - start bit, eight data bits least significant first, stop bit - so the
   headline property is a round trip: a random byte in, the same byte back out of the
   decoder. That is a stronger statement than "the line matched this waveform", because it
   is indifferent to how the transmitter chooses to time itself.

   Symbols, not samples. The block is tick-driven: it advances on [tick] and holds the
   line steady in between, so a symbol is the whole interval between two ticks, and the
   fixture records the line on *every* cycle of it rather than sampling once. A symbol
   whose line moves mid-interval is recorded as unstable and fails the decode - a real
   receiver sampling at its own phase would see a glitch there, and a single mid-bit
   sample would not.

   Sampling. [before_edge]. [uart_tx] is an [Always.Variable.wire] driven straight off the
   current state - Moore, no register between the state and the pin - so the line during a
   cycle is what this cycle's state selects. [after_edge] would report the *next* symbol's
   value and shift the whole frame by one.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Uart_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Uart_tx

(* Start bit, eight data bits, stop bit. *)
let symbols_per_frame = 10
let data_bits_per_frame = 8

(* One inter-tick interval: the line value on each cycle of it. *)
module Symbol = struct
  type t = { line : bool list } [@@deriving sexp, equal, compare]

  let value t = List.hd_exn t.line
  let is_stable t = List.for_all t.line ~f:(Bool.equal (value t))

  let to_string t =
    String.of_list (List.map t.line ~f:(fun high -> if high then '1' else '0'))
  ;;
end

module Frame = struct
  type t =
    { leading_line : bool list
    ; symbols : Symbol.t list
    ; trailing_line : bool list
    }
  [@@deriving sexp, equal, compare]
end

(* The software receiver. Deliberately total: an unexpected symbol count or an unstable
   symbol comes back in the record rather than raising, so a failing property prints what
   the line actually did instead of a backtrace. *)
module Uart_receiver = struct
  module Decoded = struct
    type t =
      { start_bit : bool
      ; data_bits : bool list (* least significant first, the order the wire carries *)
      ; stop_bit : bool
      ; byte : int option
      ; unstable_symbols : int list
      }
    [@@deriving sexp, equal, compare]
  end

  let byte_of_data_bits data_bits =
    if List.length data_bits <> data_bits_per_frame
    then None
    else
      Some
        (List.foldi data_bits ~init:0 ~f:(fun index accumulator bit ->
           if bit then accumulator lor (1 lsl index) else accumulator))
  ;;

  let decode symbols : Decoded.t =
    let unstable_symbols =
      List.filter_mapi symbols ~f:(fun index symbol ->
        if Symbol.is_stable symbol then None else Some index)
    in
    let values = List.map symbols ~f:Symbol.value in
    match values with
    | start_bit :: remaining when List.length remaining = data_bits_per_frame + 1 ->
      let data_bits = List.take remaining data_bits_per_frame in
      let stop_bit = List.last_exn remaining in
      { start_bit
      ; data_bits
      ; stop_bit
      ; byte =
          (if List.is_empty unstable_symbols then byte_of_data_bits data_bits else None)
      ; unstable_symbols
      }
    | _ ->
      (* Wrong symbol count: report it rather than guess an alignment. *)
      { start_bit = true
      ; data_bits = values
      ; stop_bit = true
      ; byte = None
      ; unstable_symbols
      }
  ;;

  (* What a correct transmission of [byte] decodes to. *)
  let expected byte : Decoded.t =
    { start_bit = false
    ; data_bits =
        List.init data_bits_per_frame ~f:(fun index -> (byte lsr index) land 1 = 1)
    ; stop_bit = true
    ; byte = Some byte
    ; unstable_symbols = []
    }
  ;;
end

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Uart_tx"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs ~rst ~en ~tick ~d_in ~d_in_valid =
    { Step.input_hold with
      rst = bit rst
    ; en = bit en
    ; tick = bit tick
    ; d_in = Bits.of_int_trunc ~width:8 d_in
    ; d_in_valid = bit d_in_valid
    }
  ;;

  let line (output : Bits.t Dut.O.t) = Bits.to_bool output.uart_tx

  (* The legacy harness's reset: enable low while the clear is applied, enable high
     afterwards. *)
  let reset ?(num_cycles = 2) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs ~rst:true ~en:false ~tick:false ~d_in:0 ~d_in_valid:false)
  ;;

  let drive (handler : Step.Handler.t @ local) ~en ~tick ~d_in ~d_in_valid =
    Step.cycle handler (inputs ~rst:false ~en ~tick ~d_in ~d_in_valid)
    |> Step.O_data.before_edge
    |> line
  ;;

  let drive_idle (handler : Step.Handler.t @ local) ~num_cycles ~d_in =
    let rec loop (handler : Step.Handler.t @ local) remaining =
      if remaining = 0
      then []
      else (
        let value = drive handler ~en:true ~tick:false ~d_in ~d_in_valid:false in
        value :: loop handler (remaining - 1))
    in
    loop handler num_cycles
  ;;

  (* One symbol: [stall_cycles] cycles with the enable low, then the rest of the interval
     enabled, then the tick that ends it. The stall cycles are inside the interval on
     purpose - a transmitter that advanced on the clock rather than on the tick would
     lengthen its symbol here and the decode would still pass, so the stability check
     across the whole interval is what actually catches it. *)
  let drive_symbol
    (handler : Step.Handler.t @ local)
    ~cycles_per_symbol
    ~stall_cycles
    ~d_in
    =
    let rec loop (handler : Step.Handler.t @ local) index =
      let total = stall_cycles + cycles_per_symbol in
      if index = total
      then []
      else (
        let en = index >= stall_cycles in
        let tick = index = total - 1 in
        let value = drive handler ~en ~tick ~d_in ~d_in_valid:false in
        value :: loop handler (index + 1))
    in
    { Symbol.line = loop handler 0 }
  ;;

  let drive_symbols
    (handler : Step.Handler.t @ local)
    ~cycles_per_symbol
    ~stall_cycles
    ~d_in
    ~num_symbols
    =
    let rec loop (handler : Step.Handler.t @ local) remaining =
      if remaining = 0
      then []
      else (
        let symbol = drive_symbol handler ~cycles_per_symbol ~stall_cycles ~d_in in
        symbol :: loop handler (remaining - 1))
    in
    loop handler num_symbols
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* Transmit one byte and report the whole line: the idle cycles before it, the ten
     symbols, and the idle cycles after. [d_in] is held for the entire frame because the
     payload mux reads it combinationally - the block latches the request, not the data. *)
  let run_byte
    ?(cycles_per_symbol = 4)
    ?(stall_cycles = 0)
    ?(num_leading = 2)
    ?(num_trailing = 4)
    ?(num_symbols = symbols_per_frame)
    byte
    =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let idle = drive_idle handler ~num_cycles:num_leading ~d_in:byte in
      (* The launch cycle: [d_in_valid] high for one cycle, which is all the FSM needs to
         leave Idle. It is dropped immediately so the return to Idle at the end of the
         frame does not start a second one. *)
      let launch = drive handler ~en:true ~tick:false ~d_in:byte ~d_in_valid:true in
      let symbols =
        drive_symbols handler ~cycles_per_symbol ~stall_cycles ~d_in:byte ~num_symbols
      in
      let trailing = drive_idle handler ~num_cycles:num_trailing ~d_in:byte in
      { Frame.leading_line = idle @ [ launch ]; symbols; trailing_line = trailing }
    in
    let timeout =
      8 + num_leading + num_trailing + (num_symbols * (cycles_per_symbol + stall_cycles))
    in
    run_with_timeout ~timeout ~testbench
  ;;

  let decode_byte ?cycles_per_symbol ?stall_cycles byte =
    let frame = run_byte ?cycles_per_symbol ?stall_cycles byte in
    Uart_receiver.decode frame.symbols
  ;;

  (* Idle with no request at all: the line must sit at mark. *)
  let run_idle ~num_cycles =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      drive_idle handler ~num_cycles ~d_in:0xFF
    in
    run_with_timeout ~timeout:(8 + num_cycles) ~testbench
  ;;

  (* [keep] is the module's synthesis anti-pruning output and is tied to zero here, unlike
     the MII blocks where it OR-reduces real internals. Worth freezing: a later edit that
     starts driving it would change what synthesis retains. *)
  let run_keep ~num_cycles byte =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) remaining =
        if remaining = 0
        then []
        else (
          let output =
            Step.cycle
              handler
              (inputs ~rst:false ~en:true ~tick:true ~d_in:byte ~d_in_valid:true)
            |> Step.O_data.before_edge
          in
          Bits.to_bool output.Dut.O.keep :: loop handler (remaining - 1))
      in
      loop handler num_cycles
    in
    run_with_timeout ~timeout:(8 + num_cycles) ~testbench
  ;;
end

(* Golden rendering: the frame as the line actually carried it, symbol by symbol. *)
let frame_to_string ({ leading_line; symbols; trailing_line } : Frame.t) =
  let idle values =
    String.of_list (List.map values ~f:(fun high -> if high then '1' else '0'))
  in
  String.concat
    ~sep:" "
    ((idle leading_line :: List.map symbols ~f:Symbol.to_string) @ [ idle trailing_line ])
;;
