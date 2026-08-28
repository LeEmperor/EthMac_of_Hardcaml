(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "helper_circuits_testbench.ml" *)

(* Testbench Support: Helper_circuits

   Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit,
   Quickcheck, and expect test suites.

   Net-new verification: [lib/common/helper_circuits.ml] had no tests at all, despite
   [rx_datapath], [mac_top] and the UART all depending on its edge detectors and delay
   line.

   There is no DUT to instantiate. These are plain [Signal.t -> Signal.t] functions over a
   [Reg_spec.t], not modules with [I] / [O] / [create], so the fixture below defines a
   wrapper that exposes every one of them as an output of a single one-bit-input block.
   Instantiating them together rather than one per suite is deliberate: they share the
   register spec, and [rising_edge_delayed] is by construction the composition of two of
   the others, which is only checkable if both are visible at once.

   Sampling. [before_edge], and this one is not a stylistic choice. The detectors are
   combinational in the current input and the registered previous one, so at [after_edge]
   the register has already taken this cycle's value and both detectors read zero
   unconditionally - the observation would be all false, always. [before_edge] is what the
   block drives during the cycle, which is the thing a consumer sees. [run_across_edge]
   reports both so the Quickcheck suite can pin that degeneracy rather than leave it as
   folklore.

   The clear. It is stimulus here, not just setup. [run_with_clear] applies one between
   two clear-free runs, which shows the delay line flushing; [run_scheduled] takes the
   clear line per cycle so it can land on the same cycle as an edge, which is the case
   RTL-6 is about and the case a fixed-shape driver cannot express. Every coincident-clear
   scenario is run twice, with the clear and without, because "no delayed pulse appeared"
   is a claim only the clear-free control turns into evidence.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml_step_testbench
open! Hardcaml_verif

(* The delay depths the wrapper instantiates. Three is deep enough that an off-by-one in
   [delay_by]'s recursion shows up as a different column rather than as a sign flip, and
   two on the composed detectors keeps them distinguishable from the plain ones. *)
let delay_depth = 3
let edge_delay_depth = 2

(* Constants folded into the wrapper for [const8]. The second one does not fit in a byte
   and pins the truncation. *)
let const_byte_value = 0xA5
let const_byte_overflowing_value = 0x1A5

(* The wrapper DUT. One input bit, one output per helper. *)
module Dut = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; x : 'a
      ; word : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { rising : 'a
      ; falling : 'a
      ; delayed_0 : 'a
      ; delayed_1 : 'a
      ; delayed_n : 'a
      ; rising_delayed : 'a
      ; falling_delayed : 'a
      ; (* The word helpers in the same module. Combinational, unrelated to the register
           spec above, but instantiated here so the suite covers the file rather than only
           its edge detectors. *)
        hi_byte : 'a [@bits 8]
      ; lo_byte : 'a [@bits 8]
      ; const_byte : 'a [@bits 8]
      ; const_byte_truncated : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
    let spec : Reg_spec.t = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    { O.rising = Helper_circuits.rising_edge_detector spec i.x
    ; falling = Helper_circuits.falling_edge_detector spec i.x
    ; (* [delay_by] with no cycles must be the identity - the recursion's base case. It is
         wired through [wireof] because a circuit output cannot be an input port directly. *)
      delayed_0 = wireof (Helper_circuits.delay_by spec ~n_cycles:0 i.x)
    ; delayed_1 = Helper_circuits.delay_by spec ~n_cycles:1 i.x
    ; delayed_n = Helper_circuits.delay_by spec ~n_cycles:delay_depth i.x
    ; rising_delayed =
        Helper_circuits.rising_edge_delayed spec ~n_cycles:edge_delay_depth i.x
    ; falling_delayed =
        Helper_circuits.falling_edge_delayed spec ~n_cycles:edge_delay_depth i.x
    ; hi_byte = Helper_circuits.hi16 i.word
    ; lo_byte = Helper_circuits.lo16 i.word
    ; const_byte = Helper_circuits.const8 const_byte_value
    ; (* [const8] is [of_int_trunc ~width:8]: a value that does not fit is truncated
         silently. The ipv4 and udp header builders pass literals through it, so freeze
         what happens when one of them overflows. *)
      const_byte_truncated = Helper_circuits.const8 const_byte_overflowing_value
    }
  ;;

  let name = "Helper_circuits"
end

module Observation = struct
  type t =
    { x : bool
    ; rising : bool
    ; falling : bool
    ; delayed_0 : bool
    ; delayed_1 : bool
    ; delayed_n : bool
    ; rising_delayed : bool
    ; falling_delayed : bool
    }
  [@@deriving sexp, equal, compare, fields ~getters]
end

(* The boolean outputs collapse to the names of the ones that are high, which is what a
   golden row should say: on any given cycle at most a couple of these are asserted. *)
module Compact_observation = struct
  type t =
    { x : bool
    ; active_outputs : string list
    }
  [@@deriving sexp, equal, compare]
end

(* The word helpers are a separate observation because they share nothing with the edge
   detectors but the file they live in - no clock, no state, no relationship to [x]. *)
module Word_observation = struct
  type t =
    { word : int
    ; hi_byte : int
    ; lo_byte : int
    }
  [@@deriving sexp, equal, compare]
end

let expected_word_observation word : Word_observation.t =
  { word; hi_byte = (word lsr 8) land 0xFF; lo_byte = word land 0xFF }
;;

(* The pure-OCaml model, written against the input history rather than as a simulation:
   during cycle [k] with inputs [xs], every output is a function of [x] at [k] and at
   earlier indices, with everything before the clear reading false. *)
let nth_input xs index = if index < 0 then false else List.nth_exn xs index

let expected_observations xs : Observation.t list =
  let at index = nth_input xs index in
  let rose index = (not (at (index - 1))) && at index in
  let fell index = at (index - 1) && not (at index) in
  List.mapi xs ~f:(fun index x ->
    { Observation.x
    ; rising = rose index
    ; falling = fell index
    ; delayed_0 = x
    ; delayed_1 = at (index - 1)
    ; delayed_n = at (index - delay_depth)
    ; (* The composed helpers are the plain detector delayed, so they read the detector's
         own value [edge_delay_depth] cycles back. *)
      rising_delayed = rose (index - edge_delay_depth)
    ; falling_delayed = fell (index - edge_delay_depth)
    })
;;

let output_names =
  [ "rising", Observation.rising
  ; "falling", Observation.falling
  ; "delayed_0", Observation.delayed_0
  ; "delayed_1", Observation.delayed_1
  ; "delayed_n", Observation.delayed_n
  ; "rising_delayed", Observation.rising_delayed
  ; "falling_delayed", Observation.falling_delayed
  ]
;;

let compact (observation : Observation.t) : Compact_observation.t =
  { x = observation.x
  ; active_outputs =
      List.filter_map output_names ~f:(fun (name, get) ->
        if get observation then Some name else None)
  }
;;

module Testbench = struct
  module Fixture = Sim_fixture.Make (Dut)
  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs ?(word = 0) ~clear ~x () =
    { Step.input_hold with
      clear = bit clear
    ; x = bit x
    ; word = Bits.of_int_trunc ~width:16 word
    }
  ;;

  let snapshot ~x (output : Bits.t Dut.O.t) : Observation.t =
    { x
    ; rising = Bits.to_bool output.rising
    ; falling = Bits.to_bool output.falling
    ; delayed_0 = Bits.to_bool output.delayed_0
    ; delayed_1 = Bits.to_bool output.delayed_1
    ; delayed_n = Bits.to_bool output.delayed_n
    ; rising_delayed = Bits.to_bool output.rising_delayed
    ; falling_delayed = Bits.to_bool output.falling_delayed
    }
  ;;

  (* The clear is held with [x] low, so the register history entering the first driven
     cycle is all zeroes - which is what [expected_observations] assumes for indices below
     zero. Held for [delay_depth + 1] cycles so the deepest delay register is flushed too,
     not just the first. *)
  let reset ?(num_cycles = delay_depth + 1) (handler : Step.Handler.t @ local) =
    Step.delay ~num_cycles handler (inputs ~clear:true ~x:false ())
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  let run_stimuli xs =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | x :: remaining ->
          let output =
            Step.cycle handler (inputs ~clear:false ~x ()) |> Step.O_data.before_edge
          in
          snapshot ~x output :: loop handler remaining
      in
      loop handler xs
    in
    run_with_timeout ~timeout:(8 + List.length xs) ~testbench
  ;;

  (* Both sides of the edge, so a suite can assert that [after_edge] is degenerate for the
     detectors rather than quietly sampling the wrong side. *)
  let run_across_edge xs =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | x :: remaining ->
          let data = Step.cycle handler (inputs ~clear:false ~x ()) in
          let before = Step.O_data.before_edge data |> snapshot ~x in
          let after = Step.O_data.after_edge data |> snapshot ~x in
          (before, after) :: loop handler remaining
      in
      loop handler xs
    in
    run_with_timeout ~timeout:(8 + List.length xs) ~testbench
  ;;

  (* A clear applied mid-stream, to show the delay line flush rather than only the
     post-reset steady state. [xs_before] runs clear-free, then one clear cycle, then
     [xs_after]. *)
  let run_with_clear ~xs_before ~xs_after =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) ~clear = function
        | [] -> []
        | x :: remaining ->
          let output =
            Step.cycle handler (inputs ~clear ~x ()) |> Step.O_data.before_edge
          in
          snapshot ~x output :: loop handler ~clear remaining
      in
      let before = loop handler ~clear:false xs_before in
      let during = loop handler ~clear:true [ false ] in
      let after = loop handler ~clear:false xs_after in
      List.concat [ before; during; after ]
    in
    run_with_timeout
      ~timeout:(10 + List.length xs_before + List.length xs_after)
      ~testbench
  ;;

  (* The clear line as part of the stimulus, one [(clear, x)] pair per cycle.
     [run_with_clear] has a fixed shape - a clear-free run, one clear cycle with [x] low,
     a clear-free tail - which cannot place a clear on the same cycle as an edge in the
     input. That coincidence is the whole of RTL-6, so it needs a driver that can express
     it, and the clear-free control it has to be compared against. *)
  let run_scheduled stimuli =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | (clear, x) :: remaining ->
          let output =
            Step.cycle handler (inputs ~clear ~x ()) |> Step.O_data.before_edge
          in
          snapshot ~x output :: loop handler remaining
      in
      reset handler;
      loop handler stimuli
    in
    run_with_timeout ~timeout:(8 + List.length stimuli) ~testbench
  ;;

  (* The cycle the edge lands on in [edge_with_clear] below. Two cycles of the starting
     level first, so the detector's history register holds it before the edge. *)
  let edge_cycle = 2

  (* An edge from [before] to [after] on cycle [edge_cycle], with the clear line asserted
     on that same cycle or not. One description for both the case and its control, so the
     control cannot drift from what it controls. *)
  let edge_with_clear ~before ~after ~clear ~tail =
    List.init edge_cycle ~f:(fun _ -> false, before)
    @ [ clear, after ]
    @ List.init tail ~f:(fun _ -> false, after)
  ;;

  (* The word helpers, driven one word per cycle. Purely combinational, so [before_edge]
     and [after_edge] agree; [before_edge] keeps it consistent with everything else here. *)
  let run_words words =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | word :: remaining ->
          let output =
            Step.cycle handler (inputs ~word ~clear:false ~x:false ())
            |> Step.O_data.before_edge
          in
          { Word_observation.word
          ; hi_byte = Bits.to_int_trunc output.Dut.O.hi_byte
          ; lo_byte = Bits.to_int_trunc output.Dut.O.lo_byte
          }
          :: loop handler remaining
      in
      loop handler words
    in
    run_with_timeout ~timeout:(8 + List.length words) ~testbench
  ;;

  (* [const8] folds at elaboration, so one cycle is enough to read both instances back. *)
  let run_constants () =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let output =
        Step.cycle handler (inputs ~clear:false ~x:false ()) |> Step.O_data.before_edge
      in
      ( Bits.to_int_trunc output.Dut.O.const_byte
      , Bits.to_int_trunc output.Dut.O.const_byte_truncated
      , Bits.width output.Dut.O.const_byte )
    in
    run_with_timeout ~timeout:8 ~testbench
  ;;

  (* A square wave of the given half-period, the shape every consumer of these helpers
     actually feeds them. *)
  let square_wave ~half_period ~num_periods =
    List.concat
      (List.init num_periods ~f:(fun _ ->
         List.init half_period ~f:(fun _ -> false)
         @ List.init half_period ~f:(fun _ -> true)))
  ;;
end
