(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "sim_fixture.ml" *)
(* Simulator construction and bounded execution for step testbenches.

   [Make] takes a DUT with the usual [I] / [O] / [create] triple plus a [name] used in the
   timeout message, and yields the [Sim] / [Step] modules together with the
   [create_simulator] and [run_with_timeout] pair that every phase-1 testbench had
   duplicated byte for byte.

   The functor deliberately exposes [Step] rather than wrapping it: suites keep calling
   [Step.cycle] / [Step.delay] / [Step.O_data] directly. An event-driven backend slots in
   later as a parallel functor over the same [S], which is why nothing here mentions
   [Cyclesim] outside the two module aliases.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml

module type S = sig
  module I : Interface.S
  module O : Interface.S

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t

  (* Used only to name the DUT in the timeout failure. *)
  val name : string
end

module Make (Dut : S) = struct
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)

  let create_simulator () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    Sim.create (Dut.create scope)
  ;;

  (* The [testbench] type is written out rather than inferred: OxCaml encodes arity in the
     arrow, so an inferred [Handler.t @ local -> (O_data.t -> 'a)] (arity one, returning a
     closure) would let the local handler escape its region and every call site would be
     rejected. Spelling it unparenthesized pins the arity at two. *)
  let run_with_timeout
    ~timeout
    ~(testbench : Step.Handler.t @ local -> Step.O_data.t -> 'a)
    =
    let simulator = create_simulator () in
    match Step.run_with_timeout ~timeout () ~simulator ~testbench with
    | Some result -> result
    | None -> failwith (Dut.name ^ " testbench timed out")
  ;;
end
