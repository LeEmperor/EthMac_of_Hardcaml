(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Testbench Support: "Pcs_tx_scrambler"

   Stateful DUT harness and an independent serial software model of the Clause 49
   recurrence. The model intentionally operates one bit at a time instead of sharing the
   DUT's parallel construction.
*)

open! Core
open! Hardcaml
open! Pcs_of_hardcaml
module Dut = Pcs_tx_scrambler

module Scrambled = struct
  type t =
    { data : Bits.t
    ; header : int
    }
  [@@deriving sexp_of]

  let equal a b = Bits.equal a.data b.data && a.header = b.header

  let printable t =
    [%sexp { data = (Bits.Hex.to_string t.data : string); header = (t.header : int) }]
  ;;
end

module Reference = struct
  type t = bool array

  let reset () = Array.create ~len:58 true

  let step state payload =
    let state = Array.copy state in
    let output = Array.create ~len:64 false in
    for bit_index = 0 to 63 do
      let input_bit = Bits.bit payload ~pos:bit_index |> Bits.to_bool in
      let scrambled_bit = Bool.(input_bit <> state.(38) <> state.(57)) in
      output.(bit_index) <- scrambled_bit;
      for state_index = 57 downto 1 do
        state.(state_index) <- state.(state_index - 1)
      done;
      state.(0) <- scrambled_bit
    done;
    let data =
      Bits.concat_lsb
        (Array.to_list output
         |> List.map ~f:(fun value -> if value then Bits.vdd else Bits.gnd))
    in
    data, state
  ;;
end

module Testbench = struct
  module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)

  type t =
    { sim : Sim.t
    ; inputs : Bits.t ref Dut.I.t
    ; outputs : Bits.t ref Dut.O.t
    ; mutable reference_state : Reference.t
    }

  let create () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let sim = Sim.create (Dut.create scope) in
    { sim
    ; inputs = Cyclesim.inputs sim
    ; outputs = Cyclesim.outputs ~clock_edge:Side.Before sim
    ; reference_state = Reference.reset ()
    }
  ;;

  let reset t =
    t.inputs.reset_i := Bits.vdd;
    t.inputs.encoded_payload_i := Bits.zero 64;
    t.inputs.encoded_header_i := Bits.zero 2;
    Cyclesim.cycle t.sim;
    t.inputs.reset_i := Bits.gnd;
    t.reference_state <- Reference.reset ()
  ;;

  let scramble t ~payload ~header =
    let expected_data, next_reference_state = Reference.step t.reference_state payload in
    t.inputs.encoded_payload_i := payload;
    t.inputs.encoded_header_i := Bits.of_int_trunc ~width:2 header;
    Cyclesim.cycle_check t.sim;
    Cyclesim.cycle_before_clock_edge t.sim;
    let actual : Scrambled.t =
      { data = !(t.outputs.scrambled_data_o)
      ; header = Bits.to_int_trunc !(t.outputs.scrambled_header_o)
      }
    in
    let expected : Scrambled.t = { data = expected_data; header } in
    if not (Scrambled.equal actual expected)
    then
      raise_s
        [%message
          "PCS TX scrambler mismatch"
            (payload : Bits.t)
            (Scrambled.printable actual : Sexp.t)
            (Scrambled.printable expected : Sexp.t)];
    Cyclesim.cycle_at_clock_edge t.sim;
    Cyclesim.cycle_after_clock_edge t.sim;
    t.reference_state <- next_reference_state;
    actual
  ;;

  let scramble_hex t ~payload ~header =
    scramble t ~payload:(Bits.of_hex ~width:64 payload) ~header
  ;;
end
