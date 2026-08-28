(* Bohdan Purtell University of Florida

   Module: Uart_tx A tick-driven 8-N-1 UART transmitter: one space start bit, eight data
   bits least significant first, one mark stop bit. The block advances on [tick] rather
   than on the clock, so the baud rate lives in whatever generates [tick] and the frame
   this module sends is the same shape at every tick spacing.

   [uart_tx] is a Moore output driven straight off the current state - there is no
   register between the state and the pin - so the line during a cycle is what that
   cycle's state selects.
*)

open! Core
open! Hardcaml
open! Signal
open! Always

module I = struct
  type 'a t =
    { clk : 'a
    ; rst : 'a
    ; en : 'a
    ; tick : 'a
    ; d_in : 'a [@bits 8]
    ; d_in_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { uart_tx : 'a
    ; (* debug lines *)
      keep : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | IDLE (* idle high *)
    | START
    | PAYLOAD
    | STOP
  [@@deriving sexp_of, compare ~localize, enumerate]
end

(* internal regs block *)
module I_Regs = struct
  type 'a t = { data_place_counter : 'a [@bits 3] } [@@deriving hardcaml]
end

(* internal wires block *)
module I_Wires = struct
  type 'a t = { tx_d : 'a } [@@deriving hardcaml]
end

let create (scope : Scope.t) i : _ O.t =
  (* port aliases *)
  let clk = i.I.clk in
  let rst = i.I.rst in
  let en = i.I.en in
  let byte = i.I.d_in in
  let byte_valid = i.I.d_in_valid in
  let rising_edge : Reg_spec.t = Reg_spec.create ~clock:clk ~clear:rst () in
  (* state machine *)
  let sm = Always.State_machine.create (module States) ~enable:en rising_edge in
  (* tagging + register creation *)
  let i_regs = I_Regs.Of_always.reg ~enable:en rising_edge in
  I_Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) i_regs;
  (* tagging + wire creation - the line idles at mark, so this wire defaults high *)
  let i_wires = I_Wires.Of_always.wire Signal.ones in
  I_Wires.Of_always.apply_names ~prefix:"wire_" ~naming_op:(Scope.naming scope) i_wires;
  let data_place_counter = i_regs.data_place_counter in
  let tx_d = i_wires.tx_d in
  Always.(
    (* moore assignment *)
    compile
      [ (* default *)
        tx_d <--. 1
      ; sm.switch
          ~default:[]
          [ IDLE, [ tx_d <--. 1 ]
          ; START, [ tx_d <--. 0 ]
          ; ( PAYLOAD
            , [ (* The counter indexes the byte least significant bit first, which is the
                   order the wire carries. A mux over [bits_lsb], not a shift register:
                   [byte] is read combinationally every cycle, so the caller has to hold
                   [d_in] for the whole frame. *)
                (let tx_bit = mux data_place_counter.value (bits_lsb byte) in
                 tx_d <-- tx_bit)
              ] )
          ; STOP, [ tx_d <--. 1 ]
          ]
      ];
    (* mealy next_state *)
    compile
      [ sm.switch
          ~default:[]
          [ IDLE, [ if_ byte_valid [ sm.set_next START ] [ sm.set_next IDLE ] ]
          ; START, [ data_place_counter <--. 0; when_ i.I.tick [ sm.set_next PAYLOAD ] ]
          ; ( PAYLOAD
            , [ (* we can do this with a hardware Always counter, but is there a more
                   ocaml-y way with ocaml? *)
                (* there is indeed with the reg_fb primitive *)
                sm.set_next PAYLOAD
              ; when_
                  i.I.tick
                  [ if_
                      (data_place_counter.value ==: of_int_trunc ~width:3 7)
                      [ (* move to stop condition, line should be driven to mark *)
                        sm.set_next STOP
                      ]
                      [ (* keep payloading *)
                        data_place_counter <-- data_place_counter.value +:. 1
                      ; sm.set_next PAYLOAD
                      ]
                  ]
              ] )
          ; STOP, [ when_ i.I.tick [ sm.set_next IDLE ] ]
          ]
      ]);
  (* Synthesis anti-pruning, in the shape the MII blocks use: OR-reduce the internals that
     nothing else drives out of the module, so they survive into the netlist and the VCD.
     The value means nothing - only that it depends on the bit counter and the state. *)
  let keep = reduce ~f:( |: ) (bits_lsb data_place_counter.value @ bits_lsb sm.current) in
  { uart_tx = tx_d.value; keep }
;;
