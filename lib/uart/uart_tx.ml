open! Core
open! Hardcaml
open! Signal
open! Always

module I = struct
  type 'a t = {
    clk : 'a;
    rst : 'a;
    en  : 'a;

    tick : 'a;

    d_in : 'a [@bits 8];
    d_in_valid : 'a;
  } [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    uart_tx : 'a;

    keep : 'a;
  } [@@deriving hardcaml]
end

module States = struct
  type t =
    | IDLE (* idle high *)
    | START
    | PAYLOAD
    | STOP
    | DONE
    [@@deriving sexp_of, compare ~localize, enumerate]
end

let create 
  (scope : Scope.t)
  (i)
  : (_ O.t)
  =

  (* scope shenanigans *)
  let _scope = Scope.sub_scope scope "uart_tx" in

  (* port aliases *)
  let clk = i.I.clk in
  let rst = i.I.rst in
  let en  = i.I.en in
  let byte = i.I.d_in in
  let byte_valid = i.I.d_in_valid in
  let rising_edge : Reg_spec.t = Reg_spec.create ~clock:clk ~clear:rst () in
  let tx_d = Always.Variable.wire ~default:(Signal.one 1) () in

  (* state machine *)
  let sm = Always.State_machine.create (module States) ~enable:en rising_edge in

  (* internals *)
  let data_place_counter = Always.Variable.reg ~enable:en ~width:3 rising_edge in

  let frame =
    concat_msb [
      Signal.zero 1;
      byte;
      Signal.one 1;
    ] in

  Always.(
    (* moore assignment *)
    compile [
      (* default *)
      tx_d <--. 1;

      sm.switch ~default:[] [
        IDLE, [
          tx_d <--. 1;
        ];

        START, [
          tx_d <--. 0;
        ];

        PAYLOAD, [
          (* bitmasked shift register, is there a more OCaml way for this? *)
          (* let target = (srl byte ~by:1) in *)
          (* tx_d <-- target; *)
          (* let shifted = srl byte data_place_counter.value in *)
          (* () *)
          let tx_bit = mux data_place_counter.value (bits_lsb byte) in
          tx_d <-- tx_bit;
        ];

        STOP, [
          tx_d <--. 1;
        ];
      ];
    ];

    (* mealy next_state *)
    compile [
      sm.switch ~default:[] [
        IDLE, [
          if_ (byte_valid) [
            sm.set_next START;
          ] [
            sm.set_next IDLE;
          ];
        ];

        START, [
          data_place_counter <--. 0;
          when_ (i.I.tick) [
            sm.set_next PAYLOAD;
          ];
        ];

        PAYLOAD, [
          (* we can do this with a hardware Always counter, but is there a more ocaml-y way with ocaml? *)
          (* there is indeed with the reg_fb primitive *)
          sm.set_next PAYLOAD;

          when_ (i.I.tick) [
            if_ (data_place_counter.value ==: of_int_trunc ~width:3 7) [
              (* move to stop condition, line should be driven to mark *)
              sm.set_next STOP;
            ] [
              (* keep payloading *)
              data_place_counter <-- data_place_counter.value +:. 1;
              sm.set_next PAYLOAD;
            ];
          ]
        ];

        STOP, [
          when_ (i.I.tick) [
            sm.set_next IDLE;
          ];
        ];
      ];
    ];
  );

  {
    uart_tx = tx_d.value;
    keep    = zero 1;
  }

