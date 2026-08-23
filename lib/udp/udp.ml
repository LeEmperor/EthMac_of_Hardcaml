(* Bohdan Purtell University of Florida

   Module: "udp.ml"

   UDP implementation in hardcaml_networking.

   Basically just a phat counter.

   Design Questions:

   can we strongly make the assertion that once data is deemed valid, that all data
   following it will be valid? apparently MACs have the ability to deassert this or
   something based on the Xilinx and Bittware cores, so it's somewhat up in the air
*)

open! Core
open! Hardcaml
open! Signal

(* need customing typing on mac configurations? *)

module Bus_Implementation = struct
  type t = BYTE_WISE [@@deriving sexp_of, enumerate]
end

module type Config = sig
  val bus_width : int
  val bus_implementation : Bus_Implementation.t
end

(* who should own this typing? the make or the global? common types even? *)
module SrcPort_t = struct
  type 'a t = { value : 'a [@bits 48] } [@@deriving hardcaml]
end

module DstPort_t = struct
  type 'a t = { value : 'a [@bits 48] } [@@deriving hardcaml]
end

module Make (C : Config) = struct
  module I = struct
    type 'a t =
      { (* reg spec *)
        clk_i : 'a
      ; rst_i : 'a
      ; data_i : 'a [@bits 8]
      ; data_valid_i : 'a
      ; sof : 'a (* passed from the ethernet frame mac to begin the state machine *)
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { (* data group *)
        (* dst_port : 'a DstPort_t.t; *)
        (* src_port : 'a SrcPort_t.t; (* omg this is so cool *) *)
        dst_port : 'a [@bits 48]
      ; src_port : 'a [@bits 48]
      ; (* error group *)
        seq_num_violated : 'a (* sticky on violtaion of the sequence number *)
      }
    [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle_s
      | Src_port_s
      | Dst_port_s
      | Payload_len_s
      | Checksum_s
      | Payload_s
    [@@deriving sexp_of, compare ~localize, enumerate]

    let width = Int.ceil_log2 (List.length all)

    let to_signal t =
      List.findi_exn all ~f:(fun _ v -> compare v t = 0)
      |> fst
      |> Signal.of_int_trunc ~width
    ;;
  end

  let create (scope : Scope.t) (* should this be subscoped into the udp sub scope? *) i
    : _ O.t
    =
    let open Always in
    let open Variable in
    let re = Reg_spec.create ~clock:i.I.clk_i ~clear:i.I.rst_i () in
    let sm = State_machine.create (module States) ~enable:vdd re in
    (* raw byte counter because nice and simple *)
    (* is it better to reset the counter than to just let it go all the way to 1500? *)
    (* fanout on comparison becomes a total bitch on that avenue then *)
    let byte_counter : Signal.t =
      reg_fb re ~width:16 ~f:(fun c ->
        mux2 (sm.is Idle_s ||: i.I.rst_i) (zero 16) (c +:. 1))
    in
    (* let src_port : Always.Variable.t = *)
    let src_port : Always.Variable.t = Variable.reg ~width:16 re in
    let dst_port_r = Variable.reg ~width:16 re in
    (* is it possible for us to label Always.Variable.reg items? *)
    let src_port_r = Variable.reg ~width:16 re in
    (* helper functions *)
    let counter_is n = byte_counter ==:. n in
    let data_valid = i.I.data_valid_i in
    let data = i.I.data_i in
    (* individual counter for the payload? *)
    (* or perhaps we can just latch in payload_len from the header, and then compare with
       an additive offset against *)

    (* i think we latch into an Always.Variable.Reg for this *)
    (* let payload_counter : Signal.t = *)
    (* reg_fb re ~width:16 = *)
    (* ~f:(fun c -> *)
    (* mux2 (sm.is Src_port_s ||: (counter_is 8)) (zero 16) (c +:. 1) *)
    (* ) *)
    (* in *)
    let payload_len_latch : Always.Variable.t = Variable.reg ~width:16 re in
    (* treat these as offsets into the main byte counter of a given frame? *)
    (* this feels awfully repeatable and possibly composable on a fun function? *)
    let bytes_src_port_n = 2 in
    let bytes_dst_port_n = 2 in
    let bytes_payload_len_n = 2 in
    let bytes_checksum_n = 2 in
    let src_port_bound = bytes_src_port_n - 1 in
    let dst_port_bound = bytes_dst_port_n - 1 in
    let payload_len_bound = bytes_payload_len_n - 1 in
    let checksum_bound = bytes_checksum_n - 1 in
    let dst_port_offset = 0 in
    let dst_port_len = 2 in
    let src_port_offset = dst_port_offset + dst_port_len in
    let payload_len_offset = src_port_offset + bytes_payload_len_n in
    let checksum_offset = payload_len_offset + bytes_checksum_n in
    (* write the byte of the thing into the specified register *)
    let accum (reg : Always.Variable.t) byte =
      reg <-- select reg.value ~high:(width reg.value - 9) ~low:0 @: byte
    in
    (* does this work for single bit casting? does 0 cast correctly to 0 in binary and
       decimal? lets hope so *)
    let rst_reg (reg : Always.Variable.t) = reg <--. 0 in
    (* is it possible to string a shit load of registers along in a row instead? *)
    (* the reset fanout is insanely large then, and the clock skew among those might also
       be bad *)

    (* FSM *)
    compile
      [ (* defaults *)
        (* i_wires.crc_en <--. 0; *)
        sm.switch
          ~default:[]
          [ ( Idle_s
            , [ (* if we do all the resets on the strat of the frame, the mux fanout
                   becomes a dominating factor, therefore we only reset the next thing
                   immediately before it's going to be used *)
                (* does the thing need to be reset at all? *)
                when_
                  i.I.data_valid_i
                  [ (* sof is implicitly telling us this is true, and a MAC is suppoesd to
                       guarantee that eveyrthing is nice and parseable from downstrema i
                       think -> aka no breaks in byte validity, though whether or not the
                       protocol has been adhere to is left to downstream consumers (aka us
                       in this instance) *)
                    when_
                      i.I.sof
                      [ sm.set_next Src_port_s
                      ; accum dst_port_r data
                      ; rst_reg payload_len_latch
                      ]
                  ]
              ] )
          ; ( Dst_port_s
            , [ when_
                  data_valid
                  [ if_
                      (counter_is dst_port_offset)
                      [ sm.set_next Dst_port_s; accum dst_port_r data ]
                      [ accum dst_port_r data ]
                  ]
              ] )
          ; ( Src_port_s
            , [ when_
                  data_valid
                  [ if_
                      (counter_is src_port_offset)
                      [ sm.set_next Payload_len_s; accum src_port_r data ]
                      [ accum src_port_r data ]
                  ]
              ] )
          ; ( Payload_len_s
            , [ when_
                  data_valid
                  [ (* TODO(udp-rx): latch payload_len, advance to Checksum_s. Stubbed out
                       for now so the tree builds — see udp_tx.ml for the TX side. *)
                    if_
                      (counter_is payload_len_offset)
                      [ accum payload_len_latch data ]
                      [ accum payload_len_latch data ]
                  ]
              ] )
          ; Checksum_s, [ when_ data_valid [] ]
          ]
      ];
    { O.seq_num_violated = Signal.gnd
    ; (* this should be a latch - sticky per rst *)
      src_port = gnd
    ; dst_port =
        dst_port_r.value
        (* O.src_port.SrcPort_t.value = {Signal.vdd}; *)
        (* dst_port = Signal.vdd; *)
    }
  ;;
end
