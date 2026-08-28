(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "tx_datapath_testbench.ml" *)

(* Testbench Support: Tx_datapath

   Shared DUT fixture, drivers, observations, and simulation scenarios used by the unit,
   Quickcheck, and expect test suites.

   Net-new verification: this block had no testbench of its own and was only exercised
   indirectly through [tx_path_tb.ml].

   The DUT is purely combinational - one byte mux with no state at all - so a
   "transaction" here is a single selector combination, and [Step.O_data.before_edge] and
   [after_edge] carry the same value. The fixture takes [after_edge] to stay consistent
   with the other suites, and [run_selectors_across_edge] reports both so a suite can
   assert they agree and a future registered stage cannot slip in unnoticed.

   Out-of-range selects. [mac_byte_sel] is three bits but indexes lists of six (the MAC
   addresses) and two (the ethertype). Hardcaml's [mux] returns the *last* element when
   the select overruns, so [saturating_nth] clamps the model's index the same way. The
   controller never drives those combinations - its counter is reset per header field -
   but the mux answers for them, so the model has to as well.

   Ethertype. [Tx_datapath.create] takes it as an optional argument defaulting to 0x9999,
   whose two bytes are identical; a byte-order fault in the ethertype mux would be
   invisible under the default. [Ipv4_testbench] instantiates the same DUT with 0x0800 so
   the high and low bytes are distinguishable.

   Tags: [{ "ACTIVE" ; "TEST" ; "TESTBENCH" ; "COMMON_ITEMS" }]
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Tx_datapath

(* The states [byte_mux_sel] carries, in the declaration order of [Common_types.States] -
   the order the RTL's mux list is built in. *)
module Byte_source = struct
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

  let to_int t =
    match t with
    | Idle -> 0
    | Preamble -> 1
    | Sfd -> 2
    | Dst_mac -> 3
    | Src_mac -> 4
    | Eth_type -> 5
    | Payload -> 6
    | Fcs -> 7
  ;;
end

(* The constants the RTL holds. Broadcast destination so a laptop accepts the frame on
   whichever port is cabled to the Arty; locally-administered source (0x02 leading byte
   means not a burned-in OUI). *)
let const_dst_mac = [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]
let const_src_mac = [ 0x02; 0x00; 0x00; 0x00; 0x00; 0x01 ]
let eth_type_bytes ethertype = [ (ethertype lsr 8) land 0xFF; ethertype land 0xFF ]

(* Hardcaml's [mux] saturates at the last element rather than wrapping or trapping. *)
let saturating_nth values index =
  List.nth_exn values (Int.min index (List.length values - 1))
;;

module Selectors = struct
  type t =
    { byte_source : Byte_source.t
    ; mac_byte_sel : int
    ; s_axis_tdata : int
    ; fcs_byte : int
    ; pad : bool
    }
  [@@deriving sexp, equal, compare]
end

module Output_snapshot = struct
  (* [keep] is the module's synthesis anti-pruning OR-reduce and carries no verification
     meaning, so it is deliberately absent. [s_axis_tready] is included because the RTL
     ties it to zero unconditionally - a fact worth freezing, since the real backpressure
     is produced elsewhere in [mac_top]. *)
  type t =
    { byte_out : int
    ; s_axis_tready : bool
    }
  [@@deriving sexp, equal, compare]
end

module Observation = struct
  type t =
    { selectors : Selectors.t
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Compact_observation = struct
  type t =
    { byte_source : Byte_source.t
    ; mac_byte_sel : int
    ; pad : bool
    ; byte_out : string
    }
  [@@deriving sexp, equal, compare]
end

module Make_testbench (Config : sig
    val ethertype : int
  end) =
struct
  module Fixture = Sim_fixture.Make (struct
      module I = Dut.I
      module O = Dut.O

      (* Written out rather than [include Dut]: [Dut.create] carries an optional
         [?ethertype] argument, and OxCaml will not erase it to match [S]'s [create]. *)
      let create scope inputs = Dut.create ~ethertype:Config.ethertype scope inputs
      let name = "Tx_datapath"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit
  let const_eth_type = eth_type_bytes Config.ethertype

  (* The pure-OCaml mux the RTL is checked against. *)
  let expected_byte
    ({ byte_source; mac_byte_sel; s_axis_tdata; fcs_byte; pad } : Selectors.t)
    =
    match byte_source with
    | Byte_source.Idle -> 0x00
    | Preamble -> 0x55
    | Sfd -> 0xD5
    | Dst_mac -> saturating_nth const_dst_mac mac_byte_sel
    | Src_mac -> saturating_nth const_src_mac mac_byte_sel
    | Eth_type -> saturating_nth const_eth_type mac_byte_sel
    (* While padding, the FIFO is not popped and the datapath emits 0x00 so the CRC covers
       the zero pad bytes that reach the 46-byte payload minimum. *)
    | Payload -> if pad then 0x00 else s_axis_tdata
    | Fcs -> fcs_byte
  ;;

  let expected_output selectors : Output_snapshot.t =
    { byte_out = expected_byte selectors; s_axis_tready = false }
  ;;

  let inputs
    ~reset
    ~en
    ~s_axis_tdata
    ~s_axis_tvalid
    ~s_axis_tuser
    ~fcs_byte
    ~byte_mux_sel
    ~mac_byte_sel
    ~pad
    =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; s_axis_tdata = Bits.of_int_trunc ~width:8 s_axis_tdata
    ; s_axis_tvalid = bit s_axis_tvalid
    ; s_axis_tuser = bit s_axis_tuser
    ; fcs_byte = Bits.of_int_trunc ~width:8 fcs_byte
    ; byte_mux_sel = Bits.of_int_trunc ~width:3 byte_mux_sel
    ; mac_byte_sel = Bits.of_int_trunc ~width:3 mac_byte_sel
    ; pad = bit pad
    }
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { byte_out = Bits.to_int_trunc output.byte_out
    ; s_axis_tready = Bits.to_bool output.s_axis_tready
    }
  ;;

  let compact ({ selectors; output } : Observation.t) : Compact_observation.t =
    { byte_source = selectors.byte_source
    ; mac_byte_sel = selectors.mac_byte_sel
    ; pad = selectors.pad
    ; byte_out = sprintf "0x%02x" output.byte_out
    }
  ;;

  let selector_inputs
    ({ byte_source; mac_byte_sel; s_axis_tdata; fcs_byte; pad } : Selectors.t)
    =
    inputs
      ~reset:false
      ~en:true
      ~s_axis_tdata
      ~s_axis_tvalid:true
      ~s_axis_tuser:false
      ~fcs_byte
      ~byte_mux_sel:(Byte_source.to_int byte_source)
      ~mac_byte_sel
      ~pad
  ;;

  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs
         ~reset:true
         ~en:false
         ~s_axis_tdata:0
         ~s_axis_tvalid:false
         ~s_axis_tuser:false
         ~fcs_byte:0
         ~byte_mux_sel:0
         ~mac_byte_sel:0
         ~pad:false)
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  (* Drive one selector combination per cycle. The block is combinational, so each cycle
     is an independent trial rather than a step in a sequence. *)
  let run_selectors selectors_list =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | selectors :: remaining ->
          let output =
            Step.cycle handler (selector_inputs selectors) |> Step.O_data.after_edge
          in
          { Observation.selectors; output = snapshot output } :: loop handler remaining
      in
      loop handler selectors_list
    in
    run_with_timeout ~timeout:(4 + List.length selectors_list) ~testbench
  ;;

  (* Same drive, but reporting both sides of the clock edge so a caller can assert the
     block really is combinational. *)
  let run_selectors_across_edge selectors_list =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      let rec loop (handler : Step.Handler.t @ local) = function
        | [] -> []
        | selectors :: remaining ->
          let data = Step.cycle handler (selector_inputs selectors) in
          let before = Step.O_data.before_edge data |> snapshot in
          let after = Step.O_data.after_edge data |> snapshot in
          (selectors, before, after) :: loop handler remaining
      in
      loop handler selectors_list
    in
    run_with_timeout ~timeout:(4 + List.length selectors_list) ~testbench
  ;;

  (* Every [byte_mux_sel] x [mac_byte_sel] pair, with fixed payload and FCS bytes so a
     misrouted mux leg shows up as the wrong constant rather than as a plausible one. *)
  let selector_space ~s_axis_tdata ~fcs_byte ~pad =
    List.concat_map Byte_source.all ~f:(fun byte_source ->
      List.init 8 ~f:(fun mac_byte_sel ->
        { Selectors.byte_source; mac_byte_sel; s_axis_tdata; fcs_byte; pad }))
  ;;

  let run_full_sweep ~s_axis_tdata ~fcs_byte ~pad =
    run_selectors (selector_space ~s_axis_tdata ~fcs_byte ~pad)
  ;;
end

module Testbench = Make_testbench (struct
    let ethertype = 0x9999
  end)

(* The same DUT with a distinguishable ethertype: 0x0800's two bytes differ, so this
   instance can tell a high/low swap in the ethertype mux from a correct one. *)
module Ipv4_testbench = Make_testbench (struct
    let ethertype = 0x0800
  end)
