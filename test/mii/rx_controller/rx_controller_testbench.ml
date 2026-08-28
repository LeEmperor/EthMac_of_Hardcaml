(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "rx_controller_testbench.ml" *)

(* Testbench Support: Rx_controller

   Shared DUT fixture, byte-level drivers, observations, and simulation scenarios used by
   the unit, Quickcheck, and expect test suites. [rx_data] is the output of the byte
   assembler, so one controller transaction is one complete byte.

   This is a stateful example of testbench architecture vs combinational. We'll see how
   good it is.
*)

open! Core
open! Hardcaml
open! Signal
open! Mii_of_hardcaml
open! Hardcaml_step_testbench
open! Hardcaml_verif
module Dut = Rx_controller

module Output_snapshot = struct
  type t =
    { byte_assembler_en : bool
    ; dst_mac_reg_en : bool
    ; src_mac_reg_en : bool
    ; eth_type_reg_en : bool
    ; payload_sel : bool
    ; emit_payload : bool
    ; fcs_present : bool
    ; in_preamble : bool
    ; in_dst_mac : bool
    ; in_payload : bool
    }
  [@@deriving sexp, equal, compare]
end

module Phase = struct
  type t =
    | Preamble of int
    | Sfd
    | Destination_mac of int
    | Source_mac of int
    | Eth_type of int
    | Payload of int
  [@@deriving sexp, equal, compare]
end

module Observation = struct
  type t =
    { phase : Phase.t
    ; byte : int
    ; output : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

(* Expect tests use this compact form so the golden output emphasizes asserted control
   lines instead of printing a wall of [false] fields. *)
module Compact_observation = struct
  type t =
    { phase : Phase.t
    ; byte : int
    ; active_outputs : string list
    }
  [@@deriving sexp, equal, compare]
end

(* I reason taht these items are a bunch of UVM items that I compose things into; not sure
   on the performance implications of runtime deciding on the observation class but alas
   this was the only way that made sense to me
*)
module Pause_observation = struct
  type t =
    { before_pause : Output_snapshot.t
    ; during_pause : Output_snapshot.t
    ; after_sixth_destination_byte : Output_snapshot.t
    ; after_first_source_byte : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Reset_observation = struct
  type t =
    { before_reset : Output_snapshot.t
    ; after_reset : Output_snapshot.t
    ; after_next_preamble : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

module Error_observation = struct
  type t =
    { before_error : Output_snapshot.t
    ; after_error : Output_snapshot.t
    ; following_idle_cycle : Output_snapshot.t
    }
  [@@deriving sexp, equal, compare]
end

(* Lifted into [Hardcaml_verif.Eth_frame] so the CRC, datapath, and integration suites
   describe frames the same way. Re-exported under the old name for readability here.

   I love aliases.
*)
module Frame = Eth_frame

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Rx_controller"
    end)

  module Sim = Fixture.Sim
  module Step = Fixture.Step

  let bit = Bits_conv.bit

  let inputs ~reset ~en ~rx_dv ~rx_er ~rx_data ~rx_data_valid =
    { Step.input_hold with
      reset = bit reset
    ; en = bit en
    ; rx_dv = bit rx_dv
    ; rx_er = bit rx_er
    ; rx_data = Bits.of_int_trunc ~width:8 rx_data
    ; rx_data_valid = bit rx_data_valid
    }
  ;;

  let snapshot (output : Bits.t Dut.O.t) : Output_snapshot.t =
    { byte_assembler_en = Bits.to_bool output.byte_assembler_en
    ; dst_mac_reg_en = Bits.to_bool output.dst_mac_reg_en
    ; src_mac_reg_en = Bits.to_bool output.src_mac_reg_en
    ; eth_type_reg_en = Bits.to_bool output.eth_type_reg_en
    ; payload_sel = Bits.to_bool output.payload_sel
    ; emit_payload = Bits.to_bool output.emit_payload
    ; fcs_present = Bits.to_bool output.fcs_present
    ; in_preamble = Bits.to_bool output.in_preamble
    ; in_dst_mac = Bits.to_bool output.in_dst_mac
    ; in_payload = Bits.to_bool output.in_payload
    }
  ;;

  (* this looks awfully functionizable *)
  let active_outputs (output : Output_snapshot.t) =
    List.filter_opt
      [ Option.some_if output.byte_assembler_en "byte_assembler_en"
      ; Option.some_if output.dst_mac_reg_en "dst_mac_reg_en"
      ; Option.some_if output.src_mac_reg_en "src_mac_reg_en"
      ; Option.some_if output.eth_type_reg_en "eth_type_reg_en"
      ; Option.some_if output.payload_sel "payload_sel"
      ; Option.some_if output.emit_payload "emit_payload"
      ; Option.some_if output.fcs_present "fcs_present"
      ; Option.some_if output.in_preamble "in_preamble"
      ; Option.some_if output.in_dst_mac "in_dst_mac"
      ; Option.some_if output.in_payload "in_payload"
      ]
  ;;

  let compact ({ phase; byte; output } : Observation.t) : Compact_observation.t =
    { phase; byte; active_outputs = active_outputs output }
  ;;

  let cycle
    (handler : Step.Handler.t @ local)
    ~reset
    ~en
    ~rx_dv
    ~rx_er
    ~rx_data
    ~rx_data_valid
    =
    Step.cycle handler (inputs ~reset ~en ~rx_dv ~rx_er ~rx_data ~rx_data_valid)
    |> Step.O_data.after_edge
  ;;

  (* they call me baby driver the way I drive things *)
  let drive_byte ?(rx_er = false) (handler : Step.Handler.t @ local) byte =
    cycle
      handler
      ~reset:false
      ~en:true
      ~rx_dv:true
      ~rx_er
      ~rx_data:byte
      ~rx_data_valid:true
  ;;

  (* valid not true here; *)
  let drive_invalid_cycle (handler : Step.Handler.t @ local) =
    cycle
      handler
      ~reset:false
      ~en:true
      ~rx_dv:true
      ~rx_er:false
      ~rx_data:0xEE
      ~rx_data_valid:false
  ;;

  (* i wonder if reset agents can be composed into an abstraction much like how reset
     agents are done in proper UVM suites?
  *)
  let reset ?(num_cycles = 1) (handler : Step.Handler.t @ local) =
    Step.delay
      ~num_cycles
      handler
      (inputs
         ~reset:true
         ~en:false
         ~rx_dv:false
         ~rx_er:false
         ~rx_data:0
         ~rx_data_valid:false)
  ;;

  (* absolute fun function to write rec functions are like little puzzles - you almost
     have to break out an ASM diagram to write one
  *)
  let observe_bytes (handler : Step.Handler.t @ local) ~phase bytes =
    let rec loop (handler : Step.Handler.t @ local) index = function
      | [] -> []
      | byte :: remaining_bytes ->
        let output = drive_byte handler byte |> snapshot in
        { Observation.phase = phase index; byte; output }
        :: loop handler (index + 1) remaining_bytes
    in
    loop handler 0 bytes
  ;;

  let create_simulator = Fixture.create_simulator
  let run_with_timeout = Fixture.run_with_timeout

  [@@@ocamlformat "disable"]

  (* formatt-er? I hardly know 'er *)
  let frame_scenario frame (handler : Step.Handler.t @ local) _initial_outputs =

    (* reset *)
    reset handler;

    (* preamble *)
    let preamble =
      observe_bytes
        handler
        ~phase:(fun index -> Phase.Preamble index)
        (List.init frame.Frame.preamble_length ~f:(fun _ -> 0x55))
    in

    (* sfd *)
    let sfd_output = drive_byte handler 0xD5 |> snapshot in
    let sfd = { Observation.phase = Sfd; byte = 0xD5; output = sfd_output } in

    (* dst mac *)
    let destination_mac =
      observe_bytes
        handler
        ~phase:(fun index -> Phase.Destination_mac index)
        frame.destination_mac
    in

    (* src mac *)
    let source_mac =
      observe_bytes handler ~phase:(fun index -> Phase.Source_mac index) frame.source_mac
    in

    (* eth type *)
    let eth_type =
      observe_bytes handler ~phase:(fun index -> Phase.Eth_type index) frame.eth_type
    in

    (* da big momma *)
    let payload =
      observe_bytes handler ~phase:(fun index -> Phase.Payload index) frame.payload
    in

    preamble @ (sfd :: destination_mac) @ source_mac @ eth_type @ payload

  [@@@ocamlformat "enable"]

  let run_frame frame =
    run_with_timeout
      ~timeout:(4 + Frame.byte_count frame)
      ~testbench:(frame_scenario frame)
  ;;

  [@@@ocamlformat "disable"]

  (* pause during destination setting *)
  let run_destination_pause () =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =

      (* reset to pause *)
      reset handler;

      (* dont care aobut sfd or preamble items in this seq *)
      ignore (drive_byte handler 0x55 : Bits.t Dut.O.t);
      ignore (drive_byte handler 0xD5 : Bits.t Dut.O.t);

      (* never knew there was an id function *)
      let destination_prefix = List.init 5 ~f:Fn.id in

      (* grab the snapshot; way more convoluted than one might think *)
      let before_pause =
        (* drive the first 5 bytes, then follow *)
        let rec drive_prefix (handler : Step.Handler.t @ local) = function (* match with sugar *)
          | [] -> failwith "destination prefix cannot be empty"
          | [ byte ] -> drive_byte handler byte
          | byte :: remaining_bytes ->
            ignore (drive_byte handler byte : Bits.t Dut.O.t);
            drive_prefix handler remaining_bytes
        in

        drive_prefix handler destination_prefix |> snapshot
      in

      (* cap until we hit pause *)
      let during_pause = drive_invalid_cycle handler |> snapshot in

      (* run the 6th byte, and check *)
      let after_sixth_destination_byte = drive_byte handler 5 |> snapshot in

      (* going to src byte, make sure we're not off by one *)
      let after_first_source_byte = drive_byte handler 0xA0 |> snapshot in

      { Pause_observation.
        before_pause
      ; during_pause
      ; after_sixth_destination_byte
      ; after_first_source_byte
      }
    in

    run_with_timeout ~timeout:14 ~testbench

  (* mid frame reset case - pretty important *)
  let run_reset_mid_frame () =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      (* rst *)
      reset handler;

      (* preamble + sfd *)
      ignore (drive_byte handler 0x55 : Bits.t Dut.O.t);
      ignore (drive_byte handler 0xD5 : Bits.t Dut.O.t);

      (* the partial header the reset is about to discard *)
      let before_reset = drive_byte handler 0x11 |> snapshot in

      (* manually drive the reset *)
      let reset_cycle =
        cycle
          handler
          ~reset:true
          ~en:false
          ~rx_dv:false
          ~rx_er:false
          ~rx_data:0
          ~rx_data_valid:false
      in

      (* capture snapshots; *)
      let after_reset = snapshot reset_cycle in
      let after_next_preamble = drive_byte handler 0x55 |> snapshot in

      { Reset_observation.
        before_reset
      ; after_reset
      ; after_next_preamble
      }
    in

    run_with_timeout ~timeout:10 ~testbench

  (* error in payload *)
  let run_payload_error () =
    let frame = Frame.create () in

    let testbench (handler : Step.Handler.t @ local) initial_outputs =
      (* manualy run whole frame scenario *)
      let observations = frame_scenario frame handler initial_outputs in

      (* log before and afte rerror events *)
      let before_error = (List.last_exn observations).output in
      let after_error = drive_byte ~rx_er:true handler 0xDE |> snapshot in

      (* manually idle after *)
      let following_idle_cycle =
        cycle
          handler
          ~reset:false
          ~en:true
          ~rx_dv:false
          ~rx_er:false
          ~rx_data:0
          ~rx_data_valid:false
        |> snapshot
      in

      { Error_observation.
        before_error
      ; after_error
      ; following_idle_cycle
      }
    in

    run_with_timeout ~timeout:(8 + Frame.byte_count frame) ~testbench

  [@@@ocamlformat "enable"]

  (* kinda usless but chiller ig *)
  let run_enable_case ~en ~rx_dv =
    let testbench (handler : Step.Handler.t @ local) _initial_outputs =
      reset handler;
      cycle handler ~reset:false ~en ~rx_dv ~rx_er:false ~rx_data:0 ~rx_data_valid:false
      |> snapshot
    in
    run_with_timeout ~timeout:5 ~testbench
  ;;
end
