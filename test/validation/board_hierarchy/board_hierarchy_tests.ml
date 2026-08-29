(* Stage 5 board-hierarchy regression: every validation harness must instantiate its
   integrated network top and the AXI-Lite register block as recorded child circuits, emit
   an implementation for each of them, and keep the Arty pin contract that the XDC binds
   to. Board clock/reset/CDC glue (reset synchronizers, [Clk_div], [Second_pulse], the
   pulse synchronizers) stays inline on purpose, so it must NOT appear here.

   [Mac_top_validation_harness_regs] is a deliberate exception. Every harness ties its
   AXI-Lite inputs off and observes none of its outputs, so the stub is recorded in the
   circuit database but is unreachable from the board outputs and is therefore pruned out
   of the top exactly as its inlined logic used to be. That is asserted below rather than
   glossed over: once the register block drives something real, these tests fail and the
   expectations move from [expected_recorded] to [expected_children]. *)

open! Core
open! Hardcaml
module Board_circuit = Circuit.With_interface (Arty_board_top.I) (Arty_board_top.O)

type design =
  { top : Circuit.t
  ; database : Circuit_database.t
  }

let build ~flatten_design ~name create =
  let scope = Scope.create ~flatten_design () in
  let top = Board_circuit.create_exn ~name (create scope) in
  { top; database = Scope.circuit_database scope }
;;

let sorted_circuit_names database =
  Circuit_database.get_circuits database
  |> List.map ~f:Circuit.name
  |> List.sort ~compare:String.compare
;;

let sorted_ports get_ports circuit =
  get_ports circuit
  |> List.map ~f:(fun signal -> List.hd_exn (Signal.names signal), Signal.width signal)
  |> List.sort ~compare:[%compare: string * int]
;;

let sorted_instantiations circuit =
  Circuit.instantiations circuit
  |> List.map ~f:(fun instantiation -> instantiation.instantiation.circuit_name)
  |> List.sort ~compare:String.compare
;;

let board_ports ports = List.sort ports ~compare:[%compare: string * int]

let board_input_ports =
  board_ports (Arty_board_top.I.to_list Arty_board_top.I.port_names_and_widths)
;;

let board_output_ports =
  board_ports (Arty_board_top.O.to_list Arty_board_top.O.port_names_and_widths)
;;

(* The MAC subtree every harness inherits through [Mac_top]. *)
let mac_children =
  [ "hardcaml_async_fifo"
  ; "mac_rx_path"
  ; "mac_top"
  ; "mac_tx_path"
  ; "rx_byte_assembler"
  ; "rx_controller"
  ; "rx_crc"
  ; "rx_datapath"
  ; "tx_byte_disassembler"
  ; "tx_controller"
  ; "tx_crc"
  ; "tx_datapath"
  ; "tx_payload_fifo"
  ]
;;

let tx_stack_children = [ "ipv4_tx"; "udp_ipv4_tx"; "udp_tx" ]
let rx_stack_children = [ "ipv4_rx"; "udp_ipv4_rx"; "udp_rx" ]
let regs = "mac_top_validation_harness_regs"
let sorted names = List.sort names ~compare:String.compare

let check_harness ~name ~create ~expected_instantiations ~expected_children =
  let expected_children = sorted expected_children in
  (* the register-block stub is recorded but not reachable; see the header comment *)
  let expected_recorded = sorted (regs :: expected_children) in
  let hierarchical = build ~flatten_design:false ~name create in
  let flat = build ~flatten_design:true ~name create in
  (* the board top instantiates exactly its network top and its register block *)
  [%test_result: string list]
    ~message:name
    (sorted_instantiations hierarchical.top)
    ~expect:(sorted expected_instantiations);
  (* every child in the tree is recorded and emitted *)
  [%test_result: string list]
    ~message:name
    (sorted_circuit_names hierarchical.database)
    ~expect:expected_recorded;
  let emitted =
    Rtl.create ~database:hierarchical.database Verilog [ hierarchical.top ]
    |> Rtl.Hierarchical_circuits.subcircuits
    |> List.map ~f:Rtl.Circuit_instance.module_name
    |> List.sort ~compare:String.compare
  in
  [%test_result: string list] ~message:name emitted ~expect:expected_children;
  let unresolved =
    Hierarchy.fold
      hierarchical.top
      hierarchical.database
      ~init:[]
      ~f:(fun unresolved circuit inst ->
        match circuit, inst with
        | None, Some inst -> inst.circuit_name :: unresolved
        | _ -> unresolved)
  in
  [%test_result: string list] ~message:name unresolved ~expect:[];
  (* the board pin contract the XDC binds to is unchanged, and identical flat *)
  [%test_result: (string * int) list]
    ~message:name
    (sorted_ports Circuit.outputs hierarchical.top)
    ~expect:board_output_ports;
  [%test_result: (string * int) list]
    ~message:name
    (sorted_ports Circuit.inputs hierarchical.top)
    ~expect:(sorted_ports Circuit.inputs flat.top);
  [%test_result: (string * int) list]
    ~message:name
    (sorted_ports Circuit.outputs hierarchical.top)
    ~expect:(sorted_ports Circuit.outputs flat.top);
  List.iter (sorted_ports Circuit.inputs hierarchical.top) ~f:(fun port ->
    assert (List.mem board_input_ports port ~equal:[%compare.equal: string * int]));
  (* the flat scope still elaborates one self-contained circuit for Cyclesim *)
  [%test_result: int]
    ~message:name
    (List.length (Circuit_database.get_circuits flat.database))
    ~expect:0;
  [%test_result: int]
    ~message:name
    (List.length (Circuit.instantiations flat.top))
    ~expect:0
;;

let%test_unit "the bare-MAC board harness has a resolved hierarchy" =
  check_harness
    ~name:"mac_top_validation_harness"
    ~create:Mac_top_validation_harness.create
    ~expected_instantiations:[ "mac_top" ]
    ~expected_children:mac_children
;;

let%test_unit "the UDP TX board harness has a resolved hierarchy" =
  check_harness
    ~name:"udp_mac_top_validation_harness"
    ~create:Udp_mac_top_validation_harness.create
    ~expected_instantiations:[ "udp_mac_top" ]
    ~expected_children:(("udp_mac_top" :: mac_children) @ tx_stack_children)
;;

let%test_unit "the UDP RX board harness has a resolved hierarchy" =
  check_harness
    ~name:"udp_rx_mac_top_validation_harness"
    ~create:Udp_rx_mac_top_validation_harness.create
    ~expected_instantiations:[ "udp_rx_mac_top" ]
    ~expected_children:(("udp_rx_mac_top" :: mac_children) @ rx_stack_children)
;;

let%test_unit "the full-duplex board harness has a resolved hierarchy" =
  check_harness
    ~name:"udp_duplex_validation_harness"
    ~create:Udp_duplex_validation_harness.create
    ~expected_instantiations:[ "udp_duplex_mac_top" ]
    ~expected_children:
      (("udp_duplex_mac_top" :: mac_children) @ tx_stack_children @ rx_stack_children)
;;

let%test_unit "the loopback board harness has a resolved hierarchy" =
  check_harness
    ~name:"udp_loopback_validation_harness"
    ~create:Udp_loopback_validation_harness.create
    ~expected_instantiations:[ "udp_loopback_mac_top" ]
    ~expected_children:
      (("udp_loopback_mac_top" :: mac_children) @ tx_stack_children @ rx_stack_children)
;;
