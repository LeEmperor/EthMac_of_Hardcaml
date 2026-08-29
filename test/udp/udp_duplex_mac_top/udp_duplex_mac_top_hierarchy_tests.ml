open! Core
open! Hardcaml
open! Udp_of_hardcaml

module Duplex_circuit =
  Circuit.With_interface (Udp_duplex_mac_top.I) (Udp_duplex_mac_top.O)

type design =
  { top : Circuit.t
  ; database : Circuit_database.t
  }

let build ~flatten_design =
  let scope = Scope.create ~flatten_design () in
  let top =
    Duplex_circuit.create_exn
      ~name:"udp_duplex_hierarchy_test"
      (Udp_duplex_mac_top.create scope)
  in
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

let expected_children =
  [ "ipv4_rx"
  ; "ipv4_tx"
  ; "rx_byte_assembler"
  ; "rx_controller"
  ; "rx_crc"
  ; "rx_datapath"
  ; "tx_byte_disassembler"
  ; "tx_controller"
  ; "tx_crc"
  ; "tx_datapath"
  ; "tx_payload_fifo"
  ; "udp_rx"
  ; "udp_tx"
  ]
;;

let%test_unit "the duplex stack records and emits the IPv4 and UDP implementations" =
  let design = build ~flatten_design:false in
  [%test_result: string list]
    (sorted_circuit_names design.database)
    ~expect:expected_children;
  let emitted =
    Rtl.create ~database:design.database Verilog [ design.top ]
    |> Rtl.Hierarchical_circuits.subcircuits
    |> List.map ~f:Rtl.Circuit_instance.module_name
    |> List.sort ~compare:String.compare
  in
  [%test_result: string list] emitted ~expect:expected_children;
  let unresolved =
    Hierarchy.fold design.top design.database ~init:[] ~f:(fun unresolved circuit inst ->
      match circuit, inst with
      | None, Some inst -> inst.circuit_name :: unresolved
      | _ -> unresolved)
  in
  [%test_result: string list] unresolved ~expect:[];
  let rtl =
    Rtl.create ~database:design.database Verilog [ design.top ]
    |> Rtl.full_hierarchy
    |> Rope.to_string
  in
  assert (String.is_substring rtl ~substring:"module ipv4_rx");
  assert (String.is_substring rtl ~substring:"module ipv4_tx");
  assert (String.is_substring rtl ~substring:"module udp_rx");
  assert (String.is_substring rtl ~substring:"module udp_tx")
;;

let%test_unit "flat and hierarchical duplex stacks expose identical ports" =
  let flat = build ~flatten_design:true in
  let hierarchical = build ~flatten_design:false in
  [%test_result: (string * int) list]
    (sorted_ports Circuit.inputs hierarchical.top)
    ~expect:(sorted_ports Circuit.inputs flat.top);
  [%test_result: (string * int) list]
    (sorted_ports Circuit.outputs hierarchical.top)
    ~expect:(sorted_ports Circuit.outputs flat.top);
  [%test_result: int]
    (List.length (Circuit_database.get_circuits flat.database))
    ~expect:0;
  [%test_result: int] (List.length (Circuit.instantiations flat.top)) ~expect:0
;;
