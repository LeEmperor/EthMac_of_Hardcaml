open! Core
open! Hardcaml
open! Mii_of_hardcaml
module Mac_circuit = Circuit.With_interface (Mac_top.I) (Mac_top.O)

type design =
  { top : Circuit.t
  ; database : Circuit_database.t
  }

let build ~flatten_design =
  let scope = Scope.create ~flatten_design () in
  let top =
    Mac_circuit.create_exn ~name:"mac_top_hierarchy_test" (Mac_top.create scope)
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

let%test_unit "the MAC RX and TX leaves form a complete emitted hierarchy" =
  let design = build ~flatten_design:false in
  let expected =
    [ "rx_byte_assembler"
    ; "rx_controller"
    ; "rx_crc"
    ; "rx_datapath"
    ; "tx_byte_disassembler"
    ; "tx_controller"
    ; "tx_crc"
    ; "tx_datapath"
    ; "tx_payload_fifo"
    ]
  in
  [%test_result: string list] (sorted_circuit_names design.database) ~expect:expected;
  let emitted =
    Rtl.create ~database:design.database Verilog [ design.top ]
    |> Rtl.Hierarchical_circuits.subcircuits
    |> List.map ~f:Rtl.Circuit_instance.module_name
    |> List.sort ~compare:String.compare
  in
  [%test_result: string list] emitted ~expect:expected;
  let unresolved =
    Hierarchy.fold design.top design.database ~init:[] ~f:(fun unresolved circuit inst ->
      match circuit, inst with
      | None, Some inst -> inst.circuit_name :: unresolved
      | _ -> unresolved)
  in
  [%test_result: string list] unresolved ~expect:[]
;;

let%test_unit "flat and hierarchical MACs expose identical ports" =
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
