open! Core
open! Hardcaml
open! Mii_of_hardcaml
module Mac_circuit = Circuit.With_interface (Mac_top.I) (Mac_top.O)
module Rx_path_circuit = Circuit.With_interface (Mac_rx_path.I) (Mac_rx_path.O)
module Tx_path_circuit = Circuit.With_interface (Mac_tx_path.I) (Mac_tx_path.O)

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

let build_rx_path ~flatten_design =
  let scope = Scope.create ~flatten_design () in
  let top =
    Rx_path_circuit.create_exn
      ~name:"mac_rx_path_hierarchy_test"
      (Mac_rx_path.create scope)
  in
  { top; database = Scope.circuit_database scope }
;;

let build_tx_path ~flatten_design =
  let scope = Scope.create ~flatten_design () in
  let top =
    Tx_path_circuit.create_exn
      ~name:"mac_tx_path_hierarchy_test"
      (Mac_tx_path.create scope)
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

let sorted_instantiations circuit =
  Circuit.instantiations circuit
  |> List.map ~f:(fun instantiation -> instantiation.instantiation.circuit_name)
  |> List.sort ~compare:String.compare
;;

let find_exn database name =
  Circuit_database.find database ~mangled_name:name
  |> Option.value_exn ~message:("missing circuit implementation: " ^ name)
;;

let%test_unit "the MAC RX and TX paths form a complete emitted hierarchy" =
  let design = build ~flatten_design:false in
  let expected =
    [ "hardcaml_async_fifo"
    ; "mac_rx_path"
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
  in
  [%test_result: string list] (sorted_circuit_names design.database) ~expect:expected;
  let emitted =
    Rtl.create ~database:design.database Verilog [ design.top ]
    |> Rtl.Hierarchical_circuits.subcircuits
    |> List.map ~f:Rtl.Circuit_instance.module_name
    |> List.sort ~compare:String.compare
  in
  [%test_result: string list] emitted ~expect:expected;
  [%test_result: string list]
    (sorted_instantiations design.top)
    ~expect:[ "hardcaml_async_fifo"; "mac_rx_path"; "mac_tx_path" ];
  [%test_result: string list]
    (sorted_instantiations (find_exn design.database "mac_rx_path"))
    ~expect:[ "rx_controller"; "rx_crc"; "rx_datapath" ];
  [%test_result: string list]
    (sorted_instantiations (find_exn design.database "rx_datapath"))
    ~expect:[ "rx_byte_assembler" ];
  [%test_result: string list]
    (sorted_instantiations (find_exn design.database "mac_tx_path"))
    ~expect:
      [ "tx_byte_disassembler"
      ; "tx_controller"
      ; "tx_crc"
      ; "tx_datapath"
      ; "tx_payload_fifo"
      ];
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

let%test_unit "RX path is a single-clock standalone hierarchy" =
  let flat = build_rx_path ~flatten_design:true in
  let hierarchical = build_rx_path ~flatten_design:false in
  [%test_result: string list]
    (sorted_circuit_names hierarchical.database)
    ~expect:[ "rx_byte_assembler"; "rx_controller"; "rx_crc"; "rx_datapath" ];
  [%test_result: (string * int) list]
    (sorted_ports Circuit.inputs hierarchical.top)
    ~expect:(sorted_ports Circuit.inputs flat.top);
  [%test_result: (string * int) list]
    (sorted_ports Circuit.outputs hierarchical.top)
    ~expect:(sorted_ports Circuit.outputs flat.top);
  [%test_result: string list]
    (Circuit.inputs hierarchical.top
     |> List.filter_map ~f:(fun signal ->
       let name = List.hd_exn (Signal.names signal) in
       Option.some_if (String.is_substring name ~substring:"clock") name))
    ~expect:[ "clock_i" ]
;;

let%test_unit "TX path is a single-clock standalone hierarchy" =
  let flat = build_tx_path ~flatten_design:true in
  let hierarchical = build_tx_path ~flatten_design:false in
  [%test_result: string list]
    (sorted_circuit_names hierarchical.database)
    ~expect:
      [ "tx_byte_disassembler"
      ; "tx_controller"
      ; "tx_crc"
      ; "tx_datapath"
      ; "tx_payload_fifo"
      ];
  (* [en_i] and [s_axis_tuser_i] are part of the stable TX-side contract but currently
     have no functional fanout inside this extracted implementation, so a standalone flat
     circuit legitimately prunes them. The enclosing MAC uses both at its own boundary. *)
  [%test_result: (string * int) list]
    (sorted_ports Circuit.inputs hierarchical.top)
    ~expect:
      [ "clock_i", 1
      ; "en_i", 1
      ; "reset_i", 1
      ; "s_axis_tdata_i", 8
      ; "s_axis_tlast_i", 1
      ; "s_axis_tuser_i", 1
      ; "s_axis_tvalid_i", 1
      ; "tx_start_i", 1
      ];
  [%test_result: (string * int) list]
    (sorted_ports Circuit.outputs hierarchical.top)
    ~expect:(sorted_ports Circuit.outputs flat.top);
  [%test_result: string list]
    (Circuit.inputs hierarchical.top
     |> List.filter_map ~f:(fun signal ->
       let name = List.hd_exn (Signal.names signal) in
       Option.some_if (String.is_substring name ~substring:"clock") name))
    ~expect:[ "clock_i" ]
;;
