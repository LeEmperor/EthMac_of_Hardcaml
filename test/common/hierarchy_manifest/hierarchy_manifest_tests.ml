open! Core
open! Hardcaml
open! Signal

(* A deliberately small child boundary used to test the hierarchy plumbing independently
   of the networking datapaths. Production blocks can adopt the same [create] plus
   [hierarchical] API without making the RTL emitter a moving target at the same time. *)
module Child = struct
  module I = struct
    type 'a t =
      { a : 'a [@bits 8]
      ; b : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { sum : 'a [@bits 9] } [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : Signal.t I.t) =
    { O.sum = uresize i.a ~width:9 +: uresize i.b ~width:9 }
  ;;

  let hierarchical ?instance scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ?instance ~scope ~name:"hierarchy_manifest_child" create i
  ;;
end

module Parent = struct
  module I = Child.I
  module O = Child.O

  let create scope i = Child.hierarchical ~instance:"child" scope i
end

module Circuit_with_interface = Circuit.With_interface (Parent.I) (Parent.O)

type design =
  { top : Circuit.t
  ; database : Circuit_database.t
  }

let build ~flatten_design =
  let scope = Scope.create ~flatten_design () in
  let top =
    Circuit_with_interface.create_exn ~name:"hierarchy_manifest_top" (Parent.create scope)
  in
  { top; database = Scope.circuit_database scope }
;;

let emitted_module_names design =
  Rtl.create ~database:design.database Verilog [ design.top ]
  |> Rtl.Hierarchical_circuits.subcircuits
  |> List.map ~f:Rtl.Circuit_instance.module_name
;;

let assert_all_instantiations_resolve design =
  List.iter (Circuit.instantiations design.top) ~f:(fun inst ->
    let name = inst.instantiation.circuit_name in
    match Circuit_database.find design.database ~mangled_name:name with
    | Some _ -> ()
    | None -> raise_s [%message "hierarchical implementation is missing" (name : string)])
;;

let%test_unit "hierarchical scope records and emits the child implementation" =
  let design = build ~flatten_design:false in
  let database_names =
    Circuit_database.get_circuits design.database |> List.map ~f:Circuit.name
  in
  [%test_result: string list]
    (List.sort database_names ~compare:String.compare)
    ~expect:[ "hierarchy_manifest_child" ];
  [%test_result: string list]
    (emitted_module_names design)
    ~expect:[ "hierarchy_manifest_child" ];
  assert_all_instantiations_resolve design;
  let rtl =
    Rtl.create ~database:design.database Verilog [ design.top ]
    |> Rtl.full_hierarchy
    |> Rope.to_string
  in
  assert (String.is_substring rtl ~substring:"module hierarchy_manifest_child");
  assert (String.is_substring rtl ~substring:"module hierarchy_manifest_top")
;;

let%test_unit "flat scope inlines the child and leaves no external implementation" =
  let design = build ~flatten_design:true in
  [%test_result: int]
    (List.length (Circuit_database.get_circuits design.database))
    ~expect:0;
  [%test_result: int] (List.length (Circuit.instantiations design.top)) ~expect:0;
  [%test_result: string list] (emitted_module_names design) ~expect:[];
  let rtl =
    Rtl.create ~database:design.database Verilog [ design.top ]
    |> Rtl.full_hierarchy
    |> Rope.to_string
  in
  assert (not (String.is_substring rtl ~substring:"module hierarchy_manifest_child"));
  assert (String.is_substring rtl ~substring:"module hierarchy_manifest_top")
;;
