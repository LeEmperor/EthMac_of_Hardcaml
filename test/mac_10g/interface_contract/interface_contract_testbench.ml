open! Core
open! Hardcaml
open! Mac_10g_of_hardcaml
open! Xgmii_of_hardcaml
open! Xgmii
module Dut = Mac_10g_top
module Dut_circuit = Circuit.With_interface (Dut.I) (Dut.O)

type parameters =
  { tx_buffer_depth_bytes : int
  ; rx_buffer_depth_bytes : int
  ; descriptor_capacity : int
  ; max_supported_frame_length : int
  }
[@@deriving sexp_of]

let default_parameters =
  { tx_buffer_depth_bytes = Dut.Config.default_tx_buffer_depth_bytes
  ; rx_buffer_depth_bytes = Dut.Config.default_rx_buffer_depth_bytes
  ; descriptor_capacity = Dut.Config.default_descriptor_capacity
  ; max_supported_frame_length = Dut.Config.default_max_supported_frame_length
  }
;;

let ports interface_to_list port_names_and_widths =
  interface_to_list port_names_and_widths |> List.sort ~compare:[%compare: string * int]
;;

let input_ports = ports Dut.I.to_list Dut.I.port_names_and_widths
let output_ports = ports Dut.O.to_list Dut.O.port_names_and_widths
let xgmii_word_fields = ports Word.to_list Word.port_names_and_widths

let axis_beat_fields =
  ports Mac_10g_axis.Beat.to_list Mac_10g_axis.Beat.port_names_and_widths
;;

let elaborate parameters =
  Or_error.try_with (fun () ->
    let scope = Scope.create ~flatten_design:true () in
    Dut_circuit.create_exn
      ~name:"mac_10g_interface_contract"
      (Dut.create
         ~tx_buffer_depth_bytes:parameters.tx_buffer_depth_bytes
         ~rx_buffer_depth_bytes:parameters.rx_buffer_depth_bytes
         ~descriptor_capacity:parameters.descriptor_capacity
         ~max_supported_frame_length:parameters.max_supported_frame_length
         scope)
    |> ignore)
;;

let lane_bytes (word : Signal.t Word.t) =
  List.init 8 ~f:(fun lane -> lane_byte word lane |> Signal.to_bits |> Bits.to_int_trunc)
;;

let control_mask (word : Signal.t Word.t) =
  Signal.to_bits word.control |> Bits.to_int_trunc
;;

let word_summary word = lane_bytes word, control_mask word

type scaffold_summary =
  { tx_ready : int
  ; tx_xgmii : int list * int
  ; rx_valid : int
  ; axi_bvalid : int
  ; axi_rvalid : int
  ; irq : int
  }
[@@deriving sexp_of, compare, equal]

let scaffold_summary () =
  let scope = Scope.create ~flatten_design:true () in
  let inputs =
    Dut.I.map Dut.I.port_names_and_widths ~f:(fun (name, width) ->
      Signal.input name width)
  in
  let outputs = Dut.create scope inputs in
  let int signal = Signal.to_bits signal |> Bits.to_int_trunc in
  { tx_ready = int outputs.s_axis_tx_tready_o
  ; tx_xgmii =
      word_summary { Word.data = outputs.xgmii_txd_o; control = outputs.xgmii_txc_o }
  ; rx_valid = int outputs.m_axis_rx_tvalid_o
  ; axi_bvalid = int outputs.s_axi_bvalid_o
  ; axi_rvalid = int outputs.s_axi_rvalid_o
  ; irq = int outputs.irq_o
  }
;;

let exception_string parameters =
  match elaborate parameters with
  | Ok () -> "ok"
  | Error error -> Error.to_string_hum error
;;
