open! Core
open! Hardcaml
open! Uart_of_hardcaml
open! Hardcaml_waveterm

let () = print_endline "=== Running UART TX Top Testbench ==="

module Waveform = Hardcaml_waveterm.Waveform
module Sim = Cyclesim.With_interface (Uart_tx.I) (Uart_tx.O)

let create_sim () =
  let top_tb_scope : Scope.t =
    Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
  in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Uart_tx.create top_tb_scope) in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs : _ Uart_tx.I.t = Cyclesim.inputs sim in
  let outputs : _ Uart_tx.O.t = Cyclesim.outputs sim in
  sim, waves, inputs, outputs
;;

let () =
  let open Bits in
  let sim, waves, inputs, outputs = create_sim () in
  Out_channel.with_file "waves_uart_top.vcd" ~f:(fun oc ->
    let sim = Vcd.wrap oc sim in
    (* signal aliases *)
    let t_clk = inputs.clk in
    let t_rst = inputs.rst in
    let t_en = inputs.en in
    let t_in = inputs.d_in in
    let t_valid = inputs.d_in_valid in
    let t_tick = inputs.tick in
    let cycle () = Cyclesim.cycle sim in
    let ( <-- ) r i = r := Bits.of_int_trunc ~width:(Bits.width !r) i in
    let idle () =
      t_en <-- 0;
      cycle ()
    in
    let reset () =
      t_en <-- 0;
      t_tick <-- 0;
      t_rst <-- 1;
      cycle ();
      t_en <-- 1;
      t_rst <-- 0
    in
    let send_data data =
      t_in <-- data;
      t_valid <-- 1;
      for i = 0 to 9 do
        cycle ();
        t_tick <-- 1;
        cycle ();
        t_tick <-- 0;
        for i = 0 to 5 do
          cycle ()
        done
      done;
      t_valid <-- 0;
      t_in <-- 0
    in
    reset ();
    send_data 0x55;
    for i = 0 to 10 do
      cycle ()
    done;
    idle ();
    print_endline "\n=== SIMULATION COMPLETE ===")
;;
