open! Core
open! Hardcaml
open! Hardcaml_waveterm

let () = print_endline "=== Running Clk_div Testbench ==="

module Sim = Cyclesim.With_interface (Clk_div.I) (Clk_div.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create ~config:Cyclesim.Config.trace_all (Clk_div.create scope) in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs : _ Clk_div.I.t = Cyclesim.inputs sim in
  let outputs : _ Clk_div.O.t = Cyclesim.outputs sim in
  sim, waves, inputs, outputs
;;

let () =
  let open Bits in
  let sim, waves, inputs, outputs = create_sim () in
  Out_channel.with_file "waves_clk_div.vcd" ~f:(fun oc ->
    let sim = Vcd.wrap oc sim in
    let t_rst = inputs.rst in
    let t_en = inputs.en in
    let o_dst_clk = outputs.dst_clk in
    let cycle () = Cyclesim.cycle sim in
    (* reset *)
    t_rst <--. 1;
    t_en <--. 0;
    cycle ();
    cycle ();
    t_rst <--. 0;
    (* 20 enabled cycles = 5 full div-4 periods *)
    printf "\n-- en=1 --\n";
    t_en <--. 1;
    for cyc = 1 to 20 do
      printf "cycle %2d: dst_clk=%d\n" cyc (to_int_trunc !o_dst_clk);
      cycle ()
    done;
    (* freeze: counter should hold *)
    printf "\n-- en=0, counter frozen --\n";
    t_en <--. 0;
    for cyc = 21 to 24 do
      printf "cycle %2d: dst_clk=%d\n" cyc (to_int_trunc !o_dst_clk);
      cycle ()
    done;
    (* resume: picks up where it left off *)
    printf "\n-- en=1, counter resumes --\n";
    t_en <--. 1;
    for cyc = 25 to 32 do
      printf "cycle %2d: dst_clk=%d\n" cyc (to_int_trunc !o_dst_clk);
      cycle ()
    done;
    print_endline "\n=== SIMULATION COMPLETE ===";
    Waveform.print ~display_width:96 waves)
;;
