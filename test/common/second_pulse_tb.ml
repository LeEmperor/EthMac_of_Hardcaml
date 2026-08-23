open! Core
open! Hardcaml
open! Hardcaml_waveterm

let () = print_endline "=== Running Second_pulse Testbench ==="

(* Use a tiny clk_freq so the counter wraps in a handful of cycles. At 100 MHz the real
   period is 100_000_000 cycles — far too long to simulate. *)
let clk_freq_sim = 10

module Sim = Cyclesim.With_interface (Second_pulse.I) (Second_pulse.O)

let create_sim () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim =
    Sim.create
      ~config:Cyclesim.Config.trace_all
      (Second_pulse.create ~clk_freq:clk_freq_sim scope)
  in
  let waves, sim = Cyclesim.Waveform.create sim in
  let inputs : _ Second_pulse.I.t = Cyclesim.inputs sim in
  let outputs : _ Second_pulse.O.t = Cyclesim.outputs sim in
  sim, waves, inputs, outputs
;;

let () =
  let open Bits in
  let sim, waves, inputs, outputs = create_sim () in
  Out_channel.with_file "waves_second_pulse.vcd" ~f:(fun oc ->
    let sim = Vcd.wrap oc sim in
    let t_rst = inputs.rst in
    let o_pulse = outputs.pulse in
    let cycle () = Cyclesim.cycle sim in
    (* reset for 2 cycles *)
    t_rst <--. 1;
    cycle ();
    cycle ();
    t_rst <--. 0;
    (* run for 3 full periods + a few extra to see the pattern clearly *)
    let n_cycles = (3 * clk_freq_sim) + 5 in
    printf
      "\n-- %d cycles, clk_freq=%d → pulse every %d cycles --\n"
      n_cycles
      clk_freq_sim
      clk_freq_sim;
    for cyc = 1 to n_cycles do
      let p = to_int_trunc !o_pulse in
      if p = 1
      then printf "cycle %3d: pulse=1  *** PULSE ***\n" cyc
      else printf "cycle %3d: pulse=0\n" cyc;
      cycle ()
    done;
    print_endline "\n=== SIMULATION COMPLETE ===";
    Waveform.print ~display_width:96 waves)
;;
