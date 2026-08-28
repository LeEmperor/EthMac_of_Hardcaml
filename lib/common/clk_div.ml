(* Very simple fabric-based clk-divider.

   Note that past like 150MHz this cannot be expected to function correctly due to clk
   skew, and that proper items from hardcaml_xilinx ought to be utilized. Parameterizing
   the ratio does not change that: this is a counter MSB, not a clock resource, and a
   [divisor] that makes it look like a general clocking primitive is exactly the use the
   paragraph above warns off.

   [divisor] is the division ratio and defaults to four, the value the ratio was hardcoded
   at before it became an argument. The output is the MSB of a [ceil_log2 divisor]-bit
   free-running counter, so the ratio has to be a power of two and at least two; anything
   else is rejected at elaboration time rather than silently rounded. Two is the floor
   because a divisor of one would ask for a zero-width register, which Hardcaml rejects
   with a message that does not mention the divisor - the same trap [second_pulse] falls
   into at [clk_freq = 1]. Duty cycle is exactly half for every legal ratio.
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { src_clk : 'a
    ; rst : 'a
    ; en : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { dst_clk : 'a } [@@deriving hardcaml]
end

(* Elaboration-time check, so a bad ratio fails where it is written rather than as a width
   error from inside Hardcaml. *)
let counter_width divisor =
  if divisor < 2 || not (Int.is_pow2 divisor)
  then
    raise_s
      [%message
        "Clk_div.create: divisor must be a power of two and at least 2" (divisor : int)];
  Int.ceil_log2 divisor
;;

let create ?(divisor = 4) _scope (i : _ I.t) : _ O.t =
  let width = counter_width divisor in
  let spec = Reg_spec.create ~clock:i.src_clk ~clear:i.rst () in
  let cnt = reg_fb spec ~enable:i.en ~width ~f:(fun x -> x +:. 1) -- "cnt" in
  { O.dst_clk = msb cnt }
;;
