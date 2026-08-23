(* Very simple fabric-based clk-divider.

   Note that past like 150MHz this cannot be expected to function correctly due to clk
   skew, and that proper items from hardcaml_xilinx ought to be utilized.
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

let create _scope (i : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock:i.src_clk ~clear:i.rst () in
  let cnt = reg_fb spec ~enable:i.en ~width:2 ~f:(fun x -> x +:. 1) -- "cnt" in
  { O.dst_clk = msb cnt }
;;
