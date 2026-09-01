(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "shared_primitives_testbench.ml" *)
(* Testbench support for the combinational 10G masked CRC and AXI keep helpers. *)

open! Core
open! Hardcaml
open! Signal
open! Hardcaml_step_testbench
open! Hardcaml_verif
open! Mac_10g_of_hardcaml

module Dut = struct
  module I = struct
    type 'a t =
      { crc_i : 'a [@bits 32]
      ; data_i : 'a [@bits 64]
      ; valid_bytes_i : 'a [@bits 8]
      ; keep_i : 'a [@bits 8]
      ; last_i : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { next_crc_o : 'a [@bits 32]
      ; next_fcs_o : 'a [@bits 32]
      ; valid_residue_o : 'a
      ; keep_contiguous_o : 'a
      ; keep_count_o : 'a [@bits 4]
      ; keep_round_trip_o : 'a [@bits 8]
      ; beat_legal_o : 'a
      }
    [@@deriving hardcaml]
  end

  let create (_scope : Scope.t) (i : _ I.t) : _ O.t =
    let keep_count = Mac_10g_axis.keep_byte_count i.keep_i in
    let next_crc =
      Mac_10g_crc32.update i.crc_i ~data:i.data_i ~valid_bytes:i.valid_bytes_i
    in
    { O.next_crc_o = next_crc
    ; next_fcs_o = Mac_10g_crc32.fcs next_crc
    ; valid_residue_o = Mac_10g_crc32.has_valid_residue next_crc
    ; keep_contiguous_o = Mac_10g_axis.keep_is_contiguous i.keep_i
    ; keep_count_o = keep_count
    ; keep_round_trip_o = Mac_10g_axis.keep_of_byte_count keep_count
    ; beat_legal_o = Mac_10g_axis.beat_has_legal_keep ~keep:i.keep_i ~last:i.last_i
    }
  ;;
end

module Output_snapshot = struct
  type t =
    { next_crc : int
    ; next_fcs : int
    ; valid_residue : bool
    ; keep_contiguous : bool
    ; keep_count : int
    ; keep_round_trip : int
    ; beat_legal : bool
    }
  [@@deriving sexp, equal, compare]
end

module Testbench = struct
  module Fixture = Sim_fixture.Make (struct
      include Dut

      let name = "Mac_10g shared primitives"
    end)

  module Step = Fixture.Step

  let inputs ~crc ~bytes ~valid_bytes ~keep ~last =
    let data =
      List.init 8 ~f:(fun lane -> List.nth bytes lane |> Option.value ~default:0)
      |> List.map ~f:(Bits.of_int_trunc ~width:8)
      |> Bits.concat_lsb
    in
    { Dut.I.crc_i = Bits.of_int_trunc ~width:32 crc
    ; data_i = data
    ; valid_bytes_i = Bits.of_int_trunc ~width:8 valid_bytes
    ; keep_i = Bits.of_int_trunc ~width:8 keep
    ; last_i = Bits_conv.bit last
    }
  ;;

  let snapshot (o : Bits.t Dut.O.t) : Output_snapshot.t =
    { next_crc = Bits.to_int_trunc o.next_crc_o
    ; next_fcs = Bits.to_int_trunc o.next_fcs_o
    ; valid_residue = Bits_conv.to_bool o.valid_residue_o
    ; keep_contiguous = Bits_conv.to_bool o.keep_contiguous_o
    ; keep_count = Bits.to_int_trunc o.keep_count_o
    ; keep_round_trip = Bits.to_int_trunc o.keep_round_trip_o
    ; beat_legal = Bits_conv.to_bool o.beat_legal_o
    }
  ;;

  let run ~crc ~bytes ~valid_bytes ~keep ~last =
    let testbench (handler : Step.Handler.t @ local) _ =
      Step.cycle handler (inputs ~crc ~bytes ~valid_bytes ~keep ~last)
      |> Step.O_data.after_edge
      |> snapshot
    in
    Fixture.run_with_timeout ~timeout:2 ~testbench
  ;;
end
