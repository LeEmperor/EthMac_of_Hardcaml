(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "packet_buffer_bank_mapping_tests.ml" *)
(* Structural verification of the lane -> ring-address -> bank mapping that lets the byte
   store be eight ordinary single-write-port RAMs instead of one multi-write-port RAM. *)

(* ------------------------------------------------------------------------------------
   WHAT IS BEING VERIFIED, AND WHY IT IS THE LOAD-BEARING PROPERTY

   The buffer presents one logical byte ring, [0, depth_bytes), addressed by
   [address_width] bits. The ring is a convention, not a structure: the RAMs are flat
   arrays, and the "circular" behaviour comes entirely from the pointer registers being
   fixed-width and wrapping by modular overflow. That is why [depth_bytes] is forced to a
   power of two -- the wrap has to land back on byte 0, with no comparator anywhere.

   That single address space is striped across eight banks:

     bank = addr[2:0]                  (which RAM)
     row  = addr[address_width-1:3]    (where in that RAM)

     logical byte:  ... 254  255 | 256  257  258  259  260  261  262  263 | 264 ...
     bank:               6    7  |   0    1    2    3    4    5    6    7 |   0
     row (depth 8192):  31   31  |  32   32   32   32   32   32   32   32 |  33

   One 64-bit AXI beat offers eight lanes, of which only the [write_keep_i] lanes carry
   real bytes. Storage is *packed*: no hole is left for a dropped lane. So lane L's
   address is not [speculative_pointer + L], it is

     pointer_for_lane.(L) = speculative_pointer + popcount(write_keep_i[L-1:0])

   i.e. a common base plus an *exclusive prefix sum* of the keep mask. Those are eight
   CANDIDATE addresses. Only the popcount(keep) of them belonging to enabled lanes are
   meaningful; a masked lane's candidate aliases its nearest enabled neighbour above (or
   the first address of the next beat), and is discarded by the [write_keep_i] term in
   [write_matches].

   THE INVARIANT. For the enabled lanes the prefix values are 0, 1, ..., k-1 where
   k = popcount(keep), so their addresses are the k consecutive integers

     p, p+1, ..., p+k-1     with k <= 8.

   Any run of at most eight consecutive integers has distinct residues mod 8, and
   bank = addr mod 8. Therefore *no two enabled lanes ever target the same bank in the
   same cycle*. Corollaries, each checked below:

     (1) k = 8 (a full mid-frame beat): the eight lanes hit all eight banks exactly once
         -- a perfect permutation, specifically a rotation by (p mod 8). Full 64-bit
         write bandwidth, every RAM busy.
     (2) k < 8: exactly k banks are enabled and 8-k sit idle. Never two lanes on one bank.
     (3) The lane -> bank map is NOT fixed. It rotates every beat by (p mod 8), because
         the pointer advances by the popcount rather than by 8. A fixed lane L -> bank L
         wiring would be simpler but could not pack: a six-byte beat would have to leave a
         two-byte hole to keep lane 0 in bank 0 next beat. The rotation is the price of
         dense packing, and [pointer_for_lane] is where it is paid -- the shared base
         supplies the rotation, the prefix sum supplies the compaction.
     (4) A beat may straddle two rows (banks at and above the rotation point take row n,
         those that wrapped below it take row n+1). Harmless: the banks are independent
         RAMs with independent address ports -- but it is exactly the case a fixed
         lane->bank wiring would get wrong.

   WHY IT MATTERS BEYOND BANDWIDTH. Read from the bank's side, the invariant is what
   makes [write_matches] one-hot, and one-hotness is what licenses the two [Array.foldi]
   priority-mux chains that gather [write_data] and [write_address]. If two lanes could
   ever match the same bank, the fold's arbitrary priority order would silently pick one
   and drop the other's byte. So this is not merely an efficiency argument: the
   crossbar's *correctness* rests on it. That is worth checking directly rather than only
   inferring it from end-to-end data integrity.

   COVERAGE NOTE. The sweep below is exhaustive over all eight rotations x all 255
   non-zero keep masks, including *non-contiguous* masks such as 0b1010_1010. AXI4-Stream
   forbids those (see [Mac_10g_axis.beat_has_legal_keep]: all-ones mid-frame, any
   contiguous run on last) and [Mac_10g_tx_ingress] rejects them upstream. They are swept
   anyway because [Mac_10g_axis.keep_byte_count] and the prefix sum are both true per-bit
   popcounts, so the compaction hardware is strictly more general than the contract it
   serves -- and that generality should be deliberate, not accidental. The end-to-end
   tests in [packet_buffer_unit_quickcheck_tests] only ever drive contiguous masks (see
   [Testbench.input_of_action], which builds keep as (1 lsl n) - 1), so this file is the
   only thing covering the sparse case.

   ON FORMAL VERIFICATION. This is a combinational property over a small, closed input
   space, which is exactly the shape a formal tool eats for breakfast. Two directions
   worth taking if this module is ever hardened further:

     - The property as stated here is a one-line SVA assertion on the write path, e.g.
         for all lanes a /= b with keep[a] & keep[b] & write_accepted:
           pointer_for_lane[a][2:0] != pointer_for_lane[b][2:0]
       plus, when write_accepted:
           $countones({bank_write_enable_7 .. bank_write_enable_0})
             == $countones(write_keep_i)
       Both are purely combinational, so a bounded proof at depth 0 is already a complete
       proof -- no induction needed. Crucially they would hold for *every* [depth_bytes],
       whereas this file can only exercise the single elaboration it instantiates.
       Emitting them alongside the generated Verilog and running SymbiYosys / JasperGold
       would close that parameterisation gap, which simulation fundamentally cannot.
     - The deeper transactional properties (rollback never exposes speculative bytes;
       committed bytes read back in order; [bytes_used] is always the true occupancy) are
       unbounded safety statements over the pointer trio and are the natural next target.
       Those genuinely need induction, and a proof would subsume much of the
       reference-model testing in [packet_buffer_unit_quickcheck_tests].

   Exhaustive simulation here is deliberately the cheaper first step: it needs no extra
   toolchain, and it pins the reasoning above to the actual elaborated RTL rather than to
   a hand-written model of it.
   ------------------------------------------------------------------------------------ *)

open! Core
open! Hardcaml
open! Mac_10g_of_hardcaml

(* Small enough to elaborate instantly, large enough that all eight rotations exist and a
   full eight-byte beat always fits in the free space. *)
let depth_bytes = 64

module Dut = Mac_10g_packet_buffer.Make (struct
    let depth_bytes = depth_bytes
    let descriptor_capacity = 4
    let error_width = 4
  end)

module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)

module Beat = struct
  (* One combinational sample of the write path: the eight candidate ring addresses and
     the eight per-bank write enables, taken with [write_keep_i] applied and settled, but
     before the clock edge that would commit them to the RAMs. *)
  type t =
    { pointer_for_lane : int array
    ; bank_write_enable : bool array
    }
  [@@deriving sexp_of]

  let bank t lane = t.pointer_for_lane.(lane) land 7
  let row t lane = t.pointer_for_lane.(lane) lsr 3
end

let all_lanes = List.range 0 8

let lanes_of_mask keep =
  List.filter all_lanes ~f:(fun lane -> keep land (1 lsl lane) <> 0)
;;

let popcount keep = List.length (lanes_of_mask keep)

(* [lookup_node_or_reg_by_name] rather than [lookup_node_by_name]: lane 0's prefix sum is
   the constant zero, so its candidate address folds to [speculative_pointer] itself and
   the name lands on the register rather than on a combinational node. That folding is
   itself a small confirmation of the reasoning above -- lane 0's address *is* the
   pointer, with no adder needed. *)
let node sim name =
  match Cyclesim.lookup_node_or_reg_by_name sim name with
  | Some node -> node
  | None ->
    raise_s
      [%message
        "packet-buffer internal signal is not traced; did the [--] naming in \
         mac_10g_packet_buffer.ml change?"
          (name : string)]
;;

(* A driver that holds the whole input record, so a beat can be issued with an arbitrary
   keep mask -- unlike [Packet_buffer_testbench.Action], which derives keep from a byte
   list and can therefore only ever produce contiguous masks. *)
module Driver = struct
  type t =
    { sim : (Bits.t ref Dut.I.t, Bits.t ref Dut.O.t) Cyclesim.t
    ; inputs : Bits.t ref Dut.I.t
    ; outputs : Bits.t ref Dut.O.t
    }

  let set t ~keep ~commit ~rollback ~read_ready =
    t.inputs.reset_i := Bits.gnd;
    (* Data is irrelevant here: this file checks addressing, not payload steering, which
       the end-to-end tests already cover. *)
    t.inputs.write_data_i := Bits.of_int_trunc ~width:64 0;
    t.inputs.write_keep_i := Bits.of_int_trunc ~width:8 keep;
    t.inputs.write_valid_i := if keep = 0 then Bits.gnd else Bits.vdd;
    t.inputs.commit_i := if commit then Bits.vdd else Bits.gnd;
    t.inputs.rollback_i := if rollback then Bits.vdd else Bits.gnd;
    t.inputs.commit_error_i := Bits.of_int_trunc ~width:4 0;
    t.inputs.read_ready_i := if read_ready then Bits.vdd else Bits.gnd
  ;;

  let idle t = set t ~keep:0 ~commit:false ~rollback:false ~read_ready:false

  let create () =
    let sim =
      Sim.create
        ~config:Cyclesim.Config.trace_all
        (Dut.create (Scope.create ~flatten_design:true ()))
    in
    let t = { sim; inputs = Cyclesim.inputs sim; outputs = Cyclesim.outputs sim } in
    idle t;
    t.inputs.reset_i := Bits.vdd;
    Cyclesim.cycle t.sim;
    Cyclesim.cycle t.sim;
    idle t;
    Cyclesim.cycle t.sim;
    t
  ;;

  (* Advance the *committed* write pointer to [target] by pushing and immediately draining
     that many bytes, so the ring is left empty (plenty of free space for the beat under
     test) with the pointer parked where we want it. Committed rather than speculative, so
     a rollback can snap back here between cases. *)
  let commit_pointer_to t ~target =
    let push count =
      set t ~keep:((1 lsl count) - 1) ~commit:true ~rollback:false ~read_ready:false;
      Cyclesim.cycle t.sim;
      (* one read beat retires the whole frame, since a frame here is at most 8 bytes *)
      set t ~keep:0 ~commit:false ~rollback:false ~read_ready:true;
      Cyclesim.cycle t.sim
    in
    for _ = 1 to target / 8 do
      push 8
    done;
    if target % 8 > 0 then push (target % 8);
    idle t;
    Cyclesim.cycle t.sim;
    [%test_result: int]
      (Bits.to_int_trunc !(t.outputs.bytes_used_o))
      ~expect:0
      ~message:"priming should leave the ring empty";
    [%test_result: int]
      (Bits.to_int_trunc !(t.outputs.descriptors_used_o))
      ~expect:0
      ~message:"priming should leave the descriptor FIFO empty"
  ;;

  (* Issue one beat, sample the write path combinationally, then roll it back so the next
     case starts from the same pointer. *)
  let sample_beat t ~keep =
    set t ~keep ~commit:false ~rollback:false ~read_ready:false;
    Cyclesim.cycle_before_clock_edge t.sim;
    let beat =
      { Beat.pointer_for_lane =
          Array.init 8 ~f:(fun lane ->
            Cyclesim.Node.to_int (node t.sim (sprintf "pointer_for_lane_%d" lane)))
      ; bank_write_enable =
          Array.init 8 ~f:(fun bank ->
            Cyclesim.Node.to_int (node t.sim (sprintf "bank_write_enable_%d" bank)) = 1)
      }
    in
    Cyclesim.cycle_at_clock_edge t.sim;
    Cyclesim.cycle_after_clock_edge t.sim;
    set t ~keep:0 ~commit:false ~rollback:true ~read_ready:false;
    Cyclesim.cycle t.sim;
    idle t;
    Cyclesim.cycle t.sim;
    beat
  ;;
end

(* One elaboration per rotation, reused across every mask and every test below. *)
let sweep =
  lazy
    (Array.init 8 ~f:(fun rotation ->
       let driver = Driver.create () in
       Driver.commit_pointer_to driver ~target:rotation;
       Array.init 256 ~f:(fun keep ->
         if keep = 0 then None else Some (Driver.sample_beat driver ~keep))))
;;

let beat ~rotation ~keep = Option.value_exn (Lazy.force sweep).(rotation).(keep)
let all_rotations = List.range 0 8
let all_non_zero_masks = List.range 1 256

let iter_cases ~f =
  List.iter all_rotations ~f:(fun rotation ->
    List.iter all_non_zero_masks ~f:(fun keep -> f ~rotation ~keep (beat ~rotation ~keep)))
;;

let where ~rotation ~keep = sprintf "rotation %d, keep 0x%02x" rotation keep

(* ---- the invariant itself ---------------------------------------------------------- *)

let%test_unit "enabled lanes occupy consecutive ring addresses starting at the pointer" =
  iter_cases ~f:(fun ~rotation ~keep beat ->
    let expected =
      List.mapi (lanes_of_mask keep) ~f:(fun index _lane ->
        (rotation + index) % depth_bytes)
    in
    let actual =
      List.map (lanes_of_mask keep) ~f:(fun lane -> beat.Beat.pointer_for_lane.(lane))
    in
    [%test_result: int list] actual ~expect:expected ~message:(where ~rotation ~keep))
;;

let%test_unit "no two enabled lanes ever target the same bank" =
  iter_cases ~f:(fun ~rotation ~keep beat ->
    let banks = List.map (lanes_of_mask keep) ~f:(Beat.bank beat) in
    [%test_result: int]
      (List.length (List.dedup_and_sort banks ~compare:Int.compare))
      ~expect:(popcount keep)
      ~message:("bank collision at " ^ where ~rotation ~keep))
;;

(* ---- corollary (1): a full beat is a perfect rotation of the banks ------------------ *)

let%test_unit "a full eight-lane beat hits every bank exactly once, rotated by the \
               pointer"
  =
  List.iter all_rotations ~f:(fun rotation ->
    let beat = beat ~rotation ~keep:0xff in
    List.iter all_lanes ~f:(fun lane ->
      [%test_result: int]
        (Beat.bank beat lane)
        ~expect:((rotation + lane) % 8)
        ~message:(sprintf "rotation %d, lane %d" rotation lane));
    [%test_result: bool array]
      beat.Beat.bank_write_enable
      ~expect:(Array.create ~len:8 true)
      ~message:(sprintf "every bank should be busy on a full beat (rotation %d)" rotation))
;;

(* ---- corollary (2): a partial beat leaves exactly the unclaimed banks idle ---------- *)

let%test_unit "exactly popcount(keep) banks are enabled, and they are the claimed ones" =
  iter_cases ~f:(fun ~rotation ~keep beat ->
    let claimed = lanes_of_mask keep |> List.map ~f:(Beat.bank beat) |> Int.Set.of_list in
    let enabled =
      List.filter all_lanes ~f:(fun bank -> beat.Beat.bank_write_enable.(bank))
      |> Int.Set.of_list
    in
    [%test_result: Int.Set.t] enabled ~expect:claimed ~message:(where ~rotation ~keep);
    [%test_result: int]
      (Set.length enabled)
      ~expect:(popcount keep)
      ~message:("wrong idle-bank count at " ^ where ~rotation ~keep))
;;

(* ---- corollary (3): the lane -> bank map rotates, it is not fixed ------------------- *)

let%test_unit "the lane to bank map rotates with the pointer rather than being fixed" =
  (* Lane 0 does not own bank 0. Walking the rotation walks lane 0 across every bank,
     which is the observable difference between packing and a fixed lane->bank wiring. *)
  [%test_result: int list]
    (List.map all_rotations ~f:(fun rotation -> Beat.bank (beat ~rotation ~keep:0xff) 0))
    ~expect:[ 0; 1; 2; 3; 4; 5; 6; 7 ]
;;

(* ---- the negative half: masked lanes alias, and must not write ---------------------- *)

let%test_unit "a masked lane aliases a real address yet drives no write" =
  (* The worked example from the module comment, scaled to this depth: pointer 6, keep
     0b0011_1111. Lanes 6 and 7 both compute the same candidate as the *next* beat's lane
     0. If the [write_keep_i] term were dropped from [write_matches], that alias would
     corrupt a byte -- so pin both the alias and its suppression. *)
  let beat = beat ~rotation:6 ~keep:0b0011_1111 in
  [%test_result: int list]
    (List.map all_lanes ~f:(fun lane -> beat.Beat.pointer_for_lane.(lane)))
    ~expect:[ 6; 7; 8; 9; 10; 11; 12; 12 ];
  [%test_result: int] (Beat.bank beat 6) ~expect:4 ~message:"masked lane 6 aliases bank 4";
  [%test_result: int] (Beat.bank beat 7) ~expect:4 ~message:"masked lane 7 aliases bank 4";
  List.iter [ 4; 5 ] ~f:(fun bank ->
    [%test_result: bool]
      beat.Beat.bank_write_enable.(bank)
      ~expect:false
      ~message:(sprintf "bank %d is unclaimed and must stay idle" bank));
  List.iter [ 6; 7; 0; 1; 2; 3 ] ~f:(fun bank ->
    [%test_result: bool]
      beat.Beat.bank_write_enable.(bank)
      ~expect:true
      ~message:(sprintf "bank %d carries a kept byte" bank))
;;

(* ---- corollary (4): a beat may straddle two rows ------------------------------------ *)

let%test_unit "a beat straddling a row boundary splits cleanly across banks" =
  let beat = beat ~rotation:6 ~keep:0xff in
  let row_of_bank =
    List.map all_lanes ~f:(fun lane -> Beat.bank beat lane, Beat.row beat lane)
    |> Int.Map.of_alist_exn
  in
  List.iter [ 6; 7 ] ~f:(fun bank ->
    [%test_result: int]
      (Map.find_exn row_of_bank bank)
      ~expect:0
      ~message:(sprintf "bank %d finishes row 0" bank));
  List.iter [ 0; 1; 2; 3; 4; 5 ] ~f:(fun bank ->
    [%test_result: int]
      (Map.find_exn row_of_bank bank)
      ~expect:1
      ~message:(sprintf "bank %d starts row 1" bank));
  [%test_result: bool array]
    beat.Beat.bank_write_enable
    ~expect:(Array.create ~len:8 true)
;;

(* ---- the addresses wrap the ring, not merely the row -------------------------------- *)

let%test_unit "candidate addresses wrap modulo depth_bytes" =
  (* Park the pointer one byte below the top of the ring and take a full beat: the
     candidates must wrap to 0 rather than run past the end. This is the whole reason
     [depth_bytes] is required to be a power of two -- the wrap is plain modular overflow
     of an [address_width]-bit register. The bank sequence stays a clean rotation across
     the seam, which is what keeps the invariant true at the wrap point too. *)
  let driver = Driver.create () in
  Driver.commit_pointer_to driver ~target:(depth_bytes - 1);
  let beat = Driver.sample_beat driver ~keep:0xff in
  [%test_result: int list]
    (List.map all_lanes ~f:(fun lane -> beat.Beat.pointer_for_lane.(lane)))
    ~expect:[ 63; 0; 1; 2; 3; 4; 5; 6 ];
  [%test_result: int list]
    (List.map all_lanes ~f:(Beat.bank beat))
    ~expect:[ 7; 0; 1; 2; 3; 4; 5; 6 ];
  [%test_result: bool array]
    beat.Beat.bank_write_enable
    ~expect:(Array.create ~len:8 true)
;;
