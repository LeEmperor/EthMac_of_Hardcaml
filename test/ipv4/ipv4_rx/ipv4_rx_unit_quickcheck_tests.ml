(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "ipv4_rx_unit_quickcheck_tests.ml" *)

(* Unit and Quickcheck Test Suite: Ipv4_rx

   Typed examples and generated properties covering header parsing, the checksum verdict
   and the policy that follows from it, the metadata channel, padding removal, and the
   frame-level status signals.

   Every check the superseded [ipv4_rx_legacy_assertion_test.ml] printed is here as an
   assertion, and the six scenarios it hardcoded are generalized over payload length,
   protocol, backpressure schedule and checksum policy. The properties that are net-new -
   [drop_on_bad_checksum = false], IHL > 5, truncated frames, and the empty datagram - are
   marked in the testbench's coverage list.

   The oracle is [Hardcaml_verif.Ip_udp], the same builder [Ipv4_tx]'s suite checks
   against, so the two directions agree on the wire format by construction rather than by
   two hand-copied header tables happening to match.

   Tags: [{ "ACTIVE" ; "TEST" ; "QUICKCHECK" ; "UNIT_TEST" }]
*)

open! Core
open! Hardcaml_verif
open! Ipv4_rx_testbench

let check_stripped runner ~protocol ~payload (observation : Frame_observation.t) =
  [%test_result: int list]
    ~message:(runner.name ^ ": L4 payload")
    observation.payload
    ~expect:payload;
  [%test_result: int]
    ~message:(runner.name ^ ": m_tlast lands on the final L4 byte")
    observation.tlast_index
    ~expect:(List.length payload - 1);
  [%test_result: int]
    ~message:(runner.name ^ ": m_tfirst lands on the first L4 byte")
    observation.tfirst_index
    ~expect:0;
  [%test_result: Metadata.t option]
    ~message:(runner.name ^ ": metadata at l4_start")
    observation.metadata
    ~expect:
      (Some
         { protocol
         ; payload_length = List.length payload
         ; src_ip = ip32 src_ip
         ; dst_ip = ip32 dst_ip
         })
;;

let%test_unit "a nominal UDP frame is stripped to its payload" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 26 in
    let observation =
      run_frame runner ~frame:(ethernet_payload ~protocol:protocol_udp ~payload ())
    in
    check_stripped runner ~protocol:protocol_udp ~payload observation;
    [%test_result: bool]
      ~message:(runner.name ^ ": checksum_ok")
      observation.settled.checksum_ok
      ~expect:true;
    [%test_result: bool]
      ~message:(runner.name ^ ": crc_error")
      observation.settled.crc_error
      ~expect:false;
    [%test_result: bool]
      ~message:(runner.name ^ ": returned to idle")
      observation.settled.busy
      ~expect:false)
;;

(* The MAC pads a short frame out to the 46-byte Ethernet minimum. [m_tlast] comes off IP
   total_length, so the padding must never reach layer 4. *)
let%test_unit "Ethernet zero-padding is dropped, not forwarded" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload ~first:0x80 10 in
    let observation =
      run_frame
        runner
        ~frame:(ethernet_payload ~pad_to:46 ~protocol:protocol_udp ~payload ())
    in
    check_stripped runner ~protocol:protocol_udp ~payload observation)
;;

let%test_unit "a TCP frame under sink backpressure is stripped unchanged" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload ~first:0x10 20 in
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      let observation =
        run_frame
          ~ready:(stall_every stride)
          runner
          ~frame:(ethernet_payload ~protocol:protocol_tcp ~payload ())
      in
      check_stripped runner ~protocol:protocol_tcp ~payload observation))
;;

(* Net-new: the legacy harness held rx_tvalid high for the whole frame, so a parser that
   advanced its header counter on an invalid beat would have gone unnoticed. *)
let%test_unit "source bubbles do not advance the header counter" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 12 in
    List.iter [ 1; 2; 3 ] ~f:(fun stride ->
      let observation =
        run_frame
          ~source_valid:(stall_every stride)
          runner
          ~frame:(ethernet_payload ~protocol:protocol_udp ~payload ())
      in
      check_stripped runner ~protocol:protocol_udp ~payload observation))
;;

let%test_unit "a bad header checksum is dropped or reported, per policy" =
  let payload = make_payload ~first:0x50 16 in
  let frame = ethernet_payload ~corrupt:true ~protocol:protocol_udp ~payload () in
  let strict_observation = run_frame strict ~frame in
  [%test_result: int list]
    ~message:"Strict: nothing reaches L4"
    strict_observation.payload
    ~expect:[];
  [%test_result: bool]
    ~message:"Strict: checksum_ok deasserted"
    strict_observation.settled.checksum_ok
    ~expect:false;
  [%test_result: bool]
    ~message:"Strict: returned to idle"
    strict_observation.settled.busy
    ~expect:false;
  let permissive_observation = run_frame permissive ~frame in
  [%test_result: int list]
    ~message:"Permissive: the payload is forwarded anyway"
    permissive_observation.payload
    ~expect:payload;
  [%test_result: bool]
    ~message:"Permissive: checksum_ok still reports the failure"
    permissive_observation.settled.checksum_ok
    ~expect:false
;;

let%test_unit "a non-IPv4 ethertype is flushed" =
  List.iter runners ~f:(fun runner ->
    let observation =
      run_frame ~eth_type:arp_ethertype runner ~frame:(make_payload ~first:0xA0 30)
    in
    [%test_result: int list] ~message:runner.name observation.payload ~expect:[];
    [%test_result: bool]
      ~message:(runner.name ^ ": returned to idle")
      observation.settled.busy
      ~expect:false)
;;

(* Net-new. The RTL parses IHL = 5 only; a header carrying options has to be dropped, and
   the check is the same [byte = 0x45] comparison that rejects a wrong version. *)
let%test_unit "a header with options (IHL > 5) is flushed" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 0x46; 0x4F; 0x55 ] ~f:(fun version_ihl ->
      let observation =
        run_frame
          runner
          ~frame:
            (ethernet_payload
               ~version_ihl
               ~protocol:protocol_udp
               ~payload:(make_payload 16)
               ())
      in
      [%test_result: int list]
        ~message:(sprintf "%s: version_ihl 0x%02x" runner.name version_ihl)
        observation.payload
        ~expect:[]))
;;

let%test_unit "the MAC's FCS error is forwarded as crc_error" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload ~first:0x30 24 in
    let observation =
      run_frame
        ~fcs_bad:true
        runner
        ~frame:(ethernet_payload ~protocol:protocol_udp ~payload ())
    in
    check_stripped runner ~protocol:protocol_udp ~payload observation;
    [%test_result: bool]
      ~message:(runner.name ^ ": the header itself was intact")
      observation.settled.checksum_ok
      ~expect:true;
    [%test_result: bool]
      ~message:(runner.name ^ ": crc_error latched")
      observation.settled.crc_error
      ~expect:true)
;;

(* Net-new. The MAC stops the frame early; the block must abort rather than wait for bytes
   that will not come. *)
let%test_unit "a frame truncated inside the header aborts to idle" =
  List.iter runners ~f:(fun runner ->
    List.iter [ 1; 4; 10; 19 ] ~f:(fun kept ->
      let frame =
        List.take
          (ethernet_payload ~protocol:protocol_udp ~payload:(make_payload 16) ())
          kept
      in
      let observation = run_frame runner ~frame in
      [%test_result: int list]
        ~message:(sprintf "%s: %d header bytes" runner.name kept)
        observation.payload
        ~expect:[];
      [%test_result: bool]
        ~message:(sprintf "%s: %d header bytes, returned to idle" runner.name kept)
        observation.settled.busy
        ~expect:false))
;;

(* Net-new. total_length promises more payload than the frame carries: the block forwards
   what arrived, never asserts m_tlast, and raises crc_error to say the datagram was cut
   short. *)
let%test_unit "a payload shorter than total_length raises crc_error" =
  List.iter runners ~f:(fun runner ->
    let promised = make_payload 16 in
    let delivered = List.take promised 8 in
    let frame =
      ip_header ~protocol:protocol_udp ~payload_length:(List.length promised) ()
      @ delivered
    in
    let observation = run_frame runner ~frame in
    [%test_result: int list]
      ~message:(runner.name ^ ": the bytes that did arrive are forwarded")
      observation.payload
      ~expect:delivered;
    [%test_result: int]
      ~message:(runner.name ^ ": m_tlast never fires")
      observation.tlast_index
      ~expect:(-1);
    [%test_result: bool]
      ~message:(runner.name ^ ": crc_error marks the truncation")
      observation.settled.crc_error
      ~expect:true;
    [%test_result: bool]
      ~message:(runner.name ^ ": returned to idle")
      observation.settled.busy
      ~expect:false)
;;

(* Findings RTL-9. [has_payload] is [total_len >= 20], so a datagram whose total_length is
   exactly the header length still enters Payload - with [payload_rem = 0], which the
   [payload_rem = 1] test for m_tlast never matches. The frame's [rx_tlast] was consumed
   in Header, so nothing is left to drive the state machine out again and the block stays
   busy forever. Pinned rather than fixed: the test is what a fix has to change. *)
let%test_unit "an empty datagram (total_length = 20) strands the parser in Payload" =
  List.iter runners ~f:(fun runner ->
    let frame = ip_header ~protocol:protocol_udp ~payload_length:0 () in
    let observation = run_frame runner ~frame in
    [%test_result: int list]
      ~message:(runner.name ^ ": nothing reaches L4")
      observation.payload
      ~expect:[];
    [%test_result: bool]
      ~message:(runner.name ^ ": still busy after the frame (RTL-9)")
      observation.settled.busy
      ~expect:true)
;;

let%test_unit "l4_start and m_tfirst are the same signal" =
  List.iter runners ~f:(fun runner ->
    let observation =
      run_frame
        ~ready:(stall_every 2)
        runner
        ~frame:(ethernet_payload ~protocol:protocol_udp ~payload:(make_payload 12) ())
    in
    List.iter observation.trace ~f:(fun (item : Observation.t) ->
      [%test_result: bool]
        ~message:(sprintf "%s: cycle %d" runner.name item.cycle)
        item.output.l4_start
        ~expect:item.output.m_tfirst))
;;

(* Findings RTL-8. [frame_done] qualifies the offered final byte with [m_axis_tready], so
   it remains low while that byte is stalled and pulses exactly once when the transfer is
   accepted. *)
let%test_unit "frame_done pulses once when the final byte is accepted" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 12 in
    let frame = ethernet_payload ~protocol:protocol_udp ~payload () in
    let free = run_frame runner ~frame in
    [%test_result: int]
      ~message:(runner.name ^ ": one pulse when nothing stalls")
      free.frame_done_pulses
      ~expect:1;
    let stalled = run_frame ~ready:(stall_every 1) runner ~frame in
    [%test_result: int]
      ~message:(runner.name ^ ": final-byte backpressure repeated frame_done")
      stalled.frame_done_pulses
      ~expect:1;
    List.iter stalled.trace ~f:(fun observation ->
      if observation.output.frame_done
      then
        [%test_result: bool]
          ~message:(runner.name ^ ": frame_done fired without accepting rx_tlast")
          (observation.output.m_axis_tready
           && Option.value_map observation.rx_index ~default:false ~f:(fun index ->
             index = List.length frame - 1))
          ~expect:true))
;;

(* Findings RTL-7: a low [en] backpressures the MAC and freezes the parser. *)
let%test_unit "en pauses and resumes a frame without losing or repeating a byte" =
  List.iter runners ~f:(fun runner ->
    let payload = make_payload 16 in
    let frame = ethernet_payload ~protocol:protocol_udp ~payload () in
    let en = stall_every 3 in
    let enabled = run_frame runner ~frame in
    let paused = run_frame ~en runner ~frame in
    check_stripped runner ~protocol:protocol_udp ~payload paused;
    [%test_result: int list]
      ~message:(runner.name ^ ": pause/resume changed the payload")
      paused.payload
      ~expect:enabled.payload;
    if paused.cycles <= enabled.cycles
    then raise_s [%message "enable pauses did not delay the frame" (runner.name : string)];
    List.iter paused.trace ~f:(fun observation ->
      if not observation.en
      then
        [%test_result: bool]
          ~message:(runner.name ^ ": a handshake or event escaped while en was low")
          (observation.output.m_axis_tready
           || observation.output.m_tvalid
           || observation.output.m_tlast
           || observation.output.m_tfirst
           || observation.output.l4_start
           || observation.output.frame_done)
          ~expect:false))
;;

let%test_unit "the debug knob decides whether keep folds anything" =
  List.iter [ true; false ] ~f:(fun drop_on_bad_checksum ->
    [%test_result: bool]
      ~message:"debug on: keep folds the internal registers"
      (keep_is_constant ~drop_on_bad_checksum ~debug:true)
      ~expect:false;
    [%test_result: bool]
      ~message:"debug off: keep is tied off"
      (keep_is_constant ~drop_on_bad_checksum ~debug:false)
      ~expect:true)
;;

let%test_unit "random payloads and protocols are stripped to themselves" =
  Quickcheck.test
    ~trials:40
    ~seed:(`Deterministic "ipv4-rx-strip")
    ~sexp_of:[%sexp_of: int list * int]
    ~shrinker:
      (Quickcheck.Shrinker.tuple2
         (List.quickcheck_shrinker Int.quickcheck_shrinker)
         Int.quickcheck_shrinker)
    ~shrink_attempts:(`Limit 100)
    ~f:(fun (payload, protocol) ->
      List.iter runners ~f:(fun runner ->
        let observation =
          run_frame runner ~frame:(ethernet_payload ~protocol ~payload ())
        in
        check_stripped runner ~protocol ~payload observation))
    (Quickcheck.Generator.both
       (Generators.byte_list ~min_length:1 ~max_length:20 ())
       Generators.byte)
;;

(* Padding is generated rather than fixed at 46 so the boundary between "the datagram ends
   the frame" and "the frame runs on past it" is swept rather than sampled. *)
let%test_unit "any amount of Ethernet padding is dropped" =
  Quickcheck.test
    ~trials:30
    ~seed:(`Deterministic "ipv4-rx-padding")
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (payload_length, padding) ->
      let payload = make_payload payload_length in
      List.iter runners ~f:(fun runner ->
        let observation =
          run_frame
            runner
            ~frame:
              (ethernet_payload
                 ~pad_to:(header_length + payload_length + padding)
                 ~protocol:protocol_udp
                 ~payload
                 ())
        in
        check_stripped runner ~protocol:protocol_udp ~payload observation))
    (Quickcheck.Generator.both
       (Generators.payload_length ~min_length:1 ~max_length:16 ())
       (Generators.payload_length ~min_length:0 ~max_length:16 ()))
;;

let schedule_generator length =
  let open Quickcheck.Generator.Let_syntax in
  let%map pattern = List.gen_with_length length Bool.quickcheck_generator in
  if List.exists pattern ~f:Fn.id then pattern else true :: List.tl_exn pattern
;;

let schedule pattern cycle = List.nth_exn pattern (cycle % List.length pattern)

(* Periods 5 and 7 are coprime and each pattern is forced to contain a [true], so some
   cycle has the source presenting while the sink is ready and the run cannot deadlock. *)
let%test_unit "random backpressure and source schedules leave the payload unchanged" =
  Quickcheck.test
    ~trials:30
    ~seed:(`Deterministic "ipv4-rx-schedules")
    ~sexp_of:[%sexp_of: bool list * bool list]
    ~f:(fun (ready_pattern, valid_pattern) ->
      let payload = make_payload 10 in
      List.iter runners ~f:(fun runner ->
        let observation =
          run_frame
            ~ready:(schedule ready_pattern)
            ~source_valid:(schedule valid_pattern)
            runner
            ~frame:(ethernet_payload ~protocol:protocol_udp ~payload ())
        in
        check_stripped runner ~protocol:protocol_udp ~payload observation))
    (Quickcheck.Generator.both (schedule_generator 5) (schedule_generator 7))
;;

(* A single corrupted header byte can never be compensated in a one's-complement sum, so
   under [drop_on_bad_checksum] no such frame may reach layer 4 - whichever byte it was,
   including the version/IHL byte, which is rejected before the checksum is even finished. *)
let%test_unit "any single corrupted header byte keeps the frame off the L4 stream" =
  Quickcheck.test
    ~trials:60
    ~seed:(`Deterministic "ipv4-rx-corruption")
    ~sexp_of:[%sexp_of: int * int]
    ~f:(fun (index, delta) ->
      let payload = make_payload 12 in
      let frame =
        List.mapi
          (ethernet_payload ~protocol:protocol_udp ~payload ())
          ~f:(fun position byte -> if position = index then byte lxor delta else byte)
      in
      let observation = run_frame strict ~frame in
      [%test_result: int list]
        ~message:(sprintf "corrupting header byte %d with 0x%02x" index delta)
        observation.payload
        ~expect:[])
    (Quickcheck.Generator.both (Int.gen_incl 0 (header_length - 1)) (Int.gen_incl 1 0xFF))
;;
