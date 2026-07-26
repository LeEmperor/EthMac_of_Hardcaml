(* IPv4 + UDP receive integration test.

   The input is the Ethernet-payload stream exposed by Mac_top:

     Ethernet payload -> Ipv4_rx -> Udp_rx -> application
*)

open! Core
open! Hardcaml
open! Ipv4_of_hardcaml
open! Udp_of_hardcaml
open! Helper_tb_functions

let () = print_endline "=== Running IPv4+UDP RX Integration Testbench ==="

let hi8 x = (x lsr 8) land 0xFF
let lo8 x = x land 0xFF
let w16 hi lo = ((hi land 0xFF) lsl 8) lor (lo land 0xFF)

let ip32 bytes =
  List.fold bytes ~init:0 ~f:(fun acc byte -> (acc lsl 8) lor (byte land 0xFF))
;;

let ip_checksum ~total_length ~protocol ~src_ip ~dst_ip =
  let words =
    [ 0x4500
    ; total_length
    ; 0x0000
    ; 0x4000
    ; w16 0x40 protocol
    ; 0x0000
    ; w16 (List.nth_exn src_ip 0) (List.nth_exn src_ip 1)
    ; w16 (List.nth_exn src_ip 2) (List.nth_exn src_ip 3)
    ; w16 (List.nth_exn dst_ip 0) (List.nth_exn dst_ip 1)
    ; w16 (List.nth_exn dst_ip 2) (List.nth_exn dst_ip 3)
    ]
  in
  let sum = List.fold words ~init:0 ~f:( + ) in
  let rec fold sum =
    if sum > 0xFFFF then fold ((sum land 0xFFFF) + (sum lsr 16)) else sum
  in
  lnot (fold sum) land 0xFFFF
;;

let udp_datagram ~src_port ~dst_port ~checksum ~payload =
  let length = 8 + List.length payload in
  [ hi8 src_port
  ; lo8 src_port
  ; hi8 dst_port
  ; lo8 dst_port
  ; hi8 length
  ; lo8 length
  ; hi8 checksum
  ; lo8 checksum
  ]
  @ payload
;;

let ipv4_udp_payload
  ?(corrupt_ip_checksum = false)
  ?(ethernet_padding = false)
  ~src_ip
  ~dst_ip
  ~src_port
  ~dst_port
  ~udp_checksum
  ~payload
  ()
  =
  let udp = udp_datagram ~src_port ~dst_port ~checksum:udp_checksum ~payload in
  let total_length = 20 + List.length udp in
  let checksum = ip_checksum ~total_length ~protocol:17 ~src_ip ~dst_ip in
  let checksum = if corrupt_ip_checksum then checksum lxor 0xFFFF else checksum in
  let header =
    [ 0x45
    ; 0x00
    ; hi8 total_length
    ; lo8 total_length
    ; 0x00
    ; 0x00
    ; 0x40
    ; 0x00
    ; 0x40
    ; 0x11
    ; hi8 checksum
    ; lo8 checksum
    ]
    @ src_ip
    @ dst_ip
  in
  let datagram = header @ udp in
  if ethernet_padding
  then datagram @ List.init (Int.max 0 (46 - List.length datagram)) ~f:(fun _ -> 0)
  else datagram
;;

type metadata =
  { src_port : int
  ; dst_port : int
  ; udp_length : int
  ; payload_length : int
  ; udp_checksum : int
  ; src_ip : int
  ; dst_ip : int
  }

type run_result =
  { payload : int list
  ; metadata : metadata option
  ; app_start_count : int
  ; tfirst_count : int
  ; tlast_indices : int list
  ; checksum_ok : bool
  ; crc_error : bool
  ; ip_busy : bool
  ; udp_busy : bool
  ; saw_app_stall : bool
  ; ready_low_on_all_stalls : bool
  }

module Stack (Checksum_policy : sig
    val drop_on_bad_checksum : bool
  end) =
struct
  module Ip = Ipv4_rx.Make (struct
      let drop_on_bad_checksum = Checksum_policy.drop_on_bad_checksum
      let debug = true
    end)

  module Udp = Udp_rx.Make (struct
      let drop_on_port_mismatch = false
      let expected_dst_port = 0x1235
      let debug = true
    end)

  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; en : 'a
      ; rx_tdata : 'a [@bits 8]
      ; rx_tvalid : 'a
      ; rx_tlast : 'a
      ; rx_tuser : 'a
      ; rx_tfirst : 'a
      ; rx_eth_type : 'a [@bits 16]
      ; app_tready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { rx_tready : 'a
      ; m_tdata : 'a [@bits 8]
      ; m_tvalid : 'a
      ; m_tlast : 'a
      ; m_tfirst : 'a
      ; app_start : 'a
      ; src_port : 'a [@bits 16]
      ; dst_port : 'a [@bits 16]
      ; udp_length : 'a [@bits 16]
      ; payload_length : 'a [@bits 16]
      ; udp_checksum : 'a [@bits 16]
      ; src_ip : 'a [@bits 32]
      ; dst_ip : 'a [@bits 32]
      ; checksum_ok : 'a
      ; crc_error : 'a
      ; ip_busy : 'a
      ; udp_busy : 'a
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) =
    let udp_ready = Signal.wire 1 in
    let ip =
      Ip.create (Scope.sub_scope scope "ipv4_rx")
        { Ip.I.clock = i.clock
        ; reset = i.reset
        ; en = i.en
        ; rx_tdata = i.rx_tdata
        ; rx_tvalid = i.rx_tvalid
        ; rx_tlast = i.rx_tlast
        ; rx_tuser = i.rx_tuser
        ; rx_tfirst = i.rx_tfirst
        ; rx_eth_type = i.rx_eth_type
        ; l4_tready = udp_ready
        }
    in
    let udp =
      Udp.create (Scope.sub_scope scope "udp_rx")
        { Udp.I.clock = i.clock
        ; reset = i.reset
        ; en = i.en
        ; rx_tdata = ip.m_tdata
        ; rx_tvalid = ip.m_tvalid
        ; rx_tlast = ip.m_tlast
        ; rx_tuser = ip.crc_error
        ; rx_tfirst = ip.m_tfirst
        ; ip_protocol = ip.protocol
        ; ip_src_ip = ip.src_ip
        ; ip_dst_ip = ip.dst_ip
        ; ip_frame_done = ip.frame_done
        ; ip_frame_error = ip.frame_error
        ; app_tready = i.app_tready
        }
    in
    Signal.(udp_ready <-- udp.m_axis_tready);
    { O.rx_tready = ip.m_axis_tready
    ; m_tdata = udp.m_tdata
    ; m_tvalid = udp.m_tvalid
    ; m_tlast = udp.m_tlast
    ; m_tfirst = udp.m_tfirst
    ; app_start = udp.app_start
    ; src_port = udp.src_port
    ; dst_port = udp.dst_port
    ; udp_length = udp.udp_length
    ; payload_length = udp.payload_length
    ; udp_checksum = udp.udp_checksum
    ; src_ip = udp.src_ip
    ; dst_ip = udp.dst_ip
    ; checksum_ok = ip.checksum_ok
    ; crc_error = udp.crc_error
    ; ip_busy = ip.busy
    ; udp_busy = udp.busy
    }
  ;;

  module Sim = Cyclesim.With_interface (I) (O)

  let bit signal = Bits.to_bool !signal

  let run ?(eth_type = 0x0800) ?(fcs_bad = false) ?(stall_every = 0) frame =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let sim = Sim.create (create scope) in
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    let cycle () = Cyclesim.cycle sim in
    i.I.reset <-- 1;
    i.en <-- 1;
    i.rx_tdata <-- 0;
    i.rx_tvalid <-- 0;
    i.rx_tlast <-- 0;
    i.rx_tuser <-- 0;
    i.rx_tfirst <-- 0;
    i.rx_eth_type <-- eth_type;
    i.app_tready <-- 1;
    cycle ();
    i.reset <-- 0;

    let bytes = Array.of_list frame in
    let length = Array.length bytes in
    let ptr = ref 0 in
    let drain_cycles = ref 0 in
    let guard = ref 0 in
    let payload_phase = ref 0 in
    let transfer_count = ref 0 in
    let collected = ref [] in
    let metadata = ref None in
    let app_start_count = ref 0 in
    let tfirst_count = ref 0 in
    let tlast_indices = ref [] in
    let saw_app_stall = ref false in
    let ready_low_on_all_stalls = ref true in
    let q_valid = ref (bit o.O.m_tvalid) in
    let q_first = ref (bit o.m_tfirst) in
    let q_last = ref (bit o.m_tlast) in
    let q_app_start = ref (bit o.app_start) in
    let guard_limit = (length * 4) + 96 in
    while (!ptr < length || !drain_cycles < 4) && !guard < guard_limit do
      let has_byte = !ptr < length in
      let present = if has_byte then bytes.(!ptr) else 0 in
      let stalling =
        !q_valid
        && stall_every > 0
        && !payload_phase mod (stall_every + 1) = stall_every
      in
      let app_ready = not stalling in
      let transfer = has_byte && !q_valid && app_ready in
      let start_event = !q_app_start && ((not !q_valid) || app_ready) in

      if start_event
      then (
        incr app_start_count;
        metadata
        := Some
             { src_port = Bits.to_int_trunc !(o.O.src_port)
             ; dst_port = Bits.to_int_trunc !(o.dst_port)
             ; udp_length = Bits.to_int_trunc !(o.udp_length)
             ; payload_length = Bits.to_int_trunc !(o.payload_length)
             ; udp_checksum = Bits.to_int_trunc !(o.udp_checksum)
             ; src_ip = Bits.to_int_trunc !(o.src_ip)
             ; dst_ip = Bits.to_int_trunc !(o.dst_ip)
             });
      if transfer
      then (
        collected := present :: !collected;
        if !q_first then incr tfirst_count;
        if !q_last then tlast_indices := !transfer_count :: !tlast_indices;
        incr transfer_count);

      i.app_tready <-- (if app_ready then 1 else 0);
      i.rx_tvalid <-- (if has_byte then 1 else 0);
      i.rx_tdata <-- present;
      i.rx_tfirst <-- (if has_byte && !ptr = 0 then 1 else 0);
      i.rx_tlast <-- (if has_byte && !ptr = length - 1 then 1 else 0);
      i.rx_tuser
      <-- (if has_byte && !ptr = length - 1 && fcs_bad then 1 else 0);

      let accepted = has_byte && ((not !q_valid) || app_ready) in
      cycle ();

      if stalling && bit o.m_tvalid
      then (
        saw_app_stall := true;
        if bit o.rx_tready then ready_low_on_all_stalls := false);
      q_valid := bit o.m_tvalid;
      q_first := bit o.m_tfirst;
      q_last := bit o.m_tlast;
      q_app_start := bit o.app_start;
      if !q_valid then incr payload_phase;
      if accepted then incr ptr;
      if !ptr >= length then incr drain_cycles else drain_cycles := 0;
      incr guard
    done;
    if !guard >= guard_limit then failwith "IPv4+UDP RX integration driver timed out";
    { payload = List.rev !collected
    ; metadata = !metadata
    ; app_start_count = !app_start_count
    ; tfirst_count = !tfirst_count
    ; tlast_indices = List.rev !tlast_indices
    ; checksum_ok = bit o.checksum_ok
    ; crc_error = bit o.crc_error
    ; ip_busy = bit o.ip_busy
    ; udp_busy = bit o.udp_busy
    ; saw_app_stall = !saw_app_stall
    ; ready_low_on_all_stalls = !ready_low_on_all_stalls
    }
  ;;
end

module Drop_bad_checksum = Stack (struct
    let drop_on_bad_checksum = true
  end)

module Report_bad_checksum = Stack (struct
    let drop_on_bad_checksum = false
  end)

let () =
  let pass_count = ref 0 in
  let test_count = ref 0 in
  let expect label condition =
    incr test_count;
    if condition then incr pass_count;
    printf "  %-48s %s\n" label (if condition then "PASS" else "FAIL")
  in
  let expect_metadata result ~src_port ~dst_port ~payload ~udp_checksum ~src_ip ~dst_ip =
    match result.metadata with
    | None -> expect "metadata captured" false
    | Some metadata ->
      expect "source port" (metadata.src_port = src_port);
      expect "destination port" (metadata.dst_port = dst_port);
      expect "UDP length" (metadata.udp_length = 8 + List.length payload);
      expect "application payload length" (metadata.payload_length = List.length payload);
      expect "UDP checksum field" (metadata.udp_checksum = udp_checksum);
      expect "source IP" (metadata.src_ip = ip32 src_ip);
      expect "destination IP" (metadata.dst_ip = ip32 dst_ip)
  in
  let src_ip = [ 192; 168; 1; 10 ] in
  let dst_ip = [ 192; 168; 1; 1 ] in
  let src_port = 0x1234 in
  let dst_port = 0x1235 in
  let make ?corrupt_ip_checksum ?ethernet_padding ?(udp_checksum = 0) payload =
    ipv4_udp_payload
      ?corrupt_ip_checksum
      ?ethernet_padding
      ~src_ip
      ~dst_ip
      ~src_port
      ~dst_port
      ~udp_checksum
      ~payload
      ()
  in

  printf "\n-- test 1: normal IPv4/UDP datagram --\n";
  let payload = List.init 32 ~f:(fun k -> ((k * 13) + 7) land 0xFF) in
  let udp_checksum = 0xBEEF in
  let result = Drop_bad_checksum.run (make ~udp_checksum payload) in
  expect "payload crosses both layers byte-perfectly"
    (List.equal Int.equal result.payload payload);
  expect "one application start" (result.app_start_count = 1);
  expect "one application tfirst" (result.tfirst_count = 1);
  expect "application tlast is on the final byte"
    (List.equal Int.equal result.tlast_indices [ List.length payload - 1 ]);
  expect "IPv4 checksum accepted" result.checksum_ok;
  expect "no lower-layer error" (not result.crc_error);
  expect "both parsers return idle" (not result.ip_busy && not result.udp_busy);
  expect_metadata result ~src_port ~dst_port ~payload ~udp_checksum ~src_ip ~dst_ip;

  printf "\n-- test 2: Ethernet minimum-frame padding is dropped --\n";
  let payload = [ 0xDE; 0xAD; 0xBE; 0xEF ] in
  let frame = make ~ethernet_padding:true payload in
  expect "test vector contains Ethernet padding" (List.length frame = 46);
  let result = Drop_bad_checksum.run frame in
  expect "padding never reaches the application"
    (List.equal Int.equal result.payload payload);
  expect "tlast follows UDP length, not Ethernet length"
    (List.equal Int.equal result.tlast_indices [ 3 ]);
  expect "both parsers drain padding and return idle"
    (not result.ip_busy && not result.udp_busy);

  printf "\n-- test 3: non-IPv4 EtherType is rejected at L3 --\n";
  let payload = [ 1; 2; 3; 4; 5 ] in
  let result = Drop_bad_checksum.run ~eth_type:0x0806 (make payload) in
  expect "non-IPv4 frame emits no application bytes" (List.is_empty result.payload);
  expect "non-IPv4 frame emits no application start" (result.app_start_count = 0);
  expect "both parsers return idle" (not result.ip_busy && not result.udp_busy);

  printf "\n-- test 4: bad IPv4 checksum is dropped when enforcement is enabled --\n";
  let payload = List.init 12 ~f:(fun k -> (0x80 + k) land 0xFF) in
  let bad_frame = make ~corrupt_ip_checksum:true payload in
  let result = Drop_bad_checksum.run bad_frame in
  expect "bad-checksum frame emits no application bytes" (List.is_empty result.payload);
  expect "bad-checksum frame emits no application start" (result.app_start_count = 0);
  expect "bad IPv4 checksum is reported" (not result.checksum_ok);

  printf "\n-- test 5: bad IPv4 checksum can be reported without dropping --\n";
  let result = Report_bad_checksum.run bad_frame in
  expect "payload is forwarded in report-only mode"
    (List.equal Int.equal result.payload payload);
  expect "bad IPv4 checksum remains visible" (not result.checksum_ok);
  expect "one application start in report-only mode" (result.app_start_count = 1);

  printf "\n-- test 6: application stalls propagate through UDP and IPv4 --\n";
  let payload = List.init 37 ~f:(fun k -> ((k * 29) + 3) land 0xFF) in
  let result = Drop_bad_checksum.run ~stall_every:3 (make payload) in
  expect "payload survives end-to-end backpressure"
    (List.equal Int.equal result.payload payload);
  expect "application stall was exercised" result.saw_app_stall;
  expect "IPv4 input ready is low throughout application stalls"
    result.ready_low_on_all_stalls;
  expect "single tfirst under stalls" (result.tfirst_count = 1);
  expect "tlast remains on the final stalled payload byte"
    (List.equal Int.equal result.tlast_indices [ List.length payload - 1 ]);

  printf "\n-- test 7: TX-format golden vector round-trips through RX --\n";
  (* These are the Udp_mac_top TX endpoint constants. This builder is independent
     of both RX parsers and matches the TX integration golden. *)
  let payload = List.init 18 ~f:(fun k -> (0x40 + k) land 0xFF) in
  let result = Drop_bad_checksum.run (make payload) in
  expect "TX-format payload is recovered"
    (List.equal Int.equal result.payload payload);
  expect_metadata
    result
    ~src_port
    ~dst_port
    ~payload
    ~udp_checksum:0
    ~src_ip
    ~dst_ip;

  printf
    "\n==== SUMMARY: %d/%d checks passed ====\n"
    !pass_count
    !test_count;
  print_endline "\n=== SIMULATION COMPLETE ===";
  if !pass_count <> !test_count
  then failwith "IPv4+UDP RX integration failures"
