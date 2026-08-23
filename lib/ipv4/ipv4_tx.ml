(*
  Bohdan Purtell
  University of Florida

  Module: Ipv4_tx

  IPv4 (OSI layer 3) transmit header generator. Sits *between* a layer-4
  protocol (UDP/TCP) and the MII MAC:

      L4 (Udp_tx/Tcp_tx) ─(payload byte stream + metadata)→ Ipv4_tx ─→ MAC.s_axis

  This is the shared L3 block lifted out of the previously-fused udp_tx: it
  prepends the 20-byte IPv4 header (with a correct header checksum) to whatever
  byte stream layer 4 hands down, then streams that payload through unchanged.
  The MAC still wraps the result with the Ethernet header + FCS.

  Design decisions (carried over from the fused udp_tx):

  - ONE monotonic byte counter over the fixed 20-byte IPv4 header, decoded by
    offset into a 20-entry byte mux (a small ROM). Field boundaries live in the
    mux, not the FSM, so the FSM is just Idle -> Header -> Payload.

  - The IPv4 header checksum is mandatory and lives *before* the addresses it
    covers, so it can't be produced streaming. It is computed combinationally
    from the (latched) [l4_length] + constant fields (src/dst IP from [Config],
    protocol from the runtime input). Only [total_length] and [protocol] are
    dynamic.

  - Framing is single-source-of-truth: layer 4 asserts [l4_tlast] on the final
    byte of its stream and Ipv4_tx forwards it as [m_tlast]. Ipv4_tx does NOT
    re-count the payload; [l4_length] feeds only the IPv4 total_length/checksum.

  Endpoints (src/dst IP) are elaboration-time constants via [Make(Config)].
*)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits

let ip_hdr_len = 20

module type Config = sig
  val src_ip : int list (* 4 bytes, network order *)
  val dst_ip : int list
end

module Make (C : Config) = struct
  let () = assert (List.length C.src_ip = 4)
  let () = assert (List.length C.dst_ip = 4)

  module I = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      en    : 'a;

      start     : 'a;               (* pulse: begin an IP datagram *)
      l4_length : 'a [@bits 16];    (* layer-4 total length, latched at [start] *)
      protocol  : 'a [@bits 8];     (* IP protocol number: 17 UDP, 6 TCP *)

      (* layer-4 payload byte stream in *)
      l4_tdata  : 'a [@bits 8];
      l4_tvalid : 'a;
      l4_tlast  : 'a;               (* asserted on the final L4 byte of the datagram *)

      (* backpressure from downstream (MAC s_axis_tready) *)
      mac_tready : 'a;
    } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      (* Ethernet-payload byte stream out -> MAC s_axis *)
      m_tdata  : 'a [@bits 8];
      m_tvalid : 'a;
      m_tlast  : 'a;

      tx_start  : 'a;               (* -> MAC tx_controller.start *)
      l4_tready : 'a;               (* backpressure -> layer 4 *)
      busy      : 'a;

      keep : 'a;
    } [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Header
      | Payload
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module I_Regs = struct
    type 'a t = {
      hdr_counter : 'a [@bits 5];   (* 0..19 header byte index *)
      len_latch   : 'a [@bits 16];  (* l4_length latched at start *)
      busy        : 'a;
    } [@@deriving hardcaml]
  end

  module I_Wires = struct
    type 'a t = {
      tvalid   : 'a;
      tlast    : 'a;
      tx_start : 'a;
      l4_ready : 'a;
    } [@@deriving hardcaml]
  end

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =
    let open Always in
    let open Variable in

    let rising_edge = Reg_spec.create ~clock:i.I.clock ~clear:i.I.reset () in
    let sm = State_machine.create (module States) ~enable:vdd rising_edge in

    let r = I_Regs.Of_always.reg ~enable:vdd rising_edge in
    I_Regs.Of_always.apply_names ~prefix:"reg_" ~naming_op:(Scope.naming scope) r;

    let w = I_Wires.Of_always.wire Signal.zero in
    I_Wires.Of_always.apply_names ~prefix:"wire_" ~naming_op:(Scope.naming scope) w;

    (* IP total length = 20 + L4 length; total_length and protocol are the only
       dynamic inputs to the checksum. *)
    let l4_length    = r.len_latch.value in
    let total_length = l4_length +:. ip_hdr_len -- "ip_total_len" in
    let protocol     = i.I.protocol in
    let ttl_proto    = const8 0x40 @: protocol in   (* TTL=64 :: protocol *)

    (* ── IPv4 header checksum: one's-complement 16-bit sum of the header words
       (checksum field taken as 0), end-around carry, then complemented. ── *)
    let ip_word_of_bytes b0 b1 = of_hex ~width:16 (sprintf "%02x%02x" b0 b1) in
    let ip_hdr_words =
      [ of_hex ~width:16 "4500"        (* version/IHL=0x45, DSCP/ECN=0 *)
      ; total_length
      ; zero 16                        (* identification *)
      ; of_hex ~width:16 "4000"        (* flags: DF, frag offset=0 *)
      ; ttl_proto                      (* TTL=64 / protocol *)
      ; zero 16                        (* checksum placeholder *)
      ; ip_word_of_bytes (List.nth_exn C.src_ip 0) (List.nth_exn C.src_ip 1)
      ; ip_word_of_bytes (List.nth_exn C.src_ip 2) (List.nth_exn C.src_ip 3)
      ; ip_word_of_bytes (List.nth_exn C.dst_ip 0) (List.nth_exn C.dst_ip 1)
      ; ip_word_of_bytes (List.nth_exn C.dst_ip 2) (List.nth_exn C.dst_ip 3)
      ]
    in
    (* sum of 10 words fits comfortably in 20 bits *)
    let raw_sum =
      List.fold ip_hdr_words ~init:(zero 20) ~f:(fun a wd -> a +: uresize wd ~width:20)
    in
    (* collapse bits above 16 back into the low 16 (end-around carry); applying
       twice is sufficient for a 20-bit input *)
    let fold_carry s =
      let lo = uresize (select s ~high:15 ~low:0) ~width:17 in
      let hi = uresize (select s ~high:(width s - 1) ~low:16) ~width:17 in
      lo +: hi
    in
    let ip_checksum = ~:(select (fold_carry (fold_carry raw_sum)) ~high:15 ~low:0)
                      -- "ip_checksum" in

    (* ── the 20-byte IPv4 header, wire order → byte mux indexed by hdr_counter ── *)
    let header_bytes =
      [ const8 0x45; const8 0x00; hi16 total_length; lo16 total_length
      ; const8 0x00; const8 0x00 (* identification *)
      ; const8 0x40; const8 0x00 (* flags/frag: DF *)
      ; const8 0x40; protocol    (* TTL / protocol *)
      ; hi16 ip_checksum; lo16 ip_checksum
      ]
      @ List.map C.src_ip ~f:const8
      @ List.map C.dst_ip ~f:const8
    in
    assert (List.length header_bytes = ip_hdr_len);
    let header_byte = mux r.hdr_counter.value header_bytes -- "ip_header_byte" in

    let start      = i.I.start in
    let mac_tready = i.I.mac_tready in

    compile
      [ (* defaults *)
        w.tvalid   <--. 0
      ; w.tlast    <--. 0
      ; w.tx_start <--. 0
      ; w.l4_ready <--. 0
      ; r.busy     <-- r.busy.value

      ; sm.switch ~default:[]
          [ ( Idle
            , [ when_ start
                  [ r.busy        <--. 1
                  ; r.hdr_counter <--. 0
                  ; r.len_latch   <-- i.I.l4_length
                  ; w.tx_start    <--. 1   (* open a frame at the MAC *)
                  ; sm.set_next Header
                  ]
              ] )

          ; ( Header
            , [ w.tvalid <--. 1                          (* header byte always valid *)
              ; when_ mac_tready                          (* advance only when accepted *)
                  [ if_ (r.hdr_counter.value ==:. ip_hdr_len - 1)
                      [ r.hdr_counter <--. 0; sm.set_next Payload ]
                      [ r.hdr_counter <-- r.hdr_counter.value +:. 1 ]
                  ]
              ] )

          ; ( Payload
            , [ w.l4_ready <-- mac_tready                  (* forward backpressure to L4 *)
              ; w.tvalid   <-- i.I.l4_tvalid
              ; w.tlast    <-- (i.I.l4_tvalid &: i.I.l4_tlast)
              ; when_ (i.I.l4_tvalid &: mac_tready &: i.I.l4_tlast)
                  [ r.busy <--. 0; sm.set_next Idle ]
              ] )
          ]
      ];

    let out_byte = mux2 (sm.is Payload) i.I.l4_tdata header_byte in

    let keep =
      reduce ~f:( |: ) (bits_lsb out_byte @ bits_lsb ip_checksum @ [ r.busy.value ])
    in

    { O.m_tdata   = out_byte
    ; m_tvalid   = w.tvalid.value
    ; m_tlast    = w.tlast.value
    ; tx_start   = w.tx_start.value
    ; l4_tready  = w.l4_ready.value
    ; busy       = r.busy.value
    ; keep
    }
  ;;
end
