(*
  Bohdan Purtell
  University of Florida

  Module: Udp_tx

  UDP (OSI layer 4) transmit header generator. Emits the UDP datagram byte
  stream — 8-byte UDP header ++ application data — DOWN to the IPv4 layer
  (Ipv4_tx), which prepends the IP header; the MAC then wraps with Ethernet.

      app data ─→ Udp_tx ─(UDP datagram bytes + metadata)→ Ipv4_tx ─→ MAC

  This used to be a fused IPv4+UDP generator (one 28-byte header mux). The IPv4
  half was lifted into Ipv4_tx; what remains here is just the 8-byte UDP header
  and the payload passthrough, plus the small metadata IPv4 needs to build its
  header ([l4_length], [protocol]) and the start pulse it forwards.

  - ONE monotonic byte counter over the fixed 8-byte UDP header, decoded by
    offset into an 8-entry byte mux. FSM is Idle -> Header -> Payload.

  - Framing: Udp_tx owns the datagram's [m_tlast] — it asserts tlast on its
    final byte (last app-data byte, or the 8th header byte for a zero-length
    datagram). Ipv4_tx forwards that tlast to the MAC.

  - UDP checksum over IPv4 is optional and emitted as 0x0000 for now. A real
    checksum needs the IPv4 pseudo-header (src/dst IP + protocol + udp_length);
    when implemented, take those from Ipv4_tx rather than duplicating IP
    constants here — see IPV4_LAYER_SPLIT_PLAN.md.

  Endpoints (src/dst port) are elaboration-time constants via [Make(Config)].
*)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits

let () = Stdio.print_endline "=== Imported UDP TX ==="

let udp_hdr_len = 8
let ip_proto_udp = 17

module type Config = sig
  val src_port : int
  val dst_port : int
end

module Make (C : Config) = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      en    : 'a;

      start       : 'a;               (* pulse: begin a datagram *)
      payload_len : 'a [@bits 16];    (* application-data length, latched at [start] *)

      (* application-data stream in *)
      payload_tdata  : 'a [@bits 8];
      payload_tvalid : 'a;

      (* backpressure from downstream (Ipv4_tx l4_tready) *)
      l4_tready : 'a;
    } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      (* down to IPv4: datagram byte stream + the metadata IP needs *)
      ip_start  : 'a;                 (* -> Ipv4_tx.start *)
      l4_length : 'a [@bits 16];      (* UDP total length = 8 + payload *)
      protocol  : 'a [@bits 8];       (* 17 = UDP *)
      m_tdata   : 'a [@bits 8];
      m_tvalid  : 'a;
      m_tlast   : 'a;                 (* asserted on final datagram byte *)

      (* up to application *)
      payload_tready : 'a;            (* backpressure -> application source *)
      busy           : 'a;

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
      hdr_counter : 'a [@bits 4];       (* 0..7 header byte index *)
      payload_rem : 'a [@bits 16];      (* payload bytes still to send *)
      len_latch   : 'a [@bits 16];      (* payload_len latched at start *)
      busy        : 'a;
    } [@@deriving hardcaml]
  end

  module I_Wires = struct
    type 'a t = {
      tvalid  : 'a;
      tlast   : 'a;
      p_ready : 'a;
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

    (* UDP length = 8 + application data (latched) *)
    let payload_len = r.len_latch.value in
    let udp_length  = payload_len +:. udp_hdr_len -- "udp_len" in

    (* ── the 8-byte UDP header, wire order → byte mux indexed by hdr_counter ── *)
    let header_bytes =
      [ const8 (C.src_port lsr 8); const8 (C.src_port land 0xff)
      ; const8 (C.dst_port lsr 8); const8 (C.dst_port land 0xff)
      ; hi16 udp_length; lo16 udp_length
      ; const8 0x00; const8 0x00 (* UDP checksum = 0 (optional over IPv4) *)
      ]
    in
    assert (List.length header_bytes = udp_hdr_len);
    let header_byte = mux r.hdr_counter.value header_bytes -- "udp_header_byte" in

    let start          = i.I.start in
    let l4_tready      = i.I.l4_tready in
    let payload_tvalid = i.I.payload_tvalid in

    compile
      [ (* defaults *)
        w.tvalid  <--. 0
      ; w.tlast   <--. 0
      ; w.p_ready <--. 0
      ; r.busy    <-- r.busy.value

      ; sm.switch ~default:[]
          [ ( Idle
            , [ when_ start
                  [ r.busy        <--. 1
                  ; r.hdr_counter <--. 0
                  ; r.len_latch   <-- i.I.payload_len
                  ; r.payload_rem <-- i.I.payload_len
                  ; sm.set_next Header
                  ]
              ] )

          ; ( Header
            , [ w.tvalid <--. 1                          (* header byte always valid *)
              ; when_
                  ((r.hdr_counter.value ==:. udp_hdr_len - 1)
                   &: (r.payload_rem.value ==:. 0))
                  [ w.tlast <--. 1 ]                    (* describe the final beat even while stalled *)
              ; when_ l4_tready                           (* advance only when accepted *)
                  [ if_ (r.hdr_counter.value ==:. udp_hdr_len - 1)
                      [ r.hdr_counter <--. 0
                      ; if_ (r.payload_rem.value ==:. 0)  (* zero-length datagram *)
                          [ r.busy <--. 0; sm.set_next Idle ]
                          [ sm.set_next Payload ]
                      ]
                      [ r.hdr_counter <-- r.hdr_counter.value +:. 1 ]
                  ]
              ] )

          ; ( Payload
            , [ w.p_ready <-- l4_tready                   (* forward backpressure *)
              ; w.tvalid  <-- payload_tvalid
              ; w.tlast   <-- (payload_tvalid &: (r.payload_rem.value ==:. 1))
              ; when_ (payload_tvalid &: l4_tready)
                  [ if_ (r.payload_rem.value ==:. 1)
                      [ r.payload_rem <--. 0
                      ; r.busy     <--. 0
                      ; sm.set_next Idle
                      ]
                      [ r.payload_rem <-- r.payload_rem.value -:. 1 ]
                  ]
              ] )
          ]
      ];

    let out_byte = mux2 (sm.is Payload) i.I.payload_tdata header_byte in

    let keep =
      reduce ~f:( |: ) (bits_lsb out_byte @ [ r.busy.value ])
    in

    { O.ip_start  = start                              (* forward datagram-start to L3 *)
    ; l4_length  = i.I.payload_len +:. udp_hdr_len     (* combinational from input: valid at [start] *)
    ; protocol   = const8 ip_proto_udp
    ; m_tdata    = out_byte
    ; m_tvalid   = w.tvalid.value
    ; m_tlast    = w.tlast.value
    ; payload_tready = w.p_ready.value
    ; busy       = r.busy.value
    ; keep
    }
  ;;
end
