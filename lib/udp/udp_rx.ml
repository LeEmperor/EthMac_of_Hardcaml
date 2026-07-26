(*
  Bohdan Purtell
  University of Florida

  Module: Udp_rx

  UDP (OSI layer 4) receive header parser. The RX-side mirror of Udp_tx, and the
  layer *above* Ipv4_rx:

      Ipv4_rx ─(UDP datagram byte stream + IP metadata)→ Udp_rx ─→ application

  Ipv4_rx has already stripped the Ethernet + IPv4 headers and hands up the UDP
  *datagram* (8-byte UDP header ++ application data) as an AXI-Stream, plus the L4
  metadata it parsed out of the IP header:

    - [rx_tfirst]   : 1-transfer SOF pulse on the first datagram byte (= UDP
                      header byte 0).
    - [ip_protocol] : IP protocol number, stable per frame. Only 17 (UDP) is
                      parsed here; anything else is flushed (a UDP block must
                      never emit TCP/ICMP payload).
    - [ip_src_ip]/[ip_dst_ip] : peer/local addresses, stable per frame. Passed
                      straight through so the application learns the sender.

  What it does, mirroring Udp_tx in reverse:

  - Parses the fixed 8-byte UDP header. One monotonic byte counter walks the
    header; fields (src_port, dst_port, length, checksum) are latched by offset,
    big-endian, via [accum] — the same shift-in helper Ipv4_rx uses.
  - Strips the header and re-emits the application payload byte stream unchanged,
    with the UDP length field driving [m_tlast] (independent of the IP-level
    length, so a short UDP length inside a larger IP payload is still honoured).
  - Surfaces the metadata the application needs: [src_port]/[dst_port],
    [udp_length]/[payload_length], [src_ip]/[dst_ip], plus an [app_start] SOF
    pulse — the RX mirror of the {start, payload_len} contract Udp_tx consumes.

  Port policy (Config):
    - [drop_on_port_mismatch = false] (default): accept every datagram, forward
      its payload, and simply *report* src/dst port. [port_match] is still
      computed against [expected_dst_port] as an informational flag.
    - [drop_on_port_mismatch = true]: behave like a bound socket — datagrams whose
      dst_port /= [expected_dst_port] are dropped in Flush and never reach the app.

  UDP checksum: latched and exposed raw as [udp_checksum]; NOT verified yet.
  [checksum_ok] is a stub (always high = "not enforced"). A real check is a
  one's-complement sum over the IPv4 pseudo-header ([src_ip]/[dst_ip]/protocol/
  udp_length — all available from Ipv4_rx) plus the whole datagram; TX currently
  emits 0x0000 (checksum disabled) so this is inert for now. See TODO below and
  IPV4_LAYER_SPLIT_PLAN.md.

  Endpoints are NOT parameterized as addresses (an RX parser reports whatever
  arrives); [Make(Config)] carries only the port-filter policy + debug knobs.
*)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits

let () = Stdio.print_endline "=== Imported UDP RX ==="

let udp_hdr_len = 8
let ip_proto_udp = 17

module type Config = sig
  (* If true, datagrams whose dst_port /= [expected_dst_port] are dropped (payload
     never reaches the application). If false, every datagram is forwarded and
     [port_match] just reports the comparison — useful for bring-up. *)
  val drop_on_port_mismatch : bool
  val expected_dst_port : int

  (* keep-folding of internal debug signals into [keep] so synthesis can't prune
     them from the netlist / waveform. *)
  val debug : bool
end

module Make (C : Config) = struct
  module I = struct
    type 'a t = {
      clock : 'a;
      reset : 'a;
      en    : 'a;

      (* from Ipv4_rx.m_axis (UDP datagram byte stream + sideband) *)
      rx_tdata  : 'a [@bits 8];
      rx_tvalid : 'a;
      rx_tlast  : 'a;               (* final datagram byte of the frame *)
      rx_tuser  : 'a;               (* CRC/error flag, valid on rx_tlast *)
      rx_tfirst : 'a;              (* SOF: first datagram byte (= UDP header byte 0) *)

      (* IP metadata, stable per frame (from Ipv4_rx) *)
      ip_protocol : 'a [@bits 8];
      ip_src_ip   : 'a [@bits 32];
      ip_dst_ip   : 'a [@bits 32];

      (* frame-level late status from Ipv4_rx (see Ipv4_rx.O.frame_done). Carries
         the FCS/CRC verdict that cannot ride the payload tlast — forwarded up to
         the application unchanged. *)
      ip_frame_done  : 'a;
      ip_frame_error : 'a;

      (* backpressure from the application *)
      app_tready : 'a;
    } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      (* backpressure up to IPv4 *)
      m_axis_tready : 'a;

      (* stripped application payload byte stream out *)
      m_tdata  : 'a [@bits 8];
      m_tvalid : 'a;
      m_tlast  : 'a;                (* on the final application byte *)
      m_tfirst : 'a;              (* SOF pulse on the first application byte *)

      (* metadata — RX mirror of the Udp_tx {start, payload_len} contract *)
      app_start      : 'a;         (* pulse with m_tfirst, or alone for an empty
                                      datagram; latch metadata here *)
      src_port       : 'a [@bits 16];
      dst_port       : 'a [@bits 16];
      udp_length     : 'a [@bits 16]; (* UDP length field (8 + app data) *)
      payload_length : 'a [@bits 16]; (* udp_length - 8 *)
      udp_checksum   : 'a [@bits 16]; (* raw header field; NOT verified (see stub) *)
      src_ip         : 'a [@bits 32]; (* passthrough from IPv4 *)
      dst_ip         : 'a [@bits 32];

      (* per-frame status *)
      port_match  : 'a;            (* dst_port == expected_dst_port (informational) *)
      checksum_ok : 'a;            (* STUB: always high — checksum not enforced yet *)
      crc_error   : 'a;            (* IPv4/MAC reported a bad frame (rx_tuser) — note:
                                      this legacy tlast-aligned flag misses the FCS
                                      verdict for padded frames; use the frame-level
                                      [frame_done]/[frame_error] channel below. *)
      busy        : 'a;

      (* frame-level late status, forwarded from Ipv4_rx (see Ipv4_rx.O.frame_done).
         Latch [frame_error] on the [frame_done] pulse to get the app-visible
         bad-frame (FCS/CRC) verdict, valid even when MAC padding delays it past
         the payload tlast. *)
      frame_done  : 'a;
      frame_error : 'a;

      keep : 'a;
    } [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Header
      | Payload
      | Flush                      (* swallow rest of datagram, emit nothing *)
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module I_Regs = struct
    type 'a t = {
      hdr_counter : 'a [@bits 4];   (* 0..7 header byte index *)
      src_port    : 'a [@bits 16];  (* bytes 0..1 *)
      dst_port    : 'a [@bits 16];  (* bytes 2..3 *)
      length      : 'a [@bits 16];  (* bytes 4..5 (8 + app data) *)
      checksum    : 'a [@bits 16];  (* bytes 6..7 *)
      payload_len : 'a [@bits 16];  (* latched length - 8 (stable metadata) *)
      payload_rem : 'a [@bits 16];  (* app-payload bytes still to forward *)
      crc_err     : 'a;             (* latched from rx_tuser at rx_tlast *)
      first_pend  : 'a;             (* drives the first-payload-byte SOF pulse *)
      empty_start : 'a;             (* one-cycle metadata pulse for length = 8 *)
      busy        : 'a;
    } [@@deriving hardcaml]
  end

  module I_Wires = struct
    type 'a t = {
      m_ready : 'a;                 (* -> Ipv4_rx l4_tready *)
      tvalid  : 'a;
      tlast   : 'a;
      tfirst  : 'a;
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

    let byte = i.I.rx_tdata in
    let idx  = r.hdr_counter.value in

    (* shift a byte into a wider big-endian field register (lifted from udp.ml /
       shared with ipv4_rx). *)
    let accum (reg : Always.Variable.t) b =
      reg <-- (select reg.value ~high:(width reg.value - 9) ~low:0) @: b
    in

    let is_udp = i.I.ip_protocol ==:. ip_proto_udp in
    let at_hdr_end = idx ==:. udp_hdr_len - 1 in
    (* payload length computed at header end from the latched UDP length field *)
    let has_payload = r.length.value >:. udp_hdr_len in
    let empty_payload = r.length.value ==:. udp_hdr_len in
    let payload_len_next = r.length.value -:. udp_hdr_len in
    (* dst_port is fully latched by idx==3, so it is valid to test at header end *)
    let port_ok = r.dst_port.value ==:. C.expected_dst_port in
    let keep_frame = if C.drop_on_port_mismatch then port_ok else vdd in

    compile
      [ (* defaults *)
        w.m_ready <--. 0
      ; w.tvalid  <--. 0
      ; w.tlast   <--. 0
      ; w.tfirst  <--. 0
      ; r.busy    <-- r.busy.value
      ; r.empty_start <--. 0

      ; sm.switch ~default:[]
          [ ( Idle
            , [ w.m_ready <--. 1                          (* ready to accept the SOF byte *)
              ; when_ (i.I.rx_tvalid &: i.I.rx_tfirst)
                  [ r.busy        <--. 1
                  ; r.crc_err     <--. 0
                  ; r.first_pend  <--. 0
                  ; accum r.src_port byte                 (* header byte 0 = src_port high *)
                  ; r.hdr_counter <--. 1
                  ; if_ is_udp
                      [ sm.set_next Header ]
                      [ sm.set_next Flush ]               (* not UDP: drop the datagram *)
                  ; when_ i.I.rx_tlast                    (* 1-byte runt frame: abort *)
                      [ r.busy <--. 0; sm.set_next Idle ]
                  ]
              ] )

          ; ( Header
            , [ w.m_ready <--. 1                          (* header bytes always accepted *)
              ; when_ i.I.rx_tvalid
                  [ (* latch the fields by offset (big-endian) *)
                    when_ (idx ==:. 1) [ accum r.src_port byte ]
                  ; when_ ((idx ==:. 2) |: (idx ==:. 3)) [ accum r.dst_port byte ]
                  ; when_ ((idx ==:. 4) |: (idx ==:. 5)) [ accum r.length byte ]
                  ; when_ ((idx ==:. 6) |: (idx ==:. 7)) [ accum r.checksum byte ]
                  ; if_ at_hdr_end
                      [ r.payload_len <-- payload_len_next
                      ; r.payload_rem <-- payload_len_next
                      ; r.first_pend  <--. 1
                      ; when_ (empty_payload &: keep_frame)
                          [ r.empty_start <--. 1 ]
                      ; if_ (has_payload &: keep_frame)
                          [ sm.set_next Payload ]
                          [ (* empty datagram or port-filtered: no app output *)
                            if_ i.I.rx_tlast
                              [ r.busy <--. 0; sm.set_next Idle ]
                              [ sm.set_next Flush ]
                          ]
                      ]
                      [ r.hdr_counter <-- idx +:. 1
                        (* frame truncated inside the header: abort *)
                      ; when_ i.I.rx_tlast [ r.busy <--. 0; sm.set_next Idle ]
                      ]
                  ]
              ] )

          ; ( Payload
            , [ w.m_ready <-- i.I.app_tready               (* forward app backpressure *)
              ; w.tvalid  <-- i.I.rx_tvalid
              ; w.tfirst  <-- (i.I.rx_tvalid &: r.first_pend.value)
              ; w.tlast   <-- (i.I.rx_tvalid &: (r.payload_rem.value ==:. 1))
              ; when_ (i.I.rx_tvalid &: i.I.app_tready)
                  [ r.first_pend  <--. 0
                  ; r.payload_rem <-- r.payload_rem.value -:. 1
                  ; when_ i.I.rx_tlast [ r.crc_err <-- i.I.rx_tuser ]
                  ; if_ (r.payload_rem.value ==:. 1)
                      [ (* last application byte forwarded *)
                        if_ i.I.rx_tlast
                          [ r.busy <--. 0; sm.set_next Idle ]
                          [ sm.set_next Flush ]            (* drop any trailing bytes *)
                      ]
                      [ (* frame ended before UDP length: truncated datagram *)
                        when_ i.I.rx_tlast
                          [ r.crc_err <--. 1; r.busy <--. 0; sm.set_next Idle ]
                      ]
                  ]
              ] )

          ; ( Flush
            , [ w.m_ready <--. 1                          (* swallow, emit nothing *)
              ; when_ (i.I.rx_tvalid &: i.I.rx_tlast)
                  [ r.busy <--. 0; sm.set_next Idle ]
              ] )
          ]
      ];

    (* TODO(udp-rx checksum): verify the UDP checksum. Build the IPv4 pseudo-header
       from {ip_src_ip, ip_dst_ip, 0x0011 (proto), udp_length} and one's-complement
       sum it together with the whole datagram (streamed in Payload); a valid
       datagram folds to 0xFFFF, and a received checksum field of 0x0000 means the
       sender disabled it (skip the check, report ok). For now checksum_ok is a
       stub so downstream can be wired up; TX emits 0x0000 today. *)
    let checksum_ok = vdd in

    let keep =
      if C.debug
      then
        reduce ~f:( |: )
          (bits_lsb r.src_port.value @ bits_lsb r.dst_port.value
           @ bits_lsb r.length.value @ bits_lsb r.checksum.value
           @ [ r.crc_err.value; r.busy.value ])
      else gnd
    in

    { O.m_axis_tready = w.m_ready.value
    ; m_tdata        = i.I.rx_tdata
    ; m_tvalid       = w.tvalid.value
    ; m_tlast        = w.tlast.value
    ; m_tfirst       = w.tfirst.value
    ; app_start      = w.tfirst.value |: r.empty_start.value
    ; src_port       = r.src_port.value
    ; dst_port       = r.dst_port.value
    ; udp_length     = r.length.value
    ; payload_length = r.payload_len.value
    ; udp_checksum   = r.checksum.value
    ; src_ip         = i.I.ip_src_ip                       (* passthrough (stable per frame) *)
    ; dst_ip         = i.I.ip_dst_ip
    ; port_match     = port_ok
    ; checksum_ok
    ; crc_error      = r.crc_err.value
    ; busy           = r.busy.value
    (* forward the frame-level late status straight up (UDP adds no FCS verdict of
       its own; the bad-frame signal originates at the MAC/IPv4 boundary) *)
    ; frame_done     = i.I.ip_frame_done
    ; frame_error    = i.I.ip_frame_error
    ; keep
    }
  ;;
end
