(*
  Bohdan Purtell
  University of Florida

  Module: Ipv4_rx

  IPv4 (OSI layer 3) receive header parser. Sits *between* the MII MAC (L2) and a
  layer-4 protocol (UDP/TCP):

      Mac_top.m_axis ─(Eth payload byte stream + sideband)→ Ipv4_rx ─→ L4 (Udp_rx/…)

  The MAC hands up the Ethernet *payload* as an AXI-Stream (m_axis_tdata/tvalid/
  tlast/tuser) plus two sidebands this block relies on:

    - [rx_tfirst]  : 1-transfer SOF pulse on the first payload byte of a frame,
                     which — for an IPv4 frame — is IPv4 header byte 0.
    - [rx_eth_type]: the latched Ethernet type, stable per frame. Used to filter:
                     only 0x0800 (IPv4) frames are parsed; everything else is
                     flushed.

  What it does, mirroring Ipv4_tx in reverse:

  - Parses the fixed 20-byte IPv4 header (IHL=5 only for now — options / IHL>5
    are dropped; see TODO). One monotonic byte counter walks the header; fields
    are latched by offset.
  - Verifies the IPv4 header checksum incrementally (one's-complement 16-bit sum
    over the 20 header bytes, end-around carry; a correct header folds to 0xFFFF).
  - Strips the header and re-emits the L4 payload byte stream unchanged, with the
    IP payload length driving [m_tlast] (so the MAC's Ethernet zero-padding on
    short frames is dropped rather than leaked to L4).
  - Surfaces the L4 metadata the layer above needs: [protocol], [payload_length],
    [src_ip]/[dst_ip], plus an [l4_start] SOF pulse — the RX-side mirror of the
    {start, l4_length, protocol} contract Ipv4_tx consumes on TX.

  Backpressure: [l4_tready] from L4 is forwarded up to the MAC as [m_axis_tready]
  during payload; header and flush bytes are always accepted (no output to stall).

  Endpoints are NOT parameterized here: an RX parser accepts whatever addresses
  arrive and reports them. [Make(Config)] only carries elaboration-time knobs
  (checksum enforcement, debug keep-folding).
*)

open! Core
open! Hardcaml
open! Signal
open! Helper_circuits

let () = Stdio.print_endline "=== Imported IPv4 RX ==="

let ip_hdr_len = 20
let ipv4_ethertype = 0x0800

module type Config = sig
  (* If true, a frame whose header checksum fails to verify is dropped (payload
     never reaches L4). If false, the payload is still forwarded and [checksum_ok]
     simply reports the result — useful for bring-up / lossy-link debugging. *)
  val drop_on_bad_checksum : bool

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

      (* from Mac_top.m_axis (Ethernet-payload byte stream + sideband) *)
      rx_tdata    : 'a [@bits 8];
      rx_tvalid   : 'a;
      rx_tlast    : 'a;             (* final Ethernet-payload byte of the frame *)
      rx_tuser    : 'a;             (* CRC error flag, valid on rx_tlast *)
      rx_tfirst   : 'a;            (* SOF: first payload byte (= IP header byte 0) *)
      rx_eth_type : 'a [@bits 16]; (* latched Ethernet type, stable per frame *)

      (* backpressure from downstream L4 *)
      l4_tready : 'a;
    } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = {
      (* backpressure up to the MAC *)
      m_axis_tready : 'a;

      (* stripped L4 payload byte stream out *)
      m_tdata  : 'a [@bits 8];
      m_tvalid : 'a;
      m_tlast  : 'a;                (* on the final IP-payload byte (padding dropped) *)
      m_tfirst : 'a;               (* SOF pulse on the first L4 payload byte *)

      (* L4 metadata — RX mirror of the Ipv4_tx {start,l4_length,protocol} contract *)
      l4_start       : 'a;         (* pulse alongside m_tfirst; latch metadata here *)
      protocol       : 'a [@bits 8];
      payload_length : 'a [@bits 16]; (* IP total_length - 20 *)
      src_ip         : 'a [@bits 32];
      dst_ip         : 'a [@bits 32];

      (* per-frame status (valid from Header end through the frame) *)
      checksum_ok : 'a;            (* IPv4 header checksum verified *)
      crc_error   : 'a;            (* MAC reported a bad Ethernet FCS (rx_tuser) *)
      busy        : 'a;

      (* frame-level late-status channel (decoupled from the payload tlast). The
         FCS/CRC verdict for a frame only becomes known at the MAC's real
         end-of-frame — which, with MAC zero-padding, lands AFTER this block's
         m_tlast. So it cannot ride the payload stream; it is delivered here as a
         1-cycle [frame_done] pulse (the MAC's rx_tlast, in any state) carrying the
         [frame_error] verdict, for a consumer to latch. *)
      frame_done  : 'a;            (* pulse on the final Ethernet-frame byte *)
      frame_error : 'a;            (* valid with frame_done: bad FCS (rx_tuser) *)

      keep : 'a;
    } [@@deriving hardcaml]
  end

  module States = struct
    type t =
      | Idle
      | Header
      | Payload
      | Flush                      (* swallow rest of frame, emit nothing *)
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module I_Regs = struct
    type 'a t = {
      hdr_counter : 'a [@bits 5];   (* 0..19 header byte index *)
      total_len   : 'a [@bits 16];  (* IPv4 total_length (bytes 2..3) *)
      payload_len : 'a [@bits 16];  (* latched total_len - 20 (stable metadata) *)
      payload_rem : 'a [@bits 16];  (* IP-payload bytes still to forward *)
      protocol    : 'a [@bits 8];   (* byte 9 *)
      version_ihl : 'a [@bits 8];   (* byte 0 *)
      src_ip      : 'a [@bits 32];  (* bytes 12..15 *)
      dst_ip      : 'a [@bits 32];  (* bytes 16..19 *)
      csum_acc    : 'a [@bits 20];  (* running one's-complement sum of header words *)
      csum_ok     : 'a;             (* latched at header end *)
      crc_err     : 'a;             (* latched from rx_tuser at rx_tlast *)
      first_pend  : 'a;             (* drives the first-payload-byte SOF pulse *)
      busy        : 'a;
    } [@@deriving hardcaml]
  end

  module I_Wires = struct
    type 'a t = {
      m_ready  : 'a;                (* -> MAC m_axis_tready *)
      tvalid   : 'a;
      tlast    : 'a;
      tfirst   : 'a;
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

    (* one's-complement running sum: even header offsets carry the high byte of a
       16-bit word, odd offsets the low byte. byte 0 is always the high byte, so
       its increment is computed directly (no reliance on the counter). *)
    let hi_inc = uresize (byte @: const8 0) ~width:20 in   (* byte << 8 *)
    let lo_inc = uresize byte ~width:20 in
    let word_inc = mux2 (lsb idx) lo_inc hi_inc in          (* idx odd -> low byte *)
    let next_sum = r.csum_acc.value +: word_inc in

    (* fold the end-around carry (>=16 bits) back into the low 16; twice is
       sufficient for a 20-bit accumulator. A valid header folds to 0xFFFF. *)
    let fold_carry s =
      let lo = uresize (select s ~high:15 ~low:0) ~width:17 in
      let hi = uresize (select s ~high:(width s - 1) ~low:16) ~width:17 in
      lo +: hi
    in
    let csum_folded = select (fold_carry (fold_carry next_sum)) ~high:15 ~low:0 in
    let csum_good   = (csum_folded ==:. 0xFFFF) -- "ip_csum_good" in

    (* shift a byte into a wider big-endian field register *)
    let accum (reg : Always.Variable.t) b =
      reg <-- (select reg.value ~high:(width reg.value - 9) ~low:0) @: b
    in

    let is_ipv4_hdr0 =
      (i.I.rx_eth_type ==:. ipv4_ethertype) &: (byte ==:. 0x45)  (* version 4, IHL 5 *)
    in
    let at_hdr_end = idx ==:. ip_hdr_len - 1 in
    (* payload length computed at header end from the latched total_length *)
    let has_payload = r.total_len.value >=:. ip_hdr_len in
    let payload_len_next = r.total_len.value -:. ip_hdr_len in
    (* whether the parsed header is acceptable to forward (elaboration-time policy) *)
    let keep_frame = if C.drop_on_bad_checksum then csum_good else vdd in

    compile
      [ (* defaults *)
        w.m_ready <--. 0
      ; w.tvalid  <--. 0
      ; w.tlast   <--. 0
      ; w.tfirst  <--. 0
      ; r.busy    <-- r.busy.value

      ; sm.switch ~default:[]
          [ ( Idle
            , [ w.m_ready <--. 1                          (* ready to accept the SOF byte *)
              ; when_ (i.I.rx_tvalid &: i.I.rx_tfirst)
                  [ r.busy        <--. 1
                  ; r.crc_err     <--. 0
                  ; r.csum_acc    <-- hi_inc              (* header byte 0 = high byte *)
                  ; r.version_ihl <-- byte
                  ; r.hdr_counter <--. 1
                  ; if_ is_ipv4_hdr0
                      [ sm.set_next Header ]
                      [ sm.set_next Flush ]               (* not IPv4 / options: drop *)
                  ; when_ i.I.rx_tlast                    (* 1-byte runt frame: abort *)
                      [ r.busy <--. 0; sm.set_next Idle ]
                  ]
              ] )

          ; ( Header
            , [ w.m_ready <--. 1                          (* header bytes always accepted *)
              ; when_ i.I.rx_tvalid
                  [ r.csum_acc <-- next_sum
                    (* latch the fields we care about, by offset *)
                  ; when_ ((idx ==:. 2) |: (idx ==:. 3)) [ accum r.total_len byte ]
                  ; when_ (idx ==:. 9) [ r.protocol <-- byte ]
                  ; when_ ((idx >=:. 12) &: (idx <=:. 15)) [ accum r.src_ip byte ]
                  ; when_ ((idx >=:. 16) &: (idx <=:. 19)) [ accum r.dst_ip byte ]
                  ; if_ at_hdr_end
                      [ r.csum_ok     <-- csum_good
                      ; r.payload_len <-- payload_len_next
                      ; r.payload_rem <-- payload_len_next
                      ; r.first_pend  <--. 1
                      ; if_ (has_payload &: keep_frame)
                          [ sm.set_next Payload ]
                          [ (* empty datagram or rejected: no L4 output *)
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
            , [ w.m_ready <-- i.I.l4_tready                (* forward L4 backpressure *)
              ; w.tvalid  <-- i.I.rx_tvalid
              ; w.tfirst  <-- (i.I.rx_tvalid &: r.first_pend.value)
              ; w.tlast   <-- (i.I.rx_tvalid &: (r.payload_rem.value ==:. 1))
              ; when_ (i.I.rx_tvalid &: i.I.l4_tready)
                  [ r.first_pend  <--. 0
                  ; r.payload_rem <-- r.payload_rem.value -:. 1
                  ; when_ i.I.rx_tlast [ r.crc_err <-- i.I.rx_tuser ]
                  ; if_ (r.payload_rem.value ==:. 1)
                      [ (* last IP-payload byte forwarded *)
                        if_ i.I.rx_tlast
                          [ r.busy <--. 0; sm.set_next Idle ]
                          [ sm.set_next Flush ]            (* drop Ethernet padding *)
                      ]
                      [ (* MAC ended the frame before total_length: truncated *)
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

    let keep =
      if C.debug
      then
        reduce ~f:( |: )
          (bits_lsb r.src_ip.value @ bits_lsb r.dst_ip.value
           @ bits_lsb r.total_len.value @ [ r.csum_ok.value; r.crc_err.value; r.busy.value ])
      else gnd
    in

    (* Frame-level late status: fire on the MAC's actual end-of-frame (rx_tlast,
       in ANY state — Payload for exact-fit frames, Flush while dropping padding),
       combinational so the verdict aligns with the byte that carries it. *)
    let frame_done  = i.I.rx_tvalid &: i.I.rx_tlast in
    let frame_error = i.I.rx_tuser in

    { O.m_axis_tready = w.m_ready.value
    ; m_tdata        = i.I.rx_tdata
    ; m_tvalid       = w.tvalid.value
    ; m_tlast        = w.tlast.value
    ; m_tfirst       = w.tfirst.value
    ; l4_start       = w.tfirst.value
    ; protocol       = r.protocol.value
    ; payload_length = r.payload_len.value
    ; src_ip         = r.src_ip.value
    ; dst_ip         = r.dst_ip.value
    ; checksum_ok    = r.csum_ok.value
    ; crc_error      = r.crc_err.value
    ; busy           = r.busy.value
    ; frame_done
    ; frame_error
    ; keep
    }
  ;;
end
