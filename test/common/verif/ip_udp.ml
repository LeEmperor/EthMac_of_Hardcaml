(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "ip_udp.ml" *)
(* Software golden models for the IPv4 and UDP header builders.

   Both were copy-pasted into [ipv4_tx_tb.ml] and [udp_tx_tb.ml]; this is the single copy
   those suites now call. The IPv4 checksum is the ordinary one's-complement sum with
   end-around carry and a final complement, computed over the header with the checksum
   field taken as zero, so an RTL checksum bug shows up as a mismatch on header bytes
   [10..11] rather than as a whole-header diff.

   The IPv4 stack here is actually very small, as it was a small airplane-ride project; a
   stronger IPv4 stack may come later, but I am more concerned with MACs and UDP than with
   IPv4 in particular. As a result, many things will be hardcoded. Bite me.

   [Udp.header] emits [checksum = 0]: UDP checksums are optional over IPv4 and the
   transmit path does not compute one.

   Tags: [{ "ACTIVE" ; "TEST" ; "REFERENCE_MODEL" ; "COMMON_ITEMS" }]
*)

open! Core

(* one could argue to put these in Helper_functions *)
let hi8 value = (value lsr 8) land 0xFF
let lo8 value = value land 0xFF
let w16 hi lo = (hi lsl 8) lor lo

(* 1s-complement sum with end-around carry, then complemented. Definitely re-usable later
   on.
*)
let ones_complement_checksum words =
  let sum = List.fold words ~init:0 ~f:( + ) in
  let rec fold sum =
    if sum > 0xFFFF
    then fold (
          (sum land 0xFFFF) + (sum lsr 16)
        )
    else sum
  in

  (* actually insane that lnot exists ->
      wonder how many secondary and tertiary level binary functions exist like AND21 and stuff.
    Ivan would be proud.
  *)
  lnot (fold sum) land 0xFFFF
  [@@ocamlformat "disable"]

module Ipv4 = struct
  let header_length = 20

  (* The 16-bit words of the header with the checksum field held at zero, which is the
     form the checksum is defined over.

     Version/IHL 0x45, DSCP/ECN 0, identification 0, flags "don't fragment" (0x4000), TTL
     0x40.
  *)
  let checksum_words ~src_ip ~dst_ip ~protocol ~total_length =
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
  ;;

  let checksum ~src_ip ~dst_ip ~protocol ~total_length =
    ones_complement_checksum (checksum_words ~src_ip ~dst_ip ~protocol ~total_length)
  ;;

  (* [payload_length] is the layer-4 byte count; total_length adds the 20-byte header. *)
  let header ~src_ip ~dst_ip ~protocol ~payload_length =
    let total_length = header_length + payload_length in
    let checksum = checksum ~src_ip ~dst_ip ~protocol ~total_length in
    [ 0x45
    ; 0x00
    ; hi8 total_length
    ; lo8 total_length
    ; 0x00
    ; 0x00
    ; 0x40
    ; 0x00
    ; 0x40
    ; protocol
    ; hi8 checksum
    ; lo8 checksum
    ]
    @ src_ip
    @ dst_ip
  ;;
end

module Udp = struct
  let header_length = 8

  (* [payload_length] is the application byte count; udp_length adds the 8-byte header. *)
  let header ~src_port ~dst_port ~payload_length =
    let udp_length = header_length + payload_length in
    [ hi8 src_port
    ; lo8 src_port
    ; hi8 dst_port
    ; lo8 dst_port
    ; hi8 udp_length
    ; lo8 udp_length
    ; 0x00
    ; 0x00
    ]
  ;;
end
