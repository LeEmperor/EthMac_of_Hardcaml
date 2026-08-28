(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_rx_mac_top_expect_tests.ml" *)
(* A concise end-to-end receive golden from MII through the UDP application stream. *)

open! Core
open! Udp_rx_mac_top_testbench

let%expect_test "a short padded datagram is stripped back to its application bytes" =
  let payload = [ 0xde; 0xad; 0xbe; 0xef ] in
  let result =
    run
      (ipv4_udp_eth_payload
         ~ethernet_padding:true
         ~src_ip
         ~dst_ip
         ~src_port
         ~dst_port
         ~udp_checksum:0
         ~payload
         ())
  in
  print_s
    [%sexp
      { payload = (result.payload : int list)
      ; metadata = (result.metadata : metadata option)
      ; checksum_ok = (result.checksum_ok : bool)
      ; crc_error = (result.crc_error : bool)
      }];
  [%expect
    {|
    ((payload (222 173 190 239))
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 12) (payload_length 4)
        (src_ip 3232235786) (dst_ip 3232235777))))
     (checksum_ok true) (crc_error false))
    |}]
;;
