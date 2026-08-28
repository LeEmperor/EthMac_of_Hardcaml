(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "udp_ipv4_rx_expect_tests.ml" *)
(* A concise golden trace for the IPv4-to-UDP receive composition. *)

open! Core
open! Udp_ipv4_rx_testbench

let%expect_test "the two parsers recover one short datagram" =
  let src_ip = [ 192; 168; 1; 10 ]
  and dst_ip = [ 192; 168; 1; 1 ]
  and payload = [ 1; 2; 3 ] in
  let frame =
    ipv4_udp_payload
      ~src_ip
      ~dst_ip
      ~src_port:0x1234
      ~dst_port:0x1235
      ~udp_checksum:0
      ~payload
      ()
  in
  let result = Drop_bad_checksum.run frame in
  print_s
    [%sexp
      { payload = (result.payload : int list)
      ; metadata = (result.metadata : metadata option)
      ; app_start_count = (result.app_start_count : int)
      ; tlast_indices = (result.tlast_indices : int list)
      ; checksum_ok = (result.checksum_ok : bool)
      }];
  [%expect
    {|
    ((payload (1 2 3))
     (metadata
      (((src_port 4660) (dst_port 4661) (udp_length 11) (payload_length 3)
        (udp_checksum 0) (src_ip 3232235786) (dst_ip 3232235777))))
     (app_start_count 1) (tlast_indices (2)) (checksum_ok true))
    |}]
;;
