open! Core
open! Hardcaml

let ( <-- ) (r : Bits.t ref) i = r := Bits.of_int_trunc ~width:(Bits.width !r) i

let send ~cycle ~(t_in : Bits.t ref) hi lo =
  t_in <-- hi;
  cycle ();
  t_in <-- lo;
  cycle ()
;;

let send_byte ~cycle ~t_in byte =
  let hi = (byte lsr 4) land 0xF in
  let lo = byte land 0xF in
  (* MII transmits lo nibble first so the assembler (lo@:hi → byte) reconstructs correctly *)
  send ~cycle ~t_in lo hi
;;

let send_bytes ~cycle ~t_in name n_bytes bytes =
  printf "\nsending %s: %d bytes : %X" name n_bytes bytes;
  for i = 0 to n_bytes - 1 do
    let target = (bytes lsr (8 * i)) land 0xFF in
    send_byte ~cycle ~t_in target
  done
;;

let repeat_bytes ~cycle ~t_in name n_times bytes =
  printf "\nsending %X: %d times " bytes n_times;
  for _ = 0 to n_times do
    send_byte ~cycle ~t_in bytes
  done
;;

let send_frame ~cycle ~t_in name ~dst_mac ~src_mac ~eth_type ~payload ~payload_length =
  printf "\n sending frame: %s with %d bytes " name payload_length;
  repeat_bytes ~cycle ~t_in "preamble" 5 0x55;
  send_bytes ~cycle ~t_in "start-frame delimiter" 1 0xD5;
  send_bytes ~cycle ~t_in "dst_mac" 6 dst_mac;
  send_bytes ~cycle ~t_in "src_mac" 6 src_mac;
  send_bytes ~cycle ~t_in "eth_type" 2 eth_type;
  send_bytes ~cycle ~t_in "payload burst" payload_length payload
;;
