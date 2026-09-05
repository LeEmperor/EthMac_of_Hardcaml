open! Core
open! Hardcaml
open! Pcs_of_hardcaml
module Sim = Cyclesim.With_interface (Pcs_top.I) (Pcs_top.O)

let expect_bits name actual expected =
  if not (Bits.equal !actual expected)
  then
    raise_s
      [%message
        "unexpected PCS output" (name : string) (!actual : Bits.t) (expected : Bits.t)]
;;

let expect_int name actual expected =
  expect_bits name actual (Bits.of_int_trunc ~width:(Bits.width !actual) expected)
;;

let () =
  let scope = Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true () in
  let sim = Sim.create (Pcs_top.create scope) in
  let i = Cyclesim.inputs sim in
  let o = Cyclesim.outputs sim in
  let cycle () = Cyclesim.cycle sim in
  i.tx_reset_i := Bits.vdd;
  i.rx_reset_i := Bits.vdd;
  cycle ();
  expect_bits "reset xgmii_rxd_o" o.xgmii_rxd_o (Bits.of_hex ~width:64 "0707070707070707");
  expect_int "reset xgmii_rxc_o" o.xgmii_rxc_o 0xff;
  expect_int "TX scaffold valid" o.encoded_tx_block_valid_o 0;
  expect_int "RX block lock" o.rx_block_lock_o 0;
  i.rx_reset_i := Bits.gnd;
  cycle ();
  expect_bits
    "link-down xgmii_rxd_o"
    o.xgmii_rxd_o
    (Bits.of_hex ~width:64 "0100009c0100009c");
  expect_int "link-down xgmii_rxc_o" o.xgmii_rxc_o 0x11
;;
