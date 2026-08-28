# Hardware Source Formatting Guide

## 1. Purpose

This guide defines the source formatting and signal-naming conventions for Hardcaml hardware
modules in this repository. New modules should follow these conventions, and existing modules
should preserve them when edited.

The rules apply primarily to synthesizable modules under `lib/`. Testbench-local variables,
software-only helpers, and direction-neutral value types follow the exceptions described
below.

## 2. File headers

Every OCaml hardware source file begins with four comments in this order:

```ocaml
(* University of Florida *)
(* Author: Bohdan Purtell *)
(* Module: "module_name.ml" *)
(* Short description of the module.

   Additional paragraphs remain inside the fourth comment. Continuation lines use the
   indentation shown here.
*)
```

The header fields mean:

1. The first comment identifies the university.
2. The second comment identifies the author.
3. The third comment contains the source filename in quotation marks.
4. The fourth comment describes the module, its boundaries, and any important implementation
   context.

When editing or formatting a file:

- Preserve the four comments as separate comments.
- Preserve their order.
- Do not combine the university, author, module, and description into one block.
- Do not remove the quotation marks around the module filename.
- Keep longer design notes inside the fourth comment.
- Update the module filename if the file itself is renamed.

The headers in `lib/pcs/pcs_top.ml` and `lib/xgmii/xgmii.ml` are the canonical repository
examples.

## 3. External port names

Hardcaml module ports use lower snake case and an explicit direction suffix.

### 3.1 Input ports

Every field in a module's `I` interface ends in `_i`:

```ocaml
module I = struct
  type 'a t =
    { clock_i : 'a
    ; reset_i : 'a
    ; data_i : 'a [@bits 64]
    ; valid_i : 'a
    }
  [@@deriving hardcaml]
end
```

Examples include:

- `tx_clock_i`
- `tx_reset_i`
- `xgmii_txd_i`
- `encoded_rx_header_valid_i`

### 3.2 Output ports

Every field in a module's `O` interface ends in `_o`:

```ocaml
module O = struct
  type 'a t =
    { data_o : 'a [@bits 64]
    ; valid_o : 'a
    ; error_o : 'a
    }
  [@@deriving hardcaml]
end
```

Examples include:

- `xgmii_rxd_o`
- `encoded_tx_block_valid_o`
- `rx_block_lock_o`
- `tx_bad_xgmii_o`

The suffix rule applies to every external port category, including:

- Clocks and resets
- Data and control buses
- Valid and ready handshakes
- Enables
- Status and error indicators
- Debug ports

Signal direction is always relative to the module declaring the `I` or `O` interface, not
relative to the board, MAC, PHY, or remote endpoint.

## 4. Direction-neutral types

Do not add `_i` or `_o` to fields of a type that represents a value rather than a directional
module interface.

For example, an XGMII word may be produced or consumed by multiple modules:

```ocaml
module Word = struct
  type 'a t =
    { data : 'a [@bits 64]
    ; control : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end
```

`Word.data` and `Word.control` remain unsuffixed because `Word` has no inherent direction.
The port containing that value receives the suffix at the module boundary.

Other direction-neutral types include:

- FIFO words
- Encoded protocol blocks
- Internal pipeline records
- Parsed headers
- Test vectors and expected-value records

## 5. Internal signal names

Internal signals and local OCaml bindings do not require `_i` or `_o`. Use a concise name
that describes the signal's role:

```ocaml
let rx_spec = Reg_spec.create ~clock:i.rx_clock_i ~clear:i.rx_reset_i () in
let xgmii_rxd = reg rx_spec decoded_word.data in
{ O.xgmii_rxd_o = xgmii_rxd }
```

This distinction keeps direction suffixes meaningful:

- `i.rx_clock_i` is an external input port.
- `xgmii_rxd` is an internal signal.
- `O.xgmii_rxd_o` is an external output port.

Avoid carrying `_i` or `_o` through an entire internal pipeline merely because the original
value entered through an input or will eventually drive an output.

## 6. Interface comments

Group related ports by function and clock domain. Place the comment immediately before the
first field in the group:

```ocaml
module I = struct
  type 'a t =
    { (* MAC/RS -> PCS, TX clock domain. *)
      tx_clock_i : 'a
    ; tx_reset_i : 'a
    ; xgmii_txd_i : 'a [@bits 64]
    ; xgmii_txc_i : 'a [@bits 8]
    ; (* Gearbox -> PCS, RX clock domain. *)
      rx_clock_i : 'a
    ; rx_reset_i : 'a
    ; encoded_rx_data_i : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end
```

Comments should identify:

- The producer and consumer when that relationship is not obvious
- The clock domain
- Whether reset is synchronous or asynchronous
- Any nonstandard validity or integration behavior
- Important ownership boundaries

Do not repeat information already made obvious by the field name and type.

## 7. Port references in implementation code

Access inputs through the typed input record:

```ocaml
let spec = Reg_spec.create ~clock:i.clock_i ~clear:i.reset_i () in
```

Construct outputs using their complete suffixed field names:

```ocaml
{ O.data_o = data
; valid_o = valid
; error_o = error
}
```

Comments that name an external port should use its complete `_i` or `_o` name. Comments that
describe a protocol concept rather than a concrete port may use the protocol name without a
suffix.

## 8. Tests and documentation

Testbenches access the same generated interface fields and therefore use the same suffixes:

```ocaml
i.reset_i := Bits.vdd;
expect_int "output valid" o.valid_o 0;
```

Interface tables and signal-specific prose in documentation must also use the complete port
names. When a port is renamed, update:

- The declaring `I` or `O` record
- All implementation references
- Testbench drivers and monitors
- Documentation tables and prose
- Generated-RTL or integration scripts that refer to the port by name

## 9. Formatting and verification

Use the repository formatter and lint configuration rather than manually aligning code:

```sh
./scripts/with-switch.sh dune build @fmt
./scripts/with-switch.sh dune build @lint
./scripts/with-switch.sh dune build
```

Before considering a naming or formatting change complete:

1. Confirm all `I` fields end in `_i`.
2. Confirm all `O` fields end in `_o`.
3. Confirm direction-neutral record fields remain unsuffixed.
4. Confirm the four-part source header remains intact.
5. Search for stale port names in source, tests, documentation, and integration code.
6. Run formatting, lint, affected tests, and the build.

