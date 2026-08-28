# Verif Sweep — standardize the whole repo onto the phase-1 test pattern

## 1. Context

Phase 1 (merged in `82a31c0`) established a Jane-Street-flavored verification pattern and
applied it to exactly three MII blocks: `rx_byte_assembler/`, `rx_controller/`, and
`tx_byte_disassembler/`. Each is a directory containing a shared `*_testbench.ml`
(step-testbench fixture, drivers, `Observation` types, scenarios), a
`*_unit_quickcheck_tests.ml` (`let%test_unit` + `Quickcheck.test`), a `*_expect_tests.ml`
(golden traces), and — where one existed — the superseded assertion harness relegated to
`*_legacy_assertion_test.ml`.

The other **17 RTL blocks** are still verified by the original style: a single `*_tb.ml`
executable that drives `Cyclesim` through mutable `Bits.t ref`s, prints `PASS`/`FAIL`
lines, and `exit 1`s or `failwith`s at the end. That style has no typed observations, no
shrinking, no golden traces, and no reusable drivers — and its reference models (SW CRC-32,
IPv4/UDP header + checksum builders) are copy-pasted across several files.

Goal of this branch: bring every remaining block onto the phase-1 pattern, and factor the
boilerplate and reference models that phase 1 left duplicated into one library so the 17th
suite costs less to write than the 4th.

Baseline confirmed green before starting: `dune build` and `dune runtest` both pass.

**Status: Phases 0, 1 and 2 landed** (see §4.1, §5.1, §6.1). Phases 3–5 not started.

---

## 2. Formatting and naming

All new and edited files follow **`docs/formatting_guide.md`**. The clauses that bind this
work:

- **§2 File headers.** Every OCaml source file opens with the *four separate comments*:
  university, author, `(* Module: "foo_testbench.ml" *)`, then the description block. The
  three phase-1 suites currently carry only three of the four (they omit the `Module:`
  line) — retro-fit them in Phase 0 so the convention is uniform. The phase-1
  `Tags: [{ "ACTIVE" ; "TEST" ; ... }]` list stays, inside the fourth comment.
- **§4 Direction-neutral types.** `Observation.t`, `Output_snapshot.t`, `Phase.t`,
  `Frame.t` and the like are values, not module interfaces — their fields take **no**
  `_i`/`_o` suffix. The guide names "test vectors and expected-value records" explicitly.
- **§5 Internal signal names.** Locals inside a testbench (`after_low_nibble`,
  `destination_prefix`) stay unsuffixed.
- **§8 Tests.** Testbenches reference interface fields by their *complete* port names,
  whatever those are — so `inputs ~...` and `snapshot` mirror the DUT's `I`/`O` records
  field-for-field.
- **§9 Verification.** Use `dune build @fmt` and `dune build @lint`, not hand-alignment.

> **Open discrepancy, flagged not resolved.** Guide §3 requires every `I` field to end in
> `_i` and every `O` field in `_o`, and cites `lib/pcs/pcs_top.ml` / `lib/xgmii/xgmii.ml`
> as canonical — neither file exists in this repo, and **no** current `lib/` module
> complies (`rx_data`, `byte_out`, `clock`, `reset`, …). This sweep does **not** rename RTL
> ports; testbenches bind to the names that exist today. Be aware that a later `_i`/`_o`
> sweep of `lib/` would touch the `inputs ~...` and `snapshot` functions of every suite
> written here. If that rename is imminent, do it *before* Phase 1 rather than after.

---

## 3. The suite convention (fixed, from phase 1)

For DUT `foo` in domain `<d>`, directory `test/<d>/foo/` contains:

| File | Role |
| --- | --- |
| `foo_testbench.ml` | `module Dut = Foo`, `Observation` / `Output_snapshot` types (`[@@deriving sexp, equal, compare]`), `inputs ~...`, `reset`, drivers, scenarios, `run_*` entry points |
| `foo_unit_quickcheck_tests.ml` | `let%test_unit` examples + `Quickcheck.test` properties with `~seed:(\`Deterministic ...)` and a shrinker |
| `foo_expect_tests.ml` | `let%expect_test` golden traces via `print_s [%sexp (obs : ...)]` |
| `foo_legacy_assertion_test.ml` | the superseded harness, tagged `DEPRECATED` |
| `dune` | `(library (name foo_inline_tests) (modules foo_testbench foo_unit_quickcheck_tests foo_expect_tests) (inline_tests (flags (:standard -source-tree-root .))) ...)` plus a separate `(executable)` stanza for the legacy module |

**The `-source-tree-root .` flag is mandatory** in every new `(inline_tests)` stanza — it
works around the ppx_expect v0.18~preview path bug documented in
`test/mii/rx_controller/dune`. Copy that comment along with the flag.

### 3.1 Legacy relegation

Legacy harnesses stay **compiled but not run**. Each DUT dir's `dune` gets:

```
(executable
 (name foo_legacy_assertion_test)
 (modules foo_legacy_assertion_test)
 (libraries core hardcaml <dut lib> hardcaml_waveterm helper_tb_functions)
 (preprocess (pps ppx_hardcaml ppx_jane))
 (lint (pps ppx_js_style))
 (flags (:standard -w -27-57-33-34-35-40-63-47-26)))
```

A bare `(executable)` is built by `dune build` but never invoked by `dune runtest`, which
is exactly the desired semantics: the historical harness keeps type-checking against the
RTL instead of silently rotting, but its `printf` spam and `exit 1` never run in CI.

The two already-relegated files
(`test/mii/rx_byte_assembler/rx_byte_assembler_legacy_assertion_test.ml`,
`test/mii/tx_byte_disassembler/tx_byte_disassembler_legacy_assertion_test.ml`) are
currently in **no** stanza and do not compile at all. Retro-fit them to this convention in
Phase 0 so all 20 dirs are uniform.

---

## 4. Phase 0 — shared verification library

New directory `test/common/verif/` → library **`hardcaml_verif`**. Extracts what phase 1
left triplicated plus the reference models scattered across the legacy tbs.

| Module | Contents | Extracted from |
| --- | --- | --- |
| `bits_conv.ml` | `bit : bool -> Bits.t`, `to_bool`, `to_int`, `of_int ~width` | the `let bit value = ...` in all three exemplar testbenches |
| `sim_fixture.ml` | Functor `Make (Dut : S)` where `S` has `module I` / `module O` / `val create` / `val name`; yields `module Sim`, `module Step`, `create_simulator`, `run_with_timeout ~timeout ~testbench` (raising `"<name> testbench timed out"`) | the `create_simulator` + `run_with_timeout` blocks, byte-identical in `rx_byte_assembler_testbench.ml:161-176` and `rx_controller_testbench.ml:236-251` |
| `crc32.ml` | `bit` / `byte` / `bytes` folds over the `0xEDB88320` reflected polynomial, plus `fcs_bytes : int list -> int list` and `residue` | `test/mii/crc_tb.ml:27-38` (`sw_crc_bit` / `sw_crc_byte` / `sw_crc`), duplicated in `test/mii/tx_crc_tb.ml` |
| `eth_frame.ml` | The validated `Frame.t` record (`preamble_length`, `destination_mac`, `source_mac`, `eth_type`, `payload`) + `create` + `byte_count`, extended with `to_bytes` and `with_fcs` | lift `Frame` verbatim out of `test/mii/rx_controller/rx_controller_testbench.ml:92-121`, then have that testbench consume it |
| `ip_udp.ml` | IPv4 header builder + ones-complement checksum; UDP header builder (`src_port`, `dst_port`, `udp_length`, `checksum=0`); `hi8` / `lo8` | `golden_header` in `test/udp/udp_tx_tb.ml:50-56` and the checksum code in `test/ipv4/ipv4_tx_tb.ml` |
| `generators.ml` | `byte`, `byte_list ?min ?max`, `mac_address`, `ipv4_address`, `port`, `payload_length` as `Quickcheck.Generator.t` | generalizes `Generators` in `rx_byte_assembler_unit_quickcheck_tests.ml:24-38` |

Also in Phase 0:

- Retro-fit the three existing suites to consume `hardcaml_verif` (drop their local `bit`,
  `create_simulator`, `run_with_timeout`, and `rx_controller`'s `Frame`). This proves the
  library against already-green tests before any new suite depends on it.
- Add the missing four-part headers (§2) to the phase-1 files.
- `dune-project`: add the missing `(depends ...)` entries `hardcaml_step_testbench`,
  `ppx_expect`, `base_quickcheck`.
- Housekeeping: move `test/mii/rx_controller_tb.ml` →
  `test/mii/rx_controller/rx_controller_legacy_assertion_test.ml` (+ executable stanza) and
  delete the `test/mii/test_rx_controller.ml` stub, folding its follow-up note (stateful
  generator weighted toward boundary bytes and control events) into a TODO comment at the
  top of `rx_controller_unit_quickcheck_tests.ml`.

**Gate:** `dune build && dune runtest` green with zero expect-test diffs.

### 4.1 What landed

Gate met: `dune build`, `dune build @lint`, and `dune runtest` are all clean, `dune
runtest` executes all three `inline-test-runner` aliases, and no golden changed — the
only diff in the three `*_expect_tests.ml` files is the added `Module:` header line plus
formatter reflow.

Shipped as planned:

- `test/common/verif/` → library `hardcaml_verif`, all six modules.
- All three phase-1 testbenches build their simulator through `Sim_fixture.Make`, use
  `Bits_conv.bit`, and no longer carry a local `create_simulator` / `run_with_timeout`.
  `rx_controller`'s `Frame` is now `module Frame = Eth_frame`; all three quickcheck files
  dropped their local `Generators`.
- Four-part §2 headers on all 18 suite and library files.
- `dune-project` gained `hardcaml_step_testbench`, `ppx_expect`, `base_quickcheck`
  (`hardcaml_networking.opam` regenerated).
- `test/mii/rx_controller_tb.ml` → `test/mii/rx_controller/rx_controller_legacy_assertion_test.ml`
  and out of the `(tests)` stanza; `test/mii/test_rx_controller.ml` deleted with its
  follow-up note folded into a TODO atop `rx_controller_unit_quickcheck_tests.ml`.
- All three legacy harnesses now sit under bare `(executable)` stanzas; all three `.exe`
  targets confirmed present after `dune build`, none invoked by `dune runtest`.

Deviations from the §4 table, all additive:

- `Crc32` exposes `fcs` (the inverted word) alongside `bytes` (the raw accumulator).
  The two legacy tbs disagreed on which one `sw_crc` meant — `crc_tb.ml` returned the raw
  accumulator, `tx_crc_tb.ml` applied the final XOR — so both are named explicitly rather
  than one `sw_crc` that silently means different things per suite.
- `Eth_frame` also exposes `crc_covered_bytes` (destination MAC through payload) and
  `fcs_bytes`; `with_fcs` is built from them, and `rx_crc`'s suite will need the covered
  span directly.
- `Generators` also exposes `eth_frame`, since `rx_controller`'s frame generator moved
  into the library along with `Frame`.

Verified against known vectors before any suite depends on them: raw accumulator
`0x340bc6d9` and FCS word `0xcbf43926` for `"123456789"`, FCS bytes `26 39 f4 cb`,
residue `0xdebb20e3` for frame ++ FCS, and an `Ipv4.header` whose 16-bit words sum to
`0xffff`. These are the same constants the legacy `crc_tb.ml` / `tx_crc_tb.ml` assert.

**One discovery worth carrying into Phase 1.** OxCaml encodes function arity in the arrow
type, so the inferred type for `Sim_fixture`'s `testbench` parameter came out as
`Handler.t @ local -> (O_data.t -> 'a)` — arity one, returning a closure — which lets the
local handler escape its region and rejects *every* call site with an error that points
at the caller's lambda rather than the functor. The parameter type is now written out
explicitly and unparenthesized to pin the arity at two. Any future helper that forwards a
`~testbench` (or wraps `Step.spawn` / `Step.wait_for`) across a module boundary will hit
the same thing. Written up in `test/test_architecture.md` under "Settled Conventions".

**Formatting scope note.** Promoting `@fmt` in the four touched directories also reflowed
pre-existing unformatted content in the exemplar and legacy files — those directories were
not formatter-clean at baseline either, and neither is most of `lib/`. Promotion was
scoped per directory rather than a repo-wide `dune fmt`, which is left as a separate
change.

---

## 5. Phase 1 — MII leaf blocks (5 suites)

New dirs under `test/mii/`. Legacy sources move in as noted.

| DUT | Legacy source → | Focus of the new suite |
| --- | --- | --- |
| `rx_crc` | `test/mii/crc_tb.ml` | Quickcheck: for random byte sequences, hardware `crc_valid` agrees with `Crc32.residue` on `frame ++ fcs_bytes frame`; a single corrupted byte must clear it. Expect: cycle trace across a short frame. |
| `tx_crc` | `test/mii/tx_crc_tb.ml` | Quickcheck: emitted FCS bytes for random payloads equal `Crc32.fcs_bytes`. Expect: `byte_sel` mux ordering across the 4 FCS bytes. |
| `rx_datapath` | `test/mii/rx_datapath_tb.ml` | Expect: header-register capture and the 4-cycle FCS-strip pipeline as a readable trace. Quickcheck: payload in == payload out for random lengths, no FCS leakage. |
| `tx_datapath` | *(none — new)* | Expect: emitted byte per `byte_mux_sel` × `mac_byte_sel` combination. Quickcheck: sweep the full selector space against a pure OCaml mux model. |
| `tx_controller` | *(none — new)* | Closest dual of `rx_controller/` — mirror its `Phase` / `Compact_observation` structure. Expect: state and control trace for one frame. Quickcheck: emitted byte count = `7+1+6+6+2+len+4` for random payload lengths. |

`tx_controller` and `tx_datapath` have **no** direct coverage today — they are only
exercised indirectly through `tx_path_tb.ml`. These two are net-new verification, not a
translation.

Leave the `test/mii/fsm_states*.txt` scratch files alone.

### 5.1 What landed

Gate met: `dune build`, `dune build @lint`, and `dune runtest` are all clean, `dune
runtest` now executes **eight** `inline-test-runner` aliases (the three from phase 1 plus
the five below), and `dune build @<dir>/fmt` is clean in all eight suite directories plus
`test/common/verif`.

All five suites shipped in the four-file layout, none of them re-rolling anything
`hardcaml_verif` already provides — and none of them needed anything added to it, so the
library is unchanged from phase 0. `Bits_conv.bit`, `Sim_fixture.Make`, `Crc32`,
`Eth_frame`, and `Generators` all carried over as-is; `Ip_udp` remains unused until phase
3:

| Suite | `let%test_unit` (of which Quickcheck) | `let%expect_test` | Legacy source |
| --- | --- | --- | --- |
| `test/mii/rx_crc/` | 10 (3) | 4 | `crc_tb.ml` |
| `test/mii/tx_crc/` | 8 (2) | 3 | `tx_crc_tb.ml` |
| `test/mii/rx_datapath/` | 9 (3) | 2 | `rx_datapath_tb.ml` |
| `test/mii/tx_datapath/` | 12 (1) | 3 | *(net-new)* |
| `test/mii/tx_controller/` | 14 (2) | 6 | *(net-new)* |

The three legacy tbs moved in as `*_legacy_assertion_test.ml` under bare `(executable)`
stanzas with the four-part header and `DEPRECATED` tag; all three `.exe` targets are
present after `dune build` and none is invoked by `dune runtest`. `test/mii/dune`'s
`(tests)` stanza is down to `rx_path_tb tx_path_tb`, which is where phase 4 picks it up.

Every value the legacy harnesses asserted was cross-checked against the new goldens
before the legacy file was relegated: residue `0xDEBB20E3` and `crc_valid` for
`"123456789" ++ FCS`, rejection of an all-zero FCS, recovery after an `en` drop
(`rx_crc`); FCS word `0xCBF43926` read out through `byte_sel` as `26 39 f4 cb`, the
enable-drop rerun, and an arbitrary payload against the model (`tx_crc`); the `0x4521`
ethertype latch and a five-byte payload surviving its FCS (`rx_datapath`).

Deviations and additions worth knowing:

- **`tx_datapath` is instantiated twice.** `Tx_datapath.create` takes `?ethertype`
  defaulting to `0x9999`, whose two bytes are identical — a high/low swap in the ethertype
  mux is invisible under the default. The testbench is a `Make_testbench` functor over the
  ethertype, and the suite runs both the default and `Ipv4_testbench` at `0x0800`. The
  optional argument also means `include Dut` will not satisfy `Sim_fixture.S`; `create` is
  written out.
- **`tx_controller` samples `before_edge`, not `after_edge`.** Its outputs instruct a
  combinational datapath for the cycle in progress, so the byte emitted on a cycle is the
  one its `before_edge` outputs select. `after_edge` carries the next state, which the
  driver uses to choose the following cycle's stimulus — so the driver is a state-following
  loop rather than a fixed script. The receive suites keep sampling `after_edge`.
- **`tx_datapath`'s selector space is swept exhaustively, not sampled.** Eight
  `byte_mux_sel` values times eight `mac_byte_sel` values is 64 combinations, cheap enough
  to enumerate; Quickcheck covers the payload and FCS byte values on top of that.
- **`rx_datapath`'s pipeline latency is asserted, not just its output.** Pairing each
  emitted byte with the cycle that emitted it pins the FCS-strip delay at four, which is
  what makes the FCS *unreachable* rather than merely absent from a particular vector.

**Findings, all written up in `docs/verif_sweep_findings.md`.** Two in the RTL.
`Tx_controller` deadlocked on a zero-length payload (RTL-1) — unreachable behind the
store-and-forward gate, so it was recorded rather than fixed at the time, and has since
been fixed: `Payload` treats an empty FIFO on arrival as padding, and the suite's length
generators now start at 0. `Tx_controller.crc_en` was dead — `mac_top` gated `Tx_crc` off
`state` and never read it (RTL-2) — and has since been given the meaning its name implies:
it is the FCS window, `mac_top` consumes it, and three of the four magic-state-number
sites went with it.

**Phase 0 correction.** §4.1's claim that `dune build @lint` was clean did not hold. Five
phase-0 files carried an item-level `[@@ocamlformat "disable"]`, which this ocamlformat
rejects as an error; all five were converted to the floating `[@@@ocamlformat "disable"]`
/ `[@@@ocamlformat "enable"]` pair. Details, and why it went unnoticed, in findings
PROC-1. Two more process entries there (PROC-2, PROC-3) cover an empty `(**)` that makes
ocamlformat skip a whole file, and concurrent edits to
`rx_controller_testbench.ml` during the phase.

---

## 6. Phase 2 — common + uart (4 suites)

| DUT | Legacy source → | Notes |
| --- | --- | --- |
| `clk_div` | `test/common/clk_div_tb.ml` → `test/common/clk_div/` | Quickcheck the output period across divisor values |
| `second_pulse` | `test/common/second_pulse_tb.ml` → `test/common/second_pulse/` | Expect: pulse position; quickcheck: exactly one pulse per period |
| `helper_circuits` | *(none — new)* | `lib/common/helper_circuits.ml` has **zero** tests today. Covers `rising_edge_detector`, `falling_edge_detector`, `delay_by`, `rising/falling_edge_delayed`. These are plain `Signal.t` functions, not `I`/`O` modules — the testbench must define a small wrapper DUT with `I`/`O` records to give `Sim_fixture` something to instantiate. Cheap suite, strong oracle (compare against a pure OCaml edge-detect/delay over a random bit stream). |
| `uart_tx` | `test/uart/uart_tx_tb.ml` → `test/uart/uart_tx/` | Quickcheck: random byte → serial stream decoded by a SW UART receiver model round-trips |

`test/common/dune` keeps only the `helper_tb_functions` library after `clk_div_tb` and
`second_pulse_tb` move out.

### 6.1 What landed

Gate met: `dune build`, `dune build @lint` and `dune runtest` are all clean — exit codes
checked bare, not through a pipe, per PROC-1 — `dune runtest` now executes **twelve**
`inline-test-runner` aliases (the eight from phases 1 and 2 plus the four below), and
`dune build @<dir>/fmt` is clean in all twelve suite directories plus `test/common` and
`test/common/verif`.

All four suites shipped in the four-file layout. As in phase 1, none of them needed
anything added to `hardcaml_verif` — `Bits_conv.bit`, `Sim_fixture.Make` and
`Generators.byte` carried over unchanged, and the library is untouched since phase 0.

| Suite | `let%test_unit` (of which Quickcheck) | `let%expect_test` | Legacy source |
| --- | --- | --- | --- |
| `test/common/clk_div/` | 8 (2) | 4 | `clk_div_tb.ml` |
| `test/common/second_pulse/` | 7 (2) | 5 | `second_pulse_tb.ml` |
| `test/common/helper_circuits/` | 17 (3) | 6 | *(net-new)* |
| `test/uart/uart_tx/` | 16 (2) | 6 | `uart_tx_tb.ml` |

The three legacy tbs moved in as `*_legacy_assertion_test.ml` under bare `(executable)`
stanzas with the four-part header and the `DEPRECATED` tag; all three `.exe` targets are
present after `dune build` and none is invoked by `dune runtest`. `test/common/dune` is
down to the `helper_tb_functions` library, and **`test/uart/dune` is deleted** — that
directory now holds nothing but the `uart_tx/` subdirectory, which is the end state §8
calls for.

None of the three legacy harnesses asserted anything: all three only printed a trace and
a waveform, with no `expect`, no `failwith` and no `exit 1`. There was therefore no
assertion to cross-check the new goldens against, so every golden was instead derived by
hand from the RTL before promotion — the divide-by-four phase and its freeze/restart
behavior, the pulse landing on cycle `clk_freq`, each edge-detector and delay-line column,
and the full ten-symbol UART frame for eight different bytes. The legacy scenarios
themselves were reproduced where they carried information: `second_pulse` runs at
`clk_freq = 10`, the value the old harness hardcoded, and `clk_div`'s freeze/resume expect
test is the old harness's 20/4/8-cycle scenario shortened to one reviewable row.

Deviations and additions worth knowing:

- **`clk_div` has no divisor to sweep.** The plan called for "quickcheck the output period
  across divisor values"; `lib/common/clk_div.ml` hardcodes `~width:2`, so the ratio is
  fixed at four and there is no such axis. The property randomizes the enable *schedule*
  instead — arbitrary interleavings of run, hold and clear — which is the only axis the
  RTL exposes and a stronger check than a free-running period measurement. See RTL-3.
- **`second_pulse` is instantiated six times**, following `tx_datapath`'s `Make_testbench`
  pattern over its optional `?clk_freq`. The frequencies are 3, 4, 5, 8, 10 and 16,
  chosen so that powers of two (where the terminal count and the counter's natural wrap
  coincide) and non-powers (where they do not) are both represented; a counter that rolled
  on the wrap rather than on the compare passes the first set and fails the second. The
  `Observation` and `Pulse_summary` types live *outside* the functor so one `runners` list
  can carry every instantiation and each property runs across all six.
- **`helper_circuits` needed a wrapper DUT, and it covers the whole file, not just the
  edge detectors.** The plan listed the five register-based helpers. `const8`, `hi16` and
  `lo16` live in the same file, had no coverage either, and are used by `ipv4_tx`,
  `ipv4_rx` and `udp_tx` — the Phase 3 DUTs — to place lengths and checksums on the wire
  most significant byte first. They are wired into the same wrapper (a `word` input and
  four byte outputs) and tested alongside, so Phase 3 can lean on their byte order rather
  than re-establish it.
- **`uart_tx`'s oracle is a software receiver, not a cycle model.** `Uart_receiver.decode`
  reconstructs a byte from the symbols the fixture sampled, exactly as a real receiver
  would, so the headline property is a round trip and is indifferent to how the
  transmitter times itself. The properties run across tick spacings of 1 to 8 and across
  enable stalls, which is what turns "it sent the right waveform once" into "it is
  tick-driven".
- **A UART symbol is an interval, not a sample.** The fixture records the line on every
  cycle between two ticks and rejects a symbol whose line moved mid-interval. A single
  mid-bit sample would accept a transmitter that glitched between ticks; a real receiver
  sampling at its own phase would not.

**Findings, written up in `docs/verif_sweep_findings.md`.** Three in the RTL, none of them
fixed here: `clk_div` has no divisor parameter (RTL-3), `second_pulse` fails to elaborate
at `clk_freq = 1` (RTL-4, verified), and `uart_tx` carries a dead `frame` signal, an
unreachable `DONE` state, an unused sub-scope and a `keep` output tied to zero (RTL-5).
One behavioral note that is correct-but-surprising is recorded as RTL-6: a clear cancels
an in-flight delayed edge in `helper_circuits`, so a consumer that resets mid-frame gets
no delayed edge for the transition the reset itself caused.

---

## 7. Phase 3 — protocol leaf blocks (4–5 suites)

| DUT | Legacy source → |
| --- | --- |
| `ipv4_tx` | `test/ipv4/ipv4_tx_tb.ml` → `test/ipv4/ipv4_tx/` |
| `ipv4_rx` | `test/ipv4/ipv4_rx_tb.ml` → `test/ipv4/ipv4_rx/` |
| `udp_tx` | `test/udp/udp_tx_tb.ml` → `test/udp/udp_tx/` |
| `udp_rx` | `test/udp/udp_rx_tb.ml` → `test/udp/udp_rx/` |

These four legacy tbs are the best-developed in the repo (274–395 lines each, with real
golden models and coverage notes in their headers). Translation is mostly mechanical: their
SW models become `Hardcaml_verif.Ip_udp` calls, their `expect "..." cond` checks become
`[%test_result]` assertions or golden traces, and their scenario drivers become
`Testbench.run_*` entry points. **Preserve the coverage lists** from each legacy header into
the new `*_testbench.ml` description comment.

`lib/udp/udp.ml` also exposes `I` / `O` / `create` but has no dedicated tb. Assess during
this phase whether it is independently instantiable; add `test/udp/udp/` only if so.

`test/udp/udp_alcotest_lib.ml` is ~90 lines of almost entirely commented-out Alcotest
scaffolding. Leave it untouched until the Alcotest question (§10) is settled — note that
`test/udp/dune` has no `(modules)` field, so it currently claims every `.ml` in that
directory; moving tbs into subdirs sidesteps this.

---

## 8. Phase 4 — integration blocks (6 suites)

Same three-file pattern as everything else — **no Alcotest** (see §10).

| DUT | Legacy source → | Notes |
| --- | --- | --- |
| `mac_top` | `test/mii/rx_path_tb.ml` **and** `test/mii/tx_path_tb.ml` → `test/mii/mac_top/` | Two legacy executables in one dir; new suite gets an RX golden trace, a TX golden trace, and a loopback quickcheck (TX a random payload, feed the MII output back into RX, expect the payload out with `tuser=0`) |
| `udp_mac_top` | `test/udp/udp_mac_top_tb.ml` → `test/udp/udp_mac_top/` | |
| `udp_rx_mac_top` | `test/udp/udp_mac_rx_tb.ml` → `test/udp/udp_rx_mac_top/` | note the filename/DUT-name mismatch; the directory follows the **DUT** name |
| `udp_ipv4_rx` | `test/udp/udp_ipv4_rx_tb.ml` → `test/udp/udp_ipv4_rx/` | tests the `ipv4_rx` + `udp_rx` composition, not a single lib module |
| `udp_duplex_mac_top` | `test/udp/udp_duplex_tb.ml` → `test/udp/udp_duplex_mac_top/` | |
| `udp_loopback_mac_top` | `test/udp/udp_loopback_tb.ml` → `test/udp/udp_loopback_mac_top/` | |

Long frame-level scenarios become `let%test_unit "…"` cases in the quickcheck file; expect
tests stay short and readable (one or two representative frames) — a 442-line tb's worth of
golden output is not a useful diff.

Delete `test/mii/dune`, `test/ipv4/dune`, `test/udp/dune`, and `test/uart/dune` once every
tb has moved into a per-DUT subdirectory.

---

## 9. Phase 5 — documentation

- `test/test_architecture.md`: ~~add a "Settled Conventions" section~~ **done in Phase 0** —
  the four-file layout, the `-source-tree-root .` workaround, the legacy-executable
  relegation rule, the `hardcaml_verif` module inventory, the OxCaml arity/local-mode
  rule, the promote-then-read expect workflow, the per-directory formatting rule, and a
  pointer to `docs/formatting_guide.md`. Alcotest, EventSim, and the `_i`/`_o` rename
  deferments are recorded there. Revisit at the end of the sweep to fold in anything
  Phases 1–4 settle.
- `docs/verif_sweep_findings.md`: the running findings log — RTL behavior the suites
  pinned down but did not change, and process/tooling incidents. Append per phase; review
  the RTL entries at the end of the sweep and decide which become real work.
- `/home/wayne/devel/CLAUDE.md`: the `test/` tree listing under "Library layout" is stale
  the moment Phase 1 lands. Regenerate it at the end, and add `hardcaml_verif` to the list
  of dune libraries.

---

## 10. Deferred (explicitly out of scope for this branch)

- **Alcotest.** `test_architecture.md` earmarks Alcotest for "longer more thought out tests
  → proper suites," and the six integration DUTs are the natural candidates. **Held off
  pending confirmation from a Jane Street dev** that Alcotest is the right vehicle here
  rather than plain `let%test_unit` suites. `test/udp/udp_alcotest_lib.ml` and the
  `alcotest` dep in `dune-project` stay in place, untouched, pending that answer. If the
  answer is yes, the retro-fit is purely additive: a fourth file
  `<dut>_integration_tests.ml` per integration dir, reusing the same `*_testbench.ml`.
- **EventSim.** `hardcaml_event_driven_sim` and
  `Hardcaml_step_testbench.Functional.Event_driven_sim` are both installed in the switch,
  and `test_architecture.md` calls for portable equivalence tests across both backends.
  `Sim_fixture` should be designed so a second backend slots in as a parallel functor later,
  but no EventSim suites are written here.
- **`_i` / `_o` port rename.** See §2. Not part of this sweep.

---

## 11. Verification

Per phase, from the repo root:

```sh
source ./env.sh
./scripts/with-switch.sh dune build @fmt     # formatting-guide §9
./scripts/with-switch.sh dune build @lint    # formatting-guide §9
./scripts/with-switch.sh dune build          # must be clean, legacy executables included
./scripts/with-switch.sh dune runtest        # must be green with no expect diffs
```

Expect-test workflow: write `[%expect {| |}]` empty, run `dune runtest`, then
`dune promote` — **and then read the promoted output against the RTL's intended behavior
before committing.** These goldens freeze current behavior; blind promotion would enshrine a
bug as the specification. Where a legacy assertion tb asserted a specific value, cross-check
the new golden against that assertion before the legacy file is relegated.

End-state check:

- `dune runtest` runs 20 inline-test libraries (3 from phase 1 + 17 new); no `(tests)`
  stanzas remain anywhere under `test/`. After phase 2 it runs 12, and the surviving
  `(tests)` stanzas are `test/mii/dune` (`rx_path_tb tx_path_tb`), `test/ipv4/dune` and
  `test/udp/dune` — phases 3 and 4.
- `dune build` compiles every `*_legacy_assertion_test.ml`, and none of them execute.
- Every `lib/` RTL module with an `I` / `O` / `create` triple has a corresponding
  `test/<domain>/<dut>/` directory.
