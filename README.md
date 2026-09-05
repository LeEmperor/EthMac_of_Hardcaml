# hardcaml_networking

Networking hardware written in Hardcaml. Currently a full-duplex Ethernet MAC for 10/100 MBPs transfers, plus an IPv4 (L3) and UDP (L4) stack layered on top of it, and board bring-up harnesses for validating the whole thing on real hardware. Targeted on Arty A7-100T DP83848x PHY present on board.

Written in [HardCaml](https://github.com/janestreet/hardcaml) as a learning project. Any suggestions or contributions welcome.

Docs for PHY can be found [here](https://www.ti.com/lit/ds/symlink/dp83848j.pdf?ts=1776919033995&ref_url=http%253A%252F%252Fwww.ti.com%252F).

A Crude Image of TX path Reception:
<img width="1280" height="960" alt="image" src="https://github.com/user-attachments/assets/5b268704-c7fa-4f06-a22a-3bd179774248" />

---
<br>

# Repository Layout

```
lib/
  common/   helper circuits, Arty board pin contract, clock divider, RTL generator entry point
  mii/      the Ethernet MAC itself — RX/TX controllers, datapaths, CRC, byte (dis)assembly
  ipv4/     IPv4 header generation (ipv4_tx) and parsing (ipv4_rx)
  udp/      UDP header generation/parsing, plus the tops that stack UDP+IPv4 onto the MAC
  uart/     UART transmitter
test/       testbenches, mirroring the lib/ layout
validation/ board-level harnesses (Arty scaffolding, XDC constraints, host-side Python)
verilog_artifacts/ hand-written SystemVerilog from earlier board bring-up, kept for reference
```

The MAC knows nothing about IP or UDP — `mii_of_hardcaml` has no dependency on the upper
layers. "Including a UDP/IP stack" is a question of what you instantiate *around* the MAC,
which is what the tops in `lib/udp/` do.

Two longer-form notes live alongside the code:

- `HardcamlDocs.md` — Hardcaml usage notes (Cyclesim, Evsim, the Always DSL, and so on).
- `test/test_architecture.md` — how the verification suites are structured, written from a
  UVM background.

<br>

---

# Installation Pre-Requisites

## Automatic
```./bootstrap.sh --install-deps``` verifies you have an [OxCaml](https://oxcaml.org/get-oxcaml/) [opam](https://opam.ocaml.org/) switch, installs the project's package dependencies into it, and writes `env.sh`.

It does **not** create the switch for you — if the switch is missing, bootstrap stops and
prints the `opam switch create` line to run. Create it once (see Manual below), then rerun
bootstrap.

WARNING: The dependency install may take up to 30 minutes!

<br>


## Manual
This is the recommended way of installing as any breaking objects won't damage the state of the repo.

#### OxCaml
OxCaml install:

```opam switch create 5.2.0+ox 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default```

The scripts in `./scripts` and `./tools` default to a switch named `5.2.0+ox`. If you name
yours something else, export `OPAM_SWITCH=<your-switch-name>` (or edit the generated
`env.sh`) so the wrappers can find it.

You will also want the following libraries for [OCaml](https://ocaml.org/)
1. ```dune```
2. ```core```
3. ```hardcaml```
4. ```hardcaml_xilinx_reports```
5. ```ppx_hardcaml```
6. ```ppx_jane```
7. ```hardcaml_circuits```
8. ```hardcaml_waveterm```
9. ```hardcaml_step_testbench```
10. ```alcotest```
11. ```ocamlformat```
12. ```ppx_js_style```
13. ```ocaml-lsp-server``` (for editor integration)

Use ```opam install --switch=5.2.0+ox -y dune core hardcaml hardcaml_xilinx_reports ppx_hardcaml ppx_jane hardcaml_circuits hardcaml_waveterm hardcaml_step_testbench alcotest ocamlformat ppx_js_style ocaml-lsp-server``` to install the set of dependencies manually,

```OR``` let the bootstrap ```--install-deps``` flag handle it for you. You can also opt to install the main OxCaml switch yourself, and then let the dependencies afterwards get handled by ```./bootstrap.sh```.

<br>

#### Ubuntu/Debian (tested on 22.04/24.04)
```sudo apt install opam build-essential pkg-config```

#### macOS
```brew install opam```

#### Windows
```lmao```

<br>

---

# Setup
Run ```./bootstrap.sh```, followed by ```source ./env.sh``` to select the OxCaml opam switch for the current shell. `env.sh` is written *by* bootstrap and is not checked in, so run bootstrap first.

The project builds entirely with [dune](https://dune.build/). All commands go through `./scripts/with-switch.sh` so they run on the `5.2.0+ox` switch:

```sh
./scripts/with-switch.sh dune build      # build everything
./scripts/with-switch.sh dune runtest    # run all testbenches
./scripts/with-switch.sh dune fmt        # format
./scripts/with-switch.sh dune build @lint # Jane Street style checks
```

`dune runtest` covers both the standalone testbench executables and the inline
expect/quickcheck suites under `test/mii/<block>/`.

Generated VCD files can be opened with `./tools/open_wave.sh <vcd-file>`.
A single testbench runs through `dune exec`. The VCD is written to the directory the
command is run from, so create `waves/` first:

```sh
mkdir -p waves && ./scripts/with-switch.sh dune exec test/mii/tx_path_tb.exe
```

<br>

---

# Generating RTL

`lib/common/generate.exe` emits Verilog, one subcommand per target, so there is no
comment-toggling of the generator source:

```sh
./scripts/with-switch.sh dune exec lib/common/generate.exe -- mac
./scripts/with-switch.sh dune exec lib/common/generate.exe -- udp
./scripts/with-switch.sh dune exec lib/common/generate.exe -- udp-rx-64
./scripts/with-switch.sh dune exec lib/common/generate.exe -- mac-validation
```

| target | what it emits |
| --- | --- |
| `mac` | standalone Ethernet MAC |
| `udp` | UDP-over-MAC stack |
| `udp-rx-64` | RX-only UDP-over-MAC stack with a 64-bit application stream |
| `mac-validation` | board MAC harness (bare MAC, both directions) |
| `udp-tx-validation` | board UDP TX harness (fpga -> laptop, `btn[3]`) |
| `udp-rx-validation` | board UDP RX harness (laptop -> fpga, 1 B/s drain) |
| `udp-duplex-validation` | full-duplex UDP harness, decoupled TX + RX |
| `udp-loopback-validation` | echo/loopback UDP harness, RX->TX bridge |

The `udp-rx-64` target is intended for functional integration with applications normally
fed by a 64-bit 10G MAC. It keeps the MII, Ethernet, IPv4, and UDP receive path byte-wide,
then packs the stripped UDP payload into `app_tdata_o[63:0]`. The first received byte is
in `app_tdata_o[7:0]`; `app_tkeep_o[7:0]` marks valid lanes on the final beat, and
`app_tvalid_o/app_tready_i/app_tlast_o/app_tfirst_o` retain normal AXI-stream backpressure
and framing semantics. This reproduces the data interface, not 10G arrival timing: an Arty
100 Mb/s PHY supplies bytes far more slowly than a 64-bit 10G MAC.

Run `dune exec lib/common/generate.exe -- -help` for the current list. Output paths are
resolved against the repo root, so the RTL lands in a stable place no matter where the
binary ran.

To emit every target at once, plus a `MANIFEST.txt` recording the commit and toolchain
that produced them, use `./scripts/generate_rtl.py` (`--list` shows the targets).

Synthesis estimates and resource reports use a separate executable; see
[`synthesis/README.md`](synthesis/README.md).

<br>

---

# Generating Release Candidates

`./scripts/make-release.sh --version v1.3 --require-clean` regenerates the RTL, stages the
release bundles into `release-group/`, checksums and verifies them, and writes the zips to
upload. `--require-clean` refuses to run against uncommitted changes so the manifest names
a published commit.

It does not invoke Vivado: the bitstream and board reports are copied from the existing
project run, so re-run implementation first if the RTL changed. See
[`release-group/README.md`](release-group/README.md) for the bundle layout and the full
release procedure.

<br>

---

# Board Validation

`validation/` holds everything needed to run a design on the Arty rather than in a
simulator:

- `board_scaffolding.ml` — shared Arty plumbing (reset synchronizers, the 25 MHz PHY
  reference clock, PHY hard-reset sequencing, heartbeat LED, CDC helpers). Each harness
  supplies only its own stimulus FSM, the core it wraps, and its LED map.
- `*_validation_harness.ml` — the harnesses themselves, emitted by the generator targets
  above.
- `constraints/unified_tx_rx.xdc` — pin constraints, named to line up with the board top's
  I/O fields exactly. `arty_master_DO_NOT_EDIT.xdc` is the untouched vendor master.
- `udp_app.py` — host-side companion. `--echo` sends a datagram and asserts the FPGA echoes
  it back (nonce-tagged, so late/lost/duplicated/corrupt are classified rather than lumped
  together); `--validate` sniffs and checks the datagrams the FPGA emits. Needs `scapy` and
  raw-socket privileges.
- `test_udp_app_echo.py` — offline check of the echo classifier, no board and no root
  required: `python3 validation/test_udp_app_echo.py`.
- `send_test_frames.py` — sends raw Ethernet frames at the older LED-drain test design.

<br>

---

## Emacs

The repository is a Git-backed Emacs project, so `project.el` and Projectile detect it
without an extra marker file. The checked-in `.dir-locals.el` sets two-space,
space-only OCaml indentation and configures these project commands:

- configure: `./bootstrap.sh`
- compile: `./scripts/with-switch.sh dune build`
- test: `./scripts/with-switch.sh dune runtest`

Run `M-x project-compile` (or Projectile's compile/test commands) from any project
buffer. Eglot users can format the current buffer with `M-x eglot-format-buffer`.
Start Emacs from a shell in which the project switch is selected when using Merlin or
Eglot, so `ocamllsp` and `ocamlformat` come from the OxCaml switch:

```sh
source ./env.sh
opam exec --switch="$OPAM_SWITCH" -- emacs .
```

OCamlFormat uses its Jane Street profile from `.ocamlformat`; `dune fmt` is the
authoritative formatter. `ppx_jane` supplies Jane Street syntax extensions and
derivers, while `ppx_js_style` is a separate style checker. The latter runs only via
the Dune `@lint` alias (or `./tools/dune_lint.sh`), rather than changing normal PPX
expansion during every build.
