# release-group — release candidate staging

Staging area for GitHub release assets. Each candidate is assembled here, checked,
then zipped and uploaded. Nothing here is authoritative: every file is a **copy** of
something produced elsewhere in the repo, and the whole directory can be deleted and
rebuilt from scratch.

`.gitignore` ignores `release-group/*` except this README, so candidates never land
in the repo history.

## Why copies

`lib/common/generate.ml` writes each target to a fixed repo-relative path
(`hardcaml_eth_mac.v`, `validation/*_validation_harness.v`). Those paths are baked into
the generator, referenced by the top-level README, and are where Vivado picks the RTL
up. Moving the originals in here would break regeneration and the board project. Always
copy.

## Layout

One directory per zip, named `hardcaml-networking-<version>-<bundle>`:

```
release-group/
  MANIFEST.txt                              provenance for the whole candidate
  hardcaml-networking-v1.3-rtl/             the generated Verilog
    hardcaml_eth_mac.v
    hardcaml_udp_with_mac.v
    validation/*_validation_harness.v       (5 board harnesses)
    MANIFEST.txt
    SHA256SUMS
  hardcaml-networking-v1.3-arty-a7/         flash-and-run bundle
    udp_duplex_validation_harness.bit       xc7a100tcsg324-1 ONLY
    unified_tx_rx.xdc
    udp_app.py                              host-side companion
    test_udp_app_echo.py                    offline classifier check
    RUNBOOK.md                              copy of validation/README.md
    MANIFEST.txt
    SHA256SUMS
  hardcaml-networking-v1.3-reports/         evidence the RTL closes timing
    board-routed/                           full Vivado board run (12 reports)
    ooc-udp-duplex-mac-top/                 out-of-context synth estimates
    MANIFEST.txt
    SHA256SUMS
```

The RTL bundle deliberately **mirrors the repo's directory structure** (`validation/`
subdir preserved) so the repo-relative paths in `MANIFEST.txt` resolve correctly from
inside the bundle.

## MANIFEST.txt vs SHA256SUMS

Two different jobs; both are needed.

- **`MANIFEST.txt`** answers *what produced this* — git SHA, tag, branch, tree state,
  opam switch, OCaml/dune/hardcaml versions, plus hashes with repo-relative paths. It is
  written by `scripts/generate_rtl.py` in the same pass that emits the RTL, so it cannot
  disagree with the files. Identical copies go in all three bundles, so someone who
  downloads only one zip still gets full provenance.
- **`SHA256SUMS`** answers *did this download intact* — bundle-relative paths, one per
  bundle, so `sha256sum -c SHA256SUMS` works from inside an unzipped bundle with no
  knowledge of the repo.

## Rebuilding a candidate

One command does everything below — generate, stage, checksum, verify, zip:

```sh
./scripts/make-release.sh --version v1.3 --require-clean
```

`--require-clean` refuses to run against uncommitted tracked changes, so the manifest
names a published commit. Drop it while iterating; the script warns loudly instead, and
stamps the dirty state into `MANIFEST.txt`.

Useful flags:

| flag | effect |
| --- | --- |
| `--skip-generate` | stage from RTL already on disk (requires an existing `MANIFEST.txt`) |
| `--no-zip` | stage and verify only |
| `--bitstream <path>` | override the board bundle's `.bit` |
| `--ooc <dir>` | stage an out-of-context report directory; repeatable |
| `--output <dir>` | staging directory (default `release-group`) |

It re-clears only the three bundle directories for the given `--version`, so re-running
is safe and idempotent. `--version` has no default on purpose: the script will not guess
your release number.

Two things it refuses to do quietly. It dies rather than staging a partial bundle if any
target is missing, and it skips any `--ooc` directory with no `post_synth_*` outputs —
`hardcaml_xilinx_reports` generates a project directory per circuit whether or not Vivado
ever ran, so a populated-looking directory is not evidence of a run. The RTL file list
comes from `generate_rtl.py --list`, so the two scripts cannot drift on which targets
exist or where they land.

Doing it by hand instead:

```sh
cd release-group/<bundle>
find . -type f ! -name SHA256SUMS -printf '%P\n' | sort | xargs sha256sum > SHA256SUMS
sha256sum -c SHA256SUMS
```

## What this candidate ships, and on what evidence

The board bundle is the one that changes the character of the release: an Arty A7-100T
owner with no OCaml and no Vivado can flash it and watch UDP round-trip. It is bound to
`xc7a100tcsg324-1` — the filename says so on purpose, so nobody tries it on a 35T.

The reports back that up. From the routed board run:

```
WNS 5.679ns   WHS 0.085ns   0 failing endpoints (3770 total)
1212 LUTs (1.91%)   1685 FFs (1.33%)   37 IOB   0 BRAM
```

## Outstanding for v1.3

- [ ] **Version decision.** Tags run `release`, `release_v1.1`, `release_v1.2`. This
      candidate is named v1.3, but it is ~60 commits and +18k lines past v1.2 (hierarchy
      rework, expect/quickcheck suites, `hardcaml_xilinx_reports` integration). If that
      reads as 2.0, rename the three directories before zipping.
- [ ] **Cut from a committed state.** `MANIFEST.txt` currently records branch
      `bpurtell/hardcaml-reports-stage1`. Merge and re-run with `--require-clean` so the
      manifest names a published commit.
- [ ] **Loopback bitstream** (optional). Only the duplex harness has a `.bit`. The
      loopback harness is what `udp_app.py --echo` targets — the one host-asserted,
      exit-code-checkable path. Shipping it needs another Vivado run.
- [ ] **Per-block OOC resource reports** (optional). `_build/xilinx-reports/resources/
      udp-duplex-mac-top/` has 21 circuit projects generated but **not run** — no
      `post_synth_*` outputs. Only the whole-design timing profile for the top has real
      reports, which is what `ooc-udp-duplex-mac-top/` contains. See `synthesis/README.md`
      for the `-run` invocation.
- [ ] **Release notes.** Nothing here drafts them.

## Not shipped, deliberately

`generate.exe` — 55 MB, unstripped, dynamically linked against the `5.2.0+ox` switch.
Anyone who can run it can build it; anyone who cannot gets a paperweight. Its entire
output is the RTL bundle above, which is the thing worth shipping.

Also excluded: `_build/`, `validation/vivado25_proj/` (the Vivado project itself, large
and machine-specific), `__pycache__/`, `.Xil/`, and
`validation/constraints/arty_master_DO_NOT_EDIT.xdc` (Digilent's vendor master, already
in the source tarball).
