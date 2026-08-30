#!/usr/bin/env bash
set -euo pipefail

# Assemble GitHub release assets into release-group/.
#
# Regenerates the RTL and its manifest, stages the three bundles, writes and
# verifies per-bundle checksums, and zips the result. Everything staged is a
# COPY: lib/common/generate.ml writes each target to a fixed repo-relative path
# that Vivado and the top-level README depend on, so the originals stay put.
#
# See release-group/README.md for the layout and the MANIFEST/SHA256SUMS split.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

VERSION=""
STAGE="$ROOT/release-group"
SKIP_GENERATE=0
REQUIRE_CLEAN=0
NO_ZIP=0

PREFIX="hardcaml-networking"
BUNDLES=(rtl arty-a7 reports)

VIVADO_RUNS="$ROOT/validation/vivado25_proj/pre_synth_validation_run.runs"
IMPL_DIR="$VIVADO_RUNS/impl_1"
SYNTH_DIR="$VIVADO_RUNS/synth_1"

# Default flash-and-run bitstream. Bound to xc7a100tcsg324-1; the asset name
# carries the part so nobody tries it on a 35T.
BITSTREAM="$IMPL_DIR/udp_duplex_validation_harness.bit"

# Out-of-context report directories. Only dirs containing post_synth_* outputs
# are staged: hardcaml_xilinx_reports generates a project per circuit whether or
# not Vivado ever ran, so a populated-looking directory is not evidence of a run.
OOC_DIRS=("$ROOT/_build/xilinx-reports/timing/udp-duplex-mac-top/udp_duplex_mac_top")

# Host-side companions. A bitstream without these is inert.
BOARD_EXTRAS=(
  "$ROOT/validation/udp_app.py"
  "$ROOT/validation/test_udp_app_echo.py"
  "$ROOT/validation/constraints/unified_tx_rx.xdc"
)

die() {
  echo "error: $*" >&2
  exit 1
}

warn() {
  echo "warning: $*" >&2
}

info() {
  echo "$*"
}

usage() {
  cat >&2 <<EOF
usage: ./scripts/make-release.sh --version <ver> [options]

Assembles release assets into release-group/ and zips them.

Required:
  --version <ver>
      Release version, e.g. v1.3. Determines bundle and zip names. There is no
      default: the script will not guess your release number.

Options:
  --skip-generate
      Stage from the RTL already on disk instead of re-running the generator.
      MANIFEST.txt must already exist.
  --require-clean
      Refuse to run if tracked files have uncommitted changes. Use when cutting
      a real release, so the manifest names a published commit.
  --bitstream <path>
      Bitstream for the board bundle.
      (default: validation/vivado25_proj/.../udp_duplex_validation_harness.bit)
  --ooc <dir>
      Out-of-context report directory to stage; repeatable. Replaces the default
      on first use. Directories with no post_synth_* reports are skipped.
  --output <dir>
      Staging directory (default: release-group).
  --no-zip
      Stage and verify, but do not create zips.
  -h, --help
      This message.

Examples:
  ./scripts/make-release.sh --version v1.3 --require-clean
  ./scripts/make-release.sh --version v1.3 --skip-generate --no-zip
EOF
  exit 1
}

parse_args() {
  local ooc_overridden=0
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --version)      [[ $# -ge 2 ]] || die "--version needs an argument"; VERSION="$2"; shift 2 ;;
      --output)       [[ $# -ge 2 ]] || die "--output needs an argument"; STAGE="$2"; shift 2 ;;
      --bitstream)    [[ $# -ge 2 ]] || die "--bitstream needs an argument"; BITSTREAM="$2"; shift 2 ;;
      --ooc)
        [[ $# -ge 2 ]] || die "--ooc needs an argument"
        if [[ $ooc_overridden -eq 0 ]]; then OOC_DIRS=(); ooc_overridden=1; fi
        OOC_DIRS+=("$2"); shift 2 ;;
      --skip-generate) SKIP_GENERATE=1; shift ;;
      --require-clean) REQUIRE_CLEAN=1; shift ;;
      --no-zip)        NO_ZIP=1; shift ;;
      -h|--help)       usage ;;
      *)               die "unknown option: $1 (try --help)" ;;
    esac
  done
  [[ -n "$VERSION" ]] || { echo "error: --version is required" >&2; usage; }
}

bundle_dir() {
  echo "$STAGE/$PREFIX-$VERSION-$1"
}

check_prereqs() {
  command -v sha256sum >/dev/null || die "sha256sum not found"
  if [[ $NO_ZIP -eq 0 ]]; then
    command -v zip >/dev/null || die "zip not found (or pass --no-zip)"
  fi
  [[ -x "$ROOT/scripts/generate_rtl.py" ]] || die "scripts/generate_rtl.py not found or not executable"
}

# Clear only the bundle directories this script owns. Names are fully determined
# by --version and the fixed suffixes, so this cannot wander outside the stage.
clean_stage() {
  mkdir -p "$STAGE"
  local b dir
  for b in "${BUNDLES[@]}"; do
    dir="$(bundle_dir "$b")"
    if [[ -d "$dir" ]]; then
      info "  clearing $(basename "$dir")"
      rm -rf "$dir"
    fi
  done
}

generate_rtl() {
  if [[ $SKIP_GENERATE -eq 1 ]]; then
    info "skipping generation (--skip-generate)"
    [[ -f "$ROOT/MANIFEST.txt" ]] || die "MANIFEST.txt not found; run without --skip-generate"
    return
  fi
  local args=(--extra "validation/constraints/unified_tx_rx.xdc")
  [[ -f "$BITSTREAM" ]] && args+=(--extra "$BITSTREAM")
  [[ $REQUIRE_CLEAN -eq 1 ]] && args+=(--require-clean)
  info "generating RTL and manifest ..."
  "$ROOT/scripts/generate_rtl.py" "${args[@]}" --sha256sums SHA256SUMS \
    || die "RTL generation failed; nothing staged"
}

# The generated .v paths come from generate_rtl.py --list, so the two scripts
# cannot drift apart on which targets exist or where they land.
assemble_rtl() {
  local dir; dir="$(bundle_dir rtl)"
  mkdir -p "$dir"
  local rel count=0
  while read -r rel; do
    [[ -n "$rel" ]] || continue
    [[ -f "$ROOT/$rel" ]] || die "expected RTL missing: $rel (regenerate without --skip-generate)"
    mkdir -p "$dir/$(dirname "$rel")"
    cp "$ROOT/$rel" "$dir/$rel"
    count=$((count + 1))
  done < <("$ROOT/scripts/generate_rtl.py" --list | awk '{print $3}')
  [[ $count -gt 0 ]] || die "no RTL targets found"
  info "  rtl: $count Verilog file(s)"
}

assemble_board() {
  local dir; dir="$(bundle_dir arty-a7)"
  mkdir -p "$dir"
  if [[ -f "$BITSTREAM" ]]; then
    cp "$BITSTREAM" "$dir/"
  else
    warn "bitstream not found: $BITSTREAM"
    warn "  board bundle will ship without a flashable image"
  fi
  local f
  for f in "${BOARD_EXTRAS[@]}"; do
    [[ -f "$f" ]] && cp "$f" "$dir/" || warn "board extra not found: $f"
  done
  # The runbook is the reason someone can use the bitstream at all.
  [[ -f "$ROOT/validation/README.md" ]] && cp "$ROOT/validation/README.md" "$dir/RUNBOOK.md"
  info "  arty-a7: $(find "$dir" -type f | wc -l) file(s)"
}

assemble_reports() {
  local dir; dir="$(bundle_dir reports)"
  mkdir -p "$dir/board-routed"
  local n=0
  if compgen -G "$IMPL_DIR/*.rpt" >/dev/null; then
    cp "$IMPL_DIR"/*.rpt "$dir/board-routed/"
    n=$(find "$dir/board-routed" -type f | wc -l)
  else
    warn "no routed board reports under $IMPL_DIR"
  fi
  if compgen -G "$SYNTH_DIR/*.rpt" >/dev/null; then
    cp "$SYNTH_DIR"/*.rpt "$dir/board-routed/"
    n=$(find "$dir/board-routed" -type f | wc -l)
  fi
  info "  reports: $n board report(s)"

  local ooc staged
  for ooc in "${OOC_DIRS[@]}"; do
    [[ -d "$ooc" ]] || { warn "ooc dir not found, skipped: $ooc"; continue; }
    if ! compgen -G "$ooc/post_synth_*" >/dev/null; then
      warn "ooc dir has no post_synth_* reports (project generated but never run), skipped:"
      warn "  ${ooc#"$ROOT"/}"
      continue
    fi
    staged="$dir/ooc-$(basename "$ooc" | tr '_' '-')"
    mkdir -p "$staged"
    cp "$ooc"/post_synth_* "$staged/"
    info "  reports: $(find "$staged" -type f | wc -l) OOC report(s) from $(basename "$ooc")"
  done
}

# Identical provenance in every bundle, so a single downloaded zip is still
# traceable to a commit and a toolchain.
copy_manifest() {
  [[ -f "$ROOT/MANIFEST.txt" ]] || die "MANIFEST.txt not found"
  cp "$ROOT/MANIFEST.txt" "$STAGE/"
  local b
  for b in "${BUNDLES[@]}"; do
    cp "$ROOT/MANIFEST.txt" "$(bundle_dir "$b")/"
  done
}

# Bundle-relative paths, so `sha256sum -c` works inside an unzipped bundle with
# no knowledge of this repo.
checksum_bundles() {
  local b dir
  for b in "${BUNDLES[@]}"; do
    dir="$(bundle_dir "$b")"
    ( cd "$dir" && find . -type f ! -name SHA256SUMS -printf '%P\n' \
        | sort | xargs sha256sum > SHA256SUMS )
  done
}

verify_bundles() {
  local b dir ok
  for b in "${BUNDLES[@]}"; do
    dir="$(bundle_dir "$b")"
    ( cd "$dir" && sha256sum -c SHA256SUMS >/dev/null 2>&1 ) \
      || die "checksum verification failed in $(basename "$dir")"
    ok=$(wc -l < "$dir/SHA256SUMS")
    info "  $(basename "$dir"): $ok file(s) verified"
  done
}

make_zips() {
  if [[ $NO_ZIP -eq 1 ]]; then
    info "skipping zips (--no-zip)"
    return
  fi
  local b name
  for b in "${BUNDLES[@]}"; do
    name="$PREFIX-$VERSION-$b"
    rm -f "$STAGE/$name.zip"
    ( cd "$STAGE" && zip -rq "$name.zip" "$name" )
    info "  $(du -h "$STAGE/$name.zip" | cut -f1)	$name.zip"
  done
}

main() {
  parse_args "$@"
  check_prereqs

  info "staging $PREFIX $VERSION into ${STAGE#"$ROOT"/}"
  clean_stage
  generate_rtl

  info "assembling bundles ..."
  assemble_rtl
  assemble_board
  assemble_reports
  copy_manifest

  info "checksumming ..."
  checksum_bundles
  verify_bundles

  info "zipping ..."
  make_zips

  info ""
  info "done. upload from ${STAGE#"$ROOT"/}:"
  local b
  for b in "${BUNDLES[@]}"; do
    if [[ $NO_ZIP -eq 0 ]]; then
      info "  $PREFIX-$VERSION-$b.zip"
    else
      info "  $PREFIX-$VERSION-$b/"
    fi
  done
  info "  MANIFEST.txt"
  grep -q '^tree state       DIRTY' "$ROOT/MANIFEST.txt" \
    && warn "tree was dirty; this candidate does not match any published commit"
  return 0
}

main "$@"
