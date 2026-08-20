#!/usr/bin/env bash
set -euo pipefail

# Default shared machine-level OxCaml switch.
# Override with:
#   OPAM_SWITCH=<switch-name> ./bootstrap.sh
SWITCH="${OPAM_SWITCH:-5.2.0+ox}"

# We expect OxCaml 5.2.x for now.
REQUIRED_OCAML_PREFIX="${REQUIRED_OCAML_PREFIX:-5.2}"

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

INSTALL_DEPS=0

# Keep this list intentionally small and explicit.
# Do not use `opam install . --deps-only` by default against a shared switch.
REQUIRED_PACKAGES=(
  dune
  core
  hardcaml
  ppx_hardcaml
  hardcaml_circuits
)

TEST_PACKAGES=(
  hardcaml_waveterm
  alcotest
)

DEV_PACKAGES=(
  ocaml-lsp-server
  ocamlformat
  ppx_js_style
)

die() {
  echo "error: $*" >&2
  exit 1
}

usage() {
  cat >&2 <<EOF
usage: ./bootstrap.sh [--install-deps]

Default behavior:
  Verify the shared OxCaml switch and required opam packages.

Options:
  --install-deps
      Install the known project package set into the shared opam switch.

Environment:
  OPAM_SWITCH=<switch-name>
      Select opam switch. Default: $SWITCH

  REQUIRED_OCAML_PREFIX=<prefix>
      Required OCaml version prefix. Default: $REQUIRED_OCAML_PREFIX
EOF
}

parse_args() {
  for arg in "$@"; do
    case "$arg" in
      --install-deps)
        INSTALL_DEPS=1
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      *)
        usage
        exit 1
        ;;
    esac
  done
}

check_opam() {
  command -v opam >/dev/null 2>&1 || die "opam is not installed"

  if ! opam var root >/dev/null 2>&1; then
    echo "Initializing opam..."
    opam init --bare --disable-sandboxing -y
  fi
}

check_oxcaml_switch() {
  if ! opam switch list --short | grep -qx "$SWITCH"; then
    cat >&2 <<EOF
error: required opam switch '$SWITCH' was not found.

Detected switches:

$(opam switch list --short 2>/dev/null || true)

Create the shared OxCaml switch once on this machine, for example:

  opam update --all
  opam switch create $SWITCH 5.2.0+ox \\
    --repos ox=git+https://github.com/oxcaml/opam-repository.git,default

Then rerun:

  ./bootstrap.sh

Or use another existing switch:

  OPAM_SWITCH=<switch-name> ./bootstrap.sh
EOF
    exit 1
  fi

  local version compiler
  version="$(opam exec --switch="$SWITCH" -- ocamlc -version)"
  compiler="$(opam exec --switch="$SWITCH" -- which ocamlc)"

  case "$version" in
    "$REQUIRED_OCAML_PREFIX"*) ;;
    *)
      die "switch '$SWITCH' has OCaml version '$version', expected prefix '$REQUIRED_OCAML_PREFIX'"
      ;;
  esac

  echo "Using opam switch: $SWITCH"
  echo "OCaml version: $version"
  echo "Compiler: $compiler"
}

package_is_installed() {
  local pkg="$1"

  opam list \
    --switch="$SWITCH" \
    --installed \
    --short \
    "$pkg" 2>/dev/null | grep -qx "$pkg"
}

check_packages() {
  local missing=()

  for pkg in "${REQUIRED_PACKAGES[@]}" "${TEST_PACKAGES[@]}" "${DEV_PACKAGES[@]}"; do
    if ! package_is_installed "$pkg"; then
      missing+=("$pkg")
    fi
  done

  if [ "${#missing[@]}" -ne 0 ]; then
    echo "error: switch '$SWITCH' is missing required packages:"
    printf '  %s\n' "${missing[@]}"
    echo
    echo "Install them with:"
    echo
    echo "  opam install --switch=$SWITCH ${missing[*]}"
    echo
    echo "Or let bootstrap install the known package set:"
    echo
    echo "  ./bootstrap.sh --install-deps"
    exit 1
  fi

  echo "Required opam packages are installed."
}

install_known_packages() {
  echo "Installing known project packages into switch '$SWITCH'..."

  opam install --switch="$SWITCH" -y \
    "${REQUIRED_PACKAGES[@]}" \
    "${TEST_PACKAGES[@]}" \
    "${DEV_PACKAGES[@]}"
}

handle_deps() {
  if [ "$INSTALL_DEPS" = "1" ]; then
    install_known_packages
  else
    check_packages
  fi
}

write_env_file() {
  cat > "$ROOT/env.sh" <<EOF
# Source this file from the repo root:
#
#   source ./env.sh
#
# This sets the default opam switch used by the dune wrapper scripts
# in ./scripts and ./tools.

export OPAM_SWITCH="$SWITCH"
EOF

  echo "Wrote env.sh"
}

main() {
  parse_args "$@"

  cd "$ROOT"

  check_opam
  check_oxcaml_switch
  handle_deps
  write_env_file

  echo
  echo "Bootstrap complete."
  echo
  echo "Select the opam switch for this shell:"
  echo
  echo "  source ./env.sh"
  echo
  echo "Then build, test, and format with dune:"
  echo
  echo "  ./scripts/with-switch.sh dune build"
  echo "  ./scripts/with-switch.sh dune runtest"
  echo "  ./scripts/with-switch.sh dune fmt"
}

main "$@"
