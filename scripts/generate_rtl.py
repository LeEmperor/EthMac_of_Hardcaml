#!/usr/bin/env python3
"""Generate every Hardcaml RTL target and write a provenance manifest.

Runs each subcommand of lib/common/generate.exe through scripts/with-switch.sh,
verifies the output was actually refreshed by this run, and records what
produced it in MANIFEST.txt.

The manifest is written by the same pass that emits the RTL on purpose. A
manifest produced separately is only a claim about the files; one produced here
cannot disagree with them.

Typical release use, from the repo root:

    ./scripts/generate_rtl.py --require-clean \
        --extra validation/constraints/unified_tx_rx.xdc \
        --extra validation/vivado25_proj/pre_synth_validation_run.runs/impl_1/udp_duplex_validation_harness.bit \
        --sha256sums SHA256SUMS

Verification is standard:

    sha256sum -c SHA256SUMS

A "refreshed" check guards against the failure mode where a target errors out
but leaves last run's .v sitting on disk looking current — the same reason
synthesis/xilinx_reports.ml requires its reports to be nonempty and newly
written before it prints a successful summary.
"""

import argparse
import datetime
import hashlib
import os
import subprocess
import sys

# Subcommand -> output path, relative to the repo root. Mirrors the Command.group
# at the bottom of lib/common/generate.ml; keep the two in step.
TARGETS = [
    ("mac", "hardcaml_eth_mac.v"),
    ("udp", "hardcaml_udp_with_mac.v"),
    ("mac-validation", "validation/mac_validation_harness.v"),
    ("udp-tx-validation", "validation/udp_tx_validation_harness.v"),
    ("udp-rx-validation", "validation/udp_rx_validation_harness.v"),
    ("udp-duplex-validation", "validation/udp_duplex_validation_harness.v"),
    ("udp-loopback-validation", "validation/udp_loopback_validation_harness.v"),
]

# Recorded in the manifest. These are the packages whose version actually
# changes the emitted Verilog.
OPAM_PACKAGES = [
    "hardcaml",
    "hardcaml_xilinx_reports",
    "ppx_hardcaml",
    "hardcaml_circuits",
    "core",
]

GENERATOR = "lib/common/generate.exe"
WITH_SWITCH = "scripts/with-switch.sh"

# Tolerance for the mtime freshness check, in seconds. Absorbs coarse filesystem
# timestamp granularity; far tighter than the gap between two real runs.
MTIME_SLACK = 2.0


def switch_env(switch):
    """scripts/with-switch.sh selects the switch from $OPAM_SWITCH, not from a
    flag. Propagating --switch through the environment is what makes the switch
    recorded in the manifest the same one that actually built the RTL."""
    env = os.environ.copy()
    env["OPAM_SWITCH"] = switch
    return env


def run(cmd, cwd, env=None, capture=True):
    """Run cmd, returning CompletedProcess. Never raises on nonzero exit."""
    return subprocess.run(
        cmd, cwd=cwd, text=True, env=env,
        stdout=subprocess.PIPE if capture else None,
        stderr=subprocess.STDOUT if capture else None,
    )


def probe(cmd, cwd, default="unknown", env=None):
    """Run cmd for its output; return default if it fails or prints nothing."""
    p = run(cmd, cwd, env=env)
    if p.returncode != 0:
        return default
    return p.stdout.strip() or default


def repo_root():
    p = run(["git", "rev-parse", "--show-toplevel"], cwd=os.path.dirname(os.path.abspath(__file__)))
    if p.returncode != 0:
        sys.exit("error: not inside a git repository")
    return p.stdout.strip()


def git_info(root):
    """Source provenance. Dirtiness considers tracked files only: the generated
    .v are untracked by design, so counting them would report every tree dirty."""
    tracked = probe(["git", "status", "--porcelain", "--untracked-files=no"], root, default="")
    return {
        "version": probe(["git", "describe", "--tags", "--always", "--dirty"], root),
        "commit": probe(["git", "rev-parse", "HEAD"], root),
        "branch": probe(["git", "rev-parse", "--abbrev-ref", "HEAD"], root),
        "dirty": bool(tracked),
        "dirty_files": tracked,
    }


def toolchain(root, switch):
    """Versions that determine what the generator emits."""
    info = {
        "opam switch": switch,
        "ocaml": probe(["opam", "exec", "--switch=" + switch, "--", "ocamlc", "-version"], root),
        "dune": probe(["opam", "exec", "--switch=" + switch, "--", "dune", "--version"], root),
    }
    listed = probe(
        ["opam", "list", "--switch=" + switch, "--installed", "--short",
         "--columns=name,version"] + OPAM_PACKAGES,
        root, default="",
    )
    for line in listed.splitlines():
        parts = line.split()
        if len(parts) >= 2:
            info[parts[0]] = parts[1]
    return info


def digest(path):
    """sha256 + size of one file."""
    h = hashlib.sha256()
    size = 0
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
            size += len(chunk)
    return h.hexdigest(), size


def generate(root, targets, started_at, verbose, env):
    """Run each target; return (records, failures).

    A target counts as successful only if its output exists, is nonempty, and
    was written by this run.
    """
    records, failures = [], []
    for name, rel in targets:
        path = os.path.join(root, rel)
        cmd = ["./" + WITH_SWITCH, "dune", "exec", GENERATOR, "--", name]
        print(f"  {name:<24} -> {rel}", flush=True)
        p = run(cmd, root, env=env)

        if p.returncode != 0:
            failures.append((name, f"generator exited {p.returncode}\n{p.stdout.rstrip()}"))
            continue
        if verbose and p.stdout.strip():
            print("    " + p.stdout.strip().replace("\n", "\n    "))
        if not os.path.exists(path):
            failures.append((name, f"expected output {rel} does not exist"))
            continue

        sha, size = digest(path)
        if size == 0:
            failures.append((name, f"{rel} is empty"))
            continue
        mtime = os.stat(path).st_mtime
        if mtime < started_at - MTIME_SLACK:
            stale = datetime.datetime.fromtimestamp(mtime).isoformat(timespec="seconds")
            failures.append((name, f"{rel} was not rewritten by this run (mtime {stale}) — stale output"))
            continue

        records.append({"target": name, "path": rel, "sha256": sha, "size": size})
    return records, failures


def collect_extras(root, extras):
    """Hash additional release artifacts (bitstream, XDC, reports)."""
    records, missing = [], []
    for rel in extras:
        rel = os.path.relpath(os.path.abspath(rel), root) if os.path.isabs(rel) else rel
        path = os.path.join(root, rel)
        if not os.path.isfile(path):
            missing.append(rel)
            continue
        sha, size = digest(path)
        records.append({"target": None, "path": rel, "sha256": sha, "size": size})
    return records, missing


def table(records):
    """sha256 / size / path, aligned."""
    lines = [f"{'sha256':<64}  {'bytes':>9}  path"]
    lines.append(f"{'-' * 64}  {'-' * 9}  {'-' * 4}")
    for r in records:
        lines.append(f"{r['sha256']:<64}  {r['size']:>9}  {r['path']}")
    return "\n".join(lines)


def write_manifest(path, git, tools, rtl, extras, stamp):
    def section(title):
        return f"\n{title}\n{'-' * len(title)}\n"

    out = []
    out.append("hardcaml_networking — RTL generation manifest")
    out.append("=" * 44)
    out.append("")
    out.append(f"generated        {stamp}")
    out.append("generated by     scripts/generate_rtl.py")

    out.append(section("Source"))
    out.append(f"version          {git['version']}")
    out.append(f"commit           {git['commit']}")
    out.append(f"branch           {git['branch']}")
    out.append(f"tree state       {'DIRTY — uncommitted tracked changes' if git['dirty'] else 'clean'}")
    if git["dirty"]:
        out.append("")
        out.append("  Uncommitted tracked files at generation time:")
        for line in git["dirty_files"].splitlines():
            out.append(f"    {line}")
        out.append("")
        out.append("  This RTL does NOT correspond to any published commit.")

    out.append(section("Toolchain"))
    width = max(len(k) for k in tools)
    for k, v in tools.items():
        out.append(f"{k:<{width}}  {v}")

    out.append(section("Generated RTL"))
    out.append(table(rtl))

    if extras:
        out.append(section("Additional artifacts"))
        out.append("Hashed as supplied; not produced by this script.")
        out.append("")
        out.append(table(extras))

    out.append("")
    out.append("Verify with:  sha256sum -c SHA256SUMS")
    out.append("")
    with open(path, "w") as f:
        f.write("\n".join(out))


def write_sha256sums(path, records):
    """sha256sum -c compatible."""
    with open(path, "w") as f:
        for r in records:
            f.write(f"{r['sha256']}  {r['path']}\n")


def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--targets", help="comma-separated subset of targets (default: all)")
    ap.add_argument("--list", action="store_true", help="list targets and exit")
    ap.add_argument("--manifest", default="MANIFEST.txt",
                    help="manifest output path, relative to repo root (default MANIFEST.txt)")
    ap.add_argument("--sha256sums", metavar="PATH",
                    help="also write a sha256sum -c compatible file here")
    ap.add_argument("--extra", action="append", default=[], metavar="PATH",
                    help="additional artifact to hash into the manifest (repeatable): "
                         "bitstream, XDC, Vivado reports")
    ap.add_argument("--require-clean", action="store_true",
                    help="refuse to run if tracked files have uncommitted changes "
                         "(use when cutting a release)")
    ap.add_argument("--switch", default=os.environ.get("OPAM_SWITCH", "5.2.0+ox"),
                    help="opam switch (default $OPAM_SWITCH or 5.2.0+ox)")
    ap.add_argument("-v", "--verbose", action="store_true", help="echo generator output")
    args = ap.parse_args()

    if args.list:
        for name, rel in TARGETS:
            print(f"{name:<24} -> {rel}")
        return 0

    root = repo_root()
    targets = TARGETS
    if args.targets:
        wanted = [t.strip() for t in args.targets.split(",") if t.strip()]
        known = dict(TARGETS)
        unknown = [t for t in wanted if t not in known]
        if unknown:
            sys.exit(f"error: unknown target(s): {', '.join(unknown)}\n"
                     f"       known: {', '.join(n for n, _ in TARGETS)}")
        targets = [(t, known[t]) for t in wanted]

    git = git_info(root)
    if git["dirty"]:
        print("warning: tracked files have uncommitted changes; "
              "generated RTL will not match any commit", file=sys.stderr)
        if args.require_clean:
            print(git["dirty_files"], file=sys.stderr)
            sys.exit("error: --require-clean specified; refusing to generate")

    # Build once so a compile error surfaces cleanly, before any target runs and
    # leaves the tree half-regenerated.
    print(f"building {GENERATOR} on switch {args.switch} ...", flush=True)
    env = switch_env(args.switch)
    env_note = run(["./" + WITH_SWITCH, "dune", "build", GENERATOR], root, env=env)
    if env_note.returncode != 0:
        print(env_note.stdout, file=sys.stderr)
        sys.exit(f"error: failed to build {GENERATOR}")

    started_at = datetime.datetime.now().timestamp()
    print(f"generating {len(targets)} target(s) ...", flush=True)
    rtl, failures = generate(root, targets, started_at, args.verbose, env)

    extras, missing = collect_extras(root, args.extra)
    for rel in missing:
        print(f"warning: --extra not found, omitted from manifest: {rel}", file=sys.stderr)

    if failures:
        print("\nFAILED:", file=sys.stderr)
        for name, why in failures:
            print(f"  {name}: {why}", file=sys.stderr)
        print("\nno manifest written — fix the above and rerun", file=sys.stderr)
        return 1

    stamp = datetime.datetime.now().astimezone().isoformat(timespec="seconds")
    manifest = os.path.join(root, args.manifest)
    write_manifest(manifest, git, toolchain(root, args.switch), rtl, extras, stamp)
    print(f"\nwrote {args.manifest}")

    if args.sha256sums:
        sums = os.path.join(root, args.sha256sums)
        write_sha256sums(sums, rtl + extras)
        print(f"wrote {args.sha256sums}")

    total = sum(r["size"] for r in rtl)
    print(f"{len(rtl)} RTL file(s), {total:,} bytes"
          + (f"; {len(extras)} additional artifact(s)" if extras else ""))
    if git["dirty"]:
        print("NOTE: tree was dirty — see the manifest before publishing", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
