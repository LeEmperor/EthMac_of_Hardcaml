#!/usr/bin/env python3
"""Offline check of udp_app.echo()'s failure classification.

Fakes the wire: AsyncSniffer/sendp are replaced with a stub FPGA that can echo a
probe on time, late, corrupted, duplicated, or not at all. Asserts the resulting
tally, so late-vs-lost is verified without hardware, without root, and without a
board. Run it after touching --echo:

    python3 validation/test_udp_app_echo.py     # exit 0 = all cases classified right
"""
import io
import os
import sys
import threading
import time
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import udp_app as U
from scapy.all import Ether, Raw


class FakeSniffer:
    """Stands in for AsyncSniffer: records prn, fires started_callback on start()."""
    live = None

    def __init__(self, **kw):
        self.prn = kw["prn"]
        self.started_callback = kw.get("started_callback")
        self.running = False
        FakeSniffer.live = self

    def start(self):
        self.running = True
        if self.started_callback:
            self.started_callback()

    def stop(self, join=True):
        self.running = False

    def deliver(self, app):
        """Push an FPGA-sourced echo carrying [app] into the sniffer callback."""
        if not self.running:
            return                      # sniffer torn down: frame is lost, as on the wire
        pkt = Ether(dst="ff:ff:ff:ff:ff:ff", src=U.FPGA_SRC_MAC, type=U.ETHERTYPE) / Raw(
            U.build_datagram(app)
        )
        self.prn(pkt)


def run_case(behaviour, count, timeout=0.3, gap=0.0, app_len=18, pattern="inc",
             drain=None):
    """behaviour(seq, app) -> list of (delay_s, app_bytes) echoes to emit."""
    timers = []

    def fake_sendp(frame, iface=None, count=1, verbose=False):
        app = bytes(frame[Raw].load)[28:]
        seq = U.probe_seq(app)
        for delay, echo_app in behaviour(seq, app):
            t = threading.Timer(delay, FakeSniffer.live.deliver, args=(echo_app,))
            t.daemon = True
            t.start()
            timers.append(t)

    orig_sniffer, orig_sendp, orig_sleep = U.AsyncSniffer, U.sendp, U.time.sleep
    U.AsyncSniffer, U.sendp = FakeSniffer, fake_sendp
    buf = io.StringIO()
    try:
        with redirect_stdout(buf):
            ok = U.echo("fake0", app_len, count, pattern, timeout, gap, drain)
    finally:
        U.AsyncSniffer, U.sendp = orig_sniffer, orig_sendp
        for t in timers:
            t.cancel()
    return ok, buf.getvalue()


def tally_of(out):
    """Pull the summary counts back out of the printed report."""
    t = {}
    in_summary = False
    for line in out.splitlines():
        if line.startswith("==== echo summary"):
            in_summary = True
            continue
        if in_summary:
            parts = line.split()
            if len(parts) == 2 and parts[1].isdigit():
                t[parts[0]] = int(parts[1])
    return t


FAILS = []


def expect(name, got, want, out):
    bad = {k: (got.get(k, 0), v) for k, v in want.items() if got.get(k, 0) != v}
    status = "PASS" if not bad else "FAIL"
    print(f"[{status}] {name}: {got}")
    if bad:
        FAILS.append(name)
        print(f"        expected {bad} (got, want)")
        print("        ---- captured ----")
        print("        " + out.replace("\n", "\n        "))


# 1. all echoes prompt and correct
ok, out = run_case(lambda s, a: [(0.01, a)], count=4)
expect("all on-time", tally_of(out), {"on-time": 4, "late": 0, "bad": 0, "lost": 0}, out)
assert ok, "all-on-time run must exit PASS"

# 2a. seq 1 answered AFTER its deadline, while a LATER probe is open -> LATE, not
#     lost. This is the whole point: with identical payloads it would have been
#     credited to that later probe instead.
ok, out = run_case(lambda s, a: [(0.45 if s == 1 else 0.01, a)], count=4,
                   timeout=0.3, gap=0.2)
expect("late echo (later window)", tally_of(out),
       {"on-time": 3, "late": 1, "bad": 0, "lost": 0}, out)
assert not ok, "a late echo must fail the run"
assert "LATE" in out, "late echo must be labelled LATE"
assert "picked up during probe" in out, "late echo should say which window caught it"

# 2b. the LAST probe answered late -> caught by the drain window, still LATE
ok, out = run_case(lambda s, a: [(0.5 if s == 2 else 0.01, a)], count=3,
                   timeout=0.3, drain=1.0)
expect("late echo (drain)", tally_of(out),
       {"on-time": 2, "late": 1, "bad": 0, "lost": 0}, out)
assert "[drain]" in out, "a straggler caught by the drain should be marked"

# 2c. same straggler, but past the drain horizon -> honestly reported as lost
ok, out = run_case(lambda s, a: [(0.9 if s == 2 else 0.01, a)], count=3,
                   timeout=0.3, drain=0.2)
expect("straggler past the horizon", tally_of(out),
       {"on-time": 2, "late": 0, "bad": 0, "lost": 1}, out)

# 3. seq 2 never comes back -> LOST (survives the drain window)
ok, out = run_case(lambda s, a: [] if s == 2 else [(0.01, a)], count=4)
expect("lost echo", tally_of(out), {"on-time": 3, "late": 0, "bad": 0, "lost": 1}, out)

# 4. seq 1 echoed with a corrupted payload byte (nonce intact) -> BAD
def corrupt(s, a):
    if s != 1:
        return [(0.01, a)]
    b = bytearray(a)
    b[9] ^= 0xFF
    return [(0.01, bytes(b))]

ok, out = run_case(corrupt, count=3)
expect("bad payload", tally_of(out), {"on-time": 2, "late": 0, "bad": 1, "lost": 0}, out)
assert "first differing byte at offset 9" in out, "mismatch must report the offset"

# 5. duplicate echo of the same seq
ok, out = run_case(lambda s, a: [(0.01, a), (0.02, a)], count=2)
expect("duplicate", tally_of(out), {"on-time": 2, "dup": 2}, out)
assert not ok, "duplicates must fail the run"

# 6. echo whose nonce itself is corrupted -> unmatched + the probe reads as lost
def nonce_corrupt(s, a):
    b = bytearray(a)
    b[3] ^= 0x80
    return [(0.01, bytes(b))]

ok, out = run_case(nonce_corrupt, count=2)
expect("nonce corrupted", tally_of(out), {"on-time": 0, "lost": 2, "unmatched": 2}, out)

# 7. one drop must NOT alias the rest of the run (the failure mode being fixed:
#    with identical payloads, probe k+1 would have "passed" on probe k's echo)
def drop_first_then_delay(s, a):
    return [] if s == 0 else [(0.05, a)]

ok, out = run_case(drop_first_then_delay, count=5)
expect("no cascade after a drop", tally_of(out),
       {"on-time": 4, "late": 0, "bad": 0, "lost": 1}, out)

# 8. --app-len below the nonce size is rejected rather than silently mis-stamped
ok, out = run_case(lambda s, a: [(0.01, a)], count=1, app_len=4)
print(f"[{'PASS' if not ok else 'FAIL'}] short app-len rejected")
if ok:
    FAILS.append("short app-len rejected")

print()
print(f"==== {'ALL PASS' if not FAILS else 'FAILURES: ' + ', '.join(FAILS)} ====")
sys.exit(1 if FAILS else 0)
