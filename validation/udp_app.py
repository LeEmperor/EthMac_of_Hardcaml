#!/usr/bin/env python3
"""Host-side companion for the UDP-over-MAC validation harness on the Arty A7.

Three modes:

  --echo       Send a datagram AND assert the FPGA echoes it back with the same
               payload (host -> FPGA -> host). Targets the loopback harness
               (udp_loopback_validation_harness), whose RX->TX bridge feeds the
               recovered payload straight back out. This is the host-asserted
               closure of the RX path: exit 0 = PASS, no LED eyeballing.

               Each probe carries a 4-byte nonce (magic 'SQ' + 16-bit seq) in
               the head of the app payload, and ONE sniffer stays armed for the
               whole run, so every failure is classified rather than lumped:

                 on-time  echo of seq k came back inside its own deadline
                 late     it came back, but after its deadline (rtt printed) —
                          nothing was dropped; look host-side/store-and-forward
                 bad      seq matched but header/payload failed the check —
                          forward-everything echoed a corrupted inbound frame
                 lost     seq never seen, even after the post-run drain — a real
                          drop (check crc_error on led2_r/led3_r at that instant)
                 dup      the same seq echoed more than once
                 unmatched  FPGA-sourced frame whose nonce is absent/unknown

               Without the nonce these are indistinguishable: identical payloads
               let a late echo be mistaken for the NEXT probe's echo, so one drop
               silently shifts the whole run by one.

  --validate   Sniff the wire and verify the UDP datagrams the FPGA emits
               (FPGA -> host). This is the currently-validatable path: the
               Udp_mac_top TX stack (Udp_tx on Mac_top) is sim-verified and
               driven on the board by a btn[3] press. Each press emits one
               datagram; this parses + checks the IPv4/UDP header and payload.

  --send       Craft + send a UDP datagram toward the FPGA (host -> FPGA) to
               exercise the RX path. Targets the Udp_rx_mac_top RX stack
               (Ipv4_rx + Udp_rx stacked on Mac_top's RX path), which is
               sim-verified and driven on the board by
               udp_rx_mac_top_validation_harness. The frame's dst UDP port
               (0x1235) and ethertype (0x0800) match what that harness accepts.

               Confirmation is VISUAL on the board LEDs — this is fire-and-forget;
               there is no host-side readback of the recovered datagram yet (that
               needs an echo-back full-duplex harness). Use --pattern alt to send
               an alternating 0xAA/0x55 payload so the harness's 1-byte/sec drain
               makes led[3:0] visibly toggle 0xA <-> 0x5 as each recovered byte
               pops — the UDP mirror of the MAC-RX FIFO-drain check.

ETHERTYPE
---------
The MAC's tx_datapath now emits ethertype 0x0800 (real IPv4), so these are
genuine IPv4/UDP frames. --validate still raw-sniffs (scapy) and hand-parses the
IPv4/UDP header rather than leaning on the OS stack — that keeps the check
self-contained (independent of kernel routing/socket state) and lets it assert
on every field. A normal `recvfrom` on a UDP socket would now also work if you
prefer to simplify. (Historically the datapath emitted a custom 0x9999 so the
OS would ignore the payload; that has since been parameterized to 0x0800.)

Golden constants below MUST match udp_tx.ml / test/udp/udp_mac_top_tb.ml.

Usage (needs CAP_NET_RAW, i.e. sudo):
    sudo python3 udp_app.py --validate --iface enx207bd25880ef
    sudo python3 udp_app.py --send     --iface enx207bd25880ef
    sudo python3 udp_app.py --send --pattern alt --app-len 8 --iface enx207bd25880ef
    sudo python3 udp_app.py --echo --pattern alt --iface enx207bd25880ef
    sudo python3 udp_app.py --echo --count 50 --iface enx207bd25880ef   # tally a run
"""

import argparse
import queue
import sys
import threading
import time

from scapy.all import AsyncSniffer, Ether, Raw, sendp, sniff

# ── golden constants (mirror udp_tx.ml / udp_mac_top_tb.ml) ──────────────────
FPGA_SRC_MAC = "02:00:00:00:00:01"   # Mac_top tx_datapath hardcoded SRC MAC
FPGA_DST_MAC = "ff:ff:ff:ff:ff:ff"   # hardcoded DST MAC (broadcast)
HOST_SRC_MAC = "de:ad:be:ef:00:02"   # src MAC we stamp on host -> FPGA frames
ETHERTYPE = 0x0800                   # IPv4 — MAC tx_datapath now emits real 0x0800

SRC_IP = "192.168.1.10"
DST_IP = "192.168.1.1"
SRC_PORT = 0x1234
DST_PORT = 0x1235

# The harness (udp_mac_top_validation_harness.ml) streams app_payload_len bytes
# of incrementing data 0x01, 0x02, …; default there is 18.
DEFAULT_APP_LEN = 18
DEFAULT_IFACE = "enx207bd25880ef"


def expected_app(n):
    """Incrementing 0x01..0x?? truncated to a byte — matches payload_byte in the TX harness."""
    return bytes(((i + 1) & 0xFF) for i in range(n))


def make_payload(pattern, n):
    """Application payload for --send.

    'inc' — incrementing 0x01,0x02,… (same as the TX harness emits).
    'alt' — alternating 0xAA,0x55,… so the RX harness's 1-byte/sec drain makes
            led[3:0] toggle 0xA <-> 0x5, the UDP mirror of the MAC-RX check.
    """
    if pattern == "alt":
        return bytes((0xAA if i % 2 == 0 else 0x55) for i in range(n))
    return expected_app(n)  # 'inc'


# ── per-probe nonce (--echo) ─────────────────────────────────────────────────
# The RX->TX bridge is a verbatim payload passthrough, so whatever we stamp into
# the app bytes comes back untouched. Stamping a sequence number is what makes
# "late" distinguishable from "lost": an echo can be attributed to the probe that
# produced it no matter which window it lands in.
ECHO_MAGIC = b"SQ"
NONCE_LEN = 4          # 2 magic + 2 seq
MIN_ECHO_APP_LEN = 6   # nonce + at least 2 pattern bytes


def probe_payload(pattern, n, seq):
    """[make_payload] with the nonce stamped over the first 4 bytes (length unchanged)."""
    app = bytearray(make_payload(pattern, n))
    app[0:2] = ECHO_MAGIC
    app[2] = (seq >> 8) & 0xFF
    app[3] = seq & 0xFF
    return bytes(app)


def probe_seq(app):
    """Sequence number stamped in [app], or None if the nonce isn't there."""
    if app is not None and len(app) >= NONCE_LEN and bytes(app[0:2]) == ECHO_MAGIC:
        return (app[2] << 8) | app[3]
    return None


def extract_app(payload):
    """App bytes out of an Ethernet payload (IPv4 ++ UDP ++ app), sized by IP total_length."""
    if len(payload) < 28:
        return None
    total_length = (payload[2] << 8) | payload[3]
    if total_length < 28 or total_length > len(payload):
        return bytes(payload[28:])   # truncated/garbled length — hand back what's there
    return bytes(payload[28:total_length])


def first_diff(a, b):
    """Index of the first differing byte, or None when equal up to the shorter length."""
    for i in range(min(len(a), len(b))):
        if a[i] != b[i]:
            return i
    return None if len(a) == len(b) else min(len(a), len(b))


def ones_complement_sum(data):
    """16-bit ones-complement sum over a byte string (odd length is zero-padded)."""
    if len(data) % 2:
        data = data + b"\x00"
    s = 0
    for i in range(0, len(data), 2):
        s += (data[i] << 8) | data[i + 1]
    while s >> 16:
        s = (s & 0xFFFF) + (s >> 16)
    return s


def ip_checksum(header20):
    return (~ones_complement_sum(header20)) & 0xFFFF


def ip_str(b):
    return ".".join(str(x) for x in b)


def hexdump(b):
    return " ".join(f"{x:02x}" for x in b)


# ── build a golden datagram (IPv4 header ++ UDP header ++ app) ────────────────
def build_datagram(app):
    n = len(app)
    total_length = 28 + n
    udp_length = 8 + n
    src_ip = bytes(int(x) for x in SRC_IP.split("."))
    dst_ip = bytes(int(x) for x in DST_IP.split("."))
    ip_hdr = bytes(
        [0x45, 0x00, (total_length >> 8) & 0xFF, total_length & 0xFF,
         0x00, 0x00, 0x40, 0x00, 0x40, 0x11, 0x00, 0x00]
    ) + src_ip + dst_ip
    ck = ip_checksum(ip_hdr)
    ip_hdr = ip_hdr[:10] + bytes([(ck >> 8) & 0xFF, ck & 0xFF]) + ip_hdr[12:]
    udp_hdr = bytes(
        [(SRC_PORT >> 8) & 0xFF, SRC_PORT & 0xFF,
         (DST_PORT >> 8) & 0xFF, DST_PORT & 0xFF,
         (udp_length >> 8) & 0xFF, udp_length & 0xFF,
         0x00, 0x00]  # UDP checksum 0 = disabled (matches udp_tx.ml)
    )
    return ip_hdr + udp_hdr + bytes(app)


# ── validation (FPGA -> host) ─────────────────────────────────────────────────
def check_datagram(payload, app_len, verbose=True, expected=None, verdict=True):
    """Parse the Ethernet payload as IPv4/UDP and check it. Returns True on PASS.

    [expected] overrides the expected application payload; when None it defaults to
    the incrementing 0x01,0x02,… the TX harness emits. --echo passes the exact bytes
    it sent so the check asserts the FPGA echoed the payload back unchanged.
    """
    ok = True

    def fail(msg):
        nonlocal ok
        ok = False
        print(f"  FAIL: {msg}")

    if len(payload) < 28:
        fail(f"payload too short for IPv4+UDP: {len(payload)} bytes")
        return False

    # IPv4 header
    ver_ihl = payload[0]
    if ver_ihl != 0x45:
        fail(f"version/IHL = 0x{ver_ihl:02x}, expected 0x45")
    total_length = (payload[2] << 8) | payload[3]
    proto = payload[9]
    if proto != 0x11:
        fail(f"IP protocol = 0x{proto:02x}, expected 0x11 (UDP)")
    if ip_checksum(payload[0:20]) != 0x0000:
        # a valid header checksums to 0 when its own field is included
        fail("IPv4 header checksum invalid")
    got_src_ip, got_dst_ip = ip_str(payload[12:16]), ip_str(payload[16:20])
    if got_src_ip != SRC_IP:
        fail(f"src IP = {got_src_ip}, expected {SRC_IP}")
    if got_dst_ip != DST_IP:
        fail(f"dst IP = {got_dst_ip}, expected {DST_IP}")

    # UDP header (IHL is 5 => header starts at byte 20)
    udp = payload[20:]
    sport = (udp[0] << 8) | udp[1]
    dport = (udp[2] << 8) | udp[3]
    ulen = (udp[4] << 8) | udp[5]
    if sport != SRC_PORT:
        fail(f"UDP src port = 0x{sport:04x}, expected 0x{SRC_PORT:04x}")
    if dport != DST_PORT:
        fail(f"UDP dst port = 0x{dport:04x}, expected 0x{DST_PORT:04x}")
    if ulen != 8 + app_len:
        fail(f"UDP length = {ulen}, expected {8 + app_len}")
    if total_length != 28 + app_len:
        fail(f"IP total_length = {total_length}, expected {28 + app_len}")

    # Application payload — sized by the IP total_length field, so trailing MAC
    # zero-padding (present when the datagram is shorter than the 46-byte min
    # Ethernet payload) is naturally excluded.
    app = payload[28:total_length]
    exp = expected_app(app_len) if expected is None else bytes(expected)
    if verbose:
        print(f"  src {got_src_ip}:0x{sport:04x} -> dst {got_dst_ip}:0x{dport:04x}"
              f"  udp_len={ulen}  app={len(app)}B")
        print(f"  payload: {hexdump(app)}")
    if bytes(app) != exp:
        off = first_diff(exp, bytes(app))
        detail = f" (first differing byte at offset {off}" if off is not None else " ("
        if off is not None and off < len(exp) and off < len(app):
            detail += f": expected 0x{exp[off]:02x}, got 0x{app[off]:02x}"
        detail += f"; {len(exp)}B expected vs {len(app)}B got)"
        fail(f"payload mismatch{detail}"
             f"\n    expected: {hexdump(exp)}"
             f"\n    got:      {hexdump(bytes(app))}")

    if verdict:
        print(f"  => {'PASS' if ok else 'FAIL'}")
    return ok


def is_fpga(pkt):
    """True for a frame emitted by the FPGA (our src MAC + IPv4 ethertype)."""
    return (
        Ether in pkt
        and pkt[Ether].src.lower() == FPGA_SRC_MAC
        and pkt[Ether].type == ETHERTYPE
    )


def validate(iface, app_len, count):
    print(f"Sniffing {iface} for FPGA frames (src {FPGA_SRC_MAC}, ethertype 0x{ETHERTYPE:04x})")
    print(f"Press btn[3] on the board to emit a datagram. Waiting for {count}...\n")
    seen = {"n": 0, "pass": 0}

    def handle(pkt):
        seen["n"] += 1
        print(f"-- frame {seen['n']} ({len(bytes(pkt))} bytes on wire) --")
        payload = bytes(pkt[Ether].payload)   # everything after the 14-byte Eth header
        if check_datagram(payload, app_len):
            seen["pass"] += 1
        print()

    sniff(iface=iface, lfilter=is_fpga, prn=handle, count=count, store=False)
    print(f"==== {seen['pass']}/{seen['n']} datagrams passed ====")
    return seen["pass"] == seen["n"] and seen["n"] > 0


# ── send (host -> FPGA, RX path) ──────────────────────────────────────────────
def send(iface, app_len, count, pattern):
    app = make_payload(pattern, app_len)
    frame = Ether(dst=FPGA_DST_MAC, src=HOST_SRC_MAC, type=ETHERTYPE) / Raw(
        build_datagram(app)
    )
    print(f"Sending on {iface}: ethertype 0x{ETHERTYPE:04x}, "
          f"{SRC_IP}:0x{SRC_PORT:04x} -> {DST_IP}:0x{DST_PORT:04x}, "
          f"app={app_len}B (pattern={pattern})")
    print(f"  app payload: {hexdump(app)}")
    sendp(frame, iface=iface, count=count, verbose=True)

    # Confirmation is on the board (udp_rx_mac_top_validation_harness); no host
    # readback yet. Print the LED checklist so it's clear what a PASS looks like.
    print("\nConfirm on the RX board harness (udp_rx_mac_top_validation_harness):")
    print("  led0_g  saw_valid_datagram  -> lights and stays lit")
    print("  led2_g  checksum_ok (IPv4)  -> lit")
    print("  led2_r / led3_r  crc_error  -> DARK (good frame)")
    print("  led[3:0] steps through the recovered payload low-nibbles, ~1/sec:")
    if pattern == "alt":
        print("           toggles 0xA <-> 0x5   (0xAA/0x55 alternating)")
    else:
        print("           0x1, 0x2, 0x3, …      (incrementing)")


# ── echo (host -> FPGA -> host, loopback RX+TX path) ──────────────────────────
def echo(iface, app_len, count, pattern, timeout, gap, drain=None):
    """Send [count] nonce-tagged datagrams and classify how each one comes back.

    This closes the loop: the loopback harness (udp_loopback_validation_harness)
    parses the received datagram and feeds the recovered app payload straight back
    out through the RX->TX bridge, re-wrapping it in fresh IPv4/UDP/Ethernet
    framing. So a PASS means the whole MAC->IPv4->UDP RX chain AND the UDP->IPv4->MAC
    TX chain are byte-correct — host-asserted, no LED eyeballing.

    Two properties make late/lost decidable, unlike a per-probe sniffer over
    identical payloads:

      * every probe carries a distinct seq nonce, so an echo is attributed to the
        probe that produced it regardless of which window it lands in;
      * ONE sniffer stays armed across the whole run (armed via started_callback
        before the first send — AsyncSniffer.start() returns before pcap is open,
        so sending straight after it can genuinely miss a microsecond-latency
        echo), and a post-run drain window catches stragglers.
    """
    if app_len < MIN_ECHO_APP_LEN:
        print(f"--echo needs --app-len >= {MIN_ECHO_APP_LEN} "
              f"({NONCE_LEN}-byte nonce + payload); got {app_len}")
        return False
    # 'lost' can only ever mean "not seen within deadline + drain" — the drain is
    # that horizon. Widen it (--drain) if stragglers are landing outside it.
    drain = timeout if drain is None else drain

    probes = {}                     # seq -> {app, t0, rtt, status, window}
    stats = {"dup": 0, "unmatched": 0}
    cur = {"k": None}               # probe window we are currently inside
    q = queue.Queue()
    armed = threading.Event()

    def on_pkt(pkt):
        q.put((time.perf_counter(), pkt))

    sniffer = AsyncSniffer(iface=iface, lfilter=is_fpga, prn=on_pkt, store=False,
                           started_callback=armed.set)
    sniffer.start()
    if not armed.wait(timeout=5.0):
        print(f"FAIL: sniffer never armed on {iface} (check --iface / CAP_NET_RAW)")
        return False
    time.sleep(0.05)   # let pcap settle past the arm callback

    def classify(t_arr, pkt):
        """Attribute one FPGA-sourced frame to its probe and record the verdict."""
        payload = bytes(pkt[Ether].payload)
        app_rx = extract_app(payload)
        seq = probe_seq(app_rx)
        rec = probes.get(seq) if seq is not None else None
        if rec is None:
            stats["unmatched"] += 1
            why = "no nonce" if seq is None else f"seq {seq} never sent"
            print(f"  ?? unmatched FPGA frame ({len(payload)}B payload, {why})")
            print(f"     app: {hexdump(app_rx or b'')}")
            return
        if rec["status"] is not None:
            stats["dup"] += 1
            print(f"  ?? duplicate echo of seq {seq} (already {rec['status']})")
            return
        rec["rtt"] = t_arr - rec["t0"]
        ok = check_datagram(payload, app_len, verbose=False, expected=rec["app"],
                            verdict=False)
        if not ok:
            rec["status"] = "bad"
        elif rec["rtt"] > timeout:
            rec["status"] = "late"
        else:
            rec["status"] = "on-time"
        where = ""
        if cur["k"] is None:
            where = " [drain]"
        elif cur["k"] != rec["window"]:
            where = f" [picked up during probe {cur['k']}]"
        print(f"  seq {seq}: {rec['status'].upper()}  rtt={rec['rtt'] * 1e3:.2f} ms{where}")

    print(f"Echo test on {iface}: {count} probe(s) of {app_len}B (pattern={pattern}), "
          f"deadline {timeout}s each, {gap}s inter-probe gap, {drain}s drain")
    print(f"  nonce: {NONCE_LEN}B ('{ECHO_MAGIC.decode()}' + 16-bit seq) at the head "
          f"of each app payload\n")

    for k in range(count):
        seq = k & 0xFFFF
        app = probe_payload(pattern, app_len, seq)
        frame = Ether(dst=FPGA_DST_MAC, src=HOST_SRC_MAC, type=ETHERTYPE) / Raw(
            build_datagram(app)
        )
        cur["k"] = k
        probes[seq] = {"app": app, "t0": time.perf_counter(), "rtt": None,
                       "status": None, "window": k}
        print(f"-- probe {k + 1}/{count} (seq {seq}) --")
        sendp(frame, iface=iface, count=1, verbose=False)
        deadline = probes[seq]["t0"] + timeout
        # Keep servicing the queue until THIS probe is answered; echoes of earlier
        # probes that show up meanwhile are classified (as late) and don't count here.
        while probes[seq]["status"] is None:
            remain = deadline - time.perf_counter()
            if remain <= 0:
                break
            try:
                t_arr, pkt = q.get(timeout=remain)
            except queue.Empty:
                break
            classify(t_arr, pkt)
        if probes[seq]["status"] is None:
            print(f"  seq {seq}: no echo within {timeout}s — late-vs-lost decided at drain")
        if gap:
            time.sleep(gap)

    # Drain: anything still unanswered gets one more timeout to show up late.
    cur["k"] = None
    unanswered = sum(1 for r in probes.values() if r["status"] is None)
    if unanswered:
        print(f"\n-- drain window ({drain}s) for {unanswered} unanswered probe(s) --")
    drain_end = time.perf_counter() + (drain if unanswered else 0.2)
    while any(r["status"] is None for r in probes.values()) or not unanswered:
        remain = drain_end - time.perf_counter()
        if remain <= 0:
            break
        try:
            t_arr, pkt = q.get(timeout=remain)
        except queue.Empty:
            break
        classify(t_arr, pkt)

    try:
        sniffer.stop()
    except Exception:
        pass
    while True:                       # whatever the sniffer queued as it wound down
        try:
            t_arr, pkt = q.get_nowait()
        except queue.Empty:
            break
        classify(t_arr, pkt)

    for rec in probes.values():
        if rec["status"] is None:
            rec["status"] = "lost"

    # ── summary ──────────────────────────────────────────────────────────────
    tally = {"on-time": 0, "late": 0, "bad": 0, "lost": 0}
    for rec in probes.values():
        tally[rec["status"]] += 1
    rtts = sorted(r["rtt"] * 1e3 for r in probes.values() if r["rtt"] is not None)

    print(f"\n==== echo summary: {count} probe(s) ====")
    for k_ in ("on-time", "late", "bad", "lost"):
        print(f"  {k_:<9} {tally[k_]}")
    if stats["dup"]:
        print(f"  {'dup':<9} {stats['dup']}")
    if stats["unmatched"]:
        print(f"  {'unmatched':<9} {stats['unmatched']}")
    if rtts:
        print(f"  rtt (delivered): min {rtts[0]:.2f} / median "
              f"{rtts[len(rtts) // 2]:.2f} / max {rtts[-1]:.2f} ms")
    bad_probes = [(s, r) for s, r in sorted(probes.items()) if r["status"] != "on-time"]
    if bad_probes:
        print("  not on-time:")
        for s, r in bad_probes:
            rtt = f"{r['rtt'] * 1e3:.2f} ms" if r["rtt"] is not None else "-"
            print(f"    seq {s:<5} {r['status']:<8} rtt={rtt}")

    # Point at the right suspect (see UDP_FULL_DUPLEX_HARNESS_PLAN.md, board finding).
    if tally["late"]:
        print("  NOTE: late echoes are NOT drops — the FPGA delivered them. Suspect the"
              "\n        host (pcap scheduling / a too-tight --timeout) or a TX"
              "\n        store-and-forward stall, not the link.")
    if tally["bad"]:
        print("  NOTE: 'bad' = forward-everything echoed a corrupted inbound frame with a"
              "\n        regenerated FCS. Fix is to gate the bridge's tx_start on"
              "\n        frame_done & ~frame_error (plan Phase 1, next-step 2).")
    if tally["lost"]:
        print(f"  NOTE: 'lost' = never seen within its {timeout}s deadline NOR the {drain}s"
              "\n        drain, i.e. a real drop. Check led2_r/led3_r (held crc_error) at"
              "\n        the instant of the failure: lit => inbound corruption; dark =>"
              "\n        the drop is on the outbound leg / RX FIFO / an ipv4_rx reject."
              "\n        Raise --drain if you suspect stragglers past the horizon.")

    ok = (count > 0 and tally["on-time"] == count
          and stats["dup"] == 0 and stats["unmatched"] == 0)
    print(f"==== {'PASS' if ok else 'FAIL'} ====")
    return ok


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    mode = ap.add_mutually_exclusive_group(required=True)
    mode.add_argument("--validate", action="store_true", help="sniff + check FPGA-emitted UDP datagrams (TX path)")
    mode.add_argument("--send", action="store_true", help="send a UDP datagram to the FPGA (RX path)")
    mode.add_argument("--echo", action="store_true", help="send a datagram + assert the FPGA echoes it back (loopback path)")
    ap.add_argument("--iface", default=DEFAULT_IFACE, help=f"network interface (default {DEFAULT_IFACE})")
    ap.add_argument("--app-len", type=int, default=DEFAULT_APP_LEN, help=f"application payload length (default {DEFAULT_APP_LEN})")
    ap.add_argument("--count", type=int, default=1, help="frames to capture/send/echo (default 1)")
    ap.add_argument("--timeout", type=float, default=2.0, help="--echo: per-probe deadline in seconds (default 2.0); an echo after this is LATE, not lost")
    ap.add_argument("--gap", type=float, default=0.05, help="--echo: seconds between probes (default 0.05); the bridge is single-frame, so pressure raises the drop rate")
    ap.add_argument("--drain", type=float, default=None, help="--echo: seconds to keep listening after the last probe before calling anything lost (default: same as --timeout)")
    ap.add_argument("--pattern", choices=("inc", "alt"), default="inc",
                    help="--send/--echo payload pattern: 'inc' incrementing (default), "
                         "'alt' alternating 0xAA/0x55 (led[3:0] toggles 0xA<->0x5)")
    args = ap.parse_args()

    if args.validate:
        ok = validate(args.iface, args.app_len, args.count)
        sys.exit(0 if ok else 1)
    elif args.echo:
        ok = echo(args.iface, args.app_len, args.count, args.pattern, args.timeout,
                  args.gap, args.drain)
        sys.exit(0 if ok else 1)
    else:
        send(args.iface, args.app_len, args.count, args.pattern)


if __name__ == "__main__":
    main()
