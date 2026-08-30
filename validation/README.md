# Board validation runbook

This directory contains the Arty A7 board harnesses, constraints, and host-side tools.
The procedure below covers the decoupled full-duplex Ethernet MAC + IPv4 + UDP harness.

The duplex harness has two independent paths around one `Mac_top`:

```text
btn[3] payload -> UDP TX -> IPv4 TX -> MAC TX -> host
host -> MAC RX -> IPv4 RX -> UDP RX -> payload LEDs
```

It is not an echo design. The FPGA-to-host direction is checked by the host script while
the host-to-FPGA direction is checked using the board LEDs. For an automated
host-to-FPGA-to-host echo test, use the loopback harness described near the end of this
document.

## Harnesses in this directory

| Generate target | Synthesis top | Validates | Host tool |
| --- | --- | --- | --- |
| `mac-validation` | `mac_validation_harness` | Bare MAC only, both directions (no IPv4/UDP) | `send_test_frames.py` |
| `udp-tx-validation` | `udp_tx_validation_harness` | UDP TX only (FPGA to host) | `udp_app.py --validate` |
| `udp-rx-validation` | `udp_rx_validation_harness` | UDP RX only (host to FPGA) | `udp_app.py --send` |
| `udp-duplex-validation` | `udp_duplex_validation_harness` | Both directions, decoupled | `udp_app.py --validate` and `--send` |
| `udp-loopback-validation` | `udp_loopback_validation_harness` | Both directions, RX to TX echo bridge | `udp_app.py --echo` |

Every harness reuses the same `Arty_board_top` pin contract, so
`validation/constraints/unified_tx_rx.xdc` and the `btn[0]` reset / `sw[0]` enable controls
apply to all five, as does the host interface setup below. The body of this document is the
duplex runbook; the single-direction and bare-MAC harnesses are covered under
[Other validation harnesses](#other-validation-harnesses), and the loopback harness under
[Automated echo alternative](#automated-echo-alternative).

Bring-up order is bottom-up: `mac-validation` first to prove the PHY, MII timing, and FCS,
then a single direction, then duplex or loopback.

## Generate and check the duplex RTL

From the repository root:

```bash
./scripts/with-switch.sh dune exec lib/common/generate.exe -- udp-duplex-validation
./scripts/with-switch.sh dune runtest test/udp/udp_duplex_mac_top
```

The generator writes:

```text
validation/udp_duplex_validation_harness.v
```

Use `udp_duplex_validation_harness` as the synthesis top and
`validation/constraints/unified_tx_rx.xdc` as the board constraints file.

## Prepare the host interface

The examples use this USB Ethernet interface:

```bash
export FPGA_IFACE=enx207bd25880ef
```

Confirm that the cable and PHY have established a link:

```bash
ip -br link show dev "$FPGA_IFACE"
sudo ethtool "$FPGA_IFACE" | grep -E 'Speed:|Duplex:|Link detected:'
```

The expected link is 100 Mb/s, full duplex, with `Link detected: yes`.

### Quiet raw-Ethernet setup (recommended)

`udp_app.py` uses Scapy layer-2 send/sniff operations, so the interface does not need an
IPv4 or IPv6 address. Leaving the interface managed normally causes the host to emit
IPv6 neighbor discovery, multicast membership, mDNS, ARP, and possibly DHCP traffic each
time the board reset cycles the PHY link.

The single most important step is removing the IPv4 address. An address implies a
kernel link route:

```text
192.168.1.0/24 proto kernel scope link src 192.168.1.1
```

and that route makes the interface a legal egress for subnet broadcast and multicast.
Everything else follows from it: host daemons broadcast into the subnet, Avahi advertises
on any interface that has an address, and ARP has something to resolve. Unmanaging the
device in NetworkManager does *not* remove an address that is already configured.

On a NetworkManager host, temporarily make the validation interface unmanaged, disable
IPv6 on that interface, remove its addresses, and leave the link up:

```bash
sudo nmcli device set "$FPGA_IFACE" managed no
sudo sysctl -w "net.ipv6.conf.${FPGA_IFACE}.disable_ipv6=1"
sudo ip address flush dev "$FPGA_IFACE"
sudo ip link set dev "$FPGA_IFACE" arp off        # NOARP: never emit ARP on this port
sudo ip link set dev "$FPGA_IFACE" multicast off  # no group joins, no MLD/mDNS egress
sudo ip link set dev "$FPGA_IFACE" up
ip -br address show dev "$FPGA_IFACE"
```

Omit the `nmcli` command on a system that does not use NetworkManager.

The `arp off` and `multicast off` flags are safe for every harness here: Scapy's `sendp`
and `sniff` go through `AF_PACKET`, which bypasses ARP, the multicast layer, and the
routing table entirely, and promiscuous capture is unaffected by the multicast flag. Clear
them with `arp on` / `multicast on` before using the addressed setup below.

### Confirming the interface is actually idle

With the board programmed and sitting untouched, the correct steady state is: PHY link LED
solid, PHY activity LED dark, and only `led0_r` blinking on the Arty. Any blinking of the
PHY activity LED at idle means the host is still talking to the board.

```bash
ip -br address show dev "$FPGA_IFACE"           # want: UP, no addresses listed
ip route show dev "$FPGA_IFACE"                 # want: empty
sudo timeout 30 tcpdump -ni "$FPGA_IFACE" -c 20 # want: no packets for 30 s
```

If packets still appear, identify the sender by port rather than guessing:

```bash
ss -ulpn | grep <port>
```

Two sources are easy to miss because they are not part of the normal desktop network
stack:

- **Vivado broadcasts on UDP port 1534.** An open Vivado session binds `0.0.0.0:1534` and
  beacons to the subnet broadcast address of every addressed interface, which shows up in
  Wireshark as `192.168.1.1 -> 192.168.1.255  UDP  1534 -> 1534  Len=8`. There is no
  setting to scope this to one interface; leaving the validation interface unaddressed is
  the fix.
- **`avahi-daemon` emits mDNS** to `224.0.0.251:5353` on any interface that has an
  address, and `cups-browsed` browses over it. Flushing the address is enough, but the
  exclusion can be made durable in `/etc/avahi/avahi-daemon.conf` so that re-adding an
  address later does not reopen it:

  ```bash
  sudo sed -i 's/^#deny-interfaces=eth1/deny-interfaces=enx207bd25880ef/' \
      /etc/avahi/avahi-daemon.conf
  sudo systemctl restart avahi-daemon
  ```

### Addressed setup (background traffic is expected)

If an address is useful for packet inspection or another experiment, the FPGA TX header
targets `192.168.1.1`:

```bash
sudo ip link set dev "$FPGA_IFACE" arp on
sudo ip link set dev "$FPGA_IFACE" multicast on
sudo ip link set dev "$FPGA_IFACE" up
sudo ip address replace 192.168.1.1/24 dev "$FPGA_IFACE"
ip -br address show dev "$FPGA_IFACE"
```

No gateway or default route is needed. Do not use `ping` as a health check: the FPGA does
not implement ARP or ICMP.

This step re-creates the `192.168.1.0/24` link route and therefore re-enables all of the
host chatter described above: Vivado's UDP 1534 subnet broadcast, Avahi mDNS, and ARP. The
board will not be idle while it is configured this way. Because the MAC applies no receive
filtering (see [Bare MAC](#bare-mac-mac-validation)), that chatter is clocked into the RX
FIFO and reaches the LEDs, so do not use this setup for LED-based debugging — flush the
address and return to the quiet setup first.

## Start the board

After programming the duplex bitstream:

1. Set `sw[0]` high to enable the design.
2. Press and release `btn[0]` to reset the design and PHY.
3. Wait for `led1_g` to light, indicating that PHY reset has completed.
4. Confirm that `led0_r` continues blinking as the fabric heartbeat.

Reset asserts the PHY reset pin, so the host may observe a link drop and recovery. On a
normally managed interface, that link recovery is what triggers automatic host network
traffic.

## Run the decoupled full-duplex check

Start the FPGA-to-host validator in terminal 1:

```bash
sudo python3 validation/udp_app.py \
  --validate \
  --iface "$FPGA_IFACE" \
  --app-len 18 \
  --count 1
```

It waits for a frame emitted by the FPGA.

Inject one alternating-pattern datagram into the FPGA in terminal 2:

```bash
sudo python3 validation/udp_app.py \
  --send \
  --iface "$FPGA_IFACE" \
  --pattern alt \
  --app-len 18 \
  --count 1
```

While the received payload is walking across the LEDs, press `btn[3]` once. This starts
an independent FPGA-to-host UDP transmission while the RX path is still draining. The
terminal 1 result should end with:

```text
  => PASS
==== 1/1 datagrams passed ====
```

The receive path intentionally drains one application byte per second. Keep the initial
send count at one; an 18-byte payload therefore remains visibly active for approximately
18 seconds, and sending several frames rapidly adds avoidable FIFO pressure.

## Duplex harness LED map

| LED | Meaning | Expected behavior |
| --- | --- | --- |
| `led[3:0]` | Low nibble of the last drained RX application byte | Alternates `0xA`, `0x5` about once per second with `--pattern alt` |
| `led0_r` | Fabric heartbeat | Blinks continuously |
| `led0_g` | Saw a UDP application start | Lights on accepted RX UDP traffic and remains lit until reset |
| `led0_b` | Unused | Dark |
| `led1_r` | MAC TX busy | Brief pulse after `btn[3]`; it may be too fast to see |
| `led1_g` | PHY ready | Solid after PHY reset completes |
| `led1_b` | UDP TX busy | Brief pulse after `btn[3]`; it may be too fast to see |
| `led2_r` | Held RX CRC error | Dark for a good frame |
| `led2_g` | IPv4 header checksum valid | Lights after a valid IPv4 header is parsed |
| `led2_b` | MAC RX payload active | Very brief pulse; it may be too fast to see |
| `led3_r` | RX CRC-error mirror | Dark for a good frame |
| `led3_g` | Unused | Dark |
| `led3_b` | UDP RX parser busy | Lit while the slowly drained UDP payload is active |

The plain LED bank is a retained data display, not a pass/fail bank. If the most recently
accepted application byte has low nibble `0xf`, all four plain LEDs remain illuminated.

## Recognize automatic host traffic

Linux network services can transmit without an explicit test command. For example, a
packet from an interface link-local address (`fe80::/64`) to `ff02::fb` is IPv6 mDNS;
an mDNS response advertising the host name is usually emitted by Avahi or
systemd-resolved. This traffic commonly appears immediately after the board reset causes
the Ethernet link to recover.

Useful Wireshark display filters are:

```text
eth.src == 20:7b:d2:58:80:ef
```

Frames generated by the example host adapter. To attribute one of these to a specific
local process, see
[Confirming the interface is actually idle](#confirming-the-interface-is-actually-idle) —
Vivado's UDP 1534 broadcast in particular is easy to mistake for board traffic.

```text
eth.src == 02:00:00:00:00:01
```

Frames generated by the FPGA. With the duplex harness, this filter should remain quiet
until `btn[3]` is pressed.

```text
eth.type == 0x0800 && udp
```

IPv4 UDP traffic that can reach the UDP parser.

The current bring-up RX policy is intentionally permissive:

- `Ipv4_rx` discards non-IPv4 EtherTypes, but all Ethernet frames still cause PHY/MAC
  activity.
- IPv4 destination addresses are reported rather than filtered.
- Bad IPv4 checksums are reported rather than dropped.
- UDP destination ports are reported rather than filtered, so IPv4 mDNS or DHCP traffic
  can reach the application LED drain.
- UDP checksums are not yet verified; a checksum field of zero is used by the TX path.

Consequently, background IPv6 traffic can flash physical link/MAC activity but cannot
reach the UDP payload LEDs, while background IPv4 UDP traffic can change the retained
payload display and set `led0_g`. Good background traffic should not set the red CRC-error
LEDs; persistent `led2_r`/`led3_r` indicates a separate receive/FCS problem.

## Other validation harnesses

These share the host interface setup, the board start sequence, and the background-traffic
notes above. Only the generate command, controls, and LED map differ.

### Bare MAC (`mac-validation`)

The first bring-up step: no IPv4 or UDP, just the MII MAC in both directions.

```bash
./scripts/with-switch.sh dune exec lib/common/generate.exe -- mac-validation
```

Emits `validation/mac_validation_harness.v`; use `mac_validation_harness` as the synthesis
top.

`btn[3]` burst-fills the TX FIFO with a fixed 46-byte payload and transmits it in one shot.
46 is the minimum Ethernet payload; the TX controller does not pad below that.

The MAC defaults to EtherType `0x9999`, so these frames are deliberately not IPv4 and the
host kernel ignores them. Use `eth.src == 02:00:00:00:00:01` in Wireshark to see them, not a
UDP filter.

That default applies to the transmit direction only. **The receive path performs no
filtering at all**: `Mac_top.create`'s `?(ethertype = 0x9999)` argument is used when
building TX headers, and the RX controller latches the received EtherType into a register
without ever comparing it. There is no destination-MAC filter either. Every frame the PHY
delivers has its payload written into the RX FIFO, so any host background traffic on the
link is drained onto `led[3:0]` interleaved with the test bytes. The
[quiet setup](#quiet-raw-ethernet-setup-recommended) is a prerequisite for this harness,
not an optimisation.

The RX FIFO is 64 entries deep (`log2_depth = 6` in `lib/mii/mac_top.ml`) and the drain
pops one byte per second, so a single 64-byte payload fills it exactly and takes just over
a minute to display. Send one frame at a time, and prefer a 46-byte payload — the minimum
Ethernet payload — to leave headroom.

For the receive direction, `send_test_frames.py` sends one broadcast `0x9999` frame with a
64-byte alternating `0x55`/`0xAA` payload. It has no argument parsing, so edit the `IFACE`
constant at the top of the file if the interface is not `enx207bd25880ef`.

```bash
sudo python3 validation/send_test_frames.py
```

| LED | Meaning |
| --- | --- |
| `led[3:0]` | Low nibble of the last drained RX byte, one byte per second |
| `led0_r` | Fabric heartbeat |
| `led1_r` | MAC TX busy |
| `led1_g` | PHY ready |
| `led2_g` | Last frame CRC OK |
| `led2_b` | MAC RX payload active |
| `led3_r` | Last frame CRC bad |

### UDP transmit only (`udp-tx-validation`)

Adds the IPv4 and UDP TX headers on top of the MAC; the RX path is the bare MAC drain, so
received bytes are raw Ethernet payload rather than recovered UDP payload.

```bash
./scripts/with-switch.sh dune exec lib/common/generate.exe -- udp-tx-validation
```

Emits `validation/udp_tx_validation_harness.v`; synthesis top `udp_tx_validation_harness`.

`btn[3]` emits one UDP datagram. Unlike the bare MAC harness this drives the UDP
*application* interface, so the payload length is not pinned to 46 and the IPv4 and UDP
headers are synthesized in hardware. EtherType is `0x0800`, so Wireshark dissects these as
real UDP.

Validate from the host with:

```bash
sudo python3 validation/udp_app.py --validate --iface "$FPGA_IFACE" --app-len 18 --count 1
```

| LED | Meaning |
| --- | --- |
| `led[3:0]` | Low nibble of the last drained RX byte |
| `led0_r` | Fabric heartbeat |
| `led1_r` | MAC TX busy |
| `led1_g` | PHY ready |
| `led1_b` | UDP TX busy |
| `led2_r` | RX AXI-Stream CRC error (`tuser` at `tlast`) |
| `led2_g` | Last frame CRC OK |
| `led2_b` | MAC RX payload active |
| `led3_r` | Last frame CRC bad |

### UDP receive only (`udp-rx-validation`)

The mirror image: the full MAC to IPv4 to UDP receive chain, with the MII TX pins held idle.
Nothing is ever transmitted, so there is no `btn[3]` stimulus.

```bash
./scripts/with-switch.sh dune exec lib/common/generate.exe -- udp-rx-validation
```

Emits `validation/udp_rx_validation_harness.v`; synthesis top `udp_rx_validation_harness`.

```bash
sudo python3 validation/udp_app.py --send --iface "$FPGA_IFACE" --pattern alt --app-len 18 --count 1
```

The recovered payload drains one application byte per second, which backpressures the whole
UDP to IPv4 to MAC chain and parks the datagram in the MAC async RX FIFO. With `--pattern
alt` the payload LEDs toggle `0xA` and `0x5` once per second, confirming every byte transits
the L2 to L3 to L4 chain intact. An 18-byte payload therefore stays visible for about 18
seconds.

| LED | Meaning |
| --- | --- |
| `led[3:0]` | Low nibble of the currently drained UDP application byte |
| `led0_r` | Fabric heartbeat |
| `led0_g` | Saw a UDP application start, held until reset |
| `led1_g` | PHY ready |
| `led1_b` | UDP RX parser busy |
| `led2_r` | Held RX CRC error |
| `led2_g` | IPv4 header checksum valid |
| `led2_b` | MAC RX payload active |
| `led3_r` | RX CRC-error mirror |

Verification is by eye here. To assert on the receive path from the host instead, use the
loopback harness below.

## Automated echo alternative

For a host-asserted test of both directions without relying on the payload LEDs, generate
and program the loopback harness:

```bash
./scripts/with-switch.sh dune exec lib/common/generate.exe -- udp-loopback-validation
```

Then run:

```bash
sudo python3 validation/udp_app.py \
  --echo \
  --iface "$FPGA_IFACE" \
  --pattern alt \
  --app-len 18 \
  --count 10
```

`--echo` requires `udp_loopback_validation_harness`; it is not supported by the
decoupled `udp_duplex_validation_harness`.

`--echo` also requires `--app-len 6` or greater. The first 4 bytes of each application
payload are a nonce (the magic `SQ` plus a 16-bit sequence number) used to attribute an echo
to the probe that produced it, so a shorter payload leaves no room for pattern bytes.

Three flags tune the echo timing. The defaults are usually right for a single probe, but
`--count 10` above puts real pressure on the bridge, so reach for these before concluding
that frames are being dropped:

| Flag | Default | Meaning |
| --- | --- | --- |
| `--timeout` | `2.0` | Per-probe deadline in seconds. An echo arriving after it is reported `LATE`, not lost. |
| `--gap` | `0.05` | Seconds between probes. The RX to TX bridge handles one frame at a time, so a smaller gap raises the drop rate. |
| `--drain` | same as `--timeout` | Seconds to keep listening after the last probe. A frame is only called lost if it misses both its deadline and this window. |

Every probe carries its own sequence nonce and a single sniffer stays armed for the whole
run, so a slow echo is still attributed to the right probe even when it lands during a later
probe's window. If the run reports `LATE` rather than lost, widen `--timeout`; if stragglers
land after the run ends, widen `--drain`; if echoes are genuinely lost, widen `--gap` first,
since the single-frame bridge is the usual cause.

## Restore normal host networking

After a quiet raw-Ethernet run:

```bash
sudo sysctl -w "net.ipv6.conf.${FPGA_IFACE}.disable_ipv6=0"
sudo ip link set dev "$FPGA_IFACE" arp on
sudo ip link set dev "$FPGA_IFACE" multicast on
sudo nmcli device set "$FPGA_IFACE" managed yes
sudo ip link set dev "$FPGA_IFACE" up
```

The `arp on` and `multicast on` lines undo the flags set by the quiet setup. Without them
the interface stays `NOARP` after it is handed back to NetworkManager, which breaks normal
IPv4 use in a way that is not obvious from `ip -br link`.

Reapply the static address manually if the connection is meant to keep one:

```bash
sudo ip address replace 192.168.1.1/24 dev "$FPGA_IFACE"
```
