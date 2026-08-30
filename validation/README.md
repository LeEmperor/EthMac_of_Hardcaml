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

On a NetworkManager host, temporarily make the validation interface unmanaged, disable
IPv6 on that interface, remove its addresses, and leave the link up:

```bash
sudo nmcli device set "$FPGA_IFACE" managed no
sudo sysctl -w "net.ipv6.conf.${FPGA_IFACE}.disable_ipv6=1"
sudo ip address flush dev "$FPGA_IFACE"
sudo ip link set dev "$FPGA_IFACE" up
ip -br address show dev "$FPGA_IFACE"
```

Omit the `nmcli` command on a system that does not use NetworkManager.

### Addressed setup (background traffic is expected)

If an address is useful for packet inspection or another experiment, the FPGA TX header
targets `192.168.1.1`:

```bash
sudo ip link set dev "$FPGA_IFACE" up
sudo ip address replace 192.168.1.1/24 dev "$FPGA_IFACE"
ip -br address show dev "$FPGA_IFACE"
```

No gateway or default route is needed. Do not use `ping` as a health check: the FPGA does
not implement ARP or ICMP.

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

Frames generated by the example host adapter.

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

## Restore normal host networking

After a quiet raw-Ethernet run:

```bash
sudo sysctl -w "net.ipv6.conf.${FPGA_IFACE}.disable_ipv6=0"
sudo nmcli device set "$FPGA_IFACE" managed yes
sudo ip link set dev "$FPGA_IFACE" up
```

Reapply the static address manually if the connection is meant to keep one:

```bash
sudo ip address replace 192.168.1.1/24 dev "$FPGA_IFACE"
```
