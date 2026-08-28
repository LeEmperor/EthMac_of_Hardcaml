# Verif Sweep — findings log

Things the sweep turned up that outlive the phase that found them. Two kinds:

- **RTL findings** — behavior in `lib/` that the new suites pinned down. The sweep's job
  is coverage, not fixes, so the default is to record rather than change; each entry says
  what the current suites do about it and what a fix would have to reckon with. An entry
  that is later fixed stays here, marked **resolved**, with what changed and why.
- **Process findings** — tooling and workflow facts that cost time once and should not
  cost it twice. The durable rules from these are folded into
  `test/test_architecture.md`'s "Settled Conventions"; this file keeps the incident and
  the reasoning.

Append per phase. Nothing here is a TODO list — resolve or delete an entry deliberately.

---

## Phase 1

### RTL-1. `Tx_controller` deadlocked on a zero-length payload — **resolved**

**Where:** `lib/mii/tx_controller.ml`, the `Payload` state.

**What:** The `Payload` state had exactly two exits, and a datagram with no payload bytes
could reach neither.

- The non-padding branch was gated on `~:fifo_empty &: dis_ready`, so it never fired when
  the FIFO was empty.
- `padding` was only latched inside that branch, on the byte carrying `payload_last`. With
  no real byte, nothing ever carried `payload_last`.

So with zero payload bytes the FSM sat in `Payload` forever: `tx_busy` stayed high, the
serializer was never handed a byte, and only a reset recovered it.

It was unreachable through `mac_top` — the store-and-forward gate (`frame_ready`) only
opens once a whole `tlast`-terminated datagram is buffered, and an empty datagram is never
launched — so it was first recorded rather than fixed.

**Fix applied.** `Payload` now reads padding through a combinational

```ocaml
let padding_now = i_regs.padding.value |: (sm.is Payload &: fifo_empty)
```

which treats "empty on arrival in `Payload`" as padding: an empty payload is sub-minimum
by definition, so the pad branch emits the full 46 zero bytes and the frame is an ordinary
minimum-length one. The pad branch also latches `i_regs.padding` now, so the run holds
even if `fifo_empty` does not, and `pad` is driven from `padding_now` so the datapath and
the FIFO read-enable see it on the same cycle the branch does.

Combinational, not a second registered latch, and this is the part worth keeping: a cycle
spent in `Payload` with `pad` low and the FIFO empty would still present a byte to the
serializer — `mac_top` ties `byte_in_valid` to `~:(state ==:. 0)` and
`Tx_byte_disassembler` latches on `ready &: byte_in_valid` — so a registered latch would
have put one garbage byte on the wire before the pad run began. Trading a deadlock for an
off-by-one byte is not a fix.

Store-and-forward means `fifo_empty` cannot rise mid-payload for a datagram with any bytes
at all, so `padding_now` reads identically to `padding` for every length >= 1. Confirmed:
the expect-test goldens did not move.

**What the suite does:** the `payload_length < 1` guard is gone from `Testbench.run_frame`,
the length generator is `Int.gen_incl 0 120` and `Generators.eth_frame` runs from
`~min_payload_length:0`, and `tx_controller_unit_quickcheck_tests.ml` carries a dedicated
"a zero-length datagram pads the whole payload phase" case. The existing byte-count oracle
(`26 + max(length, 46)`) predicted the answer with no change, as the original entry
expected. The fix was verified to bite: reverting just the `padding_now` read fails both
the new case and the Quickcheck property, which shrinks to `(input 0)`.

### RTL-2. `Tx_controller.crc_en` was dead — **resolved**

**Where:** `lib/mii/tx_controller.ml`; `lib/mii/mac_top.ml` is the only instantiation.

**What:** `mac_top` never read `tx_ctrl.crc_en`. It derived its own gating instead:

```ocaml
let crc_active = (tx_ctrl.state >=:. 3) &: (tx_ctrl.state <=:. 6) in
...
en         = ~:(tx_ctrl.state ==:. 0);
data_valid = wire_dis_ready &: crc_active;
```

So the CRC was enabled off `state` directly and `crc_en` drove nothing. Its own behavior —
asserted on exactly four cycles, the last payload cycle and `Fcs` counter values 0, 1, 2 —
corresponded to nothing any consumer wanted.

**The trap, which still stands.** `crc_en` is not `Tx_crc.en`, and nobody should "clean
this up" by wiring them together. `Tx_crc`'s `en` is a *reload*, not a stall:
`lib/mii/tx_crc.ml` reloads the accumulator with `0xFFFFFFFF` whenever `reset |: ~:en`
holds. `crc_en` is low for the whole `Fcs` state, so driving `Tx_crc.en` from it would
clear the accumulator while its own result was being read out.

**Fix applied.** `crc_en` was given the meaning its name implies and a real consumer,
rather than being deleted: it is now the FCS window, a decode of the current state —

```ocaml
let crc_active = sm.is Dst_mac |: sm.is Src_mac |: sm.is Eth_type |: sm.is Payload
```

— which is bit-identical to the `crc_active` `mac_top` was computing. `mac_top` now gates
`Tx_crc.data_valid` with `wire_dis_ready &: tx_ctrl.crc_en` and has dropped its own copy.
The controller owns the statement of which fields the FCS covers; `mac_top` no longer
re-derives FSM knowledge from raw state literals.

The two `~:(tx_ctrl.state ==:. 0)` literals went the same way, replaced by
`tx_ctrl.tx_busy` on `Tx_crc.en` and on the serializer's `byte_in_valid`. That
substitution is test-backed, not assumed: the suite already asserts `tx_busy` is high on
exactly the non-`Idle` cycles.

Three of the four magic-state-number sites are therefore gone. The fourth — `state ==:. 6`
on the FIFO read-enable — is not CRC-related and was left alone; the
`TODO(magic-state-numbers)` at the top of the TX wiring has been narrowed to it.

**Also removed:** five dead internals in the same file — the `in_preamble`, `in_sfd`,
`in_payload`, `in_fcs` registers (declared, never assigned, never read, so constant zero)
and the `byte_disassembler_en` wire. With `crc_en` no longer a wire, `I_Wires` was empty
and went with them.

**What the suite does:** the old four-cycle behavioral freeze is replaced by two tests
against the new contract — `crc_en` high on exactly `Dst_mac`/`Src_mac`/`Eth_type`/
`Payload` across four payload lengths, and a counted form asserting the CRC sees
`14 + max(length, 46)` bytes. The exclusions are the substance: a byte emitted with
`crc_en` low is a byte the receiver's CRC will not see. The expect-test goldens moved
accordingly (`crc_en` now appears across the covered fields and is absent in `Fcs`), and
the `mac_top` loopback and CRC integration tests pass unchanged, which is what confirms
the rewiring did not move the FCS.

### PROC-1. `[@@ocamlformat "disable"]` is an error, and it failed `@lint` unnoticed

**What:** This ocamlformat (Jane Street fork, `3aa293b`) rejects the item-level attribute
outright, wherever it is attached and however it is indented:

```
Error: Invalid ocamlformat attribute. Ocamlformat can only be disabled at toplevel
(e.g [@@@ocamlformat "disable"])
```

It is an **error**, not a warning, and it fails `dune build @lint`.

**Why it went unnoticed:** `dune build` and `dune runtest` are both clean with it present —
only the lint alias fails, and its output is a wall of progress lines that buries five
error blocks. Phase 0's §4.1 recorded `@lint` as clean; it was not. Five files carried the
attribute: `test/common/verif/eth_frame.ml`, `generators.ml` (two sites), `ip_udp.ml`,
`test/mii/rx_byte_assembler/rx_byte_assembler_testbench.ml`, and
`test/mii/rx_controller/rx_controller_testbench.ml`.

**Fix applied:** all converted to the floating pair, which preserves the hand alignment and
passes lint:

```ocaml
[@@@ocamlformat "disable"]

let create
  ?(preamble_length     = 7)
  ...
;;
[@@@ocamlformat "enable"]
```

Both forms work inside a `struct`, indented to the enclosing level. Every `disable` needs a
matching `enable` — an unclosed one silently exempts the rest of the file from formatting.

**How to not repeat it:** check the *exit code*, not the output. `dune build @lint` piped
into `tail` reports `tail`'s status, which is how "green" got recorded. Run it bare, or
capture `$?` before the pipe.

### PROC-2. An empty `(**)` makes ocamlformat skip the whole file

**What:** ocamlformat reports

```
ocamlformat: ignoring "<file>" (misplaced documentation comments - warning 50)
```

and formats nothing in that file. `@fmt` then stays red no matter how many times it is
promoted, which reads like a formatter bug rather than a source problem.

`(**)` is an empty *doc* comment (`(**` … `*)`), and the compiler cannot attach it to
anything in the AST. Write `(* *)`, or say something in it.

**Found in:** `test/mii/rx_controller/rx_controller_testbench.ml`, replaced with a real
comment.

### PROC-3. `test/mii/rx_controller/rx_controller_testbench.ml` was edited concurrently

**What:** the file changed under the Phase 1 session three times mid-run — new
`[@@ocamlformat "disable"]` attributes appeared twice and the `(**)` of PROC-2 once, along
with prose comments and reflowed bindings that were not there at the start of the session.

**What was done:** only the attributes were converted (PROC-1) and the `(**)` replaced
(PROC-2). No other content was touched or reverted, and the directory was re-promoted with
`dune build @test/mii/rx_controller/fmt --auto-promote`, which reflowed the parts of the
file outside the disabled regions per the usual per-directory rule.

**Worth knowing:** if that other session is still open, the attributes may come back and
`@lint` will go red again with the same message. The fix is mechanical — see PROC-1.

---

## Phase 2

### RTL-3. `Clk_div` has no divisor — the ratio is hardcoded at four

**Where:** `lib/common/clk_div.ml`.

**What:** the whole module is

```ocaml
let cnt = reg_fb spec ~enable:i.en ~width:2 ~f:(fun x -> x +:. 1) -- "cnt" in
{ O.dst_clk = msb cnt }
```

The `~width:2` is a literal, so the division ratio is fixed at four and there is no port,
optional argument or functor parameter that changes it. The plan's Phase 2 entry ("
quickcheck the output period across divisor values") assumed one; there is nothing to
sweep.

**What the suite does instead:** randomizes the *enable schedule* — arbitrary
interleavings of run, hold and clear — against a fold over the same schedule. That is the
only axis the RTL exposes, and it is the stronger test of the two: a free-running period
measurement cannot distinguish a counter that occasionally drops or double-counts an
enable, and the schedule property does, because the model tracks the count rather than the
phase.

**What a fix would have to reckon with.** Adding `?(width = 2)` or `?(divisor = 4)` to
`create` would be a one-line change to the RTL and would make the suite a `Make_testbench`
functor like `second_pulse`'s, at which point the plan's original property becomes
writable. But note the header comment on the module: it disclaims correctness above
~150 MHz and points at `hardcaml_xilinx` for real clocking primitives. A divider that
invites arbitrary ratios invites being used as a real clock source, which is exactly what
that comment warns against. Parameterizing it is not obviously an improvement, and the
decision belongs with whoever owns the board-level clocking, not with the sweep.

### RTL-4. `Second_pulse` does not elaborate at `clk_freq = 1`

**Where:** `lib/common/second_pulse.ml`.

**What:** the counter width is computed as `Int.ceil_log2 clk_freq`, which is `0` for
`clk_freq = 1`, and Hardcaml rejects a zero-width register:

```
("Width of signals must be >= 0" (width 0))
```

Verified directly rather than inferred — elaborated at 1, 2 and 3; 1 raises, 2 and 3 are
fine. So the usable domain is `clk_freq >= 2`.

**Why it is only a footnote:** `clk_freq` is a compile-time argument with a sensible
default of 100 MHz, a divide of one is not a meaningful heartbeat, and the failure is loud
and immediate rather than silent. Recorded because the suite deliberately instantiates
small frequencies to make simulation tractable, and the next person shortening one further
should know where the floor is. A fix, if wanted, is `Int.max 1 (Int.ceil_log2 clk_freq)`
plus a check that `clk_freq >= 1`.

**What the suite does:** instantiates at 3, 4, 5, 8, 10 and 16 — both powers of two, where
the terminal count coincides with the counter's natural wrap, and non-powers, where it
does not. That pairing is what would catch a counter that rolled on the wrap instead of on
the compare; a suite that only ran at powers of two could not.

### RTL-5. `Uart_tx` carries four pieces of dead weight

**Where:** `lib/uart/uart_tx.ml`. None of these is a bug; all four are things a reader has
to rule out before trusting the module, and the new suite pins the behavior around them.

- **`frame` is computed and never used.** `let frame = concat_msb [ zero 1; byte; one 1 ]`
  builds the complete ten-bit frame — start bit, payload, stop bit — and nothing reads it.
  The transmitter drives the line from `mux data_place_counter.value (bits_lsb byte)`
  instead. It reads like the remains of a shift-register implementation that was replaced
  by a mux. Note the two would not agree about bit order without care, which is why the
  suite asserts the wire order directly (`0x01` and `0x80`, one bit each at opposite ends)
  rather than only that the byte round-trips.
- **`DONE` is unreachable.** The state is enumerated and no transition targets it; `STOP`
  goes to `IDLE`. It also has no arm in the output `switch`, so it would fall through to
  the `tx_d <--. 1` default and drive mark — harmless if it were ever reached, but it
  cannot be.
- **The sub-scope is discarded.** `let _scope = Scope.sub_scope scope "uart_tx" in` binds
  to a name that starts with an underscore and is never used, so the module's internals
  are not hierarchically named in waveforms the way the MII blocks' are.
- **`keep` is tied to zero.** Every MII module OR-reduces its internal debug signals into
  `keep` to stop synthesis pruning them; this one returns `zero 1`, so it retains nothing.
  Frozen by a test, so a later edit that starts driving it is a deliberate change rather
  than an accident.

**Not fixed here** — the sweep's job is coverage, and all four are cosmetic in the sense
that no behavior depends on them. Removing `frame` and `DONE` is safe and mechanical
whenever someone touches the file; the sub-scope and `keep` are worth aligning with the
MII convention at the same time.

### RTL-6. A clear cancels an in-flight delayed edge in `Helper_circuits`

**Where:** `lib/common/helper_circuits.ml`, `rising_edge_delayed` / `falling_edge_delayed`.

**What:** these are `delay_by spec ~n_cycles (edge_detector spec x)`, and the delay chain
carries the same `spec` — so the same clear. If a clear arrives on the cycle an edge is
detected, the edge is combinationally present that cycle but the register that would carry
it is cleared on the same edge, and it never emerges. Visible in the expect-test golden as
an entirely empty `falling_delayed` row: the clear cycle drives `x` low, `falling` is high
during it, and the delayed copy never appears.

**This is correct**, not a defect — a clear is supposed to flush the pipeline — but it is
the kind of thing that gets rediscovered as a bug. The consequence worth stating: a
consumer that resets mid-frame gets no delayed edge for the transition its own reset
caused. `rx_datapath`'s FCS-strip pipeline and `mac_top` both use these helpers.

Related and separate: sampled at `before_edge`, a synchronous clear's effect appears on the
cycle *after* the clear, not during it — the registers still hold their old contents while
the clear is being applied. The suite asserts both halves of that explicitly, because an
assertion written the obvious way ("nothing survives from the clear onward") is off by one
and fails.

### PROC-4. `after_edge` is degenerate for a combinational-off-input block

**What:** `Helper_circuits`' edge detectors are `~:x_d &: x` — combinational in the current
input and the registered previous one. At `after_edge` the register has already taken this
cycle's input, so `x_d = x` and *both detectors read zero unconditionally*, whatever the
input did. A suite that sampled `after_edge` out of habit would observe an all-false
column and could still pass a carelessly written test.

This is a third case beyond the two `test_architecture.md` already records. The rule
generalizes: sample `after_edge` when the observable is a register, `before_edge` when it
is combinational in the current input — and when it is the latter, `after_edge` is not
merely the wrong choice, it is *information-free*. The `helper_circuits` suite asserts the
degeneracy directly rather than leaving it as a comment, so the reasoning cannot decay.

`Uart_tx` is the same shape for a different reason: `uart_tx` is an `Always.Variable.wire`
driven off the current state, so `after_edge` reports the *next* symbol and shifts the whole
frame by one.

### PROC-5. A `Signal.t`-function module needs a wrapper DUT, and `delay_by 0` needs a wire

**What:** `helper_circuits.ml` exports plain `Signal.t -> Signal.t` functions over a
`Reg_spec.t`, not an `I` / `O` / `create` triple, so there is nothing for
`Sim_fixture.Make` to instantiate. The suite defines a wrapper `module Dut` inside its own
testbench — one input bit, one output per helper — and instantiates every helper against a
shared spec. Instantiating them together rather than one wrapper per function is what makes
`rising_edge_delayed` checkable *against* `rising_edge_detector` in a single simulation,
which is the whole content of its definition.

One mechanical trap: `delay_by spec ~n_cycles:0 x` returns `x` itself, and a circuit output
cannot be an input port directly. Wrap it — `wireof (Helper_circuits.delay_by spec
~n_cycles:0 i.x)` — or the wrapper will not elaborate. Worth an output of its own: it is
the base case of the recursion and the only place the identity is checkable.
