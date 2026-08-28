# New Test Architectures

Given I come from a more traditional aspect of verification (UVM!), many of these test things will be related as their UVM counterparts.

Vocabulary:
    "Test Scenario" = "Test"


---

# Settled Conventions

Everything below this heading is decided and in force. Everything after the next `---`
is still exploratory notes. Source formatting and signal naming live in
`docs/formatting_guide.md`; this section covers only what that guide does not. The rules here are the
durable half of what the sweep found; the incidents behind them, and the RTL behavior the
suites pinned down without changing, are logged in `docs/verif_sweep_findings.md`.

## The four-file suite layout

For DUT `foo` in domain `<d>`, `test/<d>/foo/` holds:

| File | Role |
| --- | --- |
| `foo_testbench.ml` | `module Dut = Foo`, the `Observation` / `Output_snapshot` types, `inputs ~...`, `reset`, drivers, scenarios, `run_*` entry points |
| `foo_unit_quickcheck_tests.ml` | `let%test_unit` examples + `Quickcheck.test` properties, ``~seed:(`Deterministic ...)``, a shrinker where the input type has one |
| `foo_expect_tests.ml` | `let%expect_test` golden traces via `print_s [%sexp (obs : ...)]` |
| `foo_legacy_assertion_test.ml` | the superseded harness, if one existed |
| `dune` | one `(library ... (inline_tests ...))` over the first three modules, plus a separate `(executable)` for the legacy module |

The testbench is the only file that touches `Step` or `Bits`. The other two consume its
typed observations, which is what keeps a scenario shared between a property test and a
golden trace instead of written twice.

## `-source-tree-root .` is mandatory in every `(inline_tests)` stanza

```
(inline_tests
 (flags
  (:standard -source-tree-root .)))
```

ppx_expect v0.18~preview drops the directory from a test file's path when registering
the test, then rebuilds the path at exit from the bare filename plus `-source-tree-root`,
yielding `_build/default/<basename>` — which does not exist. The trailing flag wins over
the one dune passes (`%{workspace_root}`), and `.` points it at the runner's cwd, where
dune has copied the sources. Copy the explanatory comment along with the flag. Drop both
if a later ppx_expect fixes the path handling.

## Legacy harnesses are compiled, never run

A superseded `printf`/`exit 1` harness becomes `foo_legacy_assertion_test.ml` in the
DUT's directory under a bare `(executable)` stanza. `dune build` compiles it, so it keeps
type-checking against the RTL instead of silently rotting; `dune runtest` never invokes
it, so its output spam and `exit 1` stay out of CI. It carries the
`Tags: [{ "DEPRECATED" ; "ASSERTION_TEST" }]` marker in its header.

A legacy file that is in *no* stanza is not relegated, it is dead — it does not compile
at all and will drift from the RTL unnoticed.

## `hardcaml_verif` — the shared verification library

`test/common/verif/` → library `hardcaml_verif`. Reach for it before writing a helper.

| Module | Contents |
| --- | --- |
| `bits_conv.ml` | `bit : bool -> Bits.t`, `to_bool`, `to_int`, `of_int ~width` |
| `sim_fixture.ml` | `Make (Dut : S)` → `Sim`, `Step`, `create_simulator`, `run_with_timeout ~timeout ~testbench` |
| `crc32.ml` | `bit` / `byte` / `bytes` folds over the reflected 0xEDB88320 polynomial, plus `fcs`, `fcs_bytes`, `residue` |
| `eth_frame.ml` | validated frame record, `create`, `byte_count`, `crc_covered_bytes`, `to_bytes`, `fcs_bytes`, `with_fcs` |
| `ip_udp.ml` | `Ipv4.header` / `Ipv4.checksum` (ones-complement, end-around carry), `Udp.header`, `hi8` / `lo8` / `w16` |
| `generators.ml` | `byte`, `byte_list`, `mac_address`, `ipv4_address`, `port`, `payload_length`, `eth_frame` as `Quickcheck.Generator.t` |

Unchanged since phase 0: neither phase 1's five MII suites nor phase 2's four needed
anything added to it. A helper that only one suite wants belongs in that suite's
testbench — `uart_tx`'s software receiver stays there, because nothing else speaks UART.

`Crc32` exposes the accumulator two ways and they are not interchangeable: `bytes`
returns the raw accumulator, which is what `Rx_crc.crc_out` holds and what must equal
`residue` (`0xDEBB20E3`) once a frame *and its FCS* have both been clocked through;
`fcs` applies the final inversion and is the word `Tx_crc` emits a byte at a time.

`Sim_fixture` deliberately re-exports `Step` rather than wrapping it — suites keep
calling `Step.cycle` / `Step.delay` / `Step.O_data` directly. That is also the seam where
an EventSim backend slots in later as a parallel functor over the same `S`.

## OxCaml: arity is part of the arrow type

Not a style rule — a compile error you will otherwise spend an afternoon on. OxCaml
encodes function arity in the arrow, so `a -> b -> c` (arity two) and `a -> (b -> c)`
(arity one, returning a closure) are *different types*. A function that takes a
`Handler.t @ local` and whose arity is inferred as one lets the local handler escape its
region, and every call site is rejected with:

```
Error: This function or one of its parameters escape their region
       when it is partially applied.
```

pointing at the caller's lambda, not at the real cause. This bites any helper that
forwards a step-testbench `~testbench` argument across a module or functor boundary,
because the inference there has no reason to pick arity two. The fix is to write the
parameter's type out explicitly and unparenthesized — see `run_with_timeout` in
`sim_fixture.ml`:

```ocaml
let run_with_timeout
  ~timeout
  ~(testbench : Step.Handler.t @ local -> Step.O_data.t -> 'a)
  =
```

Same reasoning applies to any future `Sim_fixture`-like wrapper around `Step.spawn` or
`Step.wait_for`.

### The other half: a closure over the handler cannot both capture and allocate

`List.map`ing a drive function over a list of stimuli looks like the obvious way to write
a driver, and it does not compile when the closure returns a record:

```
Error: The value "handler" is "local" to the parent region
       but is expected to be "global"
       ... which is expected to be "global" because it is an allocation
```

A closure that captures the local handler must itself be local; a closure that allocates
its result must be global. Wanting both is the error. `List.iter` with a unit-returning
body is fine — no allocation — which is why `List.iter ... ~f:(fun byte -> ignore
(drive_byte handler byte : _))` compiles next to a `List.map` that does not.

The fix is the one the phase-1 testbenches already use without saying why: write the
traversal as an explicit `let rec loop (handler : Step.Handler.t @ local) = function`,
whose recursive call is a tail call in the handler's own region. For a fixed short
sequence, a run of `let` bindings works too, and has the side benefit of pinning
evaluation order — OCaml does not fix the order of a list literal's elements, which
matters when each element advances the simulation.

## Which side of the clock edge a suite samples

`Step.cycle` hands back both, and the choice is not stylistic — it decides what the
observation *means*.

- **`after_edge`** is the state the DUT settled into as a result of this cycle's inputs.
  Sample it when the thing under test is a register or a state the block just entered:
  `rx_byte_assembler` (`byte_valid` and `byte_out` land together at the edge), `rx_crc`
  and `tx_crc` (the accumulator including this byte), `rx_datapath`, `rx_controller`.
- **`before_edge`** is what the block was driving *during* the cycle. Sample it when the
  outputs are instructions to a combinational consumer, because the byte that goes on the
  wire this cycle is the one this cycle's outputs select. `tx_controller` is the case:
  `byte_mux_sel` is `sm.current` and `tx_datapath` muxes off it with no register in
  between.

A suite that samples `before_edge` usually needs `after_edge` as well — not as an
observation but as the next state, so the driver can choose the following cycle's
stimulus. That is what makes `tx_controller`'s driver a loop that follows the DUT's own
state rather than a fixed script, and it is worth copying: a fixed script silently stops
testing anything the moment the FSM's timing changes.

A purely combinational DUT has no such choice — both sides agree. `tx_datapath` asserts
exactly that rather than assuming it, so a registered stage cannot be added there without
a test noticing.

A block that is combinational in the *current input* and a registered copy of it is the
third case, and there `after_edge` is not merely the wrong choice — it is
information-free. `Helper_circuits`' edge detectors are `~:x_d &: x`; at `after_edge` the
register has already taken this cycle's input, so `x_d = x` and both detectors read zero
whatever the input did. A suite sampling there sees an all-false column and can still
pass a carelessly written test. `helper_circuits` asserts the degeneracy outright.
`uart_tx` lands in the same place by a different route: its line is an
`Always.Variable.wire` driven off the current state, so `after_edge` reports the *next*
symbol and shifts the whole frame by one.

One more, for `before_edge` specifically: a synchronous clear's effect appears on the
cycle *after* the clear, not during it — while the clear is being applied the registers
still hold their old contents. The obvious assertion ("nothing survives from the clear
onward") is off by one.

## Goldens for one-bit-per-cycle behavior are waveform rows, not sexps

A sexp list of records is the right golden when the observation is structured. When it is
one bit per cycle, it is not: the reviewable claim about a clock divider, a heartbeat
pulse, or an edge detector is an *alignment* — which cycle the pulse lands on, how far the
delayed copy trails — and stacked character rows show that where a list of booleans does
not. `clk_div` and `second_pulse` print a single row; `helper_circuits` prints one
labelled row per output with the input row on top. Keep one sexp golden per suite anyway,
over a short scenario, so the typed record itself stays visible.

## Wide values print in hex, in a `Compact_observation`

A 32-bit accumulator rendered as `3736805603` is not reviewable against a standard that
quotes `0xDEBB20E3`. Suites whose observations carry wide values keep the typed
`Observation.t` in `int` — that is what `[%test_result]` compares — and give
`Compact_observation.t` `string` fields formatted with `sprintf "0x%08x"`, which is what
the goldens print. The same record is where the boolean control lines collapse into an
`active_outputs : string list`.

## Hardcaml `mux` saturates on an out-of-range select

It returns the *last* element, not a wrap and not an error. A software model of a mux
must clamp its index the same way (`List.nth_exn values (Int.min index (length - 1))`) or
it will disagree with the RTL on every out-of-range combination. `tx_datapath` drives
`mac_byte_sel` across all eight values against six-entry MAC lists precisely to freeze
this.

## A DUT with an optional `create` argument cannot be `include`d

`Sim_fixture.S` wants `val create : Scope.t -> Signal.t I.t -> Signal.t O.t`, and OxCaml
will not erase an optional argument to match it. `Tx_datapath.create` takes
`?ethertype`, so its fixture spells the signature out:

```ocaml
module Fixture = Sim_fixture.Make (struct
    module I = Dut.I
    module O = Dut.O

    let create scope inputs = Dut.create ~ethertype:Config.ethertype scope inputs
    let name = "Tx_datapath"
  end)
```

Wrapping that in a `Make_testbench (Config : sig val ethertype : int end)` functor lets
one suite instantiate the same DUT at two parameter values. That is not a workaround, it
is the point: `tx_datapath`'s default ethertype is `0x9999`, whose two bytes are equal, so
a byte-order fault in the ethertype mux is invisible until a second instance runs at
`0x0800`. Look for the same trap in any block whose defaults are palindromic.

`second_pulse` uses the same shape for a different reason — its `?clk_freq` defaults to
100 MHz, one pulse every hundred million cycles, which no simulation reaches — and adds
the trick worth copying: the `Observation` and summary types are declared **outside** the
functor, so every instantiation produces the same type and one list of runner records can
carry all six. A property then runs across the whole set instead of being written out per
parameter value.

Choose the parameter values so they disagree about something. `second_pulse` runs at 3, 4,
5, 8, 10 and 16: at a power of two the terminal count coincides with the counter's natural
wrap, and a counter that rolled on the wrap rather than on the compare would pass every
power-of-two instance. `tx_datapath`'s palindromic default is the same hazard.

## A module of plain `Signal.t` functions needs a wrapper DUT

`lib/common/helper_circuits.ml` exports `Signal.t -> Signal.t` functions over a
`Reg_spec.t`, not an `I` / `O` / `create` triple, so there is nothing for
`Sim_fixture.Make` to instantiate. Define a wrapper `module Dut` inside the suite's own
testbench, with one input per argument and one output per function, and instantiate every
function against a shared spec.

Instantiate them *together*, not one wrapper per function. `rising_edge_delayed` is by
definition `delay_by n (rising_edge_detector x)`, and that definition is only checkable if
both are visible in the same simulation — which is the difference between testing the
composition and re-implementing it in the model.

Mechanical trap: a circuit output cannot be an input port directly, and
`delay_by spec ~n_cycles:0 x` returns `x` itself. Wrap it in `wireof` or the DUT will not
elaborate. Give it an output anyway — it is the recursion's base case and the only place
the identity is checkable.

## Prefer a protocol-level oracle to a cycle-level one

Where the DUT speaks a protocol, model the *receiver*, not the timing. `uart_tx`'s oracle
is `Uart_receiver.decode`, which reconstructs a byte from the symbols the fixture sampled
exactly as a real receiver would; the headline property is a round trip and is indifferent
to how the transmitter chooses to time itself. Running it across tick spacings and enable
stalls is what turns "it emitted the right waveform once" into "it is tick-driven".

Two supporting habits from that suite:

- **A symbol is an interval, not a sample.** The fixture records the line on every cycle
  between two ticks and rejects a symbol whose line moved mid-interval. A single mid-bit
  sample would accept a transmitter that glitched between ticks; a real receiver sampling
  at its own phase would not.
- **Keep the decoder total.** An unexpected symbol count or an unstable symbol comes back
  *in the record* rather than raising, so a failing property prints what the line actually
  did instead of a backtrace from inside the model.

## Expect tests: promote, then read

Write `[%expect {| |}]` empty, run `dune runtest`, then `dune promote` — **and then read
the promoted output against the RTL's intended behavior before committing.** A golden
freezes current behavior; blind promotion enshrines a bug as the specification. Where a
legacy assertion harness asserted a specific value, cross-check the new golden against
that assertion before relegating the legacy file.

Keep goldens short. A 400-line trace is not a reviewable diff — put the long
frame-level scenarios in `let%test_unit` cases and leave one or two representative
frames in the expect file.

## Formatting and verification

Per the formatting guide's section 9, and in this order:

```sh
./scripts/with-switch.sh dune build @fmt     # scope to a dir: @<dir>/fmt --auto-promote
./scripts/with-switch.sh dune build @lint
./scripts/with-switch.sh dune build          # must be clean, legacy executables included
./scripts/with-switch.sh dune runtest        # must be green with no expect diffs
```

Much of `lib/` and the untranslated `test/` tbs are not formatter-clean at baseline, so a
bare `dune build @fmt` reports a wall of pre-existing diffs. Promote per directory
(`dune build @test/mii/foo/fmt --auto-promote`) rather than running `dune fmt` across the
repo, or a suite's diff will arrive buried in unrelated reflows.

### Disabling the formatter: use the floating pair, never the item attribute

This ocamlformat rejects `[@@ocamlformat "disable"]` outright, wherever it is attached:

```
Error: Invalid ocamlformat attribute. Ocamlformat can only be disabled at toplevel
(e.g [@@@ocamlformat "disable"])
```

It is an **error**, not a warning, and it fails `dune build @lint` — which is how five
phase-0 files sat with a red lint alias while the plan recorded it as green. Bracket the
hand-aligned run instead:

```ocaml
[@@@ocamlformat "disable"]

let create
  ?(preamble_length     = 7)
  ...
;;
[@@@ocamlformat "enable"]
```

Both forms work inside a `struct`, indented to the enclosing level. Close every `disable`
with an `enable`: an unclosed one silently exempts the rest of the file.

### An empty `(**)` makes ocamlformat refuse the whole file

`ocamlformat: ignoring "<file>" (misplaced documentation comments - warning 50)` — the
file is skipped entirely and `@fmt` stays red no matter how many times it is promoted.
`(**)` is an empty *doc* comment the compiler cannot attach to anything. Write `(* *)`,
or say something.

## Deferred, on purpose

- **Alcotest.** The "longer more thought out tests → proper suites" idea below is on hold
  pending confirmation from a Jane Street dev that Alcotest is the right vehicle rather
  than plain `let%test_unit` suites. `test/udp/udp_alcotest_lib.ml` and the `alcotest`
  dep stay in place, untouched. If the answer is yes the retro-fit is purely additive: a
  fourth file `foo_integration_tests.ml` per integration dir, reusing the same
  `foo_testbench.ml`.
- **EventSim.** `hardcaml_event_driven_sim` and
  `Hardcaml_step_testbench.Functional.Event_driven_sim` are both installed in the switch,
  and the portable-equivalence idea below still stands, but no EventSim suites are
  written yet. `Sim_fixture` is shaped to accept a second backend when they are.
- **`_i` / `_o` port rename.** The formatting guide's section 3 requires it; no current
  `lib/` module complies. Testbenches bind to the port names that exist today. A later
  rename sweep of `lib/` would touch the `inputs ~...` and `snapshot` functions of every
  suite.

---


# Test Scenario - Agnostic, Backend-neutral
Does JS call these "test scenarios" or can I refer to them as tests as UVM does?

# Driver
Needs (2) interfaces, will drive things into the Cyclesim model AND the Eventsim model

Perhaps some other entity of some sort that sends things out to the driver?
    should we have different drivers for cyclesim vs eventsim?
    or one singular driver that speaks different languages
    same for the monitors -> should each simulator have it's own implemented monitor? or should a singular monitor "speak" 2 different languages?

the drivers accept normalized items, and the monitors take wire activity and re-emit TLM items
    the scoreboard then takes in (3) data streams:
        1. DUT via Cyclesim
        2. DUT via Eventsim
        3. Reference model


# Observations
Monitor items? is this a janestreet vocabulary? or can i use a uvm-like name for this?
Monitor should produce this normalized type based on the wire activity out of either simulation backend

```
type observation =
{
    payload : int list
    ; metadata      : metadata option
    ; crc_error     : bool
    ; port_match    : bool
}
```

# Scoreboard
```
[%test_result: ...]
```


# Test Classifications
Alcotest
    longer more thought out tests -> to be composed of proper suites
Inline Test
Expect Test
    smaller tests made to be used with waveforms
Quickcheck

# Hardcaml_step_testbench
Concurrent ```spawn``` and ```wait_for``` similar to ```fork/join``` from SV..

Functional + Imperative Supports
Both Cyclesim and Eventsim implementations.
Common API for testbench interactions with either simulator.
Explicit before-edge/after-edge  obvservations (like clocking_block)

# Map
   UVM concept          Recommended OCaml representation
  ━━━━━━━━━━━━━━━━━━━  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   Sequence item        Immutable record/variant with sexp, equality, and generator
  ───────────────────  ─────────────────────────────────────────────────────────────
   Sequence             Function or Step Testbench computation
  ───────────────────  ─────────────────────────────────────────────────────────────
   Driver               Module converting transactions into DUT inputs
  ───────────────────  ─────────────────────────────────────────────────────────────
   Monitor              Spawned task converting DUT outputs into transactions
  ───────────────────  ─────────────────────────────────────────────────────────────
   Scoreboard           Pure function comparing expected and actual transactions
  ───────────────────  ─────────────────────────────────────────────────────────────
   Agent                Module bundling driver, monitor, and configuration
  ───────────────────  ─────────────────────────────────────────────────────────────
   Environment          Composition of spawned tasks
  ───────────────────  ─────────────────────────────────────────────────────────────
   Test                 let%test_unit, let%expect_test, or Quickcheck test
  ───────────────────  ─────────────────────────────────────────────────────────────
   Objection/timeout    wait_for and explicit timeout

#
  - Portable equivalence tests: same design and scenarios on both simulators.
  - EventSim capability tests: real async FIFO, multiple clocks, async reset.
  - CycleSim approximation tests: the synchronous simulation-only replacement.

# The (3) Tests to be Done
## Inline Tests
Inline is a test runner in OCaml -> group of things associated with a PPX.

## Expect Tests
Main way alot of agile tests are done in relation to Janestreet frameworks.
Differs heavily from UVM, though some of the base principles are translateable for someone who comes from the traditional chip-testing space.

Expect tests are a "kind" of inline test.

Expect tests are good when the "printed trace" is of some use.

## Quickcheck Tests



# The OCaml PPX System
```
(preprocess
    (pps ppx_hardcaml ppx_jane)
)
```

Before the OCaml compiler type-checks the code, the PPX programs rewrite the specally-syntaxed code into regular OCaml.
Without appropriate PPX, compiler reports "uninterpreted extension" error.

```let%expect_test "name" = ...```

```let%<extension_name>``` is an example usage. The extension "expect_test" portion modififes the meaning of ```let``` itself, telling the PPX:
"Register this binding as an expect test rather than as an ordinary value".

Behind the scenes, this is transformed into something like:
```
register_expect test
    ~name:"bytes_assembled"
    (fun () ->
        (* original test body *)
    )
```

This is not truly what happens but represents the idea behind PPX extensions and some semablance of "meta-programming". Perhaps the cilic C++ programming may even think of it as templating.

What is ```[%expect {|171 18|}]```?
This is an expresion extension.

They follow a general shape of ```[%extension_name payload]```. Importantly, we do NOT have a list here, instead ```[%``` begins a PPX extension expression.

```[%expect]``` tells the expect-test framework "compare all outputs captured since the previous expectation with this string."
It also records the source location so that a failing test can produce a corrected version of the file.

What does ```{|...|}``` mean?
OCaml quoted-string literal: ```{|hello world|}```, similar to how "hello\nworld" works for working with escape sequences.

```[%sexp ...]``` follows ```[%sexp (completed_bytes : int list)]```. Converts a value into an S-expression using it's declared type.
Example ```completed_bytes = [171; 18]``` produces ```(171 18)```.

This sexp can then be fed into something like print_s via ```print_s [%sexp (completed_bytes : int list)]```.


How can the following line exist:
```[%test_result: int list] actual_bytes expected_bytes```

The left side of a function application in OCaml can be any expression, not only a function name.

For example, ```(fun x -> x + 1) 5```. ```(fun x -> x + 1)``` evalutes to a function, that then takes in 5 as it's argument.

Likewise ```[%test_result: int list]``` is an extension expression that the PPX rewrites into a function-like value specialized for int lists.

Therefore, ```[%test_result: int list] actual_bytes ~expect:expected_bytes``` is treated as ```([%test_result: int lits]) actual_bytes ~expect:expected_bytes```, and after the PPX rewrites, it *may* resemble ```generated_list_function actual_bytes ~expect:expected_bytes```.

# Useful PPX Idioms and Categories

1. ```let%expect_test "name" = ...``` extension attached to a let.

2. ```[%expect {| output |}]``` expression extension with a string payload.

3. ```[%test_result: int list] actual ~expected:expected_bytes``` expression extension with a typed payload, followed by an ordinary function application.

4. ```[%sexp (value: Some_type.t)]``` expression extension generating an S-expression.

5. ```type t = {value : int} [@@deriving sexp, equal]``` an attribute attached to a type declaration, generating functions such as sexp_of_t and eqaul.
ta
```%``` constructs compile-time requests to generate ordinary OCaml code. They are not special runtime objects by any means.

# Writing a Testbench with Hardcaml_step_testbench

Write out your standard simulator back-end instantiations, as well as your DUT.

```
module Dut = Mii_of_hardcaml.Rx_byte_assembler
module Sim = Cyclesim.With_interface (Dut.I) (Dut.O)
```

Add in your step testbench instantiation.

```
module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (Dut.I) (Dut.O)
```

Write your test scenario (think of this like a "UVM-test", aka a "base_test", or a "random_test", or a "should_be_fine_test" etc. Like a UVM-test object, this relies on a certain "executor" of it's contents. In UVM this is commonly the "agent", which delegates between the driver/monitor - sometimes both. In Hardcaml, the "executor" shall be referred to as the "handler".

```
let scenario handler _initial_outputs : Bits.t Dut.O.t list =
    (* Reset *)
    Step.delay (* Apply the reset, we don't care about outputs here *)
        handler
        {   Step.input_hold with

            reset = Bits.vdd
            ; en = Bit.gnd
            ; rx_data = Bit.zero 4
        };

    (* composable for applying some data, and then saving the results *)
    let after_low =
        Step.cycle
            handler
            { Step.input_hold with
                reset = Bits.gnd
                ; en = Bits.vdd
                ; rx_data = Bits.of_int_trunc ~width:4 0xB
            }
    in

    let after_high =
        Step.cycle
            handler
            { Step.input_hold with
                reset = Bits.gnd
                ; en = Bits.vdd
                ; rx_data = Bits.of_int_trunc ~width:4 0xA
            }
    in

    (* the cycle result after presenting the low nibble *)
    (* apply items, step, and store outputs *)
    let low_nibble_step = Step.O_data.after_edge after_low in

    (* the cycle result after presenting the high nibble *)
    (* apply items, step, and store outputs *)
    let high_nibble_step = Step.O_data.after_edge after_high in

    (* both low_nibble_step and high_nibble_step contain {before_edge...; after_edge...} entires
        we happen to only care about the data out of the DUT *after* the edge, which is why we are extracting the after_edge component of the O_data.t item from after_low
    *)

    (* pack them together *)
    [ low_outputs; high_outputs ]
    ;;
```

This may seem like alot, and it really is. Below sectinos break down individual components of the scenario.

### Cyclesim Runner
Similar to the idea of a "run" task in a UVM component, we have a function we must declare that the test can actually clal to kick off the simulation backend.
This is a single-call function that "kicks off" the scenario.

```
let run_cyclesim() =
    let scope =
        Scope.create
            ~flatten_design:true
            ~auto_label_hierarchical_ports:true
            ()
    in
    let simulator = Sim.create (Dut.create scope) in
    Step.run_with_timeout
        ~timeout:16
        ()
        ~simulator
        ~testbench:scenario
;;
```

### Expect Test Itself
```
let%expect_test "assembles 0xAB" =
    let result = run_cyclesim () in
    print_s
        [%sexp
        (result : Bits.t Dut.O.t list option)];

        [%expect
        {|
        (((byte_out 11) (byte_valid 0))
            ((byte_out 171) (byte_valid 1)))
        |}]
    ;;

```

# Step Library Manual
The ```step_testbench``` library usage compounds of (3) main things:
    1. apply a record of inputs (aka a set of signals across the inputs)
    2. advance the simulation n cycles
    3. gather the outputs as another record

#### ```Step.delay : ?num_cycles:int -> Step.Handler.t -> Bits.t Dut.I.t -> unit```:

```
Step.delay
    handler
    { Step.input_hold with
        reset = Bits.vdd
        ; en = Bits.gnd
        ; rx_data = Bits.zero 4
    }
````
"Apply these input assignments, advance the simulation by one cycle, and *discard* the output snapshot." Returns a unit, thus "discard" the outputs. Think of how ```ignore``` works to draw a paradigm between ```delay``` and ```cycle```.

#### ```Step.cycle```
Applies a record of inputs, advances the simulatoin N cycles, and returns an output record of ```Step.O_data.t```. Importantly, we do not ```ignore``` or ```discard``` the outputs.
More keenly, apply inputs, evaluate combinational logic before edge, update registers/memories at edge, evaluate combinational logic after edge, and return ```{before_edge ... ; after_edge...} ```.

Contains (2) complete output records:
```type O_data.t =
    {
        before_edge : Bits.t Dut.O.t
        ; after_edge : Bits.t Dut.O.t
    }
```

Example: ```let outputs = Step.cycle handler inputs``` returns ```Step.O_data.t```. Now can can use the outputs associated with the cycle.

#### ```Step.input_hold```
A type, as indicated in the signature ```val input_hold : Hardcaml.Bits.t I.t```, but often applied as ```Bits.t Dut.I.t```.
Every field contains ```Bits.empty```, which ```Step``` interprets as "this task is not assigning the field, retain the previous value."

Essentially, we use this in combination with record-update syntax to *override* the value of stuff from an existing record. Here we have all other fields that ```Step.delay``` is taking in on the input record hold, while specifically ```reset```, ```en```, and ```rx_data``` are driven to specific values. ```clock``` is the main thing that we are not choosing to make any changes to, and are thus having the simulator "hold" at the previous value.

```Step.run_until_finished```:
```Step.run_with_timeout```:

# Typed Test vs Expect Test
