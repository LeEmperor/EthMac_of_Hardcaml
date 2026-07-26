# New Test Architectures

Given I come from a more traditional aspect of verification (UVM!), many of these test things will be related as their UVM counterparts.

Vocabulary:
    "Test Scenario" = "Test"


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

This sexp can then be fed into something like print_s.


