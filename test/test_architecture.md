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
