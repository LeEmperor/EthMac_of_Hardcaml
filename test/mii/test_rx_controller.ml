(* This file is intentionally not included in a Dune stanza.

   It previously contained an unfinished controller-test experiment. The active, runnable
   implementation now lives in [rx_controller/]:

   - [rx_controller_testbench.ml] contains the driver, observations, and scenarios.
   - [rx_controller_unit_quickcheck_tests.ml] contains typed and generated checks.
   - [rx_controller_expect_tests.ml] contains compact golden traces.

   Follow-up work should extend that suite with a stateful generator weighted toward
   boundary bytes and control events such as valid gaps, resets, [rx_er], and [en]
   transitions. Keeping those ideas here as a stub avoids presenting unfinished code as
   active test coverage. *)
