(lang dune 3.17)

(name hardcaml_networking)

(generate_opam_files true)

(package
 (name hardcaml_networking)
 (allow_empty)
 (synopsis "Networking hardware (Ethernet MAC, UART) written in Hardcaml")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.15))
  core
  hardcaml
  hardcaml_xilinx_reports
  ppx_hardcaml
  hardcaml_circuits
  hardcaml_waveterm
  hardcaml_step_testbench
  ppx_expect
  base_quickcheck
  alcotest
  (ppx_js_style :with-dev-setup)
  (ocamlformat :with-dev-setup)
  (ocaml-lsp-server :with-dev-setup)))
