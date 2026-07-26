# hardcaml_networking

Networking hardware written in Hardcaml. Currently a full-duplex Ethernet MAC for 10/100 MBPs transfers. Targeted on Arty A7-100T DP83848x PHY present on board. 

Written in [HardCaml](https://github.com/janestreet/hardcaml) as a learning project. Any suggestions or contributions welcome.

Docs for PHY can be found [here](https://www.ti.com/lit/ds/symlink/dp83848j.pdf?ts=1776919033995&ref_url=http%253A%252F%252Fwww.ti.com%252F).

A Crude Image of TX path Reception:
<img width="1280" height="960" alt="image" src="https://github.com/user-attachments/assets/5b268704-c7fa-4f06-a22a-3bd179774248" />

---
<br>

# Installation Pre-Requisites

## Automatic
This will install a new [opam](https://opam.ocaml.org/) switch for [OxCaml](https://oxcaml.org/get-oxcaml/), as well as the relevant package dependencies for you. 

Run ```./bootstrap.sh --install-deps```

WARNING: This may take up to 30 minutes to install!

<br>


## Manual
This is the recommended way of installing as any breaking objects won't damage the state of the repo.

#### OxCaml
OxCaml install:

```opam switch create oxcaml-5.2 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default```

You will also want the following libraries for [OCaml](https://ocaml.org/)
1. ```dune```
2. ```core```
3. ```hardcaml```
4. ```ppx_hardcaml```
5. ```hardcaml_circuits```
6. ```hardcaml_waveterm```
7. ```alcotest```
8. ```ocamlformat```

Use ```opam install --switch=5.2.0+ox -y dune core hardcaml ppx_hardcaml hardcaml_circuits hardcaml_waveterm alcotest ocamlformat``` to install the set of dependencies manually, 

```OR``` let the bootstrap ```--install-deps``` flag handle it for you. You can also opt to install the main OxCaml switch yourself, and then let the dependencies afterwards get handled by ```./bootstrap.sh```.

<br>

#### Ubuntu/Debian (tested on 22.04/24.04)
```sudo apt install opam build-essential pkg-config```

#### macOS
```brew install opam```

#### Windows
```lmao```

<br>

---

# Setup
Run ```./bootstrap.sh```, followed by ```source ./env.sh``` to select the OxCaml opam switch for the current shell.

The project builds entirely with [dune](https://dune.build/). All commands go through `./scripts/with-switch.sh` so they run on the `5.2.0+ox` switch:

```sh
./scripts/with-switch.sh dune build      # build everything
./scripts/with-switch.sh dune runtest    # run all testbenches
./scripts/with-switch.sh dune fmt        # format
```

Generated VCD files can be opened with `./tools/open_wave.sh <vcd-file>`.
