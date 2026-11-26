#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Self-hosted OCaml toolchain for CI (replaces ocaml/setup-ocaml).
#
# Creates a local opam switch in the current directory on OCaml 4.14:
#   - the runner's apt OCaml (`ocaml-system`) when it is 4.14.x, which is
#     instant;
#   - otherwise a pinned `ocaml-base-compiler.4.14.2`, built from source.
#
# Why not "any OCaml >= 4.14": the with-test dependency bisect_ppx (>= 2.8)
# requires OCaml < 5.0. Ubuntu 26.04 runners ship OCaml 5.4, on which
# `opam install --deps-only --with-test` cannot be solved, so every OCaml job
# failed there before this fallback.
#
# Usage: tools/ci/setup-ocaml.sh   (from the repo root, on a Debian/Ubuntu runner)
set -euo pipefail

PINNED=ocaml-base-compiler.4.14.2

sudo apt-get update
sudo apt-get install -y --no-install-recommends opam ocaml
opam init --bare --disable-sandboxing --yes

system_version=$(ocamlc -version 2>/dev/null || true)
case "$system_version" in
  4.14.*)
    echo "Using the runner's OCaml $system_version (ocaml-system)"
    opam switch create . ocaml-system --no-install --yes
    ;;
  *)
    echo "Runner OCaml is '${system_version:-absent}', not 4.14.x: building $PINNED"
    opam switch create . "$PINNED" --no-install --yes
    ;;
esac

# Fail closed if the switch did not land on 4.14.
switch_version=$(opam exec -- ocamlc -version)
case "$switch_version" in
  4.14.*) echo "OCaml switch ready: $switch_version" ;;
  *) echo "::error::OCaml switch is $switch_version, expected 4.14.x"; exit 1 ;;
esac
