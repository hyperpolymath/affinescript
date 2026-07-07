#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# End-to-end runtime test for the DOM reconciler (INT-08 / #183).
# Concatenates src/dom.affine + driver_main.affine, compiles to core-WASM,
# runs it under Node against an Int-handle host DOM (dom_host.mjs), and
# asserts the mutation log + final tree. Exit 0 = the reconciler (including
# every for/while loop — the #255 class) executed correctly in compiled wasm.
#
# Requires: the compiler built (dune build → _build/default/bin/main.exe;
# override with AFFINESCRIPT_BIN=/path/to/main.exe)
# and Node ≥ 18 on PATH. Skips (exit 0, loud) if either is missing so the
# harness can sit in CI paths that lack the OCaml toolchain.
set -uo pipefail
cd "$(dirname "$0")"
REPO="$(cd ../.. && pwd)"
BIN="${AFFINESCRIPT_BIN:-$REPO/_build/default/bin/main.exe}"
[ -x "$BIN" ] || { echo "SKIP: compiler not built ($BIN missing) — run dune build"; exit 0; }
command -v node >/dev/null 2>&1 || { echo "SKIP: node not on PATH"; exit 0; }

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
cat ../src/dom.affine driver_main.affine > "$TMP/dom_drive.affine"
"$BIN" compile "$TMP/dom_drive.affine" -o "$TMP/dom_drive.wasm"
node dom_host.mjs "$TMP/dom_drive.wasm"
