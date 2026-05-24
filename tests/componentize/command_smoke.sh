#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# ADR-015 S6 gated smoke (WIT export lifting): compile an AffineScript
# program with `fn main()`, run it through `tools/componentize.sh
# --command`, and assert
#   (a) the resulting component is a valid WASI-0.2 component,
#   (b) it exports `wasi:cli/run@0.2.x`,
#   (c) `wasmtime run` can invoke it (exit 0), and
#   (d) the `affinescript.ownership` custom section survives the wrap.
#
# SKIPs cleanly (exit 0) when the component toolchain or wasmtime is
# not provisioned — opt-in, mirroring tests/componentize/smoke.sh.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

if ! command -v wasm-tools >/dev/null \
   || [ ! -f tools/vendor/wasi_snapshot_preview1.command.wasm ]; then
  echo "SKIP: component toolchain / command adapter not provisioned (tools/provision-component-toolchain.sh)"
  exit 0
fi
if ! command -v wasmtime >/dev/null; then
  echo "SKIP: wasmtime not on PATH (real-host run is the S6 contract)"
  exit 0
fi

COMPILER="${AFFINESCRIPT:-$ROOT/_build/default/bin/main.exe}"
[ -x "$COMPILER" ] || COMPILER="dune exec affinescript --"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Fixture: ownership-bearing imports + a parameter-less `main` so the
# S6 codegen emits `_start` (the command adapter's lift target). The
# fn main() return value is irrelevant — the shim drops it.
cp test/e2e/fixtures/verify_ownership_clean.affine "$work/cmd.affine"
printf '\nfn main() -> Int { add(2, 3) }\n' >> "$work/cmd.affine"

has_section() {
  [ "$(wasm-tools print "$1" 2>/dev/null | grep -c 'affinescript.ownership' || true)" -gt 0 ]
}

$COMPILER compile "$work/cmd.affine" -o "$work/cmd.wasm"

# Codegen contract: the unit MUST export `_start` (the S6 shim) since
# `main` is parameter-less. Catches a regression in lib/codegen.ml.
if ! wasm-tools print "$work/cmd.wasm" 2>/dev/null | grep -q '(export "_start"'; then
  echo "FAIL: S6 codegen did not emit the _start shim for fn main()"
  exit 1
fi

has_section "$work/cmd.wasm" \
  || { echo "FAIL: fixture did not emit the ownership section"; exit 1; }

tools/componentize.sh --command "$work/cmd.wasm" "$work/cmd.component.wasm"

wasm-tools validate --features component-model "$work/cmd.component.wasm"
has_section "$work/cmd.component.wasm" \
  || { echo "FAIL: ownership section lost through command componentization"; exit 1; }

# WIT contract: the lift must produce a `wasi:cli/run` export.
if ! wasm-tools component wit "$work/cmd.component.wasm" 2>/dev/null \
     | grep -q 'export wasi:cli/run'; then
  echo "FAIL: component does not export wasi:cli/run"
  exit 1
fi

# Real-host invoke: the whole point of S6 — `wasmtime run` actually
# runs the program end-to-end. Exit status MUST be 0 (the shim drops
# main's result; trap-free execution is the contract).
if ! wasmtime run "$work/cmd.component.wasm"; then
  echo "FAIL: wasmtime run rejected the S6 component"
  exit 1
fi

echo "ADR-015 S6 command smoke: PASSED ✓"
