#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# ADR-015 S3 gated smoke: compile a core module that carries the
# `affinescript.ownership` section, run it through the componentize
# on-ramp, and assert the result is a valid WASI-0.2 component with the
# ownership section intact (and that wasmtime can load it).
#
# SKIPs cleanly (exit 0) when the component toolchain is not provisioned
# — it is opt-in (run tools/provision-component-toolchain.sh first /
# the toolchain-bearing CI lane), so it never breaks the base gate.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

if ! command -v wasm-tools >/dev/null \
   || [ ! -f tools/vendor/wasi_snapshot_preview1.reactor.wasm ]; then
  echo "SKIP: component toolchain not provisioned (tools/provision-component-toolchain.sh)"
  exit 0
fi

COMPILER="${AFFINESCRIPT:-$ROOT/_build/default/bin/main.exe}"
[ -x "$COMPILER" ] || COMPILER="dune exec affinescript --"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# Known-good ownership fixture (emits the affinescript.ownership
# section) + a main so the module is non-trivial.
cp test/e2e/fixtures/verify_ownership_clean.affine "$work/cz.affine"
printf '\nfn main() -> Int { add(2, 3) }\n' >> "$work/cz.affine"

# `grep -c | … || true`: read all input (no early-close SIGPIPE that
# `set -o pipefail` would mis-report — the `grep -q` footgun).
has_section() {
  [ "$(wasm-tools print "$1" 2>/dev/null | grep -c 'affinescript.ownership' || true)" -gt 0 ]
}

$COMPILER compile "$work/cz.affine" -o "$work/cz.wasm"
has_section "$work/cz.wasm" \
  || { echo "FAIL: fixture did not emit the ownership section"; exit 1; }

tools/componentize.sh "$work/cz.wasm" "$work/cz.component.wasm"
wasm-tools validate --features component-model "$work/cz.component.wasm"
has_section "$work/cz.component.wasm" \
  || { echo "FAIL: ownership section lost through componentization"; exit 1; }

if command -v wasmtime >/dev/null; then
  # Reactor component: no command entrypoint, so a successful
  # instantiate (no trap) is the smoke. `--invoke` a missing export
  # would error; instead just validate it loads as a component.
  wasmtime compile "$work/cz.component.wasm" -o "$work/cz.cwasm" >/dev/null 2>&1 \
    && echo "wasmtime: component compiles/loads ✓" \
    || { echo "FAIL: wasmtime could not compile the component"; exit 1; }
fi

echo "ADR-015 S3 componentize smoke: PASSED ✓"
