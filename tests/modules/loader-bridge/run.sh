#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# INT-02 / #179 — compile the INT-01 cross-module fixtures and drive them
# through the real packages/affine-js loader API. Reproducible acceptance.
#
#   ./run.sh                 # uses `deno` on PATH (or $AFFINESCRIPT_DENO)
# Exit 0 = loader bridge proven on real compiler output; non-zero = regression.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
bin="$root/_build/default/bin/main.exe"
fix="$root/test/e2e/fixtures"
deno="${AFFINESCRIPT_DENO:-deno}"

export AFFINESCRIPT_STDLIB="$root/stdlib"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

[ -x "$bin" ] || { echo "build the compiler first: dune build bin/main.exe" >&2; exit 9; }
command -v "$deno" >/dev/null 2>&1 || { echo "deno not found (set \$AFFINESCRIPT_DENO)" >&2; exit 9; }

( cd "$fix" && "$bin" compile CrossCallee.affine    -o "$tmp/callee.wasm" >/dev/null )
( cd "$fix" && "$bin" compile cross_caller_ok.affine -o "$tmp/caller.wasm" >/dev/null )

# bridge.mjs imports the loader from packages/affine-js, so allow-read spans
# the repo root (loader source) + the temp dir (the .wasm).
"$deno" run --allow-read="$root","$tmp" "$here/bridge.mjs" "$tmp/callee.wasm" "$tmp/caller.wasm"
