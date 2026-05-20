#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# INT-01 / #178 — compile the two cross-module fixtures and prove they
# link + execute across the wasm boundary. Reproducible acceptance run.
#
#   ./run.sh                 # uses `deno` on PATH (or $AFFINESCRIPT_DENO)
# Exit 0 = substrate proven; non-zero = regression.
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

"$deno" run --allow-read="$tmp" "$here/link.mjs" "$tmp/callee.wasm" "$tmp/caller.wasm"
