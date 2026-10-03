#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Build + run the consumer on-ramp example (issue #771).
#
# This is the "smallest thing" the consumer asked for, in executable form:
#
#   ./build.sh          compile src/boundary.affine -> dist/boundary.wasm,
#                       then run the Node harness against it.
#
# The compiler is located the three ways a consumer legitimately has it:
# an in-tree dune build (a checkout of this repo), an `affinescript` on
# PATH (a release binary), or `dune exec` (a checkout without a prior
# build). Nothing else is required — no deno.json, no npm manifest, no
# bundler config.
set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
ROOT="$(cd "$HERE/../../.." && pwd)"

SRC="$HERE/src/boundary.affine"
OUT_DIR="$HERE/dist"
OUT="$OUT_DIR/boundary.wasm"

if [ -x "$ROOT/_build/default/bin/main.exe" ]; then
  COMPILE=("$ROOT/_build/default/bin/main.exe" compile)
elif command -v affinescript >/dev/null 2>&1; then
  COMPILE=(affinescript compile)
else
  COMPILE=(dune exec affinescript -- compile)
fi

mkdir -p "$OUT_DIR"

echo "Compiling $(basename "$SRC") -> $(basename "$OUT")"
"${COMPILE[@]}" "$SRC" -o "$OUT"

echo "Running host.mjs"
node "$HERE/host.mjs"
