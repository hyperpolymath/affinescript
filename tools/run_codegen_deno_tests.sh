#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# issue #122 — Deno-ESM backend regression runner.
#
# Mirrors tools/run_codegen_wasm_tests.sh: for every fixture in
# tests/codegen-deno/, compile FILE.affine -> FILE.deno.js with the
# --deno-esm backend, then run every *.harness.mjs with `node`.
#
# Node is used (not deno) deliberately: CI provisions Node 20 but not
# Deno, and the Phase 1 fixtures are pure logic — the generated module
# only references the `Deno` global lazily inside helper bodies that the
# harnesses never call, so plain Node ESM exercises them faithfully.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$ROOT_DIR/tests/codegen-deno"

if [ -x "$ROOT_DIR/_build/default/bin/main.exe" ]; then
  COMPILE_CMD=("$ROOT_DIR/_build/default/bin/main.exe" compile)
elif command -v affinescript >/dev/null 2>&1; then
  COMPILE_CMD=(affinescript compile)
else
  COMPILE_CMD=(dune exec affinescript -- compile)
fi

echo "Using compiler: ${COMPILE_CMD[*]}"

for src in "$TEST_DIR"/*.affine; do
  out="${src%.affine}.deno.js"
  echo "Compiling $(basename "$src") -> $(basename "$out")"
  "${COMPILE_CMD[@]}" "$src" -o "$out" --deno-esm
done

echo ""
echo "Running Deno-ESM harnesses (node)"
for js in "$TEST_DIR"/*.harness.mjs; do
  echo "node $(basename "$js")"
  (cd "$TEST_DIR" && node "$(basename "$js")")
done

echo "All codegen Deno-ESM tests passed."
