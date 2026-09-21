#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# issue #122 corpus — now compiled with the Bun-ESM backend.
#
# Directory name `tests/codegen-deno/` is historical. Every fixture is
# compiled with `--bun-esm` to FILE.bun.js. Harnesses still run under
# `node` in CI (Node 20 is provisioned; these fixtures do not need the
# Bun binary — they mock host objects). Native Bun acceptance lives in
# tools/run_codegen_bun_tests.sh.
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
  out="${src%.affine}.bun.js"
  echo "Compiling $(basename "$src") -> $(basename "$out")"
  "${COMPILE_CMD[@]}" "$src" -o "$out" --bun-esm
done

echo ""
echo "Running ESM harnesses (node, Bun-ESM artefacts)"
for js in "$TEST_DIR"/*.harness.mjs; do
  echo "node $(basename "$js")"
  (cd "$TEST_DIR" && node "$(basename "$js")")
done

echo "All codegen Bun-ESM (legacy codegen-deno corpus) tests passed."
