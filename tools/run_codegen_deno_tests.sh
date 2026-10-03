#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# issue #122 corpus — now compiled with the Bun-ESM backend.
#
# Directory name `tests/codegen-deno/` is historical. Every fixture is
# compiled with `--bun-esm` to FILE.bun.js. Harnesses still run under
# `node` in CI (Node 20 is provisioned; fixtures that need a host stub must
# stub the *Bun* host surface — process.getBuiltinModule / process.argv /
# process.exit — because that is what the emitted prelude actually reads;
# a `globalThis.Deno` stub is inert under this backend).
#
# Deliberately NOT `set -e`: failures are collected so one broken harness
# cannot hide every harness after it (the same masking bug that hid 33
# tests/codegen harnesses; see tools/run_codegen_wasm_tests.sh).
set -uo pipefail

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

compile_failures=()
for src in "$TEST_DIR"/*.affine; do
  out="${src%.affine}.bun.js"
  echo "Compiling $(basename "$src") -> $(basename "$out")"
  if ! "${COMPILE_CMD[@]}" "$src" -o "$out" --bun-esm; then
    compile_failures+=("$(basename "$src")")
  fi
done

if [ "${#compile_failures[@]}" -gt 0 ]; then
  echo ""
  echo "::error::${#compile_failures[@]} Bun-ESM fixture(s) failed to compile"
  printf '  - %s\n' "${compile_failures[@]}"
  exit 1
fi

echo ""
echo "Running ESM harnesses (node, Bun-ESM artefacts)"
harness_total=0
harness_failures=()
for js in "$TEST_DIR"/*.harness.mjs; do
  name="$(basename "$js")"
  harness_total=$((harness_total + 1))
  echo "node $name"
  if ! (cd "$TEST_DIR" && node "$name"); then
    echo "::error file=tests/codegen-deno/$name::ESM harness failed"
    harness_failures+=("$name")
  fi
done

if [ "${#harness_failures[@]}" -gt 0 ]; then
  echo ""
  echo "::error::${#harness_failures[@]} of $harness_total Bun-ESM harness(es) failed"
  echo "Failed harnesses (full output above):"
  printf '  - %s\n' "${harness_failures[@]}"
  exit 1
fi

echo ""
echo "All codegen Bun-ESM (legacy codegen-deno corpus) tests passed ($harness_total harnesses)."
