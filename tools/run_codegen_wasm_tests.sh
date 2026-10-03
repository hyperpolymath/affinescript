#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Compile every tests/codegen/*.affine fixture to wasm, then run every
# tests/codegen/*.mjs harness against it.
#
# Deliberately NOT `set -e`: this runner collects failures and reports all of
# them. It used to abort at the first failing harness, which meant that from
# the moment tests/codegen/test_dom_pilot_startup_error.mjs landed, every
# harness sorting after it (33 of them, up to test_while_loop.mjs) silently
# stopped executing — a single bug masked a whole verification surface. Fail
# loudly, fail late, fix in one pass.
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$ROOT_DIR/tests/codegen"

if [ -x "$ROOT_DIR/_build/default/bin/main.exe" ]; then
  COMPILER="$ROOT_DIR/_build/default/bin/main.exe"
  COMPILE_CMD=("$COMPILER" compile)
elif command -v affinescript >/dev/null 2>&1; then
  COMPILER="affinescript"
  COMPILE_CMD=("$COMPILER" compile)
else
  COMPILER="dune exec affinescript --"
  COMPILE_CMD=(dune exec affinescript -- compile)
fi

echo "Using compiler: $COMPILER"

compile_failures=()
for src in "$TEST_DIR"/*.affine; do
  base="${src%.affine}"
  wasm="$base.wasm"
  echo "Compiling $(basename "$src") -> $(basename "$wasm")"
  if ! "${COMPILE_CMD[@]}" "$src" -o "$wasm"; then
    compile_failures+=("$(basename "$src")")
  fi
done

if [ "${#compile_failures[@]}" -gt 0 ]; then
  echo ""
  echo "::error::${#compile_failures[@]} fixture(s) failed to compile: ${compile_failures[*]}"
  printf '  - %s\n' "${compile_failures[@]}"
  exit 1
fi

echo ""
echo "Running JS harnesses"
harness_total=0
harness_failures=()
for js in "$TEST_DIR"/*.mjs; do
  name="$(basename "$js")"
  harness_total=$((harness_total + 1))
  echo "node $name"
  if ! (cd "$ROOT_DIR" && node "${js#"$ROOT_DIR"/}"); then
    echo "::error file=tests/codegen/$name::JS harness failed"
    harness_failures+=("$name")
  fi
done

if [ "${#harness_failures[@]}" -gt 0 ]; then
  echo ""
  echo "::error::${#harness_failures[@]} of $harness_total JS harness(es) failed"
  echo "Failed harnesses (full output above):"
  printf '  - %s\n' "${harness_failures[@]}"
  exit 1
fi

echo ""
echo "All codegen WASM tests passed ($harness_total JS harnesses)."
