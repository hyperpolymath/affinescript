#!/usr/bin/env bash
set -euo pipefail

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

for src in "$TEST_DIR"/*.as; do
  base="${src%.as}"
  wasm="$base.wasm"
  echo "Compiling $(basename "$src") -> $(basename "$wasm")"
  "${COMPILE_CMD[@]}" "$src" -o "$wasm"
done

echo ""

echo "Running JS harnesses"
for js in "$TEST_DIR"/*.mjs; do
  echo "node $(basename "$js")"
  (cd "$ROOT_DIR" && node "${js#$ROOT_DIR/}")
done

echo "All codegen WASM tests passed."
