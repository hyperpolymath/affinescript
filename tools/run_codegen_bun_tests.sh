#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Issue #734 — native Bun-ESM backend acceptance runner.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$ROOT_DIR/tests/codegen-bun"

if [[ -x "$ROOT_DIR/_build/default/bin/main.exe" ]]; then
  COMPILE_CMD=("$ROOT_DIR/_build/default/bin/main.exe" compile)
elif command -v affinescript >/dev/null 2>&1; then
  COMPILE_CMD=(affinescript compile)
else
  COMPILE_CMD=(dune exec affinescript -- compile)
fi

command -v bun >/dev/null 2>&1 || {
  echo "error: Bun is required for the Bun-ESM acceptance tests" >&2
  exit 1
}

src="$TEST_DIR/host_profile.affine"
out="${src%.affine}.bun.js"
"${COMPILE_CMD[@]}" "$src" -o "$out" --bun-esm
if grep -qi 'deno' "$out"; then
  echo "error: legacy runtime reference emitted in $(basename "$out")" >&2
  exit 1
fi
bun --check "$out"
second="$TEST_DIR/reproducibility.bun.js"
"${COMPILE_CMD[@]}" "$src" -o "$second" --bun-esm
cmp "$out" "$second"

removed_log="$TEST_DIR/deno-removed.log"
if "${COMPILE_CMD[@]}" "$src" -o "$out" --deno-esm \
    >"$removed_log" 2>&1; then
  echo "error: retired --deno-esm compiled successfully" >&2
  exit 1
fi
grep -q 'Deno-ESM was removed' "$removed_log"

removed_json="$TEST_DIR/deno-removed.json"
if "${COMPILE_CMD[@]}" "$src" -o "${out%.bun.js}.deno.js" --json \
    >"$removed_json" 2>&1; then
  echo "error: retired .deno.js output compiled successfully" >&2
  exit 1
fi
grep -q '"code":"E0826"' "$removed_json"
grep -q '"success":false' "$removed_json"

for js in "$TEST_DIR"/*.harness.mjs; do
  (cd "$TEST_DIR" && AFFINESCRIPT_BUN_PROBE=estate bun "$(basename "$js")" alpha beta)
done

if "${COMPILE_CMD[@]}" "$TEST_DIR/unsupported_host.affine" \
    -o "$TEST_DIR/unsupported_host.bun.js" --bun-esm; then
  echo "error: unsupported Bun host operation compiled successfully" >&2
  exit 1
fi

echo "All native Bun-ESM tests passed."
