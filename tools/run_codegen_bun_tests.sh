#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Issue #734 — native Bun-ESM backend acceptance runner.
#
# Fail-late by design: every check runs and is reported, so one red check
# cannot hide the state of the checks behind it. Failures are echoed as
# `::error::` lines so they survive into CI annotations.
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TEST_DIR="$ROOT_DIR/tests/codegen-bun"

if [[ -x "$ROOT_DIR/_build/default/bin/main.exe" ]]; then
  COMPILE_CMD=("$ROOT_DIR/_build/default/bin/main.exe" compile)
elif command -v affinescript >/dev/null 2>&1; then
  COMPILE_CMD=(affinescript compile)
else
  COMPILE_CMD=(dune exec affinescript -- compile)
fi

if ! command -v bun >/dev/null 2>&1; then
  echo "::error::Bun is required for the Bun-ESM acceptance tests" >&2
  exit 1
fi

failures=()
fail() {
  failures+=("$1")
  echo "::error::$1" >&2
}

src="$TEST_DIR/host_profile.affine"
out="${src%.affine}.bun.js"

# 1. Compile, then scan the artefact for legacy runtime references.
compiled=1
if ! "${COMPILE_CMD[@]}" "$src" -o "$out" --bun-esm; then
  compiled=0
  fail "compile failed for $(basename "$src")"
elif grep -qi 'deno' "$out"; then
  fail "legacy runtime reference emitted in $(basename "$out")"
  grep -in 'deno' "$out" | head -4 | sed 's/^/    /' >&2
fi

if [ "$compiled" = 1 ]; then
  # 2. The emitted module must be syntactically valid Bun/JS.
  if ! bun --check "$out"; then
    fail "bun --check rejected $(basename "$out")"
  fi

  # 3. Emission must be byte-for-byte reproducible.
  second="$TEST_DIR/reproducibility.bun.js"
  if "${COMPILE_CMD[@]}" "$src" -o "$second" --bun-esm; then
    if ! cmp "$out" "$second"; then
      fail "Bun-ESM emission is not reproducible ($(basename "$out") vs $(basename "$second"))"
    fi
  else
    fail "reproducibility recompile failed for $(basename "$src")"
  fi
fi

# 4. The retired --deno-esm flag must be rejected with its explanation.
removed_log="$TEST_DIR/deno-removed.log"
if "${COMPILE_CMD[@]}" "$src" -o "$out" --deno-esm >"$removed_log" 2>&1; then
  fail "retired --deno-esm compiled successfully"
elif ! grep -q 'Deno-ESM was removed' "$removed_log"; then
  fail "retired --deno-esm rejection lacks 'Deno-ESM was removed'"
fi

# 5. The retired .deno.js output extension must be rejected as E0826.
removed_json="$TEST_DIR/deno-removed.json"
if "${COMPILE_CMD[@]}" "$src" -o "${out%.bun.js}.deno.js" --json >"$removed_json" 2>&1; then
  fail "retired .deno.js output compiled successfully"
else
  grep -q '"code":"E0826"' "$removed_json" ||
    fail "retired .deno.js rejection lacks error code E0826"
  grep -q '"success":false' "$removed_json" ||
    fail "retired .deno.js rejection lacks success:false"
fi

# 6. Native Bun harnesses.
harness_total=0
for js in "$TEST_DIR"/*.harness.mjs; do
  name="$(basename "$js")"
  harness_total=$((harness_total + 1))
  if ! (cd "$TEST_DIR" && AFFINESCRIPT_BUN_PROBE=estate bun "$name" alpha beta); then
    fail "bun harness failed: $name"
  fi
done

# 7. Unsupported host operations must not compile.
if "${COMPILE_CMD[@]}" "$TEST_DIR/unsupported_host.affine" \
    -o "$TEST_DIR/unsupported_host.bun.js" --bun-esm; then
  fail "unsupported Bun host operation compiled successfully"
fi

if [ "${#failures[@]}" -gt 0 ]; then
  echo ""
  echo "✗ ${#failures[@]} native Bun-ESM check(s) failed:"
  printf '  - %s\n' "${failures[@]}"
  exit 1
fi

echo ""
echo "All native Bun-ESM tests passed ($harness_total harnesses)."
