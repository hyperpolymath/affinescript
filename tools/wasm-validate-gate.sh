#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# WASM validation gate for the AffineScript repository.
#
# Closes the test-suite blind spot documented at test/test_e2e.ml:601 — the
# existing "WASM code generation" tests assert only that codegen *did not
# raise*, NOT that the emitted module is *valid WebAssembly*. A backend can
# therefore emit structurally-invalid bytes (wrong value type, truncated
# cell) and the suite stays green. This gate compiles a curated positive
# corpus to core wasm and runs `wasm-tools validate` on every module, so any
# silent-invalid emission is a hard failure.
#
# Outcome classes per source file:
#   PASS  compiled + `wasm-tools validate` OK
#   FAIL  compiled but INVALID wasm  (the hazard this gate exists to catch)
#   FAIL  failed to compile and is NOT on the documented allowlist
#   SKIP  failed to compile AND is allowlisted below (principled carve-out)
#
# The allowlist is deliberately explicit and reason-stamped — same spirit as
# the doc-truthing over-claim ledger and the parser-conflict disclosure.
# Adding an entry is a policy act, not a silent mask.
#
# Run from anywhere; it cd's to the repo root. Skips (exit-neutral) if
# `wasm-tools` is not on PATH.

set -uo pipefail
cd "$(dirname "$0")/.."
REPO="$(pwd)"

GREEN=$'\033[32m'; RED=$'\033[31m'; YEL=$'\033[33m'; DIM=$'\033[2m'; RST=$'\033[0m'
pass=0; fail=0; skip=0
ok ()   { printf '%s  PASS%s  %s\n' "$GREEN" "$RST" "$1"; pass=$((pass+1)); }
bad ()  { printf '%s  FAIL%s  %s\n' "$RED"   "$RST" "$1"; fail=$((fail+1)); }
note () { printf '%s  SKIP%s  %s\n' "$DIM"   "$RST" "$1"; skip=$((skip+1)); }

command -v wasm-tools >/dev/null 2>&1 || {
  printf '%s  SKIP%s  wasm-tools not on PATH (install: cargo install wasm-tools)\n' "$DIM" "$RST"
  exit 0; }

# ── Documented allowlist: source -> reason it does not produce valid core wasm.
# These are principled carve-outs, not the silent-invalid class. A file here is
# expected to FAIL TO COMPILE (a loud, honest reject) or to be a non-unit
# fixture — it must never be one that compiles to *invalid* bytes.
declare -A ALLOW=(
  ["examples/exception_test.affine"]="try/catch needs the wasm exception-handling proposal; backend loud-fails -> use -i / -julia (#555 family)"
  ["examples/typecheck_features_test.affine"]="references unbound 'show' — stdlib/test-fixture gap, not a backend hole"
  ["conformance/valid/006_type_alias.affine"]="stale conformance fixture: Type error at HEAD (tracked)"
  ["conformance/valid/007_struct_def.affine"]="stale conformance fixture: Parse error at HEAD (tracked)"
  ["conformance/valid/012_ownership.affine"]="stale conformance fixture: own-record Parse error at HEAD (tracked)"
)

BIN="$REPO/_build/default/bin/main.exe"
if [ ! -x "$BIN" ]; then
  echo "── building compiler (dune build) ──"
  dune build 2>/dev/null || { echo "dune build failed"; exit 1; }
fi

echo "── WASM validate gate: core backend over positive corpus ──"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
corpus=$( { ls "$REPO"/examples/*.affine 2>/dev/null; ls "$REPO"/conformance/valid/*.affine 2>/dev/null; } | sort )

for f in $corpus; do
  rel="${f#$REPO/}"
  out="$tmp/$(echo "$rel" | tr '/' '_').wasm"
  if "$BIN" compile "$f" -o "$out" >"$tmp/.log" 2>&1; then
    if wasm-tools validate "$out" >"$tmp/.v" 2>&1; then
      ok "$rel"
    else
      bad "$rel — emitted INVALID wasm: $(head -1 "$tmp/.v")"
    fi
  else
    reason="${ALLOW[$rel]:-}"
    if [ -n "$reason" ]; then
      note "$rel — $reason"
    else
      bad "$rel — unexpected compile failure (not allowlisted): $(tail -1 "$tmp/.log")"
    fi
  fi
done

# Float-through-heap must LOUD-FAIL, not emit (issue-draft 05 / task #8). These
# were silent-invalid/corrupt before the guard landed; pin the honest reject so
# it cannot silently regress (the corpus itself has no Float aggregates).
echo "── Float-through-heap loud-fail (issue-draft 05) ──"
rej () {  # <label> <src>
  printf '%s\n' "$2" > "$tmp/rej.affine"
  if "$BIN" compile "$tmp/rej.affine" -o "$tmp/rej.wasm" >"$tmp/.r" 2>&1; then
    bad "$1 — emitted instead of loud-failing (Float-heap guard regressed!)"
  elif grep -q 'UnsupportedFeature' "$tmp/.r"; then
    ok "$1 — loud-fails (UnsupportedFeature)"
  else
    bad "$1 — failed, but not via the Float-heap guard: $(tail -1 "$tmp/.r")"
  fi
}
rej "Array[Float] param"  'fn rd(i: Int, a: Array[Float]) -> Float { a[i] }'
rej "Float tuple literal" 'fn f() -> Float { let t: (Float, Float) = (1.0, 2.0); t.0 }'

echo
printf '%s passed, %s failed, %s skipped (allowlisted carve-outs)\n' "$pass" "$fail" "$skip"
[ "$fail" -eq 0 ]
