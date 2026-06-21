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

# Float-in-HEAP: arrays are now type-directed (8-byte f64 cells via the Float
# heap wall — ExprFloatArray/ExprFloatIndex, issue-draft 05 durable fix); they
# must VALIDATE and round-trip the f64 value. Tuples/records remain unhandled
# and must still LOUD-FAIL (no silent corruption). Pin both so neither regresses.
echo "── Float-in-heap: Array[Float] type-directed layout (issue-draft 05) ──"
val () {  # <label> <src>   — must compile to VALID wasm
  printf '%s\n' "$2" > "$tmp/val.affine"
  if "$BIN" compile "$tmp/val.affine" -o "$tmp/val.wasm" >"$tmp/.r" 2>&1; then
    if wasm-tools validate "$tmp/val.wasm" >"$tmp/.v" 2>&1; then
      ok "$1 — validates ($(wc -c <"$tmp/val.wasm" | tr -d ' ') B)"
    else
      bad "$1 — emitted INVALID wasm: $(head -1 "$tmp/.v")"
    fi
  else
    bad "$1 — failed to compile (Float-array layout regressed?): $(tail -1 "$tmp/.r")"
  fi
}
run () {  # <label> <src> <token>   — must execute on wasmtime and print <token>
  command -v wasmtime >/dev/null 2>&1 || { note "$1 — wasmtime not on PATH (value round-trip skipped)"; return; }
  printf '%s\n' "$2" > "$tmp/run.affine"
  if "$BIN" compile "$tmp/run.affine" -o "$tmp/run.wasm" >/dev/null 2>&1 \
     && wasmtime "$tmp/run.wasm" 2>/dev/null | grep -q "$3"; then
    ok "$1 — f64 round-trips through heap ('$3')"
  else
    bad "$1 — f64 heap round-trip wrong (expected '$3')"
  fi
}
val "Array[Float] param read"  'fn rd(i: Int, a: Array[Float]) -> Float { a[i] }'
val "Array[Float] construct"   'fn main() -> Int { let a: Array[Float] = [1.0, 2.5]; 0 }'
val "Array[Float] write"       'fn k(i: Int, mut o: Array[Float], a: Array[Float]) -> Unit { o[i] = a[i]; }'
val "all-Float tuple t.i"      'fn proj() -> Float { let t: (Float, Float) = (1.0, 2.0); t.0 }'
run "Array[Float] read round-trip" \
  'fn main() -> Int { let a: Array[Float] = [1.0, 2.5, 4.0]; if a[1] > 2.0 { println("FARR_OK"); 0 } else { println("FARR_BAD"); 1 } }' \
  'FARR_OK'
run "Array[Float] write round-trip" \
  'fn main() -> Int { let s: Array[Float] = [3.5, 7.25]; let mut d: Array[Float] = [0.0, 0.0]; d[1] = s[0]; if d[1] > 3.0 { println("WRITE_OK"); 0 } else { println("WRITE_BAD"); 1 } }' \
  'WRITE_OK'
val "nested Array[Array[Float]]" 'fn h(a: Array[Array[Float]]) -> Float { a[0][1] }'
run "all-Float tuple round-trip" \
  'fn main() -> Int { let t: (Float, Float) = (1.5, 9.5); if t.1 > t.0 { println("FTUP_OK"); 0 } else { println("FTUP_BAD"); 1 } }' \
  'FTUP_OK'
run "Array of all-Float tuples round-trip" \
  'fn main() -> Int { let a: Array[(Float, Float)] = [(1.0, 2.0), (3.0, 4.0)]; let p: (Float, Float) = a[1]; if p.0 > 2.5 { println("AFT_OK"); 0 } else { println("AFT_BAD"); 1 } }' \
  'AFT_OK'
val "mixed (Int, Float) tuple"  'fn f() -> Float { let t: (Int, Float) = (1, 2.0); t.1 }'
run "mixed (Int, Float) round-trip" \
  'fn main() -> Int { let t: (Int, Float) = (7, 3.5); if t.1 > 3.0 { println("MIX_OK"); t.0 } else { println("MIX_BAD"); 1 } }' \
  'MIX_OK'
run "mixed (Float, Int) round-trip" \
  'fn main() -> Int { let t: (Float, Int) = (2.5, 9); if t.0 > 2.0 { println("FI_OK"); t.1 } else { println("FI_BAD"); 1 } }' \
  'FI_OK'
val "closed Float record r.f"   'fn f(r: {x: Float, y: Float}) -> Float { r.x }'
run "Float record round-trip (literal order != name order)" \
  'fn main() -> Int { let r = #{ b: 2.0, a: 1.0 }; if r.a < r.b { println("REC_ORDER_OK"); 0 } else { println("REC_ORDER_BAD"); 1 } }' \
  'REC_ORDER_OK'
run "mixed Float/Int record round-trip" \
  'fn main() -> Int { let r = #{ value: 3.5, count: 7 }; if r.value > 3.0 { println("REC_MIX_OK"); r.count } else { println("REC_MIX_BAD"); 1 } }' \
  'REC_MIX_OK'

echo "── Float-in-heap still-unhandled shapes must LOUD-FAIL (issue-draft 05) ──"
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
rej "Float array compound-assign" 'fn k(i: Int, mut a: Array[Float]) -> Unit { a[i] += 1.0; }'
rej "closure capturing a Float"   'fn main() -> Int { let s: Float = 2.5; let f = fn(x: Float) => x * s; if f(2.0) > 4.0 { 0 } else { 1 } }'

echo
printf '%s passed, %s failed, %s skipped (allowlisted carve-outs)\n' "$pass" "$fail" "$skip"
[ "$fail" -eq 0 ]
