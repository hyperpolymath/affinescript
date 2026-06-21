#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Per-backend RUNTIME bench over real, correctness-checked workloads (the
# proven-style "assert the answer, not just ran"). Fills the per-backend-runtime
# gap (TEST-NEEDS.md): each workload (LP tropical min-plus, NLP Newton) is
# compiled to core wasm (validated) and to a native executable, RUN, its verdict
# token asserted, and its native exec time measured. Skips a backend's run leg if
# its runtime tool is absent.
set -uo pipefail
cd "$(dirname "$0")/.."
REPO="$(pwd)"
GREEN=$'\033[32m'; RED=$'\033[31m'; DIM=$'\033[2m'; RST=$'\033[0m'
pass=0; fail=0
ok(){ printf '%s  PASS%s  %s\n' "$GREEN" "$RST" "$1"; pass=$((pass+1)); }
bad(){ printf '%s  FAIL%s  %s\n' "$RED" "$RST" "$1"; fail=$((fail+1)); }
BIN="$REPO/_build/default/bin/main.exe"; [ -x "$BIN" ] || dune build 2>/dev/null
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
echo "── Workload bench: LP (tropical min,+) + NLP (Newton), per backend ──"
for spec in "lp_tropical:LP_TROPICAL_OK" "nlp_newton:NLP_NEWTON_OK"; do
  w="${spec%%:*}"; tok="${spec##*:}"; src="$REPO/bench/workloads/$w.affine"
  # core wasm: compile + validate
  if "$BIN" compile "$src" -o "$tmp/$w.wasm" >/dev/null 2>&1 && wasm-tools validate "$tmp/$w.wasm" >/dev/null 2>&1; then
    ok "$w  wasm: emit+validate ($(wc -c <"$tmp/$w.wasm") B)"
  else bad "$w  wasm: emit/validate failed"; fi
  # native: compile -> clang -> run -> assert token -> time
  if "$BIN" compile "$src" -o "$tmp/$w.ll" >/dev/null 2>&1 && command -v clang >/dev/null 2>&1 && clang "$tmp/$w.ll" -o "$tmp/$w.exe" 2>/dev/null; then
    out="$("$tmp/$w.exe")"
    if [ "$out" = "$tok" ]; then
      b=999999; for i in 1 2 3 4 5; do s=$(date +%s%6N); "$tmp/$w.exe" >/dev/null; e=$(date +%s%6N); d=$((e-s)); [ $d -lt $b ]&&b=$d; done
      ok "$w  native: runs + correct ('$out')  ${b} µs"
    else bad "$w  native: wrong verdict ('$out' != '$tok')"; fi
  else bad "$w  native: compile/link failed"; fi
done
echo; printf '%s passed, %s failed\n' "$pass" "$fail"; [ "$fail" -eq 0 ]
