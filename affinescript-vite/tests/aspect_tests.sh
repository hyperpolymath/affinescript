#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$PROJECT_DIR"
PASS=0
FAIL=0
WARN=0
green() { printf '\033[32m%s\033[0m\n' "$*"; }
red()   { printf '\033[31m%s\033[0m\n' "$*"; }
yellow(){ printf '\033[33m%s\033[0m\n' "$*"; }
bold()  { printf '\033[1m%s\033[0m\n' "$*"; }
pass() { green "  PASS: "$1"$*"; PASS=$((PASS + 1)); }
fail() { red "  FAIL: "$1"$*"; FAIL=$((FAIL + 1)); }
warn() { yellow "  WARN: "$1"$*"; WARN=$((WARN + 1)); }
echo "═══════════════════════════════════════════════════════════════"
echo "  affinescript-vite — Aspect Tests (Cross-Cutting Concerns)"
echo "═══════════════════════════════════════════════════════════════"
echo "$*"
bold "Aspect 1: SPDX license headers"
MISSING_SPDX=0
while IFS= read -r -d '' f; do
    if ! head -5 "$f" | grep -q "SPDX-License-Identifier"; then
        warn "Missing SPDX header: "$f"$*"
        MISSING_SPDX=$((MISSING_SPDX + 1))
    fi
done < <(find "src/" -type f \( -name "*.rs" -o -name "*.zig" -o -name "*.res" -o -name "*.ex" -o -name "*.exs" -o -name "*.gleam" -o -name "*.idr" -o -name "*.sh" \) -print0 2>/dev/null)
if [ "$MISSING_SPDX" -eq 0 ]; then
    pass "All source files have SPDX headers"
else
    fail "$MISSING_SPDX files missing SPDX headers"
fi
bold "Aspect 2: Dangerous patterns"
DANGEROUS_IDRIS=$(grep -rn 'believe_me\|assert_total\|really_believe_me' src/abi/ 2>/dev/null | grep -v "^Binary" | grep -v "test" || true)
if [ -n "$DANGEROUS_IDRIS" ]; then
    fail "Dangerous Idris2 patterns found:"
    echo "$DANGEROUS_IDRIS" | head -5
else
    pass "No dangerous Idris2 patterns (believe_me, assert_total)"
fi
DANGEROUS_PROOF=$(grep -rn '\bAdmitted\b\|\bsorry\b\|\bunsafeCoerce\b\|\bObj\.magic\b' src/ verification/ 2>/dev/null | grep -v "test" | grep -v "comment" || true)
if [ -n "$DANGEROUS_PROOF" ]; then
    fail "Dangerous proof patterns found:"
    echo "$DANGEROUS_PROOF" | head -5
else
    pass "No dangerous proof patterns (Admitted, sorry, unsafeCoerce)"
fi
echo "$*"
echo "═══════════════════════════════════════════════════════════════"
printf "  Results: "
green "PASS="$PASS"$*" | tr -d '\n'
printf "  "
if [ "$FAIL" -gt 0 ]; then red "FAIL="$FAIL"$*" | tr -d '\n'; else printf "FAIL=0"; fi
printf "  "
if [ "$WARN" -gt 0 ]; then yellow "WARN="$WARN"$*"; else echo "WARN=0"; fi
echo "$*"
echo "═══════════════════════════════════════════════════════════════"
exit "$FAIL"
