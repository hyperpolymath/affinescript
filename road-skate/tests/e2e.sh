#!/usr/bin/env bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PASS=0
FAIL=0
SKIP=0
green() { printf '\033[32m%s\033[0m\n' "$*"; }
red()   { printf '\033[31m%s\033[0m\n' "$*"; }
yellow(){ printf '\033[33m%s\033[0m\n' "$*"; }
bold()  { printf '\033[1m%s\033[0m\n' "$*"; }
check() {
    local name="$1" expected="$2" actual="$3"
    if printf "%s" "$actual" | grep -q "$expected"; then
        green "  PASS: $name"
        PASS=$((PASS + 1))
    else
        red "  FAIL: $name (expected '$expected', got '${actual:0:120}')"
        FAIL=$((FAIL + 1))
    fi
}
check_status() {
    local name="$1" expected="$2" actual="$3"
    if [ "$actual" = "$expected" ]; then
        green "  PASS: $name (HTTP $actual)"
        PASS=$((PASS + 1))
    else
        red "  FAIL: $name (expected HTTP $expected, got HTTP $actual)"
        FAIL=$((FAIL + 1))
    fi
}
skip_test() {
    yellow "  SKIP: "$1" ($2)"
    SKIP=$((SKIP + 1))
}
echo "═══════════════════════════════════════════════════════════════"
echo "  affinescript-vite — End-to-End Tests"
echo "═══════════════════════════════════════════════════════════════"
echo ""
bold "Preflight checks"
echo ""
echo ""
echo "═══════════════════════════════════════════════════════════════"
printf "  Results: "
green "PASS="$PASS"" | tr -d '\n'
printf "  "
if [ "$FAIL" -gt 0 ]; then red "FAIL="$FAIL"" | tr -d '\n'; else printf "FAIL=0"; fi
printf "  "
if [ "$SKIP" -gt 0 ]; then yellow "SKIP="$SKIP""; else echo "SKIP=0"; fi
echo "═══════════════════════════════════════════════════════════════"
exit "$FAIL"
