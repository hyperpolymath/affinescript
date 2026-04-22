#!/bin/bash
set -euo pipefail
REPO_ROOT="${1:-.}"
ERRORS=0
log_error() { echo "ERROR: $1"; ERRORS=$((ERRORS + 1)); }
log_pass() { echo "PASS: $1"; }
check_file() { if [ -f "$REPO_ROOT/$1" ]; then log_pass "$1"; else log_error "$1 missing"; fi; }
check_dir() { if [ -d "$REPO_ROOT/$1" ]; then log_pass "$1"; else log_error "$1 missing"; fi; }
echo "--- Phase 1: Structure ---"
for f in .machine_readable .github src/interface/abi src/interface/ffi docs; do check_dir "$f"; done
for f in Justfile README.adoc LICENSE 0-AI-MANIFEST.a2ml; do check_file "$f"; done
echo "--- Phase 2: Metadata ---"
for f in STATE.a2ml META.a2ml ECOSYSTEM.a2ml anchors/ANCHOR.a2ml policies/MAINTENANCE-AXES.a2ml; do check_file ".machine_readable/$f"; done
echo "--- Phase 3: Workflows ---"
COUNT=$(find "$REPO_ROOT/.github/workflows" -name "*.yml" | wc -l)
if [ "$COUNT" -ge 15 ]; then log_pass "Workflows: $COUNT"; else log_error "Workflows: $COUNT < 15"; fi
echo "--- Results ---"
echo "Errors: $ERRORS"
exit "$ERRORS"
