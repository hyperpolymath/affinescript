#!/bin/bash
set -euo pipefail
WORKFLOWS_DIR="${1:-.github/workflows}"
ERRORS=0
WARNINGS=0
log_error() { echo -e "\033[0;31mERROR\033[0m: $1"; ERRORS=$((ERRORS + 1)); }
log_warning() { echo -e "\033[1;33mWARN\033[0m: $1"; WARNINGS=$((WARNINGS + 1)); }
log_pass() { echo -e "\033[0;32mPASS\033[0m: $1"; }
log_info() { echo -e "\033[0;34mINFO\033[0m: $1"; }
if [ ! -d "$WORKFLOWS_DIR" ]; then
    log_error "Workflows directory not found: $WORKFLOWS_DIR"
    exit 1
fi
FILES=$(find "$WORKFLOWS_DIR" \( -name "*.yml" -o -name "*.yaml" \) 2>/dev/null | sort)
COUNT=$(echo "$FILES" | grep -c "." || true)
log_info "Found $COUNT workflow file(s)"
while IFS= read -r f; do
    if [ -n "$f" ]; then
        name=$(basename "$f")
        if head -10 "$f" | grep -q "SPDX-License-Identifier"; then log_pass "$name: SPDX ok"; else log_warning "$name: No SPDX"; fi
        if grep -q "^name:" "$f"; then log_pass "$name: Name ok"; else log_error "$name: No name"; fi
    fi
done <<< "$FILES"
REQUIRED=("hypatia-scan.yml" "codeql.yml" "scorecard.yml" "quality.yml" "mirror.yml" "instant-sync.yml" "guix-nix-policy.yml" "rsr-antipattern.yml" "security-policy.yml" "wellknown-enforcement.yml" "workflow-linter.yml" "npm-bun-blocker.yml" "ts-blocker.yml" "scorecard-enforcer.yml" "secret-scanner.yml")
FOUND=0
for r in "${REQUIRED[@]}"; do
    if [ -f "$WORKFLOWS_DIR/$r" ]; then log_pass "Found: $r"; FOUND=$((FOUND + 1)); else log_warning "Missing: $r"; fi
done
echo "Found $FOUND/${#REQUIRED[@]} required"
exit "$ERRORS"
