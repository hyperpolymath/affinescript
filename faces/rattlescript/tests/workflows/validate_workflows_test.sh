#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Test: GitHub Workflows Validation
# Verifies that all workflows follow the standards

set -euo pipefail

WORKFLOWS_DIR="${1:-.github/workflows}"
ERRORS=0
WARNINGS=0

# ANSI colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_error() {
    echo -e "${RED}ERROR${NC}: $*" >&2
    ERRORS=$((ERRORS + 1))
}

log_warning() {
    echo -e "${YELLOW}WARN${NC}: $*" >&2
    WARNINGS=$((WARNINGS + 1))
}

log_pass() {
    echo -e "${GREEN}PASS${NC}: $*" >&2
}

log_info() {
    echo -e "${BLUE}INFO${NC}: $*" >&2
}

# Verify workflows directory exists
if [ ! -d "$WORKFLOWS_DIR" ]; then
    log_error "Workflows directory not found: $WORKFLOWS_DIR"
    exit 1
fi

echo ""
log_info "Validating workflows in: $WORKFLOWS_DIR"
echo ""

#==============================================================================
# TEST 1: CHECK EACH WORKFLOW FILE
#==============================================================================

WORKFLOW_COUNT=0
while IFS= read -r workflow_file; do
    [ -z "$workflow_file" ] && continue
    WORKFLOW_COUNT=$((WORKFLOW_COUNT + 1))
done < <(find "$WORKFLOWS_DIR" \( -name "*.yml" -o -name "*.yaml" \) 2>/dev/null | sort)

echo "Found $WORKFLOW_COUNT workflow file(s)"
echo ""

while IFS= read -r workflow_file; do
    [ -z "$workflow_file" ] && continue

    WORKFLOW_NAME=$(basename "$workflow_file")

    # TEST 1a: SPDX Header
    if head -10 "$workflow_file" 2>/dev/null | grep -q "SPDX-License-Identifier"; then
        log_pass "  $WORKFLOW_NAME: SPDX header present"
    else
        log_warning "  $WORKFLOW_NAME: No SPDX header"
    fi

    # TEST 1b: Has 'name' field
    if grep -q "^name:" "$workflow_file" 2>/dev/null; then
        log_pass "  $WORKFLOW_NAME: Has 'name' field"
    else
        log_error "  $WORKFLOW_NAME: Missing 'name' field"
    fi

done < <(find "$WORKFLOWS_DIR" \( -name "*.yml" -o -name "*.yaml" \) 2>/dev/null | sort)

#==============================================================================
# TEST 2: REQUIRED WORKFLOWS
#==============================================================================

echo ""
log_info "Checking for required workflows"
echo ""

REQUIRED_WORKFLOWS=(
    "hypatia-scan.yml"
    "codeql.yml"
    "scorecard.yml"
    "quality.yml"
    "mirror.yml"
    "instant-sync.yml"
    "guix-nix-policy.yml"
    "rsr-antipattern.yml"
    "security-policy.yml"
    "wellknown-enforcement.yml"
    "workflow-linter.yml"
    "npm-bun-blocker.yml"
    "ts-blocker.yml"
    "scorecard-enforcer.yml"
    "secret-scanner.yml"
)

FOUND_COUNT=0
for required in "${REQUIRED_WORKFLOWS[@]}"; do
    if [ -f "$WORKFLOWS_DIR/$required" ]; then
        log_pass "Found: $required"
        FOUND_COUNT=$((FOUND_COUNT + 1))
    else
        log_warning "Missing: $required"
        WARNINGS=$((WARNINGS + 1))
    fi
done

echo ""
echo "Found $FOUND_COUNT/${#REQUIRED_WORKFLOWS[@]} required workflows"
echo ""

#==============================================================================
# SUMMARY
#==============================================================================

echo "═══════════════════════════════════════════════════════════════════════════════"
echo "WORKFLOW VALIDATION SUMMARY"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""
echo -e "Errors:   ${RED}${ERRORS}${NC}"
echo -e "Warnings: ${YELLOW}${WARNINGS}${NC}"
echo ""

if [ "$ERRORS" -eq 0 ]; then
    echo -e "${GREEN}✓ Workflow validation PASSED${NC}"
    [ "$WARNINGS" -gt 0 ] && echo -e "  (with $WARNINGS recommendations)"
    exit 0
else
    echo -e "${RED}✗ Workflow validation FAILED${NC}"
    echo "  Please fix the errors above."
    exit 1
fi
