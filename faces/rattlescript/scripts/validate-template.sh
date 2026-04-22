#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# RSR Template Validation Script
# Verifies that a repository follows the RSR template structure and contains all required files
#
# Exit codes:
#   0 = validation passed
#   1 = validation failed with errors
#   2 = validation failed with warnings (but can proceed)

set -euo pipefail

REPO_ROOT="${1:-.}"
VERBOSE="${2:-0}"
ERRORS=0
WARNINGS=0

# ANSI colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_error() {
    echo -e "${RED}ERROR${NC}: $*" >&2
    ERRORS=$((ERRORS + 1))
}

log_warning() {
    echo -e "${YELLOW}WARN${NC}: $*" >&2
    WARNINGS=$((WARNINGS + 1))
}

log_info() {
    echo -e "${BLUE}INFO${NC}: $*" >&2
}

log_pass() {
    echo -e "${GREEN}PASS${NC}: $*" >&2
}

check_file_exists() {
    local file="$1"
    local description="${2:-}"
    if [ -f "$REPO_ROOT/$file" ]; then
        [ "$VERBOSE" = "1" ] && log_pass "File exists: $file"
        return 0
    else
        log_error "Required file missing: $file ${description:+(${description})}"
        return 1
    fi
}

check_dir_exists() {
    local dir="$1"
    local description="${2:-}"
    if [ -d "$REPO_ROOT/$dir" ]; then
        [ "$VERBOSE" = "1" ] && log_pass "Directory exists: $dir"
        return 0
    else
        log_error "Required directory missing: $dir ${description:+(${description})}"
        return 1
    fi
}

has_spdx_header() {
    local file="$1"
    if head -10 "$file" | grep -q "SPDX-License-Identifier"; then
        return 0
    fi
    return 1
}

has_placeholder() {
    local file="$1"
    if grep -q "{{REPO\|{{OWNER\|{{FORGE\|{{PROJECT\|{{project\|{{AUTHOR" "$file" 2>/dev/null; then
        return 0
    fi
    return 1
}

#==============================================================================
# VALIDATION PHASE 1: CORE STRUCTURE
#==============================================================================

echo ""
log_info "Phase 1: Core repository structure"
echo ""

# Root files
check_file_exists "0-AI-MANIFEST.a2ml" "AI manifest (universal entry point)"
check_file_exists "README.adoc" "High-level pitch"
check_file_exists "EXPLAINME.adoc" "Developer deep-dive"
check_file_exists "LICENSE" "License file"
check_file_exists "Justfile" "Task runner"
check_file_exists "AUDIT.adoc" "Release audit gate"

# Directories
check_dir_exists ".machine_readable" "Machine-readable metadata"
check_dir_exists ".github" "GitHub community metadata"
check_dir_exists "src/interface/abi" "Idris2 ABI definitions"
check_dir_exists "src/interface/ffi" "Zig FFI implementation"
check_dir_exists "src/interface/generated/abi" "Generated C headers"
check_dir_exists "docs" "Documentation"

#==============================================================================
# VALIDATION PHASE 2: MACHINE-READABLE METADATA
#==============================================================================

echo ""
log_info "Phase 2: Machine-readable metadata (.machine_readable/)"
echo ""

check_file_exists ".machine_readable/STATE.a2ml" "Project state"
check_file_exists ".machine_readable/META.a2ml" "Architecture decisions"
check_file_exists ".machine_readable/ECOSYSTEM.a2ml" "Ecosystem position"
check_file_exists ".machine_readable/anchors/ANCHOR.a2ml" "Semantic boundary anchor"
check_file_exists ".machine_readable/policies/MAINTENANCE-AXES.a2ml" "Maintenance axes"

#==============================================================================
# VALIDATION PHASE 3: REQUIRED WORKFLOWS (17 minimum)
#==============================================================================

echo ""
log_info "Phase 3: GitHub Actions workflows"
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

# Check required workflows
for workflow in "${REQUIRED_WORKFLOWS[@]}"; do
    if [ -f "$REPO_ROOT/.github/workflows/$workflow" ]; then
        [ "$VERBOSE" = "1" ] && log_pass "Workflow found: $workflow"
    else
        log_error "Required workflow missing: $workflow"
    fi
done

# Verify all workflows have SPDX headers and proper structure
WORKFLOW_FILES=$(find "$REPO_ROOT/.github/workflows" -name "*.yml" -type f 2>/dev/null || true)
WORKFLOW_COUNT=$(echo "$WORKFLOW_FILES" | grep -c "." || true)

if [ "$WORKFLOW_COUNT" -ge 15 ]; then
    log_pass "Found $WORKFLOW_COUNT workflows (>= 15 expected)"
else
    log_warning "Found only $WORKFLOW_COUNT workflows (expected >= 15)"
fi

# Spot-check workflow files for issues
while IFS= read -r workflow_file; do
    if [ -z "$workflow_file" ]; then continue; fi

    # Check for SPDX header (optional in YAML workflows, but best practice)
    if ! head -5 "$workflow_file" | grep -q "SPDX-License-Identifier"; then
        log_warning "Workflow missing SPDX header: $(basename "$workflow_file")"
    fi

    # Check for proper YAML structure
    if ! grep -q "^name:" "$workflow_file"; then
        log_error "Workflow missing 'name' field: $(basename "$workflow_file")"
    fi
done <<< "$WORKFLOW_FILES"

#==============================================================================
# VALIDATION PHASE 4: ABI/FFI SOURCE FILES
#==============================================================================

echo ""
log_info "Phase 4: Idris2 ABI and Zig FFI source files"
echo ""

# Idris2 ABI files
check_file_exists "src/interface/abi/Types.idr" "Core type definitions"
check_file_exists "src/interface/abi/Layout.idr" "Memory layout specifications"
check_file_exists "src/interface/abi/Foreign.idr" "FFI foreign declarations"

# Zig FFI files
check_file_exists "src/interface/ffi/build.zig" "Zig build configuration"
check_file_exists "src/interface/ffi/src/main.zig" "Zig implementation"
check_file_exists "src/interface/ffi/test/integration_test.zig" "Integration tests"

#==============================================================================
# VALIDATION PHASE 5: PLACEHOLDER TOKENS
#==============================================================================

echo ""
log_info "Phase 5: Placeholder token replacement (skipped in template repo)"
echo ""

# Note: Template repo is allowed to have placeholders
# For derived repos, we'd check that placeholders are replaced
if [ "$(basename "$REPO_ROOT")" = "rsr-template-repo" ]; then
    log_pass "Skipping placeholder check for template repo"
else
    # Check that key files don't have unresolved placeholders
    for file in "$REPO_ROOT/README.adoc" "$REPO_ROOT/Justfile" "$REPO_ROOT/.machine_readable/STATE.a2ml"; do
        if [ -f "$file" ]; then
            if has_placeholder "$file"; then
                log_warning "File contains unresolved placeholders: $(basename "$file")"
            fi
        fi
    done
fi

#==============================================================================
# VALIDATION PHASE 6: SPDX LICENSE HEADERS
#==============================================================================

echo ""
log_info "Phase 6: SPDX License Headers"
echo ""

# Check source files for SPDX headers (excluding build artifacts)
SOURCE_FILES=$(find "$REPO_ROOT/src" -type f \( -name "*.idr" -o -name "*.zig" \) \
              ! -path "*/.zig-cache/*" ! -path "*/zig-cache/*" 2>/dev/null || true)
SOURCE_COUNT=$(echo "$SOURCE_FILES" | grep -c "." || true)
SPDX_COUNT=0

while IFS= read -r src_file; do
    if [ -z "$src_file" ]; then continue; fi
    if has_spdx_header "$src_file"; then
        SPDX_COUNT=$((SPDX_COUNT + 1))
    else
        log_warning "Source file missing SPDX header: $(basename "$src_file")"
    fi
done <<< "$SOURCE_FILES"

if [ "$SOURCE_COUNT" -gt 0 ]; then
    PERCENT=$((SPDX_COUNT * 100 / SOURCE_COUNT))
    log_pass "SPDX headers: $SPDX_COUNT/$SOURCE_COUNT ($PERCENT%)"
    if [ "$PERCENT" -lt 100 ]; then
        log_warning "Not all source files have SPDX headers"
    fi
fi

#==============================================================================
# VALIDATION PHASE 7: BUILD VERIFICATION
#==============================================================================

echo ""
log_info "Phase 7: Build system verification"
echo ""

# Check Zig build
if [ -f "$REPO_ROOT/src/interface/ffi/build.zig" ]; then
    if command -v zig &> /dev/null; then
        cd "$REPO_ROOT/src/interface/ffi"
        if zig build 2>&1 | grep -q "error"; then
            log_error "Zig build failed"
        else
            log_pass "Zig build successful"
        fi
        cd - > /dev/null
    else
        log_warning "Zig compiler not found - skipping Zig build check"
    fi
else
    log_error "Zig build.zig not found"
fi

# Check Idris2 syntax (if available)
if command -v idris2 &> /dev/null; then
    IDS_FILES=$(find "$REPO_ROOT/src/interface/abi" -name "*.idr" -type f 2>/dev/null || true)
    while IFS= read -r ids_file; do
        if [ -z "$ids_file" ]; then continue; fi
        if ! idris2 --check "$ids_file" 2>&1 | grep -q "Error"; then
            log_pass "Idris2 syntax OK: $(basename "$ids_file")"
        else
            log_warning "Idris2 syntax issue: $(basename "$ids_file")"
        fi
    done <<< "$IDS_FILES"
else
    log_warning "Idris2 compiler not found - skipping Idris2 syntax checks"
fi

#==============================================================================
# VALIDATION PHASE 8: DOCUMENTATION
#==============================================================================

echo ""
log_info "Phase 8: Documentation requirements"
echo ""

check_file_exists "docs/developer/ABI-FFI-README.adoc" "ABI/FFI documentation"
check_file_exists "TOPOLOGY.md" "Architecture topology" || check_file_exists "docs/architecture/TOPOLOGY.md" "Architecture topology (in docs)"
check_file_exists "CONTRIBUTING.md" "Contribution guide"

# Governance can be at root or in docs/governance/
if [ -f "$REPO_ROOT/GOVERNANCE.adoc" ] || [ -f "$REPO_ROOT/GOVERNANCE.md" ] || [ -d "$REPO_ROOT/docs/governance" ]; then
    [ "$VERBOSE" = "1" ] && log_pass "Governance files found"
else
    log_warning "Governance documentation not found"
fi

#==============================================================================
# VALIDATION SUMMARY
#==============================================================================

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo "VALIDATION SUMMARY"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""
echo -e "Errors:   ${RED}${ERRORS}${NC}"
echo -e "Warnings: ${YELLOW}${WARNINGS}${NC}"
echo ""

if [ "$ERRORS" -eq 0 ]; then
    echo -e "${GREEN}✓ Validation PASSED${NC}"
    [ "$WARNINGS" -gt 0 ] && echo -e "  (with $WARNINGS warnings)"
    exit 0
else
    echo -e "${RED}✗ Validation FAILED${NC}"
    echo "  Please fix the errors above."
    exit 1
fi
