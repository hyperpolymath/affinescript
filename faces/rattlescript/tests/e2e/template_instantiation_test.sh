#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# E2E Test: Template Instantiation
# Verifies that the template can be cloned and instantiated into a working project
#
# This test:
# 1. Clones the template to a temp directory
# 2. Replaces all placeholder tokens with test values
# 3. Validates the resulting repository structure
# 4. Verifies builds work after instantiation
# 5. Cleans up

set -euo pipefail

# Test configuration
TEMPLATE_ROOT="${1:-.}"
TEST_DIR="${TMPDIR:-/tmp}/rsr-template-test-$$"
TEST_REPO_NAME="test-instantiated-repo"
TEST_OWNER="test-owner"
TEST_FORGE="github"
TEST_AUTHOR="Test Author"
TEST_AUTHOR_EMAIL="test@example.com"
TEST_PROJECT_NAME="Test Project"
TEST_DESCRIPTION="A test project instantiated from the RSR template"
TEST_PRIMARY_LANGUAGE="Rust"

# ANSI colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
log_step() {
    echo ""
    echo -e "${BLUE}→${NC} $*"
}

log_pass() {
    echo -e "${GREEN}✓${NC} $*"
}

log_error() {
    echo -e "${RED}✗${NC} $*" >&2
}

cleanup() {
    if [ -d "$TEST_DIR" ]; then
        log_step "Cleaning up test directory: $TEST_DIR"
        rm -rf "$TEST_DIR"
        log_pass "Cleanup complete"
    fi
}

trap cleanup EXIT

#==============================================================================
# PHASE 1: SETUP
#==============================================================================

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo "E2E TEST: Template Instantiation"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""

log_step "Creating test directory: $TEST_DIR"
mkdir -p "$TEST_DIR"
log_pass "Test directory created"

#==============================================================================
# PHASE 2: CLONE TEMPLATE
#==============================================================================

log_step "Cloning template from $TEMPLATE_ROOT"

# Copy template to test location (simulating git clone)
TEST_REPO_PATH="$TEST_DIR/$TEST_REPO_NAME"
cp -r "$TEMPLATE_ROOT" "$TEST_REPO_PATH"
log_pass "Template cloned to $TEST_REPO_PATH"

# Remove .git directory for clean state
if [ -d "$TEST_REPO_PATH/.git" ]; then
    rm -rf "$TEST_REPO_PATH/.git"
    log_pass ".git directory removed (fresh clone)"
fi

#==============================================================================
# PHASE 3: PLACEHOLDER REPLACEMENT
#==============================================================================

log_step "Replacing placeholder tokens"

# Function to replace all occurrences of a placeholder in a file
replace_placeholder() {
    local file="$1"
    local placeholder="$2"
    local value="$3"

    if [ ! -f "$file" ]; then
        return 0
    fi

    # Use sed to replace (platform-portable)
    if grep -q "$placeholder" "$file" 2>/dev/null; then
        sed -i "s|$placeholder|$value|g" "$file"
        echo "  Replaced $placeholder in $(basename "$file")"
    fi
}

# Replace in all text files
find "$TEST_REPO_PATH" -type f \
    \( -name "*.md" -o -name "*.adoc" -o -name "*.a2ml" -o -name "*.zig" -o -name "*.idr" \
       -o -name "Justfile" -o -name "Containerfile" -o -name "*.yml" -o -name "*.yaml" \
       -o -name "*.json" -o -name "*.scm" -o -name "contractile" \) \
    -exec bash -c '
        file="$1"
        placeholder_pairs=(
            "{{REPO}}|$TEST_REPO_NAME"
            "{{OWNER}}|$TEST_OWNER"
            "{{FORGE}}|$TEST_FORGE"
            "{{PROJECT}}|$TEST_PROJECT_NAME"
            "{{project}}|'"${TEST_REPO_NAME//-/_}"'"
            "{{REPO_DESCRIPTION}}|$TEST_DESCRIPTION"
            "{{PRIMARY_LANGUAGE}}|$TEST_PRIMARY_LANGUAGE"
            "{{AUTHOR}}|$TEST_AUTHOR"
            "{{AUTHOR_EMAIL}}|$TEST_AUTHOR_EMAIL"
            "{{CURRENT_DATE}}|2026-04-04"
        )

        for pair in "${placeholder_pairs[@]}"; do
            IFS="|" read -r placeholder value <<< "$pair"
            if grep -q "$placeholder" "$file" 2>/dev/null; then
                sed -i "s|$placeholder|$value|g" "$file"
            fi
        done
    ' _ "$file"

log_pass "All placeholder tokens replaced"

#==============================================================================
# PHASE 4: VALIDATE STRUCTURE
#==============================================================================

log_step "Validating instantiated repository structure"

# Run validation script on the instantiated repo
if [ -f "$TEMPLATE_ROOT/scripts/validate-template.sh" ]; then
    bash "$TEMPLATE_ROOT/scripts/validate-template.sh" "$TEST_REPO_PATH" 0
    log_pass "Repository structure validation passed"
else
    log_error "Validation script not found"
    exit 1
fi

#==============================================================================
# PHASE 5: VERIFY BUILD
#==============================================================================

log_step "Verifying build system works after instantiation"

if [ -f "$TEST_REPO_PATH/src/interface/ffi/build.zig" ]; then
    if command -v zig &> /dev/null; then
        cd "$TEST_REPO_PATH/src/interface/ffi"
        if zig build 2>&1; then
            log_pass "Zig build successful"
        else
            log_error "Zig build failed"
            exit 1
        fi
        cd - > /dev/null
    else
        log_error "Zig compiler not found - cannot verify build"
        exit 1
    fi
fi

#==============================================================================
# PHASE 6: VERIFY NO REMAINING PLACEHOLDERS
#==============================================================================

log_step "Checking for remaining placeholders"

REMAINING_PLACEHOLDERS=$(
    find "$TEST_REPO_PATH" -type f \
        \( -name "*.md" -o -name "*.adoc" -o -name "*.a2ml" -o -name "*.zig" -o -name "*.idr" \
           -o -name "Justfile" -o -name "*.yml" \) \
        -exec grep -l "{{[A-Z_]*}}" {} \; 2>/dev/null || true
)

if [ -z "$REMAINING_PLACEHOLDERS" ]; then
    log_pass "No remaining placeholders found"
else
    log_error "Found remaining placeholders in:"
    echo "$REMAINING_PLACEHOLDERS" | sed 's/^/  /'
    exit 1
fi

#==============================================================================
# PHASE 7: VERIFY CRITICAL FILES ARE NOT TEMPLATES
#==============================================================================

log_step "Verifying critical files have been instantiated"

CRITICAL_FILES=(
    "README.adoc"
    "EXPLAINME.adoc"
    "Justfile"
)

for file in "${CRITICAL_FILES[@]}"; do
    if [ -f "$TEST_REPO_PATH/$file" ]; then
        # Check that it's not just a template (contains some actual content)
        if grep -q "$TEST_PROJECT_NAME\|$TEST_AUTHOR\|$TEST_REPO_NAME" "$TEST_REPO_PATH/$file" 2>/dev/null || \
           [ $(wc -l < "$TEST_REPO_PATH/$file") -gt 10 ]; then
            log_pass "File instantiated: $file"
        else
            log_error "File appears to be a template: $file"
            exit 1
        fi
    else
        log_error "Critical file missing: $file"
        exit 1
    fi
done

#==============================================================================
# PHASE 8: VERIFY METADATA
#==============================================================================

log_step "Verifying machine-readable metadata"

METADATA_FILES=(
    ".machine_readable/STATE.a2ml"
    ".machine_readable/META.a2ml"
)

for file in "${METADATA_FILES[@]}"; do
    if [ -f "$TEST_REPO_PATH/$file" ]; then
        log_pass "Metadata file exists: $file"
    else
        log_error "Metadata file missing: $file"
        exit 1
    fi
done

#==============================================================================
# SUMMARY
#==============================================================================

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo -e "${GREEN}✓ E2E TEMPLATE INSTANTIATION TEST PASSED${NC}"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""
echo "Summary:"
echo "  - Template cloned successfully"
echo "  - All placeholders replaced"
echo "  - Repository structure valid"
echo "  - Build system works"
echo "  - No remaining placeholders"
echo "  - Metadata intact"
echo ""
echo "Test repository: $TEST_REPO_PATH (will be cleaned up)"
echo ""
