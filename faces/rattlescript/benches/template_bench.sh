#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Template Benchmarks
# Measures performance characteristics of template validation and build system

set -euo pipefail

REPO_ROOT="${1:-.}"
OUTPUT_FORMAT="${2:-human}"  # human | json | csv

# ANSI colors
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}→${NC} $*"
}

log_pass() {
    echo -e "${GREEN}✓${NC} $*"
}

# Ensure we have required commands
command -v /usr/bin/time >/dev/null 2>&1 || {
    echo "Warning: /usr/bin/time not available, using built-in time"
    TIME_CMD="time"
}

TIME_CMD="/usr/bin/time -f %e" 2>/dev/null || TIME_CMD="time"

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo "Template Benchmarks"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""

declare -A results

#==============================================================================
# BENCHMARK 1: Template Validation
#==============================================================================

log_info "Running template validation benchmark"

# Warm-up run
if [ -f "$REPO_ROOT/scripts/validate-template.sh" ]; then
    bash "$REPO_ROOT/scripts/validate-template.sh" "$REPO_ROOT" 0 > /dev/null 2>&1 || true
fi

# Timed runs
BENCH_RUNS=3
TOTAL_TIME=0

for i in $(seq 1 $BENCH_RUNS); do
    START=$(date +%s%N)
    bash "$REPO_ROOT/scripts/validate-template.sh" "$REPO_ROOT" 0 > /dev/null 2>&1 || true
    END=$(date +%s%N)

    # Convert to milliseconds
    RUN_TIME=$(( (END - START) / 1000000 ))
    TOTAL_TIME=$(( TOTAL_TIME + RUN_TIME ))

    [ "$OUTPUT_FORMAT" = "human" ] && echo "  Run $i: ${RUN_TIME}ms"
done

AVG_VALIDATION_TIME=$(( TOTAL_TIME / BENCH_RUNS ))
results[validation]=$AVG_VALIDATION_TIME
log_pass "Validation: ${AVG_VALIDATION_TIME}ms average (${BENCH_RUNS} runs)"

#==============================================================================
# BENCHMARK 2: Zig Build
#==============================================================================

log_info "Running Zig build benchmark"

if ! command -v zig &> /dev/null; then
    echo "  ⚠ Zig compiler not found - skipping Zig build benchmark"
    results[zig_build]="skipped"
else
    cd "$REPO_ROOT/src/interface/ffi"

    # Warm-up
    zig build --summary off > /dev/null 2>&1 || true

    # Clean build
    BENCH_RUNS=2
    TOTAL_TIME=0

    for i in $(seq 1 $BENCH_RUNS); do
        rm -rf zig-cache

        START=$(date +%s%N)
        zig build --summary off > /dev/null 2>&1 || true
        END=$(date +%s%N)

        RUN_TIME=$(( (END - START) / 1000000 ))
        TOTAL_TIME=$(( TOTAL_TIME + RUN_TIME ))

        [ "$OUTPUT_FORMAT" = "human" ] && echo "  Run $i: ${RUN_TIME}ms"
    done

    AVG_BUILD_TIME=$(( TOTAL_TIME / BENCH_RUNS ))
    results[zig_build]=$AVG_BUILD_TIME
    log_pass "Zig build: ${AVG_BUILD_TIME}ms average (clean build, ${BENCH_RUNS} runs)"

    cd - > /dev/null
fi

#==============================================================================
# BENCHMARK 3: Zig Tests
#==============================================================================

log_info "Running Zig test benchmark"

if ! command -v zig &> /dev/null; then
    echo "  ⚠ Zig compiler not found - skipping Zig test benchmark"
    results[zig_test]="skipped"
else
    cd "$REPO_ROOT/src/interface/ffi"

    # Warm-up
    zig build test --summary off > /dev/null 2>&1 || true

    START=$(date +%s%N)
    TEST_OUTPUT=$(zig build test --summary off 2>&1 || true)
    END=$(date +%s%N)

    TEST_TIME=$(( (END - START) / 1000000 ))
    results[zig_test]=$TEST_TIME
    log_pass "Zig tests: ${TEST_TIME}ms"

    # Count tests
    TEST_COUNT=$(echo "$TEST_OUTPUT" | grep -c "^test " || echo "unknown")
    echo "  Test count: $TEST_COUNT"

    cd - > /dev/null
fi

#==============================================================================
# BENCHMARK 4: Workflow Validation
#==============================================================================

log_info "Running workflow validation benchmark"

if [ -f "$REPO_ROOT/tests/workflows/validate_workflows_test.sh" ]; then
    START=$(date +%s%N)
    bash "$REPO_ROOT/tests/workflows/validate_workflows_test.sh" "$REPO_ROOT/.github/workflows" > /dev/null 2>&1 || true
    END=$(date +%s%N)

    WORKFLOW_TIME=$(( (END - START) / 1000000 ))
    results[workflow_validation]=$WORKFLOW_TIME
    log_pass "Workflow validation: ${WORKFLOW_TIME}ms"
fi

#==============================================================================
# BENCHMARK 5: Template Instantiation
#==============================================================================

log_info "Running template instantiation benchmark"

if [ -f "$REPO_ROOT/tests/e2e/template_instantiation_test.sh" ]; then
    START=$(date +%s%N)
    bash "$REPO_ROOT/tests/e2e/template_instantiation_test.sh" "$REPO_ROOT" > /dev/null 2>&1 || true
    END=$(date +%s%N)

    INSTANTIATION_TIME=$(( (END - START) / 1000000 ))
    results[instantiation]=$INSTANTIATION_TIME
    log_pass "Template instantiation: ${INSTANTIATION_TIME}ms"
fi

#==============================================================================
# SUMMARY
#==============================================================================

echo ""
echo "═══════════════════════════════════════════════════════════════════════════════"
echo "BENCHMARK RESULTS"
echo "═══════════════════════════════════════════════════════════════════════════════"
echo ""

if [ "$OUTPUT_FORMAT" = "json" ]; then
    echo "{"
    echo "  \"timestamp\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\","
    echo "  \"repo\": \"$REPO_ROOT\","
    echo "  \"results\": {"

    count=0
    for key in "${!results[@]}"; do
        value="${results[$key]}"
        [ $count -gt 0 ] && echo ","
        if [ "$value" = "skipped" ]; then
            echo -n "    \"$key\": \"skipped\""
        else
            echo -n "    \"$key\": $value"
        fi
        count=$((count + 1))
    done
    echo ""
    echo "  }"
    echo "}"
elif [ "$OUTPUT_FORMAT" = "csv" ]; then
    echo "metric,value_ms,timestamp"
    for key in "${!results[@]}"; do
        value="${results[$key]}"
        if [ "$value" != "skipped" ]; then
            echo "$key,$value,$(date -u +%Y-%m-%dT%H:%M:%SZ)"
        fi
    done
else
    # Human-readable format
    for key in "${!results[@]}"; do
        value="${results[$key]}"
        if [ "$value" = "skipped" ]; then
            printf "  %-30s %s\n" "$key:" "SKIPPED"
        else
            printf "  %-30s %5d ms\n" "$key:" "$value"
        fi
    done
fi

echo ""
echo "Benchmark complete."
echo ""
