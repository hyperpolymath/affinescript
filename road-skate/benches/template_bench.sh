#!/bin/bash
set -euo pipefail
REPO_ROOT="${1:-.}"
echo "--- Benchmarking ---"
START=$(date +%s%N)
bash scripts/validate-rsr.sh "$REPO_ROOT" > /dev/null
END=$(date +%s%N)
DIFF=$(( (END - START) / 1000000 ))
echo "Validation: ${DIFF}ms"
echo "--- Done ---"
