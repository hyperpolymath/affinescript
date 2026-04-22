#!/bin/bash
set -euo pipefail
TEMPLATE_ROOT="${1:-.}"
TEST_DIR=$(mktemp -d)
# directory already created
trap 'rm -rf "$TEST_DIR"' EXIT
echo "--- Instantiating ---"
cp -r "$TEMPLATE_ROOT" "$TEST_DIR/repo"
cd "$TEST_DIR/repo"
rm -rf .git
echo "--- Replacing ---"
find . -type f -exec sed -i "s/affinescript-vite/test-project/g" {} +
echo "--- Validating ---"
bash scripts/validate-rsr.sh .
echo "--- Done ---"
