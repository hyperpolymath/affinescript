#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Fetch and build the pinned tree-sitter-rescript grammar.
# Output goes under ../../.build/tree-sitter-rescript/ (gitignored).

set -euo pipefail

UPSTREAM_COMMIT="990214a83f25801dfe0226bd7e92bb71bba1970f"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
# tools/vendor/ is the repo's convention for fetched-not-committed deps
# (see .gitignore line 103, mirrors the WASI adapter provisioning).
BUILD_DIR="${REPO_ROOT}/tools/vendor/tree-sitter-rescript"

if ! command -v tree-sitter >/dev/null 2>&1; then
  echo "error: tree-sitter CLI not found on PATH" >&2
  echo "       install via: npm install -g tree-sitter-cli" >&2
  exit 2
fi

if ! command -v git >/dev/null 2>&1; then
  echo "error: git not found on PATH" >&2
  exit 2
fi

mkdir -p "$(dirname "$BUILD_DIR")"

if [ -d "$BUILD_DIR/.git" ]; then
  git -C "$BUILD_DIR" fetch --quiet origin "$UPSTREAM_COMMIT" || true
  git -C "$BUILD_DIR" checkout --quiet "$UPSTREAM_COMMIT"
else
  rm -rf "$BUILD_DIR"
  git clone --quiet https://github.com/rescript-lang/tree-sitter-rescript.git "$BUILD_DIR"
  git -C "$BUILD_DIR" checkout --quiet "$UPSTREAM_COMMIT"
fi

cd "$BUILD_DIR"
tree-sitter generate

echo "tree-sitter-rescript built at ${BUILD_DIR} (commit ${UPSTREAM_COMMIT})"
