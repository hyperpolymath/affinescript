#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Standalone governance gate.
#
# Replaces the external `hyperpolymath/standards` governance-reusable.yml so
# this repo's CI carries no cross-repo workflow dependency. It is deliberately
# conservative and delta-aware: it enforces the unambiguous estate rules that
# the current tree already satisfies, and treats DOC-FORMAT as a PR-delta
# check (matching the canonical "on any PR that adds a docs/ .md" semantics)
# so the 59 pre-existing docs/*.md files are never retro-flagged.
#
# This is a best-effort local reimplementation; it is NOT byte-for-byte the
# canonical estate bundle (which is not visible from this repo). See the
# `standalone-ci` PR for the trade-off discussion.
#
# Usage: tools/ci/governance-standalone.sh [BASE_REF]
#   BASE_REF (or $GOVERNANCE_BASE_REF / $GITHUB_BASE_REF) enables the
#   DOC-FORMAT delta check; if absent the delta check is skipped.
set -uo pipefail

fail=0
note() { printf '  %s\n' "$*"; }
err()  { printf '::error::%s\n' "$*"; fail=1; }

echo "== Governance gate (standalone) =="

# --- [1] Jekyll artifacts are banned (estate SSG is hyperpolymath/casket-ssg)
echo "[1/3] Jekyll artifacts"
jekyll=$(find . \( -path ./.git -o -path ./_build -o -path ./node_modules \) -prune -o \
  \( -name '_config.yml' -o -name 'Gemfile' -o -iname 'jekyll*.yml' \
     -o -iname 'jekyll-gh-pages*.yml' \) -print 2>/dev/null || true)
if [ -n "$jekyll" ]; then
  err "Jekyll artifacts present (migrate to hyperpolymath/casket-ssg):"
  printf '%s\n' "$jekyll" | sed 's/^/    /'
else note "none"; fi

# --- [2] MPL-1.0 license headers are banned (must be MPL-2.0) ---------------
# Match the actual SPDX identifier, not mere prose mentions of the ban (the
# policy docs legitimately discuss MPL-1.0).
echo "[2/3] MPL-1.0 SPDX headers"
mpl1=$(grep -rIl --exclude-dir=.git --exclude-dir=_build --exclude-dir=node_modules \
  -E 'SPDX-License-Identifier:[[:space:]]*MPL-1\.0' . 2>/dev/null || true)
if [ -n "$mpl1" ]; then
  err "MPL-1.0 SPDX headers found (rewrite to MPL-2.0):"
  printf '%s\n' "$mpl1" | sed 's/^/    /'
else note "none"; fi

# --- [3] DOC-FORMAT (delta): newly-added docs/ files must be .adoc ----------
# Community-health files keep their canonical .md names.
echo "[3/3] DOC-FORMAT (newly-added docs/ files must be .adoc)"
base="${1:-${GOVERNANCE_BASE_REF:-${GITHUB_BASE_REF:-}}}"
if [ -n "$base" ] && git rev-parse --verify --quiet "origin/$base" >/dev/null 2>&1; then
  added=$(git diff --name-only --diff-filter=A "origin/$base...HEAD" -- 'docs/' 2>/dev/null \
    | grep -E '\.md$' \
    | grep -viE '/(CONTRIBUTING|CODE_OF_CONDUCT|SECURITY|CHANGELOG|README)\.md$' || true)
  if [ -n "$added" ]; then
    err "new docs/ .md files added (rename to .adoc per DOC-FORMAT):"
    printf '%s\n' "$added" | sed 's/^/    /'
  else note "no newly-added docs/ .md files"; fi
else
  note "no base ref available — delta check skipped"
fi

echo
if [ "$fail" -ne 0 ]; then echo "Governance gate: FAIL"; exit 1; fi
echo "Governance gate: PASS"
