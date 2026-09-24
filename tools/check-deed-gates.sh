#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# CI gate: DEED owns the manifest grammar. A2ML is retired.
# Fails if any *.a2ml remains, if the repo deed is missing, or if
# in-repo gates still key off STATE.a2ml / a2ml-validate.
set -euo pipefail
cd "$(dirname "$0")/.."

fail=0
note() { printf '%s\n' "$*" >&2; }

leftover="$(find . -name '*.a2ml' -not -path './.git/*' -print | LC_ALL=C sort || true)"
if [[ -n "$leftover" ]]; then
  note "ERROR: leftover .a2ml files — A2ML is retired; rename to .deed:"
  printf '%s\n' "$leftover" | sed 's/^/  /' >&2
  fail=1
fi

deed_count="$(find . \( -name '*.deed' \) -not -path './.git/*' | wc -l | tr -d ' ')"
if [[ "$deed_count" -eq 0 ]]; then
  note "ERROR: no .deed manifests found. Author DEED, not A2ML."
  fail=1
fi

if [[ ! -f affinescript_chora.deed ]]; then
  note "ERROR: missing repo deed affinescript_chora.deed"
  fail=1
else
  if ! grep -q '(repo-deed' affinescript_chora.deed; then
    note "ERROR: affinescript_chora.deed is not a DEED s-expression (expected (repo-deed ...))"
    fail=1
  fi
  if ! grep -q ':schema-version' affinescript_chora.deed; then
    note "ERROR: affinescript_chora.deed missing :schema-version"
    fail=1
  fi
fi

state=".machine_readable/descriptiles/STATE.deed"
if [[ ! -f "$state" ]]; then
  note "ERROR: missing $state (DOC-05 mirror; not STATE.a2ml)"
  fail=1
else
  for key in authoritative-status-doc drift-flag; do
    if ! grep -q "$key" "$state"; then
      note "ERROR: $state lost the '$key' key"
      fail=1
    fi
  done
fi

# In-repo gates must not still require the dead extension.
if grep -nE 'STATE\.a2ml|a2ml-validate|0-AI-MANIFEST\.a2ml' \
    tools/check-doc-truthing.sh \
    .github/workflows/*.yml justfile 2>/dev/null; then
  note "ERROR: CI/tools still gate on .a2ml paths — point them at .deed"
  fail=1
fi

if [[ "$fail" -ne 0 ]]; then
  note ""
  note "DEED gate failed. Manifest grammar is .deed (standards 1-formats/deed/)."
  note "A2ML is not a CI format."
  exit 1
fi

echo "OK: DEED gates — ${deed_count} .deed file(s), no leftover .a2ml, repo deed present."
