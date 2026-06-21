#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Anti-staleness gate for the capability matrix's test anchors
# (docs/CAPABILITY-MATRIX.adoc "== Test anchors").
#
# The matrix is the authoritative feature-readiness doc. Its "Test anchors"
# section pins each enforcement claim ("works" / "enforced" / "partial") to the
# executable test(s) that exercise it — the same test-anchoring discipline the
# soundness ledger uses (tools/check-soundness-ledger.sh). This gate makes those
# anchors falsifiable: it extracts every test path the matrix names and fails the
# build if any has gone missing, so a feature cannot keep a green status row
# after its test is deleted or renamed without this section (and CI) noticing.
#
# Checks:
#   1. The matrix exists and carries a "== Test anchors" section.
#   2. Every test/*.ml and test/e2e/fixtures/*.affine path named anywhere in the
#      matrix actually exists on disk.
#
# Usage:  ./tools/check-capability-anchors.sh
# Wired into:  just check (via the `guard` recipe) and CI (.github/workflows/ci.yml).
# Run from anywhere; it cd's to the repo root itself.

set -euo pipefail

cd "$(dirname "$0")/.."

MATRIX="docs/CAPABILITY-MATRIX.adoc"

fail=0
note() { printf '%s\n' "$*" >&2; }

# --- 1. The matrix exists and carries its Test-anchors section ---------------
if [ ! -f "$MATRIX" ]; then
  note "ERROR: the capability matrix is missing: $MATRIX"
  exit 1
fi
if ! grep -q "^== Test anchors" "$MATRIX"; then
  note "ERROR: $MATRIX lost its '== Test anchors' section."
  note "       That section anchors each feature-readiness claim to an"
  note "       executable test. Restore it (see the soundness ledger for the"
  note "       same pattern: docs/SOUNDNESS.adoc)."
  fail=1
fi

# --- 2. Every test path the matrix names actually exists ---------------------
missing=0
while IFS= read -r path; do
  [ -z "$path" ] && continue
  if [ ! -f "$path" ]; then
    if [ "$missing" -eq 0 ]; then
      note "ERROR: $MATRIX names test anchors that no longer exist:"
      missing=1
      fail=1
    fi
    note "         - $path"
  fi
done < <(grep -oE 'test/[A-Za-z0-9_./-]+\.(ml|affine)' "$MATRIX" | LC_ALL=C sort -u)
if [ "$missing" -eq 1 ]; then
  note "       Either restore the test or update the matrix to its new anchor"
  note "       in the same change. A renamed/deleted test must not leave a"
  note "       feature-readiness claim unmoored."
fi

if [ "$fail" -ne 0 ]; then
  note ""
  note "Capability-anchor guard failed. Feature-readiness claims in"
  note "$MATRIX are drifting from the tests that back them."
  exit 1
fi

echo "OK: capability anchors intact — Test-anchors section present + every named test exists."
