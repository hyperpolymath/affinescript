#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Anti-staleness gate for the soundness ledger (docs/SOUNDNESS.adoc).
#
# Soundness-hole status used to live in ~6 documents that drifted
# independently, so whoever opened the stale one got a stale answer. The fix is
# structural: docs/SOUNDNESS.adoc is the single source of truth, it is
# *test-anchored*, and every other status surface must point back at it. This
# gate enforces that contract in-repo, toolchain-free, mirroring the sibling
# guard tools/check-doc-truthing.sh (which does the same job for the feature
# matrix).
#
# It checks four invariants:
#
#   1. The ledger exists and self-declares primacy ("single source of truth").
#   2. It carries a freshness stamp (:ground-truth-sha:) so readers know when it
#      was last reconciled against the running compiler.
#   3. Every test fixture the ledger names as an *anchor* actually exists. This
#      is the binding that stops drift: you cannot delete the behaviour a row
#      claims is proven without this gate (and the test suite) noticing.
#   4. Every status surface that summarises soundness still links to the ledger,
#      so no competing per-issue ledger can re-grow elsewhere.
#
# Usage:  ./tools/check-soundness-ledger.sh
# Wired into:  just check (via the `guard` recipe) and CI (.github/workflows/ci.yml).
# Run from anywhere; it cd's to the repo root itself.

set -euo pipefail

cd "$(dirname "$0")/.."

LEDGER="docs/SOUNDNESS.adoc"

# Status surfaces that must point back at the ledger (single-source invariant).
# A doc may summarise soundness, but it must link here rather than maintain a
# rival per-issue table — that rivalry is exactly what went stale.
POINTER_DOCS=(
  "README.adoc"
  "docs/CAPABILITY-MATRIX.adoc"
  "docs/PROOF-NEEDS.adoc"
  "docs/NAVIGATION.adoc"
  ".claude/CLAUDE.md"
)

fail=0
note() { printf '%s\n' "$*" >&2; }

# --- 1. The ledger exists and asserts its own primacy -----------------------
if [ ! -f "$LEDGER" ]; then
  note "ERROR: the soundness ledger is missing: $LEDGER"
  note "       It is the single source of truth for soundness-hole status."
  note "       Restore it or amend this guard."
  exit 1
fi
if ! grep -q "single source of truth" "$LEDGER"; then
  note "ERROR: $LEDGER no longer self-declares primacy."
  note "       Expected the phrase 'single source of truth'."
  fail=1
fi

# --- 2. Freshness stamp is present ------------------------------------------
if ! grep -q ":ground-truth-sha:" "$LEDGER"; then
  note "ERROR: $LEDGER lost its freshness stamp (:ground-truth-sha:)."
  note "       Readers rely on it to know when the ledger was last reconciled"
  note "       against the running compiler. Restore and bump it."
  fail=1
fi

# --- 3. Every anchored fixture the ledger names actually exists -------------
# The ledger pins each claim to a test/e2e/fixtures/*.affine file. If one is
# renamed or deleted without updating the ledger, the claim is unmoored — fail.
missing_anchor=0
while IFS= read -r anchor; do
  [ -z "$anchor" ] && continue
  if [ ! -f "$anchor" ]; then
    if [ "$missing_anchor" -eq 0 ]; then
      note "ERROR: $LEDGER cites anchor fixtures that no longer exist:"
      missing_anchor=1
      fail=1
    fi
    note "         - $anchor"
  fi
done < <(grep -oE 'test/e2e/fixtures/[A-Za-z0-9_]+\.affine' "$LEDGER" | LC_ALL=C sort -u)
if [ "$missing_anchor" -eq 1 ]; then
  note "       Either restore the fixture or update the ledger row to its new"
  note "       anchor in the same change (see $LEDGER, 'Keeping this honest')."
fi

# --- 4. Every status surface still links to the ledger ----------------------
for doc in "${POINTER_DOCS[@]}"; do
  if [ ! -f "$doc" ]; then
    note "ERROR: expected status surface is missing: $doc"
    note "       If it was intentionally removed, drop it from POINTER_DOCS in"
    note "       this guard. Otherwise restore it."
    fail=1
    continue
  fi
  if ! grep -q "SOUNDNESS.adoc" "$doc"; then
    note "ERROR: $doc no longer points at the soundness ledger ($LEDGER)."
    note "       Every doc that summarises soundness status must link to the"
    note "       single source of truth so a rival ledger cannot re-grow and"
    note "       go stale. Add a reference to docs/SOUNDNESS.adoc."
    fail=1
  fi
done

if [ "$fail" -ne 0 ]; then
  note ""
  note "Soundness-ledger guard failed. The single-source contract for soundness"
  note "status is drifting. See docs/SOUNDNESS.adoc (authoritative)."
  exit 1
fi

echo "OK: soundness ledger intact — primacy + freshness stamp + anchors exist + back-links present."
