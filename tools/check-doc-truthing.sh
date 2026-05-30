#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Doc-truthing re-drift guard for issue #176 (DOC-01..09).
#
# Authoritative feature-readiness status lives in docs/CAPABILITY-MATRIX.adoc.
# Several historical/architectural docs are known to over-claim (backend
# breadth, "production-ready", stdlib percentage). DOC-04/05 neutralised them
# with an *authoritative banner* that points every reader back at the matrix,
# and made the matrix self-declare its primacy + carry an anti-over-claim
# section.
#
# Until now that invariant was enforced only by the external Hypatia/gitbot
# review bots (the MONITOR posture in issue #176). This guard makes it a
# first-class, in-repo, toolchain-free gate: it fails if any of the banner
# pointers, the matrix's self-declaration, the anti-over-claim section, or the
# STATE.a2ml mirror keys are removed — i.e. if the docs start to re-drift.
#
# It deliberately checks *banner presence*, not phrase blocklists: the word
# "production-ready" legitimately appears inside the negating banners and in
# clearly-marked future-roadmap sections, so a naive phrase grep would
# false-positive. Presence-of-the-correction is the durable invariant.
#
# Wired into:
#   - just check  (via the `guard` recipe in justfile)
#   - CI          (.github/workflows/ci.yml, build job)
#
# Run from anywhere; it cd's to the repo root itself.

set -euo pipefail

cd "$(dirname "$0")/.."

# The single authoritative status document.
MATRIX="docs/CAPABILITY-MATRIX.adoc"

# Docs that historically over-claim and MUST carry a pointer back at $MATRIX
# (DOC-04). Each must mention CAPABILITY-MATRIX.adoc somewhere in its body.
BANNERED_DOCS=(
  "README.adoc"
  "docs/architecture/BACKEND-IMPLEMENTATION.md"
  "docs/reference/COMPILER-CAPABILITIES.md"
  "docs/history/ALPHA-1-RELEASE-NOTES.md"
)

# The machine-readable mirror (DOC-05): it follows the matrix, it does not
# lead. These keys assert that contract in-band.
STATE_FILE=".machine_readable/6a2/STATE.a2ml"
STATE_KEYS=(
  "authoritative-status-doc"
  "drift-flag"
)

fail=0
note() { printf '%s\n' "$*" >&2; }

# --- 1. The matrix itself exists and asserts its own primacy ----------------
if [ ! -f "$MATRIX" ]; then
  note "ERROR: authoritative status doc is missing: $MATRIX"
  note "       This file is the single source of truth for feature readiness"
  note "       (DOC-01, issue #176). Restore it or amend this guard."
  fail=1
else
  if ! grep -q "single authoritative source" "$MATRIX"; then
    note "ERROR: $MATRIX no longer self-declares primacy."
    note "       Expected the phrase 'single authoritative source' (DOC-01)."
    fail=1
  fi
  # DOC-08/09: the anti-over-claim section must survive.
  if ! grep -q "What AffineScript is NOT" "$MATRIX"; then
    note "ERROR: $MATRIX lost its 'What AffineScript is NOT' section."
    note "       That section is the anti-over-claim record (DOC-08/09,"
    note "       issue #176). Do not delete it."
    fail=1
  fi
fi

# --- 2. Every over-claiming doc still points back at the matrix (DOC-04) -----
for doc in "${BANNERED_DOCS[@]}"; do
  if [ ! -f "$doc" ]; then
    note "ERROR: bannered doc is missing: $doc"
    note "       If it was intentionally removed, drop it from BANNERED_DOCS"
    note "       in this guard. Otherwise restore the file + its banner."
    fail=1
    continue
  fi
  if ! grep -q "CAPABILITY-MATRIX.adoc" "$doc"; then
    note "ERROR: $doc no longer points at the authoritative status matrix."
    note "       DOC-04 requires an authoritative banner referencing"
    note "       docs/CAPABILITY-MATRIX.adoc so readers are not misled by"
    note "       this doc's historical over-claims (issue #176)."
    fail=1
  fi
done

# --- 3. STATE.a2ml still declares itself a mirror, not a leader (DOC-05) -----
if [ ! -f "$STATE_FILE" ]; then
  note "ERROR: machine-readable state file is missing: $STATE_FILE"
  fail=1
else
  for key in "${STATE_KEYS[@]}"; do
    if ! grep -q "$key" "$STATE_FILE"; then
      note "ERROR: $STATE_FILE lost the '$key' key."
      note "       DOC-05 requires STATE.a2ml to flag that it MIRRORS the"
      note "       capability matrix and does not lead it (issue #176)."
      fail=1
    fi
  done
fi

if [ "$fail" -ne 0 ]; then
  note ""
  note "Doc-truthing guard failed. The status docs are drifting back toward"
  note "over-claiming. See docs/CAPABILITY-MATRIX.adoc (authoritative) and"
  note "docs/TECH-DEBT.adoc section A (DOC-01..09) for the contract."
  exit 1
fi

echo "OK: doc-truthing banners intact (matrix primacy + DOC-04 banners + DOC-05 mirror keys)."
