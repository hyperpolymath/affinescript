#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Unified doc-truthing re-drift guard for issue #176 (DOC-01..09).
#
# Authoritative feature-readiness status lives in docs/CAPABILITY-MATRIX.adoc.
# Several historical/architectural docs are known to over-claim (backend
# breadth, "production-ready", stdlib percentage). DOC-04/05 neutralised them
# with an *authoritative banner* that points every reader back at the matrix,
# and made the matrix self-declare its primacy + carry an anti-over-claim
# section. Until now those invariants were enforced only by the external
# Hypatia/gitbot review bots (the MONITOR posture in issue #176).
#
# This is a first-class, in-repo, toolchain-free gate that enforces *both*
# halves of the MONITOR in a single run (consolidated from the two guards that
# briefly co-existed as DOC-16 + DOC-17 — check-doc-truthing.sh and
# check-doc-overclaims.sh — into one script with no overlap):
#
#   Presence invariants (DOC-04/05) — fails if any banner pointer, the matrix's
#   self-declaration, the anti-over-claim section, or the STATE.a2ml mirror keys
#   are removed. This deliberately checks *presence of the correction*, not a
#   phrase blocklist: "production-ready" legitimately appears inside the
#   negating banners and future-roadmap sections, so a naive grep over the
#   whole doc would false-positive.
#
#   Over-claim ratchet (DOC-08/09) — closes exactly that phrase-detection gap
#   without the false positives, via a frozen baseline (tools/doc-overclaims.allow):
#   the current accepted set of over-claim-phrase occurrences (every one
#   legitimate today — future-tense roadmap milestones, dated history snapshots,
#   the corrective banners) is frozen, and any *new* occurrence fails the build.
#   The two governance docs that quote the rule itself (CAPABILITY-MATRIX +
#   TECH-DEBT) are excluded so editing the rule-text never churns the baseline.
#   Signatures are path + normalised-line, robust to line moves; the baseline's
#   growth is a reviewable diff — the human-review signal the DOC rules describe.
#
# Usage:
#   check-doc-truthing.sh            run all checks (CI / `just check`)
#   check-doc-truthing.sh --update   re-baseline the over-claim ratchet, e.g.
#                                    after a new dated roadmap milestone, then
#                                    commit the tools/doc-overclaims.allow diff
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

# Over-claim ratchet (DOC-08/09): the frozen baseline + the phrase families it
# guards. Patterns are intentionally broad — the frozen baseline gives zero
# false positives on existing content, so breadth only raises recall on new
# occurrences.
ALLOW="tools/doc-overclaims.allow"
PATTERN='production[- ]ready|production backends?|[0-9]+[[:space:]]+(production|complete)[[:space:]]+backends?|stdlib[^|]*100%|100%[^|]*stdlib|N production backend'

# Emit one normalised signature per over-claim hit: "relpath<TAB>line", with the
# line's internal whitespace collapsed + trimmed (robust to re-indent / re-flow;
# sensitive to the claim text itself). CAPABILITY-MATRIX + TECH-DEBT are excluded
# (they define/quote the rule itself; their banner/primacy presence is checked
# above instead). Sorted + de-duplicated.
scan_overclaims() {
  grep -rniE "$PATTERN" README.adoc docs/ 2>/dev/null \
    --exclude="CAPABILITY-MATRIX.adoc" \
    --exclude="TECH-DEBT.adoc" \
    | sed -E 's/^([^:]+):[0-9]+:/\1\t/' \
    | awk -F'\t' 'BEGIN { OFS = "\t" }
        {
          line = $2
          gsub(/[[:space:]]+/, " ", line)
          sub(/^ /, "", line)
          sub(/ $/, "", line)
          print $1, line
        }' \
    | LC_ALL=C sort -u
}

# --- --update / --bless mode: regenerate the ratchet baseline ---------------
if [ "${1:-}" = "--update" ] || [ "${1:-}" = "--bless" ]; then
  {
    echo "# SPDX-License-Identifier: MPL-2.0"
    echo "# SPDX-FileCopyrightText: 2024-2026 hyperpolymath"
    echo "# Over-claim baseline (issue #176, DOC-08/09) — the frozen set of"
    echo "# accepted over-claim-phrase occurrences guarded by"
    echo "# tools/check-doc-truthing.sh. Regenerate with"
    echo "# \`./tools/check-doc-truthing.sh --update\`. Each line is a"
    echo "# \"relpath<TAB>normalised-line\" signature. A new signature not listed"
    echo "# here fails CI; adding one is a deliberate, reviewable act. Current"
    echo "# entries are future-tense roadmap milestones, dated history snapshots,"
    echo "# and the corrective banners — none is a live over-claim."
    scan_overclaims
  } > "$ALLOW"
  echo "OK: regenerated $ALLOW ($(grep -cvE '^#' "$ALLOW") signatures)."
  exit 0
fi

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

# --- 4. No NEW over-claim phrasing beyond the frozen baseline (DOC-08/09) ----
if [ ! -f "$ALLOW" ]; then
  note "ERROR: over-claim baseline is missing: $ALLOW"
  note "       Run ./tools/check-doc-truthing.sh --update to create it."
  fail=1
else
  current="$(scan_overclaims)"
  allowed="$(grep -vE '^#' "$ALLOW" | LC_ALL=C sort -u || true)"
  new_hits="$(LC_ALL=C comm -13 <(printf '%s\n' "$allowed") <(printf '%s\n' "$current") | sed '/^$/d')"
  if [ -n "$new_hits" ]; then
    note "ERROR: new over-claim(s) detected in status docs (DOC-08/09)."
    note "       The authoritative readiness source is $MATRIX, which forbids"
    note "       re-introducing backend-breadth / \"production-ready\" /"
    note "       stdlib-percentage over-claims. Not in the frozen baseline"
    note "       ($ALLOW):"
    note ""
    printf '%s\n' "$new_hits" | sed 's/\t/  ::  /; s/^/         /' >&2
    note ""
    note "       Fix the over-claim (preferred), or — if it is genuinely"
    note "       legitimate (e.g. a new dated roadmap milestone) — re-baseline"
    note "       with: ./tools/check-doc-truthing.sh --update  (just doc-truth-bless)"
    note "       and commit the $ALLOW diff in the same PR."
    fail=1
  fi
fi

if [ "$fail" -ne 0 ]; then
  note ""
  note "Doc-truthing guard failed. The status docs are drifting back toward"
  note "over-claiming. See docs/CAPABILITY-MATRIX.adoc (authoritative) and"
  note "docs/TECH-DEBT.adoc section A (DOC-01..09) for the contract."
  exit 1
fi

echo "OK: doc-truthing intact — presence invariants + over-claim ratchet (DOC-04/05/08/09)."
