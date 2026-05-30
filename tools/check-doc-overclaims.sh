#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2024-2026 hyperpolymath
#
# Over-claim ratchet for issue #176 (DOC-08/09, ledger row DOC-17).
#
# Companion to tools/check-doc-truthing.sh (DOC-16), which enforces the
# *presence* of the corrective banners + matrix primacy + STATE.a2ml mirror
# keys. That guard deliberately does NOT phrase-scan, because "production-ready"
# legitimately appears inside the negating banners and future-roadmap sections,
# so a naive grep false-positives — and it therefore leaves novel over-claim
# *phrasings* (the DOC-08/09 MONITOR) to the external Hypatia/gitbot bots.
#
# This guard closes exactly that gap without the false-positive problem, via a
# ratchet: the current accepted set of over-claim-phrase occurrences (every one
# legitimate today — future-tense roadmap milestones, dated history snapshots,
# the corrective banners) is frozen in tools/doc-overclaims.allow. Any *new*
# occurrence not in that baseline fails the build. The baseline is a sorted,
# signature-keyed (path + normalised line) ledger, robust to line moves, and
# its growth is a reviewable diff — the human-review signal the DOC rules
# describe, now in-repo and automatic.
#
# The two authoritative governance docs that define/quote the rule itself
# (docs/CAPABILITY-MATRIX.adoc, docs/TECH-DEBT.adoc) are excluded — they
# legitimately contain the anti-claims, and editing their rule-text must not
# churn the baseline. The banner docs are covered by check-doc-truthing.sh.
#
# Re-baselining (only when a NEW occurrence is genuinely legitimate — e.g. a
# new dated roadmap milestone): run `./tools/check-doc-overclaims.sh --update`
# (or `just doc-overclaims-bless`) and commit the doc-overclaims.allow diff in
# the same PR. The diff is the audit trail.
#
# Wired into:
#   - just check  (via the `guard` recipe in justfile)
#   - CI          (.github/workflows/ci.yml, build job)

set -euo pipefail

cd "$(dirname "$0")/.."

ALLOW="tools/doc-overclaims.allow"

# Over-claim phrase families (DOC-08 backend-breadth / DOC-09 production-ready
# + stdlib-percentage). Patterns are intentionally broad: the frozen baseline
# gives zero false positives on existing content, so breadth only raises recall
# on genuinely new occurrences.
PATTERN='production[- ]ready|production backends?|[0-9]+[[:space:]]+(production|complete)[[:space:]]+backends?|stdlib[^|]*100%|100%[^|]*stdlib|N production backend'

# Emit one normalised signature per over-claim hit: "relpath<TAB>line", with
# the line's internal whitespace collapsed and trimmed (robust to re-indent /
# re-flow; sensitive to the claim text itself). Sorted + de-duplicated.
scan_overclaims() {
  # The two authoritative governance docs are excluded: they define and quote
  # the banned phrases as the rule itself (anti-claims), so scanning them would
  # only re-flag the rule text. Banner presence in these + the over-claiming
  # docs is the job of tools/check-doc-truthing.sh (DOC-16).
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

# --- --update / --bless mode: regenerate the baseline -----------------------
if [ "${1:-}" = "--update" ] || [ "${1:-}" = "--bless" ]; then
  {
    echo "# SPDX-License-Identifier: MPL-2.0"
    echo "# SPDX-FileCopyrightText: 2024-2026 hyperpolymath"
    echo "# Over-claim baseline (issue #176, DOC-08/09 / ledger DOC-17) — the"
    echo "# frozen set of accepted over-claim-phrase occurrences. Regenerate with"
    echo "# \`./tools/check-doc-overclaims.sh --update\`. Each line is a"
    echo "# \"relpath<TAB>normalised-line\" signature. A new signature not listed"
    echo "# here fails CI; adding one is a deliberate, reviewable act. Current"
    echo "# entries are future-tense roadmap milestones, dated history snapshots,"
    echo "# and the corrective banners — none is a live over-claim."
    scan_overclaims
  } > "$ALLOW"
  echo "OK: regenerated $ALLOW ($(grep -cvE '^#' "$ALLOW") signatures)."
  exit 0
fi

if [ ! -f "$ALLOW" ]; then
  echo "ERROR: $ALLOW is missing. Run ./tools/check-doc-overclaims.sh --update" >&2
  echo "       to create the over-claim baseline." >&2
  exit 1
fi

current="$(scan_overclaims)"
allowed="$(grep -vE '^#' "$ALLOW" | LC_ALL=C sort -u || true)"
new_hits="$(LC_ALL=C comm -13 <(printf '%s\n' "$allowed") <(printf '%s\n' "$current") | sed '/^$/d')"

if [ -n "$new_hits" ]; then
  echo "ERROR: new over-claim(s) detected in status docs (issue #176, DOC-08/09)." >&2
  echo "" >&2
  echo "The authoritative readiness source is docs/CAPABILITY-MATRIX.adoc, which" >&2
  echo "forbids re-introducing backend-breadth / \"production-ready\" /" >&2
  echo "stdlib-percentage over-claims. These occurrences are not in the frozen" >&2
  echo "baseline ($ALLOW):" >&2
  echo "" >&2
  printf '%s\n' "$new_hits" | sed 's/\t/  ::  /; s/^/  /' >&2
  echo "" >&2
  echo "Fix the over-claim (preferred), or — if it is genuinely legitimate" >&2
  echo "(e.g. a new dated roadmap milestone) — re-baseline with:" >&2
  echo "    ./tools/check-doc-overclaims.sh --update   # or: just doc-overclaims-bless" >&2
  echo "and commit the tools/doc-overclaims.allow diff in the same PR." >&2
  exit 1
fi

echo "OK: no new over-claims beyond the frozen baseline ($ALLOW)."
