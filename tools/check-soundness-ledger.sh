#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# ============================================================================
# ## What this gate enforces
# ============================================================================
# docs/SOUNDNESS.adoc is the single source of truth for soundness-hole status.
# This gate enforces FIVE properties. A green run means "the status table is
# honest and the fixtures still say what they said" — NOT "AffineScript is
# proven sound". Every check fails CLOSED: any ambiguity (missing manifest,
# unparseable row, git/tooling error, missing fixture, shallow clone) fails the
# build rather than passing it.
#
#   Property 1  Anchors exist             -> check_anchors_exist   (Jonathan's #622 design)
#   Property 2  Back-links                -> check_backlinks       (Jonathan's #622 design)
#   Property 3  Content-binding (digests) -> check_content_binding (NEW; manifest + --reseal)
#   Property 4  Stamp-enforcement         -> check_stamp           (NEW)
#   Property 5  Pin-liveness (xfail)      -> check_pins            (NEW)
#
# ## Parse contract (canonical anchor set; used by properties 1/3/4/5)
# Anchors are named in the ledger table's column 4 ("Proven in code"). From the
# table region of docs/SOUNDNESS.adoc we extract three anchor kinds:
#   FIXTURE  test/e2e/fixtures/<name>.affine   -> digest = sha256 of the file
#   SUITE    test/<name>.ml                    -> digest = sha256 of the whole
#                                                 file (a suite is its own unit)
#   TEST     backtick-quoted `test_<name>`     -> digest = sha256 of the test's
#            function body, extracted from its `let <name> ` line to the next
#            top-level `let ` in whichever single test/**/*.ml defines it.
# Every issue row MUST yield >= 1 anchor; a row that yields none fails closed.
#
# ## Usage
#   check-soundness-ledger.sh            run all five checks (CI / `just check`)
#   check-soundness-ledger.sh --reseal   regenerate tools/soundness-anchors.sha256
#                                        after an intentional anchor change
#
# Wired into:  just check (the `guard` recipe) and CI (.github/workflows/ci.yml).
# CI must run on history deep enough to contain :ground-truth-sha: (property 4).
# Run from anywhere; it cd's to the repo root itself.
# ============================================================================

set -euo pipefail
cd "$(dirname "$0")/.."

LEDGER="docs/SOUNDNESS.adoc"
MANIFEST="tools/soundness-anchors.sha256"
XFAIL_EXE="test/xfail/test_xfail_pins.exe"
POINTER_DOCS=(README.adoc docs/CAPABILITY-MATRIX.adoc docs/PROOF-NEEDS.adoc docs/NAVIGATION.adoc .claude/CLAUDE.md)
# lib soundness paths cited by the table; a change here must advance the stamp.
SOUNDNESS_LIB_PATHS=(lib/borrow.ml lib/codegen.ml lib/codegen_deno.ml lib/interp.ml lib/typecheck.ml lib/trait.ml)

die()  { printf 'FATAL: %s\n' "$*" >&2; exit 1; }
note() { printf '%s\n' "$*" >&2; }

[ -f "$LEDGER" ] || die "ledger missing: $LEDGER (single source of truth)"
command -v sha256sum >/dev/null 2>&1 || die "sha256sum not found"

# --- table region: the header row through the closing |=== --------------------
table_region() {
  awk '/^\| Issue \| Soundness/{intab=1} intab{print} intab&&/^\|===/{exit}' "$LEDGER"
}

# --- extract anchors as "<kind>:<locator>" lines, deduped --------------------
extract_anchors() {
  local region; region="$(table_region)"
  {
    printf '%s\n' "$region" | grep -oE 'test/e2e/fixtures/[A-Za-z0-9_]+\.affine' | sed 's#^#fixture:#'
    printf '%s\n' "$region" | grep -oE 'test/[A-Za-z0-9_]+\.ml'                  | sed 's#^#suite:#'
    printf '%s\n' "$region" | grep -oE '`test_[A-Za-z0-9_]+`' | tr -d '`'        | sed 's#^#test:#'
  } | LC_ALL=C sort -u
}

# --- per-row fail-closed: every issue row yields >= 1 anchor -----------------
rows_without_anchor() {
  table_region | awk '
    /^\| Issue/ {next}
    /^\| #/ { emit(); row=$0; started=1; next }
    started && /^\|===/ { emit(); started=0; next }
    started { row = row "\n" $0 }
    END { emit() }
    function emit(   has,a,iss) {
      if (row=="") return
      has = (row ~ /test\/e2e\/fixtures\/[A-Za-z0-9_]+\.affine/) ||
            (row ~ /test\/[A-Za-z0-9_]+\.ml/) ||
            (row ~ /`test_[A-Za-z0-9_]+`/)
      if (!has) { split(row,a,"\n"); iss=a[1]; sub(/^\| */,"",iss); print iss }
      row=""
    }'
}

# --- locate the single test/**/*.ml defining `let <name> ` -------------------
test_def_file() {
  local name="$1" files n
  files="$(grep -rlE "^let ${name} " test --include='*.ml' 2>/dev/null || true)"
  n="$(printf '%s' "$files" | grep -c . || true)"
  [ "$n" = "1" ] || die "anchor test:${name}: expected exactly one defining file, found ${n} (fail closed)"
  printf '%s\n' "$files"
}

# --- digest one anchor (fail closed on anything unexpected) ------------------
hash_anchor() {
  local a="$1" kind loc file
  kind="${a%%:*}"; loc="${a#*:}"
  case "$kind" in
    fixture)
      [ -f "$loc" ] || die "anchor ${a}: fixture not found"
      sha256sum "$loc" | cut -d' ' -f1 ;;
    test)
      file="$(test_def_file "$loc")"
      # body = the `let <name> ` line through the line before the next top-level `let `
      awk -v name="$loc" '
        index($0, "let " name " ")==1 {grab=1; print; next}
        grab && index($0,"let ")==1 {exit}
        grab {print}
      ' "$file" | sha256sum | cut -d' ' -f1 ;;
    suite)
      [ -f "$loc" ] || die "anchor ${a}: suite file not found"
      sha256sum "$loc" | cut -d' ' -f1 ;;
    *) die "internal: hash_anchor on non-digest kind: ${a}" ;;
  esac
}

# --- the digest manifest (fixtures + test bodies + suite files) -------------
generate_manifest() {
  local a
  while IFS= read -r a; do
    [ -z "$a" ] && continue
    case "$a" in fixture:*|test:*|suite:*) printf '%s  %s\n' "$(hash_anchor "$a")" "$a" ;; esac
  done < <(extract_anchors) | LC_ALL=C sort
}

# ============================================================================
# --reseal: regenerate the manifest (deliberate, reviewable act)
# ============================================================================
if [ "${1:-}" = "--reseal" ]; then
  bad="$(rows_without_anchor)"; [ -z "$bad" ] || die "cannot reseal — rows with no anchor: ${bad}"
  {
    echo "# SPDX-License-Identifier: MPL-2.0"
    echo "# Soundness anchor digests for docs/SOUNDNESS.adoc — DO NOT hand-edit."
    echo "# Regenerate: tools/check-soundness-ledger.sh --reseal"
    echo "# Format: <sha256>  <kind:locator>. FIXTURE = file hash; TEST = the"
    echo "# test's function-body hash; SUITE = whole-file hash."
    generate_manifest
  } > "$MANIFEST"
  echo "OK: resealed ${MANIFEST} ($(grep -cvE '^#' "$MANIFEST") anchors)."
  exit 0
fi

[ -z "${1:-}" ] || die "unknown argument: ${1} (use --reseal or no args)"

fail=0

# ---- Property 1: anchors exist ---------------------------------------------
check_anchors_exist() {
  local bad; bad="$(rows_without_anchor)"
  [ -z "$bad" ] || { note "ERROR (property 1): ledger rows with no parseable anchor:"; printf '%s\n' "$bad" | sed 's/^/    /' >&2; return 1; }
  local a miss=0
  while IFS= read -r a; do
    case "$a" in
      fixture:*|suite:*) [ -f "${a#*:}" ] || { note "ERROR (property 1): anchor missing: ${a#*:}"; miss=1; } ;;
      test:*) grep -rqE "^let ${a#*:} " test --include='*.ml' 2>/dev/null || { note "ERROR (property 1): test anchor not defined: ${a#*:}"; miss=1; } ;;
    esac
  done < <(extract_anchors)
  return $miss
}

# ---- Property 2: back-links -------------------------------------------------
check_backlinks() {
  local doc r=0
  for doc in "${POINTER_DOCS[@]}"; do
    [ -f "$doc" ] || { note "ERROR (property 2): status surface missing: ${doc}"; r=1; continue; }
    grep -q "SOUNDNESS.adoc" "$doc" || { note "ERROR (property 2): ${doc} no longer links to ${LEDGER}"; r=1; }
  done
  return $r
}

# ---- Property 3: content-binding -------------------------------------------
check_content_binding() {
  [ -f "$MANIFEST" ] || { note "ERROR (property 3): manifest missing: ${MANIFEST} (run --reseal)"; return 1; }
  local cur committed
  cur="$(generate_manifest)"
  committed="$(grep -vE '^#' "$MANIFEST" | sed '/^$/d' | LC_ALL=C sort)"
  if [ "$cur" != "$committed" ]; then
    note "ERROR (property 3): anchor content drift vs ${MANIFEST} (a fixture or pinned test changed content without --reseal):"
    diff <(printf '%s\n' "$committed") <(printf '%s\n' "$cur") | sed 's/^/    /' >&2 || true
    note "    Fix the regression, or run: tools/check-soundness-ledger.sh --reseal  (if intentional)"
    return 1
  fi
}

# ---- Property 4: stamp-enforcement -----------------------------------------
check_stamp() {
  local S; S="$(sed -n 's/^:ground-truth-sha: *//p' "$LEDGER" | head -1 | tr -d '[:space:]')"
  [ -n "$S" ] || { note "ERROR (property 4): :ground-truth-sha: missing/empty"; return 1; }
  git rev-parse --git-dir >/dev/null 2>&1 || { note "ERROR (property 4): not a git repo"; return 1; }
  git cat-file -e "${S}^{commit}" 2>/dev/null || { note "ERROR (property 4): stamp ${S} not present in history (shallow clone? deepen fetch / fetch the stamp commit)"; return 1; }
  git merge-base --is-ancestor "$S" HEAD 2>/dev/null || { note "ERROR (property 4): stamp ${S} is not an ancestor of HEAD; re-point :ground-truth-sha: to a main commit you verified against"; return 1; }
  # soundness path set: ledger + cited lib files + anchored fixtures/suites
  local paths=("$LEDGER" "${SOUNDNESS_LIB_PATHS[@]}") a
  while IFS= read -r a; do case "$a" in fixture:*|suite:*) paths+=("${a#*:}") ;; esac; done < <(extract_anchors)
  # Diff against the WORKING TREE (not HEAD): this catches uncommitted soundness
  # edits locally, and equals `S..HEAD` in CI where the tree is clean.
  local changed; changed="$(git diff --name-only "$S" -- "${paths[@]}" 2>/dev/null || true)"
  if [ -n "$changed" ]; then
    # was the stamp advanced in the same set of changes?
    if ! git diff "$S" -- "$LEDGER" 2>/dev/null | grep -qE '^[+-]:ground-truth-sha:'; then
      note "ERROR (property 4): soundness paths changed since the stamp ${S}, but :ground-truth-sha: was not advanced in this change:"
      printf '%s\n' "$changed" | sed 's/^/    /' >&2
      note "    Advance :ground-truth-sha: to the commit you verified against (and --reseal if anchors changed)."
      return 1
    fi
  fi
  # Freshness vs main (best-effort; skipped if origin/main is absent, e.g. a
  # shallow CI checkout): if this branch is BEHIND main on a soundness path, the
  # ledger may be stale relative to a change not yet rebased in. Fires only when
  # main actually moved a soundness path under you — never for a fresh branch.
  if git rev-parse --verify -q origin/main >/dev/null 2>&1; then
    local mb; mb="$(git merge-base HEAD origin/main 2>/dev/null || true)"
    if [ -n "$mb" ]; then
      local behind; behind="$(git diff --name-only "$mb" origin/main -- "$LEDGER" "${SOUNDNESS_LIB_PATHS[@]}" 2>/dev/null || true)"
      if [ -n "$behind" ]; then
        note "ERROR (property 4): main moved a soundness path under this branch; rebase onto origin/main and re-verify the ledger:"
        printf '%s\n' "$behind" | sed 's/^/    /' >&2
        return 1
      fi
    fi
  fi
}

# ---- Property 5: pin-liveness (xfail harness) ------------------------------
# pinned/open rows -> their `test_*_xfail` pin id; NOPIN if a pinned row has none.
pinned_row_pins() {
  table_region | awk '
    /^\| Issue/ {next}
    /^\| #/ { emit(); row=$0; started=1; next }
    started && /^\|===/ { emit(); started=0; next }
    started { row = row "\n" $0 }
    END { emit() }
    function emit() {
      if (row=="") return
      # Match the STATUS CELL (a line that is `| `status``), not a prose mention
      # of the status word elsewhere in the row.
      if (row ~ /\n\| `residual \(pinned\)`/ || row ~ /\n\| `open \(tracked\)`/) {
        if (match(row, /test_[A-Za-z0-9_]+_xfail/)) print substr(row, RSTART, RLENGTH)
        else if (match(row, /test_[A-Za-z0-9_]+/)) print substr(row, RSTART, RLENGTH)
        else print "NOPIN"
      }
      row=""
    }'
}

check_pins() {
  local pins; pins="$(pinned_row_pins)"
  [ -n "$pins" ] || { note "ERROR (property 5): no 'residual (pinned)'/'open (tracked)' rows found (fail closed)"; return 1; }
  printf '%s\n' "$pins" | grep -q '^NOPIN$' && { note "ERROR (property 5): a pinned/open row names no test_* pin (fail closed)"; return 1; }
  dune build "$XFAIL_EXE" >/dev/null 2>&1 || { note "ERROR (property 5): cannot build ${XFAIL_EXE}"; return 1; }
  local report; report="$(AFFINE_FIXTURES="$PWD/test/e2e/fixtures" "_build/default/${XFAIL_EXE}" 2>&1 || true)"
  local pin r=0
  while IFS= read -r pin; do
    [ -z "$pin" ] && continue
    if printf '%s\n' "$report" | grep -qE "^XFAIL-OK ${pin}( |$)"; then
      :
    elif printf '%s\n' "$report" | grep -qE "^XPASS ${pin}( |$)"; then
      note "ALARM (property 5): pin ${pin} is PASSING — the hole may be fixed. Open ${LEDGER} and update the row to 'fixed' (do NOT just silence the pin)."
      r=1
    else
      note "ERROR (property 5): pin ${pin} (named by a pinned/open row) is not a live XFAIL-OK pin in ${XFAIL_EXE} (XERROR or absent). Harness report:"
      printf '%s\n' "$report" | sed 's/^/    /' >&2
      r=1
    fi
  done < <(printf '%s\n' "$pins")
  return $r
}

check_anchors_exist   || fail=1
check_backlinks       || fail=1
check_content_binding || fail=1
check_stamp           || fail=1
check_pins            || fail=1

if [ "$fail" -ne 0 ]; then
  note ""
  note "Soundness-ledger gate FAILED. See ${LEDGER} (authoritative) and the"
  note "'What this gate enforces' header of this script."
  exit 1
fi
echo "OK: soundness ledger — all 5 properties hold (anchors exist + back-links + content-bound + stamp-fresh + pins live)."
