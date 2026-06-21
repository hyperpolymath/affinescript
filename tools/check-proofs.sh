#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Proof-check harness for the AffineScript repository.
#
# Re-runs every mechanised proof in-tree against its proof assistant and
# fails on (a) a type-check error or (b) any "dangerous" escape hatch
# (believe_me / assert_total / postulate / sorry / axiom / native_decide
# / idris_crash). Unfinished proofs (Idris2 `?` holes) are reported as a
# WARNING, not a failure, so the harness can run during development.
#
# This is the backing for the `just proof-check-*` recipes referenced by
# the PROOF-STATUS templates; before this script those recipes had no
# implementation. Run from anywhere; it cd's to the repo root.
#
# Coverage:
#   --idris2  Solo-core QTT metatheory (docs/academic/formal-verification/solo-core)
#   --lean    Tropical session types  (docs/academic/tropical-session-types)
#   --agda    Echo-boundary certificates (proposals/**), iff the external
#             echo-types proofs + agda-stdlib are reachable (see env vars).
#   --all     all of the above (default)
#
# Agda external dependencies (the proofs import modules that live OUTSIDE
# this repo, so they cannot be checked hermetically here):
#   AFFINESCRIPT_ECHO_TYPES_DIR  -> <echo-types>/proofs/agda
#   AGDA_STDLIB                  -> <agda-stdlib>/src   (matching the agda in PATH)
# If either is unset/missing the Agda stage SKIPS with a clear message
# (exit-neutral), it does not fail the run.

set -uo pipefail

cd "$(dirname "$0")/.."
REPO="$(pwd)"

GREEN=$'\033[32m'; RED=$'\033[31m'; YEL=$'\033[33m'; DIM=$'\033[2m'; RST=$'\033[0m'
pass=0; fail=0; skip=0

ok ()   { printf '%s  PASS%s  %s\n' "$GREEN" "$RST" "$1"; pass=$((pass+1)); }
bad ()  { printf '%s  FAIL%s  %s\n' "$RED"   "$RST" "$1"; fail=$((fail+1)); }
warn () { printf '%s  WARN%s  %s\n' "$YEL"   "$RST" "$1"; }
note () { printf '%s  SKIP%s  %s\n' "$DIM"   "$RST" "$1"; skip=$((skip+1)); }

# Dangerous escape hatches that must never appear in a finished proof.
# Comment lines (-- … / // … / -- in Agda, Idris, Lean) are stripped first.
DANGER='believe_me|really_believe_me|assert_total|assert_smaller|idris_crash|postulate|\bsorry\b|\baxiom\b|native_decide'

scan_danger () {  # <glob-root> <ext...>; prints offending lines, returns 1 if any
  local root="$1"; shift
  local found=0 f hits
  while IFS= read -r f; do
    # strip whole-line comments (Idris/Agda --, Lean --) before scanning
    hits=$(grep -nE "$DANGER" "$f" 2>/dev/null | grep -vE ':\s*(--|//)' || true)
    if [ -n "$hits" ]; then echo "      $f:"; echo "$hits" | sed 's/^/        /'; found=1; fi
  done < <(for e in "$@"; do find "$root" -name "*.$e" 2>/dev/null; done)
  return $found
}

# ─────────────────────────────────────────────────────────────────────────────
check_idris2 () {
  local dir="$REPO/docs/academic/formal-verification/solo-core"
  command -v idris2 >/dev/null 2>&1 || { note "Idris2 (idris2 not on PATH)"; return; }
  echo "── Idris2: Solo-core QTT metatheory ──"
  # Soundness.idr transitively builds Quantity/Syntax/Context/ContextLemmas/Typing/Subst.
  if ( cd "$dir" && idris2 --check Soundness.idr ) >/tmp/.pc_idris 2>&1; then
    ok "idris2 --check Soundness.idr (+ all imports)"
  else
    bad "idris2 --check Soundness.idr"; sed 's/^/      /' /tmp/.pc_idris | tail -25
  fi
  if scan_danger "$dir" idr; then ok "no dangerous primitives (Idris2)"; else bad "dangerous primitive in Idris2 proof"; fi
  # holes are incompleteness, not unsoundness → warn only
  local holes
  holes=$(grep -rnE '\?[a-zA-Z_][a-zA-Z0-9_]*' "$dir"/*.idr 2>/dev/null | grep -vE ':\s*--' || true)
  [ -n "$holes" ] && { warn "unfinished holes remain:"; echo "$holes" | sed 's/^/        /'; }
}

# ─────────────────────────────────────────────────────────────────────────────
check_lean () {
  local dir="$REPO/docs/academic/tropical-session-types"
  command -v lean >/dev/null 2>&1 || { note "Lean (lean not on PATH)"; return; }
  echo "── Lean 4: Tropical session types ──"
  if ( cd "$dir" && lean TropicalSessionTypes.lean ) >/tmp/.pc_lean 2>&1; then
    ok "lean TropicalSessionTypes.lean"
  else
    bad "lean TropicalSessionTypes.lean"; sed 's/^/      /' /tmp/.pc_lean | tail -25
  fi
  if scan_danger "$dir" lean; then ok "no dangerous tactics (Lean)"; else bad "dangerous tactic in Lean proof"; fi
}

# ─────────────────────────────────────────────────────────────────────────────
check_agda () {
  command -v agda >/dev/null 2>&1 || { note "Agda (agda not on PATH)"; return; }
  local echo_dir="${AFFINESCRIPT_ECHO_TYPES_DIR:-}"
  local stdlib="${AGDA_STDLIB:-}"
  echo "── Agda: echo-boundary certificates ──"
  if [ -z "$echo_dir" ] || [ ! -d "$echo_dir" ]; then
    note "Agda (set AFFINESCRIPT_ECHO_TYPES_DIR to <echo-types>/proofs/agda)"; return; fi
  if [ -z "$stdlib" ] || [ ! -f "$stdlib/Data/Nat/Base.agda" ]; then
    note "Agda (set AGDA_STDLIB to the agda-stdlib 'src' dir)"; return; fi
  local self="$REPO/proposals/echo-types"
  export AGDA_DIR; AGDA_DIR="$(mktemp -d)"; trap 'rm -rf "$AGDA_DIR"' RETURN
  # only delete .agdai we create
  local snap; snap="$(mktemp)"
  find "$REPO/proposals" "$echo_dir" "$stdlib" -name '*.agdai' 2>/dev/null | sort >"$snap"
  local agda_fail=0 f dir
  while IFS= read -r f; do
    dir="$(dirname "$f")"
    if agda --no-libraries -i "$stdlib" -i "$echo_dir" -i "$self" -i "$dir" "$f" >/dev/null 2>&1; then :; else
      echo "      FAIL $f"; agda_fail=1; fi
  done < <( { echo "$REPO/proposals/echo-types/EchoEncodingFaithfulness.agda";
              find "$REPO/proposals/nextgen-evangelist/echo-boundary/samples" -name '*.agda';
              find "$REPO/proposals/idaptik/migrated" -name '*Boundary.agda'; } | sort -u )
  [ "$agda_fail" -eq 0 ] && ok "all in-repo .agda boundary proofs type-check" || bad "an Agda boundary proof failed"
  if scan_danger "$REPO/proposals" agda; then ok "no dangerous primitives (Agda)"; else bad "dangerous primitive in Agda proof"; fi
  find "$REPO/proposals" "$echo_dir" "$stdlib" -name '*.agdai' 2>/dev/null | sort >"$snap.after"
  comm -13 "$snap" "$snap.after" | while IFS= read -r a; do [ -n "$a" ] && rm -f "$a"; done
  rm -f "$snap" "$snap.after"
}

# ─────────────────────────────────────────────────────────────────────────────
case "${1:---all}" in
  --idris2) check_idris2 ;;
  --lean)   check_lean ;;
  --agda)   check_agda ;;
  --all)    check_idris2; echo; check_lean; echo; check_agda ;;
  *) echo "usage: $0 [--idris2|--lean|--agda|--all]" >&2; exit 2 ;;
esac

echo
printf '%s passed, %s failed, %s skipped\n' "$pass" "$fail" "$skip"
[ "$fail" -eq 0 ]
