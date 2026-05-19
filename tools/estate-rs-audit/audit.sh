#!/usr/bin/env bash
# Estate AffineScript dialect-conformance audit driver.
# Per repo: shallow-clone default branch, run the oracle `check` on every
# .affine, classify on stdout text (rc is always 0).
#
# Durable home (survives /tmp wipes): /home/hyperpolymath/dev/repos/.affine-audit
# Destined for affinescript tools/ as #229 campaign infrastructure.
#
# Env overrides:
#   ORC   path to a built affinescript main.exe (the oracle)
#   STDL  path to the matching stdlib dir
set -u
ORC=${ORC:-/home/hyperpolymath/dev/repos/affinescript/_build/default/bin/main.exe}
export AFFINESCRIPT_STDLIB=${STDL:-/home/hyperpolymath/dev/repos/affinescript/stdlib}
BASE=/home/hyperpolymath/dev/repos/.affine-audit
WS="$BASE/clones"
OUT="$BASE/results.tsv"
REPOS="$BASE/affine_repos.txt"
mkdir -p "$WS"
: > "$OUT"

while read -r NWO; do
  [ -z "$NWO" ] && continue
  case "$NWO" in '{'*|*'rate limit'*) continue;; esac   # skip stray API-error lines
  REPO=${NWO#hyperpolymath/}
  D="$WS/$REPO"
  if [ ! -d "$D/.git" ]; then
    git clone --depth 1 --quiet "https://github.com/$NWO.git" "$D" 2>/dev/null \
      || { echo -e "$REPO\t<clone-failed>\tCLONE-FAIL\t-" >> "$OUT"; continue; }
  fi
  SHA=$(git -C "$D" rev-parse --short HEAD 2>/dev/null)
  mapfile -t FILES < <(find "$D" -name '*.affine' -type f 2>/dev/null | sort)
  if [ "${#FILES[@]}" -eq 0 ]; then
    echo -e "$REPO\t<none>\tNO-AFFINE\t$SHA" >> "$OUT"; continue
  fi
  for F in "${FILES[@]}"; do
    REL=${F#$D/}
    MSG=$("$ORC" check "$F" 2>&1 | head -1 | tr '\t' ' ' | cut -c1-180)
    if echo "$MSG" | grep -q "Type checking passed"; then
      CLASS=PASS
    elif echo "$MSG" | grep -qiE "parse error|Syntax error"; then
      CLASS=DRIFT-SYNTAX
    else
      CLASS=TYPE-ONLY
    fi
    echo -e "$REPO\t$REL\t$CLASS\t$MSG" >> "$OUT"
  done
done < "$REPOS"

echo "AUDIT-COMPLETE $(date -u +%Y-%m-%dT%H:%M:%SZ)" >> "$OUT"
