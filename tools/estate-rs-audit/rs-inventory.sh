#!/usr/bin/env bash
# #229 step 2: per-file ReScript-surface construct inventory.
# Scans file TEXT (the oracle stops at the first parse error and hides the
# full RS inventory). Constructs = the set named in issue #229.
set -u
BASE=/home/hyperpolymath/dev/repos/.affine-audit
CL="$BASE/clones"
INV="$BASE/rs-inventory.tsv"
: > "$INV"
echo -e "repo\trelpath\tn_constructs\tconstructs" >> "$INV"

# scan one file, emit comma-list of detected RS constructs
scan() {
  local f="$1" hits=()
  grep -qE -- '\barray<'                 "$f" && hits+=("array<T>")
  grep -qE -- '\b(option|result|promise|Js\.[A-Za-z]+)<' "$f" && hits+=("rs-generic<>")
  grep -qE -- '\bDict\.t\b|\bDict\.(make|get|set|fromArray)\b' "$f" && hits+=("Dict.t")
  grep -qE -- '\bmutable[[:space:]]'      "$f" && hits+=("mutable-field")
  grep -qE -- '\btype[[:space:]]+rec\b'   "$f" && hits+=("type-rec")
  grep -qE -- '^[[:space:]]*open[[:space:]]+[A-Z]' "$f" && hits+=("open-Mod")
  grep -qE -- '%%?raw'                    "$f" && hits+=("%%raw")
  grep -qE -- '\(~[a-z_]'                 "$f" && hits+=("labelled-(~x)")
  grep -qE -- '\bList\('                  "$f" && hits+=("List(X)")
  grep -qE -- '\bJSON\.t\b'               "$f" && hits+=("JSON.t")
  grep -qE -- '\bimport\b.*\bas\b'        "$f" && hits+=("import-as")
  grep -qE -- '\bBelt\.|\bJs\.|\bRescript' "$f" && hits+=("rs-stdlib")
  grep -qE -- '->[[:space:]]*[A-Za-z].*=>|\b->\b' "$f" && true  # fn-type arrows are valid AS; skip
  ( IFS=,; echo "${hits[*]:-}" )
}

# iterate DRIFT-SYNTAX + TYPE-ONLY rows from the authoritative re-audit
awk -F'\t' '$3=="DRIFT-SYNTAX" || $3=="TYPE-ONLY"{print $1"\t"$2}' "$BASE/results.post228.tsv" \
| while IFS=$'\t' read -r repo rel; do
    f="$CL/$repo/$rel"
    [ -f "$f" ] || continue
    cl=$(scan "$f")
    if [ -n "$cl" ]; then
      n=$(awk -F, '{print NF}' <<<"$cl")
      echo -e "$repo\t$rel\t$n\t$cl" >> "$INV"
    else
      echo -e "$repo\t$rel\t0\t<no-RS-construct:non-RS-drift>" >> "$INV"
    fi
  done

echo "=== inventory written: $INV ($(wc -l <"$INV") rows) ==="
echo "=== files WITH RS surface, by repo (excluding affinescript own corpus) ==="
awk -F'\t' 'NR>1 && $4!="<no-RS-construct:non-RS-drift>" && $1!="affinescript"{print $1}' "$INV" \
  | sort | uniq -c | sort -rn
echo "=== non-RS drift (NOT #229 scope), by repo ==="
awk -F'\t' 'NR>1 && $4=="<no-RS-construct:non-RS-drift>"{print $1}' "$INV" \
  | sort | uniq -c | sort -rn
echo "=== construct frequency (estate, excl. affinescript) ==="
awk -F'\t' 'NR>1 && $1!="affinescript" && $4!="<no-RS-construct:non-RS-drift>"{n=split($4,a,",");for(i=1;i<=n;i++)print a[i]}' "$INV" \
  | sort | uniq -c | sort -rn
