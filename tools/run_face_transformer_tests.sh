#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell

# Face-transformer regression tests.
#
# For each of the five non-canonical face examples in examples/faces/, run
# the corresponding `preview-*` command to capture the canonical lowering,
# then diff it against a committed snapshot in tests/faces/. Also run
# `parse` on every example (canonical included) to confirm the resulting
# canonical text is syntactically valid.
#
# Modes:
#   default              compare against committed snapshot; fail on diff.
#   --update             overwrite snapshots with current output (intentional change).
#   --record-missing     record any missing snapshot; never fail on missing.
#                        Useful for the first run after introducing a face.
#
# Exit 0 on success, 1 on any diff/parse failure (default mode).

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
EXAMPLES_DIR="$ROOT_DIR/examples/faces"
SNAPSHOT_DIR="$ROOT_DIR/tests/faces"

mode="check"
for arg in "$@"; do
  case "$arg" in
    --update)         mode="update" ;;
    --record-missing) mode="record-missing" ;;
    -h|--help)
      sed -n '3,/^$/p' "$0"
      exit 0
      ;;
    *)
      echo "unknown flag: $arg" >&2
      exit 2
      ;;
  esac
done

# Resolve the compiler entry point in the same priority order as
# tools/run_codegen_wasm_tests.sh so this works locally and in CI.
if [ -x "$ROOT_DIR/_build/default/bin/main.exe" ]; then
  AS=("$ROOT_DIR/_build/default/bin/main.exe")
elif command -v affinescript >/dev/null 2>&1; then
  AS=("affinescript")
else
  AS=(dune exec affinescript --)
fi

mkdir -p "$SNAPSHOT_DIR"

# face,example_basename,preview_subcommand
SPECS=(
  "rattle:hello-rattle:preview-python"
  "jaffa:hello-jaffa:preview-js"
  "pseudo:hello-pseudo:preview-pseudocode"
  "lucid:hello-lucid:preview-lucid"
  "cafe:hello-cafe:preview-cafe"
)

failures=0
recorded=0
checked=0

run_preview() {
  local subcmd="$1" path="$2"
  "${AS[@]}" "$subcmd" "$path"
}

run_parse() {
  local path="$1"
  # Suppress AST output; we only care about exit status.
  "${AS[@]}" parse "$path" >/dev/null
}

# Canonical baseline: just confirm it parses.
echo "[parse] hello-canonical.affine"
if ! run_parse "$EXAMPLES_DIR/hello-canonical.affine"; then
  echo "  FAIL: canonical example does not parse" >&2
  failures=$((failures + 1))
else
  checked=$((checked + 1))
fi

for spec in "${SPECS[@]}"; do
  IFS=":" read -r face base subcmd <<< "$spec"
  src="$EXAMPLES_DIR/${base}.affine"
  expected="$SNAPSHOT_DIR/${base}.expected.txt"

  echo
  echo "[${face}] $base.affine  (subcommand: $subcmd)"

  if [ ! -f "$src" ]; then
    echo "  FAIL: example missing: $src" >&2
    failures=$((failures + 1))
    continue
  fi

  # 1. Capture the canonical lowering via preview-*.
  actual="$(mktemp)"
  if ! run_preview "$subcmd" "$src" > "$actual" 2>/dev/null; then
    echo "  FAIL: preview command exited non-zero" >&2
    rm -f "$actual"
    failures=$((failures + 1))
    continue
  fi

  # 2. Snapshot logic.
  case "$mode" in
    update)
      cp "$actual" "$expected"
      echo "  recorded snapshot: $expected"
      recorded=$((recorded + 1))
      ;;
    record-missing)
      if [ ! -f "$expected" ]; then
        cp "$actual" "$expected"
        echo "  recorded missing snapshot: $expected"
        recorded=$((recorded + 1))
      else
        if diff -u "$expected" "$actual" >/dev/null; then
          echo "  snapshot OK"
          checked=$((checked + 1))
        else
          echo "  FAIL: snapshot diff (use --update to accept changes)" >&2
          diff -u "$expected" "$actual" || true
          failures=$((failures + 1))
        fi
      fi
      ;;
    check)
      if [ ! -f "$expected" ]; then
        echo "  FAIL: missing snapshot $expected" >&2
        echo "        run with --record-missing or --update to create it" >&2
        failures=$((failures + 1))
      elif diff -u "$expected" "$actual" >/dev/null; then
        echo "  snapshot OK"
        checked=$((checked + 1))
      else
        echo "  FAIL: snapshot diff (use --update to accept changes)" >&2
        diff -u "$expected" "$actual" || true
        failures=$((failures + 1))
      fi
      ;;
  esac

  # 3. Parse the example (auto-detects face via pragma) — confirms the
  #    transformer output is syntactically valid canonical AffineScript.
  if ! run_parse "$src"; then
    echo "  FAIL: lowered text did not parse as canonical AffineScript" >&2
    failures=$((failures + 1))
  fi

  rm -f "$actual"
done

echo
echo "─────────────────────────────────────────────"
case "$mode" in
  update)
    echo "Recorded $recorded snapshot(s); review and commit."
    ;;
  record-missing)
    if [ "$recorded" -gt 0 ]; then
      echo "Recorded $recorded missing snapshot(s); checked $checked."
      echo "Review the new snapshots and commit them."
    else
      echo "Checked $checked snapshot(s); no missing snapshots."
    fi
    ;;
  check)
    echo "Checked $checked snapshot(s)."
    ;;
esac

if [ "$failures" -gt 0 ]; then
  echo "FAILED: $failures issue(s)" >&2
  exit 1
fi

echo "All face transformer tests passed."
