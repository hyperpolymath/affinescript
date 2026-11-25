#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
#
# Emit the Proven-Tests run report for the lexer suite and validate it with
# the framework's own Rust checker, proven-evidence (hyperpolymath/
# proven-tests-and-benches, pinned below). See VENDOR.adoc#proven-tests.
#
# Usage: tools/ci/proven/proven-report.sh [OUT_DIR]   (default _build/proven)
#   PROVEN_EVIDENCE=/path/to/proven-evidence skips the fetch and build.
#
# Exit codes follow the contract: 0 conforms, 1 does not conform (or a test
# did not pass), 2 no check was performed (a tool or input was missing).
set -euo pipefail

readonly pin_repo='https://github.com/hyperpolymath/proven-tests-and-benches.git'
readonly pin_commit='f90bacbe3a5fa1869239fcb9175757509464aa13'
readonly crate='integrations/contract/proven-tests/downstream/rust/proven-evidence'

out="${1:-_build/proven}"
mkdir -p "$out"
out="$(cd "$out" && pwd)"

checker="${PROVEN_EVIDENCE:-}"
if [[ -z "$checker" ]]; then
  for tool in git cargo; do
    command -v "$tool" >/dev/null 2>&1 || { echo "proven-report: '$tool' not found" >&2; exit 2; }
  done
  src="$out/proven-tests-and-benches"
  if [[ ! -d "$src/.git" ]]; then
    git init -q "$src"
    git -C "$src" remote add origin "$pin_repo"
  fi
  git -C "$src" fetch -q --depth 1 origin "$pin_commit" || { echo "proven-report: cannot fetch $pin_commit" >&2; exit 2; }
  git -C "$src" checkout -q --detach FETCH_HEAD
  [[ "$(git -C "$src" rev-parse HEAD)" == "$pin_commit" ]] || { echo "proven-report: pin mismatch" >&2; exit 2; }
  cargo build -q --release --locked --manifest-path "$src/$crate/Cargo.toml" || exit 2
  checker="$src/$crate/target/release/proven-evidence"
fi
[[ -x "$checker" ]] || { echo "proven-report: no proven-evidence at $checker" >&2; exit 2; }

report="$out/proven-run-report.json"
summary="$out/proven-summary.json"
rc=0
PROVEN_REPORT="$report" dune exec --no-print-directory test/test_main.exe >/dev/null || rc=$?
[[ -f "$report" ]] || { echo "proven-report: no report written" >&2; exit 2; }

"$checker" emit "$report" "$summary"
"$checker" check "$report" "$summary"
if [[ "$rc" -ne 0 ]]; then
  echo "proven-report: a lexer test did not pass (see above)" >&2
  exit 1
fi
echo "proven-report: $report conforms to the Proven-Tests contract (pinned $pin_commit)"
