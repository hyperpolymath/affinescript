#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
#
# Userscript target acceptance runner.
#
# Builds test/e2e/userscript/probe.affine with the userscript target and
# checks the contract that makes the artefact shippable:
#
#   1. one ES2020 IIFE file — no import/export/dynamic import/eval/top-level
#      await/require, one capsule, strict mode, within the size budget;
#   2. `--target userscript`, `--target iife` and `--iife` are the same build;
#   3. `--banner FILE` prepends a complete ==UserScript== block, and every
#      malformed banner is refused with E0827 instead of being pasted in;
#   4. `--wasm-gc` is refused (no module to emit), with E0827 in --json mode;
#   5. reachable-but-unlowerable host calls and leaked affine handles fail the
#      build loudly (negatives/);
#   6. the compiled script runs in a page: appends a div, registers a menu
#      command, round-trips a setting, and removes the div on teardown with no
#      listener left behind — under `@grant none`, under `@grant GM_*`, and on
#      a page that denies storage.
#
# Fail-late by design: every check runs and is reported, so one red check
# cannot hide the state of the checks behind it. Failures echo `::error::`
# lines so they survive into CI annotations.
#
# Runtime engine: `playwright` (headless Chromium, pinned by
# affinescript-tea/e2e — the same install CI already performs) or `jsdom`
# (no browser download needed) is required for check 6; the static checks and
# every refusal check need no engine at all. With neither engine present the
# script prints a loud SKIP for check 6 and still exits 0, unless
# AFFINESCRIPT_E2E_STRICT=1, which turns a missing engine into a failure (CI
# sets it).
set -uo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../../.." && pwd)"
FIXTURE_DIR="$ROOT_DIR/test/e2e/userscript"
OUT_DIR="$ROOT_DIR/_build/userscript-e2e"

if [[ -x "$ROOT_DIR/_build/default/bin/main.exe" ]]; then
  COMPILE_CMD=("$ROOT_DIR/_build/default/bin/main.exe" compile)
elif command -v affinescript >/dev/null 2>&1; then
  COMPILE_CMD=(affinescript compile)
else
  COMPILE_CMD=(dune exec affinescript -- compile)
fi

# Bun first (repo policy), Node only as the historical e2e fallback.
if command -v bun >/dev/null 2>&1; then
  JS_RUN=(bun)
elif command -v node >/dev/null 2>&1; then
  JS_RUN=(node)
else
  JS_RUN=()
fi

# One pinned Playwright for the whole repo; jsdom may come from the caller.
E2E_MODULES="$ROOT_DIR/affinescript-tea/e2e/node_modules"
export AFFINESCRIPT_E2E_MODULES="${AFFINESCRIPT_E2E_MODULES:-$E2E_MODULES}"

STRICT="${AFFINESCRIPT_E2E_STRICT:-0}"

failures=()
fail() {
  failures+=("$1")
  echo "::error::$1" >&2
}
note() { echo "    $*"; }

mkdir -p "$OUT_DIR"
src="$FIXTURE_DIR/probe.affine"
out="$OUT_DIR/probe.user.js"
rm -f "$out"

# ── 1. Compile ──────────────────────────────────────────────────────────────
echo "userscript target: compiling $(basename "$src")"
compile_log="$("${COMPILE_CMD[@]}" "$src" --target userscript -o "$out" 2>&1)"
compile_status=$?
if [[ $compile_status -ne 0 ]]; then
  fail "compile failed for $(basename "$src")"
  note "${compile_log//$'\n'/$'\n'    }"
elif [[ ! -s "$out" ]]; then
  fail "compile reported success but produced no artefact"
else
  note "$compile_log"
  if [[ "$compile_log" != *"(userscript,"*"bytes)"* ]]; then
    fail "compile did not report the userscript byte count"
  fi
fi

bytes=0
if [[ -s "$out" ]]; then
  bytes=$(wc -c <"$out" | tr -d ' ')
  note "artefact: $(basename "$out") — $bytes bytes"
  if command -v node >/dev/null 2>&1; then
    if node --check "$out" >/dev/null 2>&1; then
      note "node --check: ok"
    else
      fail "node --check rejected $(basename "$out")"
    fi
  fi
fi

# ── 2. --target userscript == --target iife == --iife ───────────────────────
echo "userscript target: target/alias equivalence"
if [[ -s "$out" ]]; then
  for alias in "--target iife" "--iife"; do
    # shellcheck disable=SC2086 # deliberate word splitting of the alias form
    "${COMPILE_CMD[@]}" "$src" $alias -o "$OUT_DIR/alias.user.js" >/dev/null 2>&1
    if [[ ! -s "$OUT_DIR/alias.user.js" ]]; then
      fail "\`$alias\` produced no artefact"
    elif cmp -s "$out" "$OUT_DIR/alias.user.js"; then
      note "$alias: byte-identical"
    else
      fail "\`$alias\` output differs from \`--target userscript\`"
    fi
  done
fi

# ── 3. --banner: happy path and refusals ────────────────────────────────────
echo "userscript target: --banner"
banner_out="$OUT_DIR/banner.user.js"
rm -f "$banner_out"
if "${COMPILE_CMD[@]}" "$src" --target userscript --banner "$FIXTURE_DIR/banner.txt" -o "$banner_out" >/dev/null 2>&1; then
  if head -1 "$banner_out" | grep -q '^// ==UserScript==$'; then
    note "banner is the first line"
  else
    fail "banner is not the first line of the artefact"
  fi
  if grep -q '^// ==/UserScript==$' "$banner_out"; then
    note "banner closure present verbatim"
  else
    fail "banner closure missing from the artefact"
  fi
  if grep -q '^// @grant       none$' "$banner_out"; then
    note "banner grants preserved verbatim"
  else
    fail "banner @grant line was rewritten"
  fi
  if grep -q '(function () {' "$banner_out"; then
    note "capsule follows the banner"
  else
    fail "no capsule after the banner"
  fi
  if command -v node >/dev/null 2>&1 && ! node --check "$banner_out" >/dev/null 2>&1; then
    fail "bannered artefact is not valid JS"
  fi
else
  fail "--banner banner.txt was refused"
fi

for bad in banner_no_opener.txt banner_closes_first.txt; do
  if "${COMPILE_CMD[@]}" "$src" --target userscript --banner "$FIXTURE_DIR/$bad" \
      -o "$OUT_DIR/bad-banner.user.js" >/dev/null 2>&1; then
    fail "malformed banner $bad was accepted"
  else
    note "$bad: refused"
  fi
done

# ── 4. --wasm-gc conflict, with E0827 in --json ─────────────────────────────
echo "userscript target: --wasm-gc refusal"
wasm_json="$OUT_DIR/wasm-gc.json"
# --json writes the diagnostics document to stderr (shared emitter, unchanged
# by this target), so both streams are captured before grepping for the code.
if "${COMPILE_CMD[@]}" "$src" --target userscript --wasm-gc --json \
     -o "$OUT_DIR/never.js" >"$wasm_json" 2>&1; then
  fail "--wasm-gc combined with --target userscript was accepted"
elif grep -q '"code":"E0827"' "$wasm_json"; then
  note "--wasm-gc refused with E0827"
else
  fail "--wasm-gc refusal did not carry E0827 (looked in $wasm_json)"
fi

# ── 5. Negative controls ────────────────────────────────────────────────────
echo "userscript target: negative controls"
neg_out="$OUT_DIR/neg.js"
neg_log="$OUT_DIR/neg.log"
if "${COMPILE_CMD[@]}" "$FIXTURE_DIR/negatives/missing_binding.affine" \
     --target userscript -o "$neg_out" >"$neg_log" 2>&1; then
  fail "an extern with no userscript binding was accepted"
else
  if grep -q 'unsupported .extern fn us_not_a_capability' "$neg_log"; then
    note "unlowerable extern: named in the diagnostic"
  else
    fail "unlowerable extern: diagnostic does not name the construct"
  fi
  if grep -q 'missing_binding.affine' "$neg_log"; then
    note "unlowerable extern: module named in the diagnostic"
  else
    fail "unlowerable extern: diagnostic does not name the module"
  fi
  if grep -qi 'import/export\|non-module' "$neg_log"; then
    note "unlowerable extern: shape hint attached"
  else
    fail "unlowerable extern: no shape hint in the diagnostic"
  fi
fi

if "${COMPILE_CMD[@]}" "$FIXTURE_DIR/negatives/handle_leak.affine" \
     --target userscript -o "$neg_out" >"$neg_log" 2>&1; then
  fail "a leaked @linear handle was accepted"
elif grep -q "@linear" "$neg_log"; then
  note "leaked handle: rejected by the quantity checker"
else
  fail "leaked handle: rejected without a @linear diagnostic"
fi

# ── 5b. Entry-reachability pruning ──────────────────────────────────────────
echo "userscript target: entry reachability"
reach_out="$OUT_DIR/reachability.user.js"
rm -f "$reach_out"
if "${COMPILE_CMD[@]}" "$FIXTURE_DIR/reachability.affine" --target userscript \
     -o "$reach_out" >/dev/null 2>&1; then
  if grep -q 'reachability:live' "$reach_out"; then
    note "live path kept"
  else
    fail "reachability: the live path was pruned away"
  fi
  if grep -q 'dead-code-marker' "$reach_out"; then
    fail "reachability: an unreachable top-level fn was emitted"
  else
    note "dead top-level fn pruned"
  fi
  if grep -q '__as_gmMenu\|GM_registerMenuCommand' "$reach_out"; then
    fail "reachability: unused manager runtime section emitted"
  else
    note "unused manager runtime section pruned"
  fi
  note "reachability artefact: $(wc -c <"$reach_out" | tr -d ' ') bytes"
else
  fail "reachability fixture failed to compile"
fi

# ── 6. Static artefact checks (no engine needed) ────────────────────────────
echo "userscript target: static artefact checks"
if [[ ${#JS_RUN[@]} -eq 0 ]]; then
  fail "no JavaScript runtime (bun or node) available for the static checks"
elif [[ ! -s "$out" ]]; then
  fail "static checks skipped: no artefact"
else
  if ! "${JS_RUN[@]}" "$FIXTURE_DIR/harness.mjs" "$out" --static-only; then
    fail "static artefact checks failed"
  fi
fi

# ── 7. Runtime fixture in a page ────────────────────────────────────────────
echo "userscript target: runtime fixture"
if [[ ${#JS_RUN[@]} -eq 0 || ! -s "$out" ]]; then
  fail "runtime fixture skipped: no JS runtime or no artefact"
else
  runtime_log="$OUT_DIR/runtime.log"
  "${JS_RUN[@]}" "$FIXTURE_DIR/harness.mjs" "$out" >"$runtime_log" 2>&1
  runtime_status=$?
  case $runtime_status in
    0)
      grep -E '^(userscript harness|all userscript fixture checks passed)' "$runtime_log" | sed 's/^/    /'
      grep -c '  ok ' "$runtime_log" | sed 's/^/    checks passed: /'
      ;;
    3)
      echo "SKIP: no userscript runtime engine (playwright or jsdom) found." >&2
      echo "SKIP: install the pinned engine with \`bun install\` in affinescript-tea/e2e," >&2
      echo "SKIP: or provide jsdom via AFFINESCRIPT_E2E_MODULES=<node_modules dir>." >&2
      echo "SKIP: the static and refusal checks above still ran." >&2
      if [[ "$STRICT" = "1" ]]; then
        fail "runtime fixture did not run and AFFINESCRIPT_E2E_STRICT=1"
      fi
      ;;
    *)
      fail "runtime fixture failed (engine log: $runtime_log)"
      tail -40 "$runtime_log" | sed 's/^/    /' >&2
      ;;
  esac
fi

# ── Summary ─────────────────────────────────────────────────────────────────
echo
if [[ ${#failures[@]} -gt 0 ]]; then
  echo "userscript target: ${#failures[@]} check(s) failed" >&2
  for f in "${failures[@]}"; do echo "  - $f" >&2; done
  exit 1
fi
echo "userscript target: all checks passed (artefact: $bytes bytes)"
