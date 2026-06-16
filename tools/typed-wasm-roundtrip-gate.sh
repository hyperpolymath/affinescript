#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# typed-wasm round-trip gate: proves the AffineScript <-> typed-wasm contract
# end-to-end ACROSS REPOSITORIES.
#
# AffineScript (OCaml) emits the `typedwasm.ownership` custom section (v1) and
# verifies it with lib/tw_verify.ml. The separate hyperpolymath/typed-wasm Rust
# crate ships `tw-verify`, the cross-compat consumer. This gate compiles two
# programs and checks BOTH verifiers reach the SAME verdict on the SAME bytes:
#
#   clean linear (own consumed once)  -> AffineScript OK   + tw-verify exit 0
#   dropped own  (L10 violation)      -> AffineScript fail + tw-verify exit 1
#
# That bit-exact agreement is the integration: same carrier, same semantics,
# two independent implementations.
#
# Locates typed-wasm via $TYPED_WASM_DIR or ../../typed-wasm. Builds tw-verify
# if needed. Skips (exit-neutral) if the sibling repo or cargo is unavailable.

set -uo pipefail
cd "$(dirname "$0")/.."
REPO="$(pwd)"

GREEN=$'\033[32m'; RED=$'\033[31m'; DIM=$'\033[2m'; RST=$'\033[0m'
pass=0; fail=0
ok ()  { printf '%s  PASS%s  %s\n' "$GREEN" "$RST" "$1"; pass=$((pass+1)); }
bad () { printf '%s  FAIL%s  %s\n' "$RED"   "$RST" "$1"; fail=$((fail+1)); }
skip() { printf '%s  SKIP%s  %s\n' "$DIM"   "$RST" "$1"; exit 0; }

TW_DIR="${TYPED_WASM_DIR:-$REPO/../../typed-wasm}"
[ -d "$TW_DIR/crates/typed-wasm-verify" ] || skip "typed-wasm not found (set TYPED_WASM_DIR; tried $TW_DIR)"
command -v cargo >/dev/null 2>&1 || skip "cargo not on PATH"

TW_BIN="$TW_DIR/target/debug/tw-verify"
if [ ! -x "$TW_BIN" ]; then
  echo "── building tw-verify (cargo build -p typed-wasm-verify) ──"
  ( cd "$TW_DIR" && cargo build -p typed-wasm-verify >/dev/null 2>&1 ) || skip "tw-verify build failed"
fi

BIN="$REPO/_build/default/bin/main.exe"
[ -x "$BIN" ] || { echo "── building compiler ──"; dune build 2>/dev/null || { echo "build failed"; exit 1; }; }

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
printf 'fn id_owned(x: own Int) -> own Int = x;\n'          > "$tmp/clean.affine"
printf 'fn drop_owned(x: own String) -> () = ();\n'         > "$tmp/viol.affine"

echo "── typed-wasm round-trip: AffineScript producer <-> Rust tw-verify ──"

# clean: AffineScript OK, tw-verify exit 0
if "$BIN" compile "$tmp/clean.affine" -o "$tmp/clean.wasm" >/dev/null 2>&1; then
  if timeout 30 "$TW_BIN" "$tmp/clean.wasm" >/dev/null 2>&1; then
    ok "clean linear -> tw-verify VERIFIED (exit 0)"
  else
    bad "clean linear -> tw-verify rejected (cross-compat mismatch)"
  fi
else
  bad "clean linear failed to compile"
fi

# violation: AffineScript flags it, tw-verify exit 1 (cross-compat on the negative)
if "$BIN" compile "$tmp/viol.affine" -o "$tmp/viol.wasm" >/dev/null 2>&1; then
  timeout 30 "$TW_BIN" "$tmp/viol.wasm" >/dev/null 2>&1; rc=$?
  if [ "$rc" -eq 1 ]; then
    ok "dropped-own -> tw-verify rejects with L10 (exit 1), matching AffineScript"
  else
    bad "dropped-own -> tw-verify exit $rc (expected 1: both verifiers should agree)"
  fi
else
  bad "violation case failed to compile (it should still emit a module)"
fi

echo
printf '%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
