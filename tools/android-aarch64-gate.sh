#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Android aarch64 native-codegen spine gate for AffineScript.
#
# Pins the load-bearing claim of the native Android target (ADR-0024): an
# AffineScript program compiles, via the LLVM backend with the Android triple,
# to a real AArch64 relocatable object. This is the codegen spine; the .so
# link + APK packaging steps are NDK-gated and live downstream (see ADR-0024).
#
# Verified here:
#   1. `AFFINESCRIPT_LLVM_TRIPLE=aarch64-linux-android compile -> .ll` carries
#      that triple (target selection is real, not an llc afterthought).
#   2. `llc -filetype=obj` lowers it to an ELF AArch64 object with no triple
#      override (the IR is self-describing).
#
# Run from anywhere; cd's to repo root. Skips (exit-neutral) if llc is absent.

set -uo pipefail
cd "$(dirname "$0")/.."
REPO="$(pwd)"

GREEN=$'\033[32m'; RED=$'\033[31m'; DIM=$'\033[2m'; RST=$'\033[0m'
pass=0; fail=0
ok ()  { printf '%s  PASS%s  %s\n' "$GREEN" "$RST" "$1"; pass=$((pass+1)); }
bad () { printf '%s  FAIL%s  %s\n' "$RED"   "$RST" "$1"; fail=$((fail+1)); }

command -v llc >/dev/null 2>&1 || {
  printf '%s  SKIP%s  llc not on PATH (install LLVM to run the aarch64 spine gate)\n' "$DIM" "$RST"
  exit 0; }

BIN="$REPO/_build/default/bin/main.exe"
[ -x "$BIN" ] || { echo "── building compiler ──"; dune build 2>/dev/null || { echo "build failed"; exit 1; }; }

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
printf 'fn add(a: Int, b: Int) -> Int { a + b }\n' > "$tmp/spine.affine"

echo "── Native-codegen spine: aarch64 + riscv64 (ADR-0024) ──"

if AFFINESCRIPT_LLVM_TRIPLE=aarch64-linux-android "$BIN" compile "$tmp/spine.affine" -o "$tmp/spine.ll" >/dev/null 2>&1 \
   && grep -q 'target triple = "aarch64-linux-android"' "$tmp/spine.ll"; then
  ok "compile emits aarch64-linux-android triple"
else
  bad "compile did not emit the Android triple"
fi

if llc -filetype=obj "$tmp/spine.ll" -o "$tmp/spine.o" >/dev/null 2>&1; then
  desc="$(file "$tmp/spine.o" 2>/dev/null)"
  case "$desc" in
    *"ARM aarch64"*) ok "llc -> AArch64 object ($desc)" ;;
    *)               bad "object is not AArch64: $desc" ;;
  esac
else
  bad "llc failed to lower the Android-triple IR"
fi

# RISC-V downstream (ADR-0024 Consequences): same parameterised triple, no code
# change — proves the native spine is multi-target, not Android-specific.
if AFFINESCRIPT_LLVM_TRIPLE=riscv64-unknown-linux-gnu "$BIN" compile "$tmp/spine.affine" -o "$tmp/spine-rv.ll" >/dev/null 2>&1 \
   && grep -q 'target triple = "riscv64-unknown-linux-gnu"' "$tmp/spine-rv.ll" \
   && llc -filetype=obj "$tmp/spine-rv.ll" -o "$tmp/spine-rv.o" >/dev/null 2>&1; then
  desc="$(file "$tmp/spine-rv.o" 2>/dev/null)"
  case "$desc" in
    *"RISC-V"*) ok "riscv64 triple -> RISC-V object (downstream: RISC-V native)" ;;
    *)          bad "riscv64 object is not RISC-V: $desc" ;;
  esac
else
  bad "riscv64 native spine failed"
fi

echo
printf '%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
