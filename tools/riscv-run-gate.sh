#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# RISC-V native run gate (ADR-0024): AffineScript -> riscv64 -> link -> RUN.
#
# Completes the native tier beyond the codegen spine (objects, pinned by
# android-validate) to an actually-executing riscv64 binary:
#   1. compile examples/hello.affine with the riscv64 triple
#   2. llc -> rv64gc / lp64d object (hard-float ABI, matching riscv64 glibc)
#   3. riscv64-linux-gnu-gcc links it against the cross sysroot
#   4. qemu-riscv64 runs it; we assert the expected stdout
#
# Needs `gcc-riscv64-linux-gnu` + `qemu-user` (apt). Skips (exit-neutral) if
# either is absent, so it never reddens CI on a host without the cross-toolchain.
# Install:  sudo apt install gcc-riscv64-linux-gnu qemu-user

set -uo pipefail
cd "$(dirname "$0")/.."
REPO="$(pwd)"

GREEN=$'\033[32m'; RED=$'\033[31m'; DIM=$'\033[2m'; RST=$'\033[0m'
pass=0; fail=0
ok ()  { printf '%s  PASS%s  %s\n' "$GREEN" "$RST" "$1"; pass=$((pass+1)); }
bad () { printf '%s  FAIL%s  %s\n' "$RED"   "$RST" "$1"; fail=$((fail+1)); }
skip() { printf '%s  SKIP%s  %s\n' "$DIM"   "$RST" "$1"; exit 0; }

GCC=riscv64-linux-gnu-gcc
QEMU=qemu-riscv64
command -v "$GCC"  >/dev/null 2>&1 || skip "$GCC not found — sudo apt install gcc-riscv64-linux-gnu"
command -v "$QEMU" >/dev/null 2>&1 || { command -v qemu-riscv64-static >/dev/null 2>&1 && QEMU=qemu-riscv64-static || skip "$QEMU not found — sudo apt install qemu-user"; }
command -v llc >/dev/null 2>&1 || skip "llc not found"

BIN="$REPO/_build/default/bin/main.exe"
[ -x "$BIN" ] || { echo "── building compiler ──"; dune build 2>/dev/null || { echo "build failed"; exit 1; }; }

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
SRC="$REPO/examples/hello.affine"
EXPECT="Hello, AffineScript!"
SYSROOT="/usr/riscv64-linux-gnu"

echo "── RISC-V native run gate (ADR-0024) ──"

AFFINESCRIPT_LLVM_TRIPLE=riscv64-unknown-linux-gnu "$BIN" compile "$SRC" -o "$tmp/h.ll" >/dev/null 2>&1 \
  && ok "compile -> riscv64 IR" || { bad "compile failed"; }

# rv64gc + lp64d (hard-float) to match the standard riscv64-linux-gnu glibc.
if llc -mtriple=riscv64-linux-gnu -mattr=+m,+a,+f,+d,+c -target-abi=lp64d \
       -filetype=obj "$tmp/h.ll" -o "$tmp/h.o" >/dev/null 2>&1; then
  ok "llc -> rv64gc/lp64d object"
else
  bad "llc lowering failed"
fi

if "$GCC" "$tmp/h.o" -o "$tmp/h" >"$tmp/.link" 2>&1; then
  ok "link via $GCC"
else
  bad "link failed: $(tail -1 "$tmp/.link")"
fi

if [ -x "$tmp/h" ]; then
  got="$("$QEMU" -L "$SYSROOT" "$tmp/h" 2>/dev/null)"
  if [ "$got" = "$EXPECT" ]; then ok "qemu-riscv64 runs it -> '$got'"
  else bad "ran but wrong output: got '$got', want '$EXPECT'"; fi
fi

echo
printf '%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
