#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# Coprocessor / accelerator backend emission gate for AffineScript.
#
# The Tier-C kernel-sublanguage backends (WGSL, SPIR-V, CUDA, Metal, OpenCL,
# MLIR, ONNX, Faust, Verilog) are wired into `bin/main.ml` but had ZERO test
# coverage — nothing pinned "the GPU/audio/FPGA emitters actually work". This
# gate compiles canonical kernels to every coprocessor target and checks the
# output, validating with a real tool wherever one exists on PATH:
#   WGSL   -> naga (if present)         SPIR-V -> magic word 0x07230203
# The rest are emit-and-nonempty checks (no hermetic validator shipped here).
#
# Run from anywhere; cd's to repo root. Skips a target's validator if the tool
# is absent (emit check still runs).

set -uo pipefail
cd "$(dirname "$0")/.."
REPO="$(pwd)"

GREEN=$'\033[32m'; RED=$'\033[31m'; YEL=$'\033[33m'; DIM=$'\033[2m'; RST=$'\033[0m'
pass=0; fail=0
ok ()  { printf '%s  PASS%s  %s\n' "$GREEN" "$RST" "$1"; pass=$((pass+1)); }
bad () { printf '%s  FAIL%s  %s\n' "$RED"   "$RST" "$1"; fail=$((fail+1)); }

BIN="$REPO/_build/default/bin/main.exe"
[ -x "$BIN" ] || { echo "── building compiler ──"; dune build 2>/dev/null || { echo "build failed"; exit 1; }; }

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

# ── canonical kernels (the accepted Tier-C subset) ───────────────────────────
cat > "$tmp/saxpy.affine" <<'EOF'
fn kernel(i: Int, mut out: Array[Float], a: Array[Float], b: Array[Float]) -> Unit {
  out[i] = a[i] + b[i];
}
EOF
cat > "$tmp/graph.affine" <<'EOF'
fn relu(x: Array[Float]) -> Array[Float] { x }
fn add(a: Array[Float], b: Array[Float]) -> Array[Float] { a }
fn graph(x: Array[Float], y: Array[Float]) -> Array[Float] {
  let s: Array[Float] = add(x, y);
  let r: Array[Float] = relu(s);
  r
}
EOF
printf 'fn process(x: Float) -> Float { x * 0.5 }\n'      > "$tmp/dsp.affine"
printf 'fn add(a: Int, b: Int) -> Int { a + b }\n'        > "$tmp/scalar.affine"

emit () { # <label> <src> <ext> ; sets $OUT, returns 0 if emitted nonempty
  OUT="$tmp/out.$3"; rm -f "$OUT"
  "$BIN" compile "$2" -o "$OUT" >"$tmp/.log" 2>&1 && [ -s "$OUT" ]
}

check () { # <label> <src> <ext>
  if emit "$1" "$2" "$3"; then ok "$1 ($(wc -c < "$OUT") bytes)"; else bad "$1 — $(tail -1 "$tmp/.log")"; fi
}

echo "── Coprocessor / accelerator emission gate ──"
# GPU compute family (compute-kernel shape)
for t in "WGSL:wgsl" "SPIR-V:spv" "CUDA:cu" "Metal:metal" "OpenCL:cl"; do
  check "${t%%:*}" "$tmp/saxpy.affine" "${t##*:}"
  case "$t" in
    WGSL:*)   if command -v naga >/dev/null 2>&1; then
                naga "$tmp/out.wgsl" >/dev/null 2>&1 && ok "WGSL naga-validate" || bad "WGSL naga-validate"
              fi ;;
    SPIR-V:*) if [ -s "$tmp/out.spv" ]; then
                m=$(xxd -l4 -p "$tmp/out.spv" 2>/dev/null)
                [ "$m" = "03022307" ] && ok "SPIR-V magic 0x07230203" || bad "SPIR-V magic (got $m)"
              fi ;;
  esac
done
# Tensor / Audio / FPGA-class
check "ONNX (tensor)" "$tmp/graph.affine"  "onnx"
check "Faust (audio)" "$tmp/dsp.affine"    "dsp"
check "Verilog (FPGA)" "$tmp/scalar.affine" "v"
# Scalar IR backends (Int/Float single-fn scope)
check "MLIR (scalar IR)" "$tmp/scalar.affine" "mlir"
check "LLVM (scalar IR)" "$tmp/scalar.affine" "ll"

echo
printf '%s passed, %s failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
