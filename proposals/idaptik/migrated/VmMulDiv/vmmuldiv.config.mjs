// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmMulDiv.affine (reversible VM multiply/divide
// opcodes; scalar i32 ABI). Each oracle independently re-derives the ORIGINAL
// vm/lib/ocaml/{Mul,Div}.res value transform in JS -- it is NOT read off the
// .affine. Both sides normalise to i32 (| 0), so two's-complement wraparound is
// covered.
//
// Two correctness points, both real ABI facts surfaced by this opcode pair:
//   1. JS `a * b` is exact only below 2^53; for i32 products it must use
//      Math.imul to match wasm i32.mul (e.g. 2147483647*2147483647 -> 1).
//   2. ReScript int `/` truncates through `| 0`, so INT_MIN / -1 wraps to
//      INT_MIN; `b == 0` is guarded by the source (q := 0, r := a). The oracle
//      reproduces the source's observable output; the .affine reproduces it
//      with explicit guards because i32.div_s would otherwise TRAP.
const I = { values: [-2147483648, -1000000, -7, -1, 0, 1, 7, 1000000, 2147483647] };
const i32 = (x) => x | 0;
const imul = (a, b) => Math.imul(a, b);                 // i32-exact product
const sdiv = (a, b) => (b === 0 ? 0 : i32(a / b));      // Div.res quotient branch
const srem = (a, b) => (b === 0 ? a : i32(a % b));      // Div.res remainder branch

export default {
  affine: "VmMulDiv.affine",
  cases: [
    // --- MUL.make ancilla pair + reversibility ---
    { name: "mul_fwd c+a*b", export: "mul_fwd", args: [I, I, I], oracle: (a, b, c) => i32(c + imul(a, b)) },
    { name: "mul_inv c-a*b", export: "mul_inv", args: [I, I, I], oracle: (a, b, c) => i32(c - imul(a, b)) },
    { name: "mul_roundtrip == c", export: "mul_roundtrip", args: [I, I, I], oracle: (a, b, c) => i32(c) },

    // --- DIV.make quotient/remainder + reconstruction (reversibility) ---
    { name: "div_q a/b (guarded)", export: "div_q", args: [I, I], oracle: (a, b) => sdiv(a, b) },
    { name: "div_r a mod b (guarded)", export: "div_r", args: [I, I], oracle: (a, b) => srem(a, b) },
    { name: "div_reconstruct == a", export: "div_reconstruct", args: [I, I], oracle: (a, b) => i32(a) },

    // --- MUL.makeInPlace value transforms (intentional-flaw variant; no rt==id) ---
    { name: "mul_inplace_fwd a*b", export: "mul_inplace_fwd", args: [I, I], oracle: (a, b) => imul(a, b) },
    { name: "mul_inplace_inv a/b (b==0 -> a)", export: "mul_inplace_inv", args: [I, I], oracle: (a, b) => (b === 0 ? i32(a) : i32(a / b)) },

    // --- DIV.makeSimple quotient (b==0 keeps prior q) + invert clears ---
    { name: "div_simple_q (b==0 -> q_prev)", export: "div_simple_q", args: [I, I, I], oracle: (a, b, q) => (b === 0 ? i32(q) : i32(a / b)) },
    { name: "div_simple_invert == 0", export: "div_simple_invert", args: [], oracle: () => 0 },
  ],
};
