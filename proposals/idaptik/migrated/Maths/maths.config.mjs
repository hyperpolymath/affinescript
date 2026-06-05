// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Maths.affine (integer math utilities; scalar i32
// ABI). Oracles re-derive the integer core of src/engine/utils/Maths.res
// (clamp; lerp in milli-units; squared distance). Domains kept moderate so the
// lerp blend and the squared terms never overflow i32 (these are screen/value
// magnitudes, not stress cases). i32-normalised both sides.
const C = { values: [-1000, -7, -1, 0, 1, 7, 1000] };           // clamp / lerp operands
const T = { values: [0, 1, 250, 500, 750, 999, 1000] };          // t in milli-units [0,1000]
const K = { values: [-1000, -7, 0, 1, 7, 1000] };                // coordinates for dist_sq
const i32 = (x) => x | 0;
const imul = (a, b) => Math.imul(a, b);

export default {
  affine: "Maths.affine",
  cases: [
    { name: "clamp v into [lo,hi] (swap if inverted)", export: "clamp", args: [C, C, C],
      oracle: (v, lo, hi) => { const mn = Math.min(lo, hi), mx = Math.max(lo, hi); return Math.max(mn, Math.min(v, mx)); } },
    { name: "lerp_milli (1-t)a+tb truncated", export: "lerp_milli", args: [C, C, T],
      oracle: (a, b, tm) => Math.trunc(((1000 - tm) * a + tm * b) / 1000) },
    { name: "dist_sq dx*dx+dy*dy", export: "dist_sq", args: [K, K, K, K],
      oracle: (ax, ay, bx, by) => { const dx = bx - ax, dy = by - ay; return i32(imul(dx, dx) + imul(dy, dy)); } },
  ],
};
