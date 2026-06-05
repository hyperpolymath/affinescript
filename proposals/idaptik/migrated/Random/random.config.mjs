// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Random.affine (deterministic xmur3 + mulberry32
// PRNG; scalar i32 ABI). Each oracle re-derives the ORIGINAL src/engine/utils/
// Random.res mixing using JS-native Math.imul and >>> -- the source of truth.
// This is the multiplayer-reproducibility backbone, so the sweep is bit-exact
// over the full i32 domain (accumulators are arbitrary i32; code units add
// real UTF-16 values). If xmur3_step matches for every (h, c) the folded
// multi-char hash matches by induction; the host loop never leaves i32.
const I = { values: [-2147483648, -1000000, -7, -1, 0, 1, 7, 1000000, 2147483647] };
const CU = { values: [0, 1, 65, 97, 255, 256, 65535, -1, 2147483647] };   // UTF-16 code units + extremes
const i32 = (x) => x | 0;

const xmur3_finalise = (h) => {
  let h1 = Math.imul(h ^ (h >>> 16), 2246822507);
  let h2 = Math.imul(h1 ^ (h1 >>> 13), 3266489909);
  return i32(h2 ^ (h2 >>> 16));
};
const mulberry32_output = (a) => {
  let t1 = Math.imul(a ^ (a >>> 15), a | 1);
  let t2 = t1 ^ (t1 + Math.imul(t1 ^ (t1 >>> 7), t1 | 61));
  return i32(t2 ^ (t2 >>> 14));
};

export default {
  affine: "Random.affine",
  cases: [
    // --- xmur3 seed hash ---
    { name: "xmur3_init 1779033703^len", export: "xmur3_init", args: [I], oracle: (len) => i32(1779033703 ^ len) },
    { name: "xmur3_step fold one code unit", export: "xmur3_step", args: [I, CU],
      oracle: (h, c) => { const m = Math.imul(h ^ c, 3432918353); return i32((m << 13) | (m >>> 19)); } },
    { name: "xmur3_finalise avalanche -> u32 seed", export: "xmur3_finalise", args: [I], oracle: xmur3_finalise },
    // --- mulberry32 generator ---
    { name: "mulberry32_advance (a+0x6d2b79f5)|0", export: "mulberry32_advance", args: [I], oracle: (a) => i32(a + 1831565813) },
    { name: "mulberry32_output raw u32 draw", export: "mulberry32_output", args: [I], oracle: mulberry32_output },
    { name: "mulberry32_draw_raw advance+output", export: "mulberry32_draw_raw", args: [I], oracle: (s) => mulberry32_output(i32(s + 1831565813)) },
  ],
};
