// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Coprocessor_Security.affine (idaptik security
// co-processor scalar kernels; scalar i32 ABI). Every oracle is an INDEPENDENT
// JS reimplementation derived from src/shared/Coprocessor_Security.res semantics
// -- NOT a copy of the .affine logic. Per the established Random.affine
// convention, the multiply-bearing LCG oracle re-derives the ORIGINAL .res
// mixing with Math.imul + |0 to model the wasm i32 wraparound (the .res
// `seed*1664525 + ... & 0x7FFFFFFF` agrees with this after ToInt32). The pure
// bit-op oracles use native &, >>, <<, ^, | (no large multiply, no wrap risk).

const i32 = (x) => x | 0;

// LCG: next = (seed*1664525 + 1013904223) & 0x7FFFFFFF  (Crypto.lcgStep).
// Independent route uses Math.imul for the 32-bit-wrapping product.
const oLcgNext = (seed) => i32(Math.imul(seed, 1664525) + 1013904223) & 2147483647;
const oLcgByte = (seed) => oLcgNext(seed) & 255;

// xorFold step: acc lxor x.
const oXorStep = (acc, x) => i32(acc ^ x);

// intTo4Bytes element i (big-endian byte): land(lsr(n, 8*(3-i)), 0xFF).
const oByteAt = (n, i) => {
  if (i < 0 || i > 3) return 0;
  const shift = (3 - i) * 8;
  return (n >> shift) & 255;
};

// sign's 2-byte checksum split + verify's recombination.
const oCsLo = (f) => f & 255;
const oCsHi = (f) => (f >> 8) & 255;
const oCombine = (lo, hi) => i32(lo | (hi << 8));
// verify gate: low 16 bits of fold vs the 16-bit stored checksum.
const oVerify = (f, lo, hi) => ((f & 65535) === i32(lo | (hi << 8)) ? 1 : 0);

// crack success key: target lxor strength.
const oCrackKey = (t, s) => i32(t ^ s);

// sample domains
const SEEDS = { values: [-2147483648, -1000000, -42, -1, 0, 1, 42, 1000, 65535, 1000000, 2147483647] };
const BYTES = { values: [0, 1, 7, 16, 42, 64, 127, 128, 200, 255] };

export default {
  affine: "Coprocessor_Security.affine",
  cases: [
    // --- LCG generator (keygen/noise/scramble spine) ---
    { name: "lcg_next over i32 seed samples", export: "lcg_next", args: [SEEDS], oracle: oLcgNext },
    { name: "lcg_byte over i32 seed samples", export: "lcg_byte", args: [SEEDS], oracle: oLcgByte },
    // a contiguous seed sweep too, to catch any off-by-one in the constant fold
    { name: "lcg_next over [0..512]", export: "lcg_next", args: [[0, 512]], oracle: oLcgNext },

    // --- XOR-fold step (hash/sign spine) ---
    {
      name: "xor_fold_step over acc,x i32 samples",
      export: "xor_fold_step",
      args: [{ values: [-2147483648, -1, 0, 1, 255, 65535, 1000000, 2147483647] },
             { values: [-2147483648, -1, 0, 1, 255, 65535, 1000000, 2147483647] }],
      oracle: oXorStep,
    },

    // --- byte extraction (intTo4Bytes) ---
    {
      name: "byte_at over n samples x i in [-1..5]",
      export: "byte_at",
      args: [{ values: [0, 1, 255, 256, 0x12345678, -1, 0x7FFFFFFF, 0xABCDEF] }, [-1, 5]],
      oracle: oByteAt,
    },

    // --- checksum split / recombine / verify (sign/verify) ---
    { name: "checksum_lo over folded samples", export: "checksum_lo",
      args: [{ values: [0, 1, 255, 256, 0x1234, 0xFFFF, 0x12345678, -1, 2147483647] }], oracle: oCsLo },
    { name: "checksum_hi over folded samples", export: "checksum_hi",
      args: [{ values: [0, 1, 255, 256, 0x1234, 0xFFFF, 0x12345678, -1, 2147483647] }], oracle: oCsHi },
    { name: "checksum_combine over lo,hi bytes", export: "checksum_combine",
      args: [BYTES, BYTES], oracle: oCombine },
    {
      name: "verify_match over folded x lo x hi",
      export: "verify_match",
      args: [{ values: [0, 0x1234, 0xFFFF, 0xAB12, 0x12345678, 65536, 131073] }, BYTES, BYTES],
      oracle: oVerify,
    },

    // --- crack key derivation ---
    {
      name: "crack_key over target x strength",
      export: "crack_key",
      args: [{ values: [0, 1, 255, 0x12345678, -1, 2147483647] }, [0, 9]],
      oracle: oCrackKey,
    },
  ],
};
