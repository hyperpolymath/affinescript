// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PuzzleFormat.affine (idaptik difficulty/tier
// taxonomy + DLCLoader clamp; scalar i32 ABI). Oracles re-derive both the
// lossless *_to_int identity and the controlled-loss clamp_* default.
const vTier = (c) => c >= 0 && c <= 4;
const vDiff = (c) => c >= 0 && c <= 3;
export default {
  affine: "PuzzleFormat.affine",
  cases: [
    { name: "difficulty_count()", export: "difficulty_count", args: [], oracle: () => 4 },
    { name: "tier_count()", export: "tier_count", args: [], oracle: () => 5 },
    { name: "is_valid_difficulty over [-3..8]", export: "is_valid_difficulty", args: [[-3, 8]], oracle: (c) => (vDiff(c) ? 1 : 0) },
    { name: "is_valid_tier over [-3..8]", export: "is_valid_tier", args: [[-3, 8]], oracle: (c) => (vTier(c) ? 1 : 0) },
    { name: "tier_to_int over [-3..8]", export: "tier_to_int", args: [[-3, 8]], oracle: (c) => (vTier(c) ? c : -1) },
    { name: "difficulty_to_int over [-3..8]", export: "difficulty_to_int", args: [[-3, 8]], oracle: (c) => (vDiff(c) ? c : -1) },
    { name: "clamp_tier over [-3..8]", export: "clamp_tier", args: [[-3, 8]], oracle: (c) => (vTier(c) ? c : 0) },
    { name: "clamp_difficulty over [-3..8]", export: "clamp_difficulty", args: [[-3, 8]], oracle: (c) => (vDiff(c) ? c : 0) },
  ],
};
