// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for DLCLoader.affine (idaptik DLC puzzle-pack normaliser;
// scalar i32 ABI). The oracles re-derive, in plain independent JS straight from
// DLCLoader.res, the 0..4 tier band fold (parseTier's `| _ => Tier0` wildcard)
// and the two "absent vs explicit" selections (parMoves and the hint default 3),
// so a codegen regression surfaces as a differential mismatch.

// Tier band fold: parseTier maps an ordinal/number onto 0..4, anything else -> 0.
const tierFold = (c) => (c >= 0 && c <= 4 ? c : 0);
// parMoves: present flag 0 -> fallback, else explicit (parsePuzzleJson).
const parMoves = (present, explicit, fallback) => (present === 0 ? fallback : explicit);
// hintThreshold: present flag 0 -> default 3, else explicit (parseHints).
const hintThreshold = (present, explicit) => (present === 0 ? 3 : explicit);

export default {
  affine: "DLCLoader.affine",
  cases: [
    { name: "tier_count()", export: "tier_count", args: [], oracle: () => 5 },
    {
      name: "tier_clamp over [-3..8]",
      export: "tier_clamp",
      args: [[-3, 8]],
      oracle: (c) => tierFold(c),
    },
    {
      name: "tier_of_number over [-3..8]",
      export: "tier_of_number",
      args: [[-3, 8]],
      oracle: (n) => tierFold(n),
    },
    {
      name: "par_moves over present{0,1,2} x explicit{7,42} x fallback{50,99}",
      export: "par_moves",
      args: [{ values: [0, 1, 2] }, { values: [7, 42] }, { values: [50, 99] }],
      oracle: (present, explicit, fallback) => parMoves(present, explicit, fallback),
    },
    {
      name: "hint_threshold over present{0,1,2} x explicit{0,3,9}",
      export: "hint_threshold",
      args: [{ values: [0, 1, 2] }, { values: [0, 3, 9] }],
      oracle: (present, explicit) => hintThreshold(present, explicit),
    },
  ],
};
