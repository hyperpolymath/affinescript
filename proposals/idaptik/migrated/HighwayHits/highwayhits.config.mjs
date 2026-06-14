// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for HighwayHits.affine (idaptik HighwayCrossingTraining
// hit-budget + lane-direction kernel; scalar i32 ABI). Each oracle re-derives the
// rule straight from HighwayCrossingTraining.res INDEPENDENTLY of the .affine, so a
// codegen regression surfaces as a differential mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine:
//   lane_count()        = 5                                  (Tuning.laneCount, res:39)
//   max_hits()          = 3                                  (Tuning.maxHits, res:78)
//   is_game_over(h)     = h >= 3 ? 1 : 0                      (res:791, `hits >= maxHits`)
//   hits_remaining(h)   = h<0 ? 3 : h>=3 ? 0 : 3-h           (HUD maxHits-hits, res:489)
//   lane_direction(i)   = (i % 2) === 0 ? 1 : -1             (res:564 sign / res:344 arrow)

// GREATER-OR-EQUAL boundary, faithful to the .res `>=` at res:791.
const isGameOver = (h) => (h >= 3 ? 1 : 0);
// Clamped maxHits - hits: full at negatives, floored at 0 past the cap.
const hitsRemaining = (h) => (h < 0 ? 3 : h >= 3 ? 0 : 3 - h);
// Lane traffic-direction sign by index parity; written as the SAME `i % 2` expression
// the .affine uses, so truncated-modulo semantics for negatives must agree on both
// sides (a differential failure here would itself be the signal).
const laneDirection = (i) => ((i % 2) === 0 ? 1 : -1);

// Hit count across the band: malformed negatives, 0, the 1/2/3 progression
// (2 = still alive, 3 = exact game-over), over-cap (4..5) plus an overflow.
const HITS = [-3, -1, 0, 1, 2, 3, 4, 5, 100];
// Lane index over the real 0..laneCount-1 domain (0..4) plus a couple of negatives to
// probe modulo agreement between the JS oracle and the AffineScript `%`.
const LANES = [-2, -1, 0, 1, 2, 3, 4, 5];

export default {
  affine: "HighwayHits.affine",
  cases: [
    { name: "lane_count()", export: "lane_count", args: [], oracle: () => 5 },
    { name: "max_hits()", export: "max_hits", args: [], oracle: () => 3 },
    {
      name: "is_game_over over hit band",
      export: "is_game_over",
      args: [{ values: HITS }],
      oracle: isGameOver,
    },
    {
      name: "hits_remaining over hit band",
      export: "hits_remaining",
      args: [{ values: HITS }],
      oracle: hitsRemaining,
    },
    {
      name: "lane_direction over lane indices",
      export: "lane_direction",
      args: [{ values: LANES }],
      oracle: laneDirection,
    },
  ],
};
