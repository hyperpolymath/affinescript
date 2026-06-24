// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for SkillRank.affine (idaptik skill-rank progression;
// scalar i32 ABI). The oracle re-derives each function from the original
// JessicaBackground.res + SkillRankCoprocessor.res semantics so a codegen
// regression surfaces as a differential mismatch.
//
// Source semantics (from JessicaBackground.res rankValue / rankXpThreshold /
// can_promote contract in SkillRankBridge.js / SkillRankCoprocessor.res):
//   rankValue:    Novice=1 .. Expert=5; off-domain -> 0
//   xpThreshold:  Novice=0, Trained=100, Proficient=350, Veteran=800, Expert=1500; off-domain->0
//   canPromote:   rank in 1..4 AND xp >= xpThreshold(rank+1) ? 1 : 0

// Re-derive rankValue from JessicaBackground.res rankValue switch.
const rankValue = (rank) => {
  if (rank === 1) return 1;
  if (rank === 2) return 2;
  if (rank === 3) return 3;
  if (rank === 4) return 4;
  if (rank === 5) return 5;
  return 0;
};

// Re-derive rankXpThreshold from JessicaBackground.res rankXpThreshold switch:
//   Novice=0, Trained=100, Proficient=350, Veteran=800, Expert=1500
const xpThreshold = (rank) => {
  if (rank === 1) return 0;
  if (rank === 2) return 100;
  if (rank === 3) return 350;
  if (rank === 4) return 800;
  if (rank === 5) return 1500;
  return 0;
};

// Re-derive canPromote from SkillRankBridge.js semantics and JessicaBackground
// addXp logic: a rank-up is possible iff a next rank exists (rank < 5 and
// rank >= 1) and xp meets the next rank's threshold.
const canPromote = (rank, xp) => {
  if (rank < 1) return 0;
  if (rank > 4) return 0; // Expert (5) or off-domain above
  const next = rank + 1;
  return xp >= xpThreshold(next) ? 1 : 0;
};

export default {
  affine: "SkillRank.affine",
  cases: [
    {
      name: "rank_value over [-2..7]",
      export: "rank_value",
      args: [[-2, 7]],
      oracle: (r) => rankValue(r),
    },
    {
      name: "xp_threshold over [-2..7]",
      export: "xp_threshold",
      args: [[-2, 7]],
      oracle: (r) => xpThreshold(r),
    },
    {
      name: "can_promote over rank[-2..7] × xp[-10..1600]",
      export: "can_promote",
      args: [
        [-2, 7],
        // Sweep key XP boundaries: just below/at/above each threshold
        // (0, 99, 100, 349, 350, 799, 800, 1499, 1500, 1501) plus extremes
        {
          values: [
            -10, 0, 1, 99, 100, 101, 349, 350, 351, 799, 800, 801, 1499, 1500,
            1501, 2000,
          ],
        },
      ],
      oracle: (rank, xp) => canPromote(rank, xp),
    },
  ],
};
