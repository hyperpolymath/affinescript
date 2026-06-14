// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for SkillTreeScreen.affine (idaptik XP-threshold brain;
// scalar i32 ABI). The oracle re-derives each function from the ORIGINAL
// SkillTreeScreen.res semantics (NOT from the .affine) so a codegen regression
// surfaces as a differential mismatch.
//
// From SkillTreeScreen.res xpForNextRank (lines 61-69):
//   Novice => 100 | Trained => 350 | Proficient => 800
//   Veteran => 1500 | Expert => 1500  (// Already maxed)
// Rank variant Novice/Trained/Proficient/Veteran/Expert maps to ordinals
// 0/1/2/3/4 (the rankOrder/rankMeetsRequirement ascending order).
//
// From the XP-bar fill (lines 211-218 / 309-314):
//   let fillPct = if threshold > 0 { xp/threshold } else { 1.0 }
//   let fillWidth = Math.min(fillPct * barW, barW)
// Expressed here as an integer permille (fraction * 1000) so it crosses as i32:
//   threshold <= 0  -> 1000 (the else-1.0 branch, i.e. full bar)
//   else            -> min( trunc(max(xp,0) * 1000 / threshold), 1000 )
// The max(xp,0) guards an empty bar from going negative; the min caps a full
// bar at 1000, matching Math.min(.., barW).

// Independent re-derivation of the rank validity from the .res ordinal domain.
const rankValid = (r) => (r >= 0 && r <= 4 ? 1 : 0);

// Independent re-derivation of xpForNextRank from the .res.
const xpForNextRank = (r) => {
  if (!rankValid(r)) return -1;
  return [100, 350, 800, 1500, 1500][r];
};

// Independent re-derivation of "is the maxed top rank" (Expert == ordinal 4).
const isMaxed = (r) => (r === 4 ? 1 : 0);

// Independent re-derivation of the XP-bar fill permille from the .res.
const fillPermille = (xp, threshold) => {
  if (threshold <= 0) return 1000;
  const clampedXp = xp < 0 ? 0 : xp;
  const raw = Math.trunc((clampedXp * 1000) / threshold);
  return raw > 1000 ? 1000 : raw;
};

export default {
  affine: "SkillTreeScreen.affine",
  cases: [
    {
      name: "skill_rank_count()",
      export: "skill_rank_count",
      args: [],
      oracle: () => 5,
    },
    {
      name: "skill_rank_valid over rank[-3..8]",
      export: "skill_rank_valid",
      args: [[-3, 8]],
      oracle: (r) => rankValid(r),
    },
    {
      name: "xp_for_next_rank over rank[-3..8]",
      export: "xp_for_next_rank",
      args: [[-3, 8]],
      oracle: (r) => xpForNextRank(r),
    },
    {
      name: "xp_for_next_rank exact ranks 0..4",
      export: "xp_for_next_rank",
      args: [{ values: [0, 1, 2, 3, 4] }],
      oracle: (r) => xpForNextRank(r),
    },
    {
      name: "skill_rank_is_maxed over rank[-3..8]",
      export: "skill_rank_is_maxed",
      args: [[-3, 8]],
      oracle: (r) => isMaxed(r),
    },
    {
      // xp swept against each real threshold + the maxed/zero edge cases.
      name: "xp_fill_permille over (xp[-50..1600 sample], threshold{thresholds})",
      export: "xp_fill_permille",
      args: [
        { values: [-50, 0, 1, 50, 100, 175, 350, 700, 800, 1499, 1500, 1600] },
        { values: [0, -10, 100, 350, 800, 1500] },
      ],
      oracle: (xp, t) => fillPermille(xp, t),
    },
  ],
};
