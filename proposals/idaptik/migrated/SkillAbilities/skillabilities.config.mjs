// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for SkillAbilities.affine (idaptik ability-unlock
// decision; scalar i32 ABI). The oracle re-derives each function from the
// original SkillAbilities.res semantics so a codegen regression surfaces as a
// differential mismatch.
//
// Source semantics (from SkillAbilities.res / SkillAbilitiesCoprocessorBridge.js):
//
//   rankValue:    dense rank index 1..5; off-domain -> 0  (same as SkillRank's)
//
//   rankMeetsRequirement(current, required):
//     rankValue(current) >= rankValue(required) ? 1 : 0
//     (mirrors rankMeetsRequirement in SkillAbilities.res which uses
//      JessicaBackground.rankValue on both sides)
//
//   backgroundOk(abilityBg, playerBg):
//     if abilityBg == 0: generic => 1
//     if playerBg == 0:  player has no background => 0 (exclusive fails)
//     abilityBg == playerBg ? 1 : 0
//     (mirrors the exclusiveBackground switch in isAbilityUnlocked)
//
//   unlockOk(currentRank, requiredRank, abilityBg, playerBg):
//     backgroundOk(abilityBg, playerBg) == 0 => 0
//     rankMeetsRequirement(currentRank, requiredRank) == 1 => 1
//     else 0

// Re-derive from ReScript semantics.
const rankValue = (r) => {
  if (r === 1) return 1;
  if (r === 2) return 2;
  if (r === 3) return 3;
  if (r === 4) return 4;
  if (r === 5) return 5;
  return 0;
};

const rankMeets = (current, required) =>
  rankValue(current) >= rankValue(required) ? 1 : 0;

const backgroundOk = (abilityBg, playerBg) => {
  if (abilityBg === 0) return 1; // generic ability
  if (playerBg === 0) return 0; // player has no background => exclusive fails
  return abilityBg === playerBg ? 1 : 0;
};

const unlockOk = (currentRank, requiredRank, abilityBg, playerBg) => {
  if (backgroundOk(abilityBg, playerBg) === 0) return 0;
  return rankMeets(currentRank, requiredRank);
};

export default {
  affine: "SkillAbilities.affine",
  cases: [
    {
      name: "rank_value over [-2..7]",
      export: "rank_value",
      args: [[-2, 7]],
      oracle: (r) => rankValue(r),
    },
    {
      name: "rank_meets over current[-1..6] × required[-1..6]",
      export: "rank_meets",
      args: [[-1, 6], [-1, 6]],
      oracle: (cur, req) => rankMeets(cur, req),
    },
    {
      name: "background_ok over abilityBg[-1..4] × playerBg[-1..4]",
      export: "background_ok",
      args: [[-1, 4], [-1, 4]],
      oracle: (ab, pb) => backgroundOk(ab, pb),
    },
    {
      name: "unlock_ok over currentRank[0..6]×requiredRank[0..6]×abilityBg[0..3]×playerBg[0..3]",
      export: "unlock_ok",
      args: [[0, 6], [0, 6], [0, 3], [0, 3]],
      oracle: (cr, rr, ab, pb) => unlockOk(cr, rr, ab, pb),
    },
  ],
};
