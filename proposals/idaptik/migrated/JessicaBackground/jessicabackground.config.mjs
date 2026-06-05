// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for JessicaBackground.affine (idaptik operative-background
// attribute-bonus matrix and starting-skill grant; scalar i32 ABI). The oracle
// re-derives each function from the original JessicaBackground.res getBonus and
// makeSkillSet semantics so a codegen regression surfaces as a differential
// mismatch.
//
// Background encoding (JessicaBackground.all order):
//   0 Assault  1 Recon  2 Engineer  3 Signals  4 Medic  5 Logistics
// Attribute-column encoding (attributeBonus field order):
//   0 str  1 dex  2 con  3 int_  4 wil  5 cha
// Skill-category encoding (skillCategory variant order):
//   0 Infiltration  1 CombatSkill  2 Athletics  3 Observation
//   4 TechLiteracy  5 Fieldcraft   6 Composure
//
// getBonus matrix from JessicaBackground.res (all shifts are integer):
//   Assault:    str+20 dex+10 con+15 int_-5  wil+5  cha-5
//   Recon:      str-5  dex+15 con+0  int_+10 wil+10 cha+0
//   Engineer:   str+5  dex+5  con+5  int_+15 wil+5  cha-5
//   Signals:    str-5  dex+5  con+0  int_+10 wil+10 cha+10
//   Medic:      str+0  dex+5  con+20 int_+10 wil+10 cha+0
//   Logistics:  str+10 dex+0  con+10 int_+5  wil+0  cha+10
//   off-domain bg or attr: 0

// Bonus matrix re-derived from JessicaBackground.res getBonus (NOT from .affine).
const BONUS = [
  //  str   dex   con   int_  wil   cha
  [20, 10, 15, -5, 5, -5], // 0 Assault
  [-5, 15, 0, 10, 10, 0], // 1 Recon
  [5, 5, 5, 15, 5, -5], // 2 Engineer
  [-5, 5, 0, 10, 10, 10], // 3 Signals
  [0, 5, 20, 10, 10, 0], // 4 Medic
  [10, 0, 10, 5, 0, 10], // 5 Logistics
];

const bonus = (bg, attr) => {
  if (bg < 0 || bg > 5) return 0;
  if (attr < 0 || attr > 5) return 0;
  return BONUS[bg][attr];
};

// start_skill from JessicaBackground.res makeSkillSet background switch:
//   Assault(0)   -> CombatSkill(1)
//   Recon(1)     -> Observation(3)
//   Engineer(2)  -> TechLiteracy(4)
//   Signals(3)   -> TechLiteracy(4)
//   Medic(4)     -> Composure(6)
//   Logistics(5) -> Fieldcraft(5)
//   off-domain   -> -1
const startSkill = (bg) => {
  if (bg === 0) return 1;
  if (bg === 1) return 3;
  if (bg === 2) return 4;
  if (bg === 3) return 4;
  if (bg === 4) return 6;
  if (bg === 5) return 5;
  return -1;
};

export default {
  affine: "JessicaBackground.affine",
  cases: [
    {
      name: "bonus over bg[-2..7] × attr[-1..7]",
      export: "bonus",
      args: [[-2, 7], [-1, 7]],
      oracle: (bg, attr) => bonus(bg, attr),
    },
    {
      name: "start_skill over [-2..8]",
      export: "start_skill",
      args: [[-2, 8]],
      oracle: (bg) => startSkill(bg),
    },
  ],
};
