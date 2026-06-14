// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for AmbushObjective.affine (idaptik AssassinTraining
// survival-objective kernel; scalar i32 ABI). Each oracle re-derives the rule
// straight from AssassinTraining.res INDEPENDENTLY of the .affine, so a codegen
// regression surfaces as a differential mismatch.
//
// Oracle re-derivation (rule <- source site), independent of the .affine:
//   ambush_target()         = 3                                   (res:152, the literal 3)
//   is_objective_met(a)     = a >= 3 ? 1 : 0                      (res:152, `attempts >= 3`)
//   ambushes_survived(a)    = clamp a onto 0..3                   (HUD "N / 3", res:150,160)
//   ambushes_remaining(a)   = a<0 ? 3 : a>=3 ? 0 : 3-a            (complement of progress)

// GREATER-OR-EQUAL boundary, faithful to the .res `>=` at res:152.
const isObjectiveMet = (a) => (a >= 3 ? 1 : 0);
// Clamp the host count onto the closed 0..3 band (negatives -> 0, over -> 3).
const ambushesSurvived = (a) => (a < 0 ? 0 : a > 3 ? 3 : a);
// Complement of the clamped progress against the target 3; total, never negative.
const ambushesRemaining = (a) => (a < 0 ? 3 : a >= 3 ? 0 : 3 - a);

// Sweep the host count across the band edges and just-inside/just-outside each cut:
// the malformed negatives, 0, the 1/2/3 progression (2 = not-yet, 3 = exact win),
// and over-counts (4..6) plus a large overflow so every clamp branch fires.
const ATTEMPTS = [-5, -1, 0, 1, 2, 3, 4, 5, 6, 100];

export default {
  affine: "AmbushObjective.affine",
  cases: [
    { name: "ambush_target()", export: "ambush_target", args: [], oracle: () => 3 },
    {
      name: "is_objective_met over [-5..6]",
      export: "is_objective_met",
      args: [{ values: ATTEMPTS }],
      oracle: isObjectiveMet,
    },
    {
      name: "ambushes_survived over [-5..6]",
      export: "ambushes_survived",
      args: [{ values: ATTEMPTS }],
      oracle: ambushesSurvived,
    },
    {
      name: "ambushes_remaining over [-5..6]",
      export: "ambushes_remaining",
      args: [{ values: ATTEMPTS }],
      oracle: ambushesRemaining,
    },
  ],
};
