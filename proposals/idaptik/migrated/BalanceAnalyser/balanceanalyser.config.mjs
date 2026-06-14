// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for BalanceAnalyser.affine (idaptik impact-classification
// brain; scalar i32 ABI). The oracle re-derives each function from the ORIGINAL
// BalanceAnalyserModel.res semantics (NOT from the .affine) so a codegen
// regression surfaces as a differential mismatch.
//
// From BalanceAnalyserModel.res:
//   getByImpact (lines 348-356): impactValue Minor=>0 Moderate=>1 Major=>2;
//     keep iff impactValue(item.impact) >= impactValue(minImpact)
//   hasCriticalIssues (lines 435-439): r.majorIssues > 0
// Impact variant Minor/Moderate/Major maps to ordinals 0/1/2.

// Independent re-derivation of the impact validity from the .res ordinal domain.
const impactValid = (o) => (o >= 0 && o <= 2 ? 1 : 0);

// Independent re-derivation of impactValue (the variant's ordinal) from the .res.
const impactValue = (o) => (impactValid(o) ? o : -1);

// Independent re-derivation of getByImpact's row filter from the .res.
const passesImpact = (item, min) => (item >= min ? 1 : 0);

// Independent re-derivation of hasCriticalIssues from the .res.
const hasCritical = (n) => (n > 0 ? 1 : 0);

export default {
  affine: "BalanceAnalyser.affine",
  cases: [
    {
      name: "impact_level_count()",
      export: "impact_level_count",
      args: [],
      oracle: () => 3,
    },
    {
      name: "impact_valid over ord[-3..6]",
      export: "impact_valid",
      args: [[-3, 6]],
      oracle: (o) => impactValid(o),
    },
    {
      name: "impact_value over ord[-3..6]",
      export: "impact_value",
      args: [[-3, 6]],
      oracle: (o) => impactValue(o),
    },
    {
      name: "passes_impact over (item[-1..3], min[-1..3])",
      export: "passes_impact",
      args: [[-1, 3], [-1, 3]],
      oracle: (item, min) => passesImpact(item, min),
    },
    {
      name: "passes_impact exact impact pairs 0..2 x 0..2",
      export: "passes_impact",
      args: [{ values: [0, 1, 2] }, { values: [0, 1, 2] }],
      oracle: (item, min) => passesImpact(item, min),
    },
    {
      name: "has_critical_issues over major_count[-2..10]",
      export: "has_critical_issues",
      args: [[-2, 10]],
      oracle: (n) => hasCritical(n),
    },
  ],
};
