// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for StateDiff.affine (idaptik VM state-diff kernel;
// scalar i32 ABI). The oracle re-derives the per-variable change/mismatch truth
// tables straight from StateDiff.res in plain JS, so a codegen regression
// surfaces as a differential mismatch. option<int> is lowered to a (present,
// value) pair: present in {0,1}; value swept over a small signed band so the
// equal / not-equal split is exercised on both sides.
//
// Oracle re-derivations (independent of the .affine):
//   change_flag   <- StateDiff.res:25-30  (the `changed` switch)
//   matches_target<- StateDiff.res:146-149 (getMismatches per-key switch, negated)
//   is_mismatch   <- StateDiff.res:142-150 (the filter predicate itself)
const changeFlag = (bp, bv, ap, av) => {
  if (bp === 1 && ap === 1) return bv !== av ? 1 : 0; // (Some,Some) -> v1!=v2
  if (bp === 0 && ap === 1) return 1; // (None,Some) -> true
  if (bp === 1 && ap === 0) return 1; // (Some,None) -> true
  return 0; // (None,None) -> false
};
const matchesTarget = (cp, cv, tp, tv) => {
  if (cp === 1 && tp === 1) return cv === tv ? 1 : 0; // both Some -> equal?
  return 0; // any absence -> getMismatches `_` arm -> not a match
};
const isMismatch = (cp, cv, tp, tv) => (matchesTarget(cp, cv, tp, tv) === 1 ? 0 : 1);

// Presence in {0,1}; values swept over a 3-wide band {-1,0,1} so equal and
// not-equal cases both fire under the Cartesian product.
const PRESENT = { values: [0, 1] };
const VAL = { values: [-1, 0, 1] };

export default {
  affine: "StateDiff.affine",
  cases: [
    {
      name: "change_flag over present^2 x value^2",
      export: "change_flag",
      args: [PRESENT, VAL, PRESENT, VAL],
      oracle: changeFlag,
    },
    {
      name: "matches_target over present^2 x value^2",
      export: "matches_target",
      args: [PRESENT, VAL, PRESENT, VAL],
      oracle: matchesTarget,
    },
    {
      name: "is_mismatch over present^2 x value^2",
      export: "is_mismatch",
      args: [PRESENT, VAL, PRESENT, VAL],
      oracle: isMismatch,
    },
  ],
};
