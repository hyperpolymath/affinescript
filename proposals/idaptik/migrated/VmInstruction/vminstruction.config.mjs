// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmInstruction.affine (VM opcode taxonomy; scalar i32
// ABI). Oracle re-derives the 0..22 opcode band, the tier classifier, and the
// tier-admission predicate.
const validOp = (op) => op >= 0 && op <= 22;
const tierOf = (op) => !validOp(op) ? -1 : op <= 12 ? 0 : op <= 15 ? 1 : op <= 19 ? 2 : op === 20 ? 3 : 4;
export default {
  affine: "VmInstruction.affine",
  cases: [
    { name: "opcode_count()", export: "opcode_count", args: [], oracle: () => 23 },
    { name: "tier_count()", export: "tier_count", args: [], oracle: () => 5 },
    { name: "is_valid_opcode over [-3..26]", export: "is_valid_opcode", args: [[-3, 26]], oracle: (op) => (validOp(op) ? 1 : 0) },
    { name: "clamp_opcode over [-3..26]", export: "clamp_opcode", args: [[-3, 26]], oracle: (op) => (validOp(op) ? op : -1) },
    { name: "tier_of_opcode over [-3..26]", export: "tier_of_opcode", args: [[-3, 26]], oracle: (op) => tierOf(op) },
    {
      name: "opcode_in_tier over [-2..24] x [-1..5]",
      export: "opcode_in_tier",
      args: [[-2, 24], [-1, 5]],
      oracle: (op, pt) => {
        const ot = tierOf(op);
        if (ot < 0) return 0;
        if (pt < 0) return 0;
        return ot <= pt ? 1 : 0;
      },
    },
  ],
};
