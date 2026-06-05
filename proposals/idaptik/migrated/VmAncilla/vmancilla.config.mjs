// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmAncilla.affine (Bennett-ancilla AND/OR; scalar i32
// ABI). Oracles re-derive the forward computations and the ancilla round-trip
// (which, starting from a zeroed ancilla, returns 0 regardless of a,b).
const I = { values: [-2147483648, -1000000, -1, 0, 1, 7, 12, 10, 1000000, 2147483647] };
export default {
  affine: "VmAncilla.affine",
  cases: [
    { name: "and_compute a&b", export: "and_compute", args: [I, I], oracle: (a, b) => (a & b) | 0 },
    { name: "or_compute a|b", export: "or_compute", args: [I, I], oracle: (a, b) => (a | b) | 0 },
    { name: "ancilla_clear == 0", export: "ancilla_clear", args: [], oracle: () => 0 },
    // --- ancilla round-trip (precondition c0==0): 0 -> a∘b -> 0 ---
    { name: "and_ancilla_roundtrip == 0", export: "and_ancilla_roundtrip", args: [I, I], oracle: (a, b) => 0 },
    { name: "or_ancilla_roundtrip == 0", export: "or_ancilla_roundtrip", args: [I, I], oracle: (a, b) => 0 },
  ],
};
