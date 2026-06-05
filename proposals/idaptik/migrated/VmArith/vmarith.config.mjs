// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmArith.affine (reversible VM arithmetic opcodes;
// scalar i32 ABI). Oracles re-derive each opcode value transform AND the
// reversibility round-trips (oracle = identity) independently in JS. Results are
// normalised to i32 (| 0) on both sides, so two's-complement overflow is covered.
const I = { values: [-2147483648, -1000000, -7, -1, 0, 1, 7, 1000000, 2147483647] };
export default {
  affine: "VmArith.affine",
  cases: [
    { name: "add_fwd a+b", export: "add_fwd", args: [I, I], oracle: (a, b) => (a + b) | 0 },
    { name: "add_inv a-b", export: "add_inv", args: [I, I], oracle: (a, b) => (a - b) | 0 },
    { name: "sub_fwd a-b", export: "sub_fwd", args: [I, I], oracle: (a, b) => (a - b) | 0 },
    { name: "sub_inv a+b", export: "sub_inv", args: [I, I], oracle: (a, b) => (a + b) | 0 },
    { name: "negate -a", export: "negate", args: [I], oracle: (a) => (-a) | 0 },
    { name: "noop a", export: "noop", args: [I], oracle: (a) => a | 0 },
    { name: "swap_first -> b", export: "swap_first", args: [I, I], oracle: (a, b) => b | 0 },
    { name: "swap_second -> a", export: "swap_second", args: [I, I], oracle: (a, b) => a | 0 },
    // --- reversibility: invert undoes execute (oracle = identity) ---
    { name: "add_roundtrip == a", export: "add_roundtrip", args: [I, I], oracle: (a, b) => a | 0 },
    { name: "sub_roundtrip == a", export: "sub_roundtrip", args: [I, I], oracle: (a, b) => a | 0 },
    { name: "negate_roundtrip == a", export: "negate_roundtrip", args: [I], oracle: (a) => a | 0 },
    { name: "noop_roundtrip == a", export: "noop_roundtrip", args: [I], oracle: (a) => a | 0 },
    { name: "swap_roundtrip_first == a", export: "swap_roundtrip_first", args: [I, I], oracle: (a, b) => a | 0 },
    { name: "swap_roundtrip_second == b", export: "swap_roundtrip_second", args: [I, I], oracle: (a, b) => b | 0 },
  ],
};
