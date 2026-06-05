// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmBitwise.affine (reversible VM bitwise opcodes;
// scalar i32 ABI). Oracles re-derive FLIP/XOR/ROL/ROR independently using JS's
// native 32-bit bitwise ops (including the logical >>> the kernel has to emulate)
// plus the reversibility round-trips (oracle = identity).
const I = { values: [-2147483648, -1000000, -2, -1, 0, 1, 7, 1000000, 1431655765, -1431655766, 2147483647] };
const BITS = { values: [1, 2, 7, 8, 16, 24, 31] };
const rol = (a, n) => ((a << n) | (a >>> (32 - n))) | 0;
const ror = (a, n) => ((a >>> n) | (a << (32 - n))) | 0;
export default {
  affine: "VmBitwise.affine",
  cases: [
    { name: "flip ~a", export: "flip", args: [I], oracle: (a) => ~a | 0 },
    { name: "flip_roundtrip == a", export: "flip_roundtrip", args: [I], oracle: (a) => a | 0 },
    { name: "xor a^b", export: "xor", args: [I, I], oracle: (a, b) => (a ^ b) | 0 },
    { name: "xor_roundtrip == a", export: "xor_roundtrip", args: [I, I], oracle: (a, b) => a | 0 },
    { name: "rol == js rotl", export: "rol", args: [I, BITS], oracle: (a, n) => rol(a, n) },
    { name: "ror == js rotr", export: "ror", args: [I, BITS], oracle: (a, n) => ror(a, n) },
    // --- reversibility: ROR undoes ROL and vice versa (oracle = identity) ---
    { name: "rol_roundtrip == a", export: "rol_roundtrip", args: [I, BITS], oracle: (a, n) => a | 0 },
    { name: "ror_roundtrip == a", export: "ror_roundtrip", args: [I, BITS], oracle: (a, n) => a | 0 },
  ],
};
