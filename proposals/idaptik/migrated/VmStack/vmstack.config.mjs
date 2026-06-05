// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmStack.affine (PUSH/POP stack-pointer arithmetic;
// scalar i32 ABI). Oracles re-derive vm/lib/ocaml/{Push,Pop}.res. The stack
// slots + pointer are host-side state; only the pointer arithmetic + register
// clear cross the boundary. Both sides normalise to i32 (| 0).
const I = { values: [-2147483648, -1000000, -7, -1, 0, 1, 7, 1000000, 2147483647] };
const i32 = (x) => x | 0;

export default {
  affine: "VmStack.affine",
  cases: [
    { name: "push_sp sp+1", export: "push_sp", args: [I], oracle: (sp) => i32(sp + 1) },
    { name: "pop_sp sp-1", export: "pop_sp", args: [I], oracle: (sp) => i32(sp - 1) },
    { name: "sp_roundtrip == sp", export: "sp_roundtrip", args: [I], oracle: (sp) => i32(sp) },
    { name: "reg_clear == 0", export: "reg_clear", args: [], oracle: () => 0 },
  ],
};
