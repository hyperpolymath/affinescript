// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmPort.affine (RECV/SEND port pointer + additive
// accumulate; scalar i32 ABI). Oracles re-derive vm/lib/ocaml/{Recv,Send}.res.
// The port buffers + pointers are host-side state; only the pointer arithmetic
// and the additive accumulate cross the boundary. Both sides normalise to i32.
const I = { values: [-2147483648, -1000000, -7, -1, 0, 1, 7, 1000000, 2147483647] };
const i32 = (x) => x | 0;

export default {
  affine: "VmPort.affine",
  cases: [
    { name: "recv_acc_fwd reg+val", export: "recv_acc_fwd", args: [I, I], oracle: (reg, val) => i32(reg + val) },
    { name: "recv_acc_inv reg-val", export: "recv_acc_inv", args: [I, I], oracle: (reg, val) => i32(reg - val) },
    { name: "recv_acc_roundtrip == reg", export: "recv_acc_roundtrip", args: [I, I], oracle: (reg, val) => i32(reg) },
    { name: "recv_ptr_fwd ptr+1", export: "recv_ptr_fwd", args: [I], oracle: (ptr) => i32(ptr + 1) },
    { name: "recv_ptr_inv ptr-1", export: "recv_ptr_inv", args: [I], oracle: (ptr) => i32(ptr - 1) },
    { name: "recv_ptr_roundtrip == ptr", export: "recv_ptr_roundtrip", args: [I], oracle: (ptr) => i32(ptr) },
    { name: "send_ptr_fwd ptr+1", export: "send_ptr_fwd", args: [I], oracle: (ptr) => i32(ptr + 1) },
    { name: "send_ptr_inv ptr-1", export: "send_ptr_inv", args: [I], oracle: (ptr) => i32(ptr - 1) },
    { name: "send_ptr_roundtrip == ptr", export: "send_ptr_roundtrip", args: [I], oracle: (ptr) => i32(ptr) },
    { name: "send_clears_slot == 0", export: "send_clears_slot", args: [], oracle: () => 0 },
  ],
};
