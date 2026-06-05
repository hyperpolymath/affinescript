// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmMemory.affine (additive LOAD/STORE; scalar i32
// ABI). Oracles re-derive vm/lib/ocaml/{Load,Store}.res value transforms
// independently. The memory array is host-side; only the additive value
// transform crosses the boundary. Both sides normalise to i32 (| 0).
const I = { values: [-2147483648, -1000000, -7, -1, 0, 1, 7, 1000000, 2147483647] };
const i32 = (x) => x | 0;

export default {
  affine: "VmMemory.affine",
  cases: [
    { name: "load_fwd reg+mem", export: "load_fwd", args: [I, I], oracle: (reg, mem) => i32(reg + mem) },
    { name: "load_inv reg-mem", export: "load_inv", args: [I, I], oracle: (reg, mem) => i32(reg - mem) },
    { name: "load_roundtrip == reg", export: "load_roundtrip", args: [I, I], oracle: (reg, mem) => i32(reg) },
    { name: "store_fwd mem+reg", export: "store_fwd", args: [I, I], oracle: (mem, reg) => i32(mem + reg) },
    { name: "store_inv mem-reg", export: "store_inv", args: [I, I], oracle: (mem, reg) => i32(mem - reg) },
    { name: "store_roundtrip == mem", export: "store_roundtrip", args: [I, I], oracle: (mem, reg) => i32(mem) },
  ],
};
