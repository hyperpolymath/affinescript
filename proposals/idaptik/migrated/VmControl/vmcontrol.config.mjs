// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VmControl.affine (IF_POS/IF_ZERO/LOOP branch
// predicates; scalar i32 ABI). Oracles re-derive vm/lib/ocaml/{IfPos,IfZero,
// Loop}.res predicates. The branch bodies are host-orchestrated; only the
// scalar predicate (1 = take/continue, 0 = skip/stop) crosses the boundary.
const I = { values: [-2147483648, -1000000, -7, -1, 0, 1, 7, 1000000, 2147483647] };
const i32 = (x) => x | 0;

export default {
  affine: "VmControl.affine",
  cases: [
    { name: "if_pos v>0 -> 1/0", export: "if_pos", args: [I], oracle: (v) => (v > 0 ? 1 : 0) },
    { name: "if_zero v==0 -> 1/0", export: "if_zero", args: [I], oracle: (v) => (v === 0 ? 1 : 0) },
    { name: "loop_continue exit!=0 && iters<max", export: "loop_continue", args: [I, I, I], oracle: (e, i, m) => (e === 0 ? 0 : (i < m ? 1 : 0)) },
    { name: "loop_iter_next iters+1", export: "loop_iter_next", args: [I], oracle: (i) => i32(i + 1) },
  ],
};
