// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Kernel.affine (idaptik scheduler gate + domain
// router; scalar i32 ABI). Oracles re-derive the 503/404/0 pre-flight gate and
// the domain->handler-class routing table.
const route = (d) => {
  if (d === 3) return 1; // io
  if (d === 0) return 2; // crypto
  if (d === 5) return 3; // quantum
  if (d === 1 || d === 2 || d === 4 || d === 6 || d === 7 || d === 8) return 4; // compute
  if (d === 9) return 5; // graphics direct
  return 0; // out of band
};
export default {
  affine: "Kernel.affine",
  cases: [
    {
      name: "execute_gate over [0,1]^2",
      export: "execute_gate",
      args: [[0, 1], [0, 1]],
      oracle: (cap, hb) => (cap === 0 ? 503 : hb === 0 ? 404 : 0),
    },
    {
      name: "route_of over [-2..12]",
      export: "route_of",
      args: [[-2, 12]],
      oracle: (d) => route(d),
    },
  ],
};
