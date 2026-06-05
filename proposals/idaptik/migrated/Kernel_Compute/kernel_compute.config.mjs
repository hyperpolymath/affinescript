// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Kernel_Compute.affine (idaptik compute-domain
// scheduler gate; scalar i32 ABI). The oracle re-derives the per-domain element
// cap and the 504/413/404/0 precedence in plain JS.
const limit = (d) =>
  (d === 1 || d === 4 || d === 7) ? 513 : (d === 2 || d === 6) ? 16 : (d === 8) ? 259 : 1024;
export default {
  affine: "Kernel_Compute.affine",
  cases: [
    { name: "max_concurrent_compute()", export: "max_concurrent_compute", args: [], oracle: () => 10 },
    {
      name: "data_limit_for_domain over [-2..12]",
      export: "data_limit_for_domain",
      args: [[-2, 12]],
      oracle: (d) => limit(d),
    },
    {
      // domain x active_calls x data_len x has_backend = 10 x 4 x 9 x 2 = 720 inputs
      name: "compute_gate grid (precedence: 504 > 413 > 404 > 0)",
      export: "compute_gate",
      args: [
        [0, 9],
        [8, 11],
        { values: [0, 16, 17, 259, 260, 513, 514, 1024, 1025] },
        [0, 1],
      ],
      oracle: (domain, active, len, hb) => {
        if (active >= 10) return 504;
        if (len > limit(domain)) return 413;
        if (hb === 0) return 404;
        return 0;
      },
    },
  ],
};
