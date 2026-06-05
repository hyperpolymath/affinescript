// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Diagnostics.affine (idaptik health-band classifier +
// registry sanity check; scalar i32 ABI). Oracle re-derives the <70 / <90 / else
// ladder and the ==10 registry check.
export default {
  affine: "Diagnostics.affine",
  cases: [
    { name: "health_band_count()", export: "health_band_count", args: [], oracle: () => 3 },
    { name: "expected_domain_count()", export: "expected_domain_count", args: [], oracle: () => 10 },
    {
      name: "health_of over [-5..105]",
      export: "health_of",
      args: [[-5, 105]],
      oracle: (pct) => (pct < 70 ? 0 : pct < 90 ? 1 : 2),
    },
    {
      name: "all_domains_registered over [0..15]",
      export: "all_domains_registered",
      args: [[0, 15]],
      oracle: (count) => (count === 10 ? 1 : 0),
    },
  ],
};
