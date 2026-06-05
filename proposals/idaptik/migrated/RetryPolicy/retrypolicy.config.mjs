// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for RetryPolicy.affine (idaptik transient classifier +
// policy table; scalar i32 ABI). Oracles re-derive the 503/504/429 transient
// set and the three built-in policies (unknown code -> default).
const transient = (s) => (s === 503 || s === 504 || s === 429 ? 1 : 0);
export default {
  affine: "RetryPolicy.affine",
  cases: [
    {
      name: "is_transient over [420..510]",
      export: "is_transient",
      args: [[420, 510]],
      oracle: (s) => transient(s),
    },
    {
      name: "is_transient over the canonical status set",
      export: "is_transient",
      args: [{ values: [0, 200, 400, 402, 404, 413, 429, 500, 503, 504, 599] }],
      oracle: (s) => transient(s),
    },
    { name: "policy_count()", export: "policy_count", args: [], oracle: () => 3 },
    { name: "policy_max_retries over [-1..5]", export: "policy_max_retries", args: [[-1, 5]], oracle: (p) => (p === 1 ? 1 : p === 2 ? 5 : 3) },
    { name: "policy_base_delay_ms over [-1..5]", export: "policy_base_delay_ms", args: [[-1, 5]], oracle: (p) => (p === 1 ? 10 : p === 2 ? 200 : 50) },
    { name: "policy_backoff_factor_pct over [-1..5]", export: "policy_backoff_factor_pct", args: [[-1, 5]], oracle: (p) => (p === 1 ? 100 : p === 2 ? 150 : 200) },
  ],
};
