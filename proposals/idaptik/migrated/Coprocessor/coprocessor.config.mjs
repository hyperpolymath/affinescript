// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Coprocessor.affine (idaptik co-processor framework
// core; scalar i32 ABI). Every oracle is an INDEPENDENT JS reimplementation
// derived from src/shared/Coprocessor.res semantics (the Domain variant order
// and the resourceStats mutable folds) -- NOT a copy of the .affine logic -- so
// the differential sweep is a genuine cross-check. The .res frames the Domain as
// a string-keyed variant and the stats as a mutable record; here only the scalar
// ordinal / fold result crosses.

const validDomain = (c) => c >= 0 && c <= 9;

export default {
  affine: "Coprocessor.affine",
  cases: [
    {
      name: "domain_count()",
      export: "domain_count",
      args: [],
      oracle: () => 10,
    },
    {
      name: "is_valid_domain over [-3..14]",
      export: "is_valid_domain",
      args: [[-3, 14]],
      oracle: (c) => (validDomain(c) ? 1 : 0),
    },
    {
      name: "domain_clamp over [-3..14] (sentinel -1 out of band)",
      export: "domain_clamp",
      args: [[-3, 14]],
      oracle: (c) => (validDomain(c) ? c : -1),
    },
    {
      name: "accumulate_metric over total in {0,10,9999}, delta in [-5..50]",
      export: "accumulate_metric",
      args: [{ values: [0, 10, 9999] }, [-5, 50]],
      oracle: (t, d) => t + d,
    },
    {
      name: "increment_calls over [-1..2000]",
      export: "increment_calls",
      args: [[-1, 2000]],
      oracle: (n) => n + 1,
    },
  ],
};
