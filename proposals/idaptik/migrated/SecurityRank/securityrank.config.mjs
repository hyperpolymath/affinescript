// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for SecurityRank.affine (idaptik device security-level
// ranking co-processor). Oracle re-derives the 0..3 clamp and filter from the
// original DeviceRegistry.byMinSecurityLevel switch in SecurityRankCoprocessor.res
// and SecurityRank.res source semantics.
//
// Original logic (ReScript):
//   rank switch: Open=0, Weak=1, Medium=2, Strong=3; out-of-band clamps
//   filter: rank(device) >= rank(threshold) -> 1 (keep), else 0

// Independent oracle: rank from the original ReScript switch semantics
function oracleRank(level) {
  if (level === 0) return 0; // Open
  if (level === 1) return 1; // Weak
  if (level === 2) return 2; // Medium
  if (level === 3) return 3; // Strong
  // Invalid: clamp -- below 0 -> 0, above 3 -> 3
  return level < 0 ? 0 : 3;
}

function oraclePassesMin(deviceLevel, threshold) {
  return oracleRank(deviceLevel) >= oracleRank(threshold) ? 1 : 0;
}

export default {
  affine: "SecurityRank.affine",
  cases: [
    {
      name: "rank_security_level over [-2..6]",
      export: "rank_security_level",
      args: [[-2, 6]],
      oracle: (level) => oracleRank(level) | 0,
    },
    {
      name: "passes_min_security: device x threshold over [-1..4] x [-1..4]",
      export: "passes_min_security",
      args: [[-1, 4], [-1, 4]],
      oracle: (d, t) => oraclePassesMin(d, t) | 0,
    },
  ],
};
