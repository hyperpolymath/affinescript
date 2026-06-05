// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for SecurityRank.affine (idaptik device-security
// ranking co-processor; scalar i32 ABI).
//
// Two parity cases:
//   1. rank_security_level(level) over level in [-5..8]            -> 14 inputs
//   2. passes_min_security(device_level, threshold) over [-2..5]^2 -> 64 inputs
// Total: 78 inputs. Expect 78/78 pass.
//
//## The oracle (independent of the .affine)
// The .affine clamps a host integer onto the closed 0..3 rank band:
//   level   rank
//     0      0   (Open)
//     1      1   (Weak)
//     2      2   (Medium)
//     3      3   (Strong)
//   <0       0   (clamp down -- Invalid below Open)
//   >3       3   (clamp up   -- Invalid above Strong)
// We re-derive that here in plain JS rather than reusing any AffineScript code,
// so a regression in the compiler/codegen surfaces as a differential mismatch.
const oracleRank = (lv) =>
  lv === 0 ? 0 : lv === 1 ? 1 : lv === 2 ? 2 : lv === 3 ? 3 : (lv < 0 ? 0 : 3);

export default {
  affine: "SecurityRank.affine",
  cases: [
    {
      name: "rank_security_level over [-5..8]",
      export: "rank_security_level",
      args: [[-5, 8]],
      oracle: (level) => oracleRank(level),
    },
    {
      name: "passes_min_security over [-2..5]^2",
      export: "passes_min_security",
      args: [[-2, 5], [-2, 5]],
      // keep a device iff its rank >= the threshold's rank
      oracle: (dl, th) => (oracleRank(dl) >= oracleRank(th) ? 1 : 0),
    },
  ],
};
