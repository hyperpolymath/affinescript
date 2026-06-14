// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Kernel_Crypto.affine (idaptik crypto-domain kernel
// gate; scalar i32 ABI). Every oracle is an INDEPENDENT JS reimplementation
// derived from src/shared/Kernel_Crypto.res semantics (the rate-limit check, the
// strength>=4 quantum route, the 429/402/proceed selection) -- NOT a copy of the
// .affine logic -- so the differential sweep is a genuine cross-check. The .res
// frames these as async promise<executionResult> with message strings; here only
// the scalar status crosses.

const MAX = 3;
const THRESHOLD = 4;

const oCryptoStatus = (active, isCrack, strength, hasQuantum) => {
  if (active >= MAX) return 429;
  if (isCrack === 1) {
    if (strength >= THRESHOLD) {
      return hasQuantum === 1 ? 0 : 402;
    }
    return 0;
  }
  return 0;
};

export default {
  affine: "Kernel_Crypto.affine",
  cases: [
    {
      name: "max_concurrent_crypto()",
      export: "max_concurrent_crypto",
      args: [],
      oracle: () => 3,
    },
    {
      name: "crack_strength_threshold()",
      export: "crack_strength_threshold",
      args: [],
      oracle: () => 4,
    },
    {
      name: "crypto_status over active in [0..5], is_crack in {0,1}, strength in [0..6], has_quantum in {0,1}",
      export: "crypto_status",
      args: [[0, 5], { values: [0, 1] }, [0, 6], { values: [0, 1] }],
      oracle: oCryptoStatus,
    },
  ],
};
