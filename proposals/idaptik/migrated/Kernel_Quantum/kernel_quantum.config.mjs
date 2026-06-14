// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Kernel_Quantum.affine (idaptik quantum-domain kernel
// gate; scalar i32 ABI). Every oracle is an INDEPENDENT JS reimplementation
// derived from src/shared/Kernel_Quantum.res semantics (the >50 J decoherence
// guard scaled to mJ, the 5000 ms cooldown, the 503/proceed selection) -- NOT a
// copy of the .affine logic -- so the differential sweep is a genuine cross-check.
// The .res frames these as async promise<executionResult> with message strings
// and a float Date.now clock; here only the scalar status crosses, with energy in
// milli-Joules and time in whole milliseconds.

const LIMIT_MJ = 50000;
const COOLDOWN_MS = 5000;

const oQuantumStatus = (energyMj, elapsedMs, hasLast) => {
  if (energyMj > LIMIT_MJ) return 503;
  if (hasLast === 1) {
    return elapsedMs < COOLDOWN_MS ? 503 : 0;
  }
  return 0;
};

export default {
  affine: "Kernel_Quantum.affine",
  cases: [
    {
      name: "decoherence_limit_mj()",
      export: "decoherence_limit_mj",
      args: [],
      oracle: () => 50000,
    },
    {
      name: "cooldown_ms()",
      export: "cooldown_ms",
      args: [],
      oracle: () => 5000,
    },
    {
      name: "quantum_status over energy {0,49999,50000,50001,80000}, elapsed {0,4999,5000,5001,12000}, has_last {0,1}",
      export: "quantum_status",
      args: [
        { values: [0, 49999, 50000, 50001, 80000] },
        { values: [0, 4999, 5000, 5001, 12000] },
        { values: [0, 1] },
      ],
      oracle: oQuantumStatus,
    },
  ],
};
