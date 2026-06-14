// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for VeriSimModality.affine (idaptik VeriSimDB taxonomy;
// scalar i32 ABI). The oracle re-derives the closed modality band, the ordinal
// drift-level band, and the actionable-drift decision from VeriSimTypes.res
// semantics in plain JS, so a codegen regression surfaces as a differential
// mismatch.
//
// Modality ordinals (VeriSimTypes.res modality declaration order):
//   0 Graph 1 Vector 2 Tensor 3 Semantic 4 Document 5 Temporal 6 Provenance 7 Spatial
// Drift-level ordinals (VeriSimTypes.res driftLevel order, severity-ranked):
//   0 Stable 1 Low 2 Moderate 3 High 4 Critical

const validModality = (c) => c >= 0 && c <= 7;
const validDrift = (c) => c >= 0 && c <= 4;

export default {
  affine: "VeriSimModality.affine",
  cases: [
    {
      name: "modality_count()",
      export: "modality_count",
      args: [],
      oracle: () => 8,
    },
    {
      name: "is_valid_modality over [-3..11]",
      export: "is_valid_modality",
      args: [[-3, 11]],
      oracle: (c) => (validModality(c) ? 1 : 0),
    },
    {
      name: "clamp_modality over [-3..11]",
      export: "clamp_modality",
      args: [[-3, 11]],
      oracle: (c) => (validModality(c) ? c : -1),
    },
    {
      name: "drift_level_count()",
      export: "drift_level_count",
      args: [],
      oracle: () => 5,
    },
    {
      name: "is_valid_drift_level over [-3..8]",
      export: "is_valid_drift_level",
      args: [[-3, 8]],
      oracle: (c) => (validDrift(c) ? 1 : 0),
    },
    {
      name: "clamp_drift_level over [-3..8]",
      export: "clamp_drift_level",
      args: [[-3, 8]],
      oracle: (c) => (validDrift(c) ? c : -1),
    },
    {
      // Actionable iff the level is Moderate(2) or worse, AND in band.
      name: "drift_is_actionable over [-3..8]",
      export: "drift_is_actionable",
      args: [[-3, 8]],
      oracle: (c) => (validDrift(c) && c >= 2 ? 1 : 0),
    },
    {
      // Critical iff level is exactly Critical(4).
      name: "drift_is_critical over [-3..8]",
      export: "drift_is_critical",
      args: [[-3, 8]],
      oracle: (c) => (validDrift(c) && c === 4 ? 1 : 0),
    },
  ],
};
