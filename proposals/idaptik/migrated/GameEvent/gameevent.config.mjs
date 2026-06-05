// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for GameEvent.affine (idaptik alertLevel + direction
// taxonomies; scalar i32 ABI). alertLevel clamps to nearest (ordered);
// direction maps out-of-band to -1 (flat). Oracles re-derive both.
const vAlert = (c) => c >= 0 && c <= 3;
const vDir = (c) => c >= 0 && c <= 5;
export default {
  affine: "GameEvent.affine",
  cases: [
    { name: "alert_level_count()", export: "alert_level_count", args: [], oracle: () => 4 },
    { name: "direction_count()", export: "direction_count", args: [], oracle: () => 6 },
    { name: "is_valid_alert_level over [-3..7]", export: "is_valid_alert_level", args: [[-3, 7]], oracle: (c) => (vAlert(c) ? 1 : 0) },
    { name: "clamp_alert_level over [-3..7]", export: "clamp_alert_level", args: [[-3, 7]], oracle: (c) => (vAlert(c) ? c : (c < 0 ? 0 : 3)) },
    { name: "is_valid_direction over [-3..9]", export: "is_valid_direction", args: [[-3, 9]], oracle: (c) => (vDir(c) ? 1 : 0) },
    { name: "clamp_direction over [-3..9]", export: "clamp_direction", args: [[-3, 9]], oracle: (c) => (vDir(c) ? c : -1) },
  ],
};
