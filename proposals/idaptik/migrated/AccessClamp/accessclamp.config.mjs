// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for AccessClamp.affine (idaptik accessibility-preference
// range clamps in milli-units; scalar i32 ABI). The oracle re-derives the
// Math.max(lo, Math.min(hi, v)) clamps from AccessibilitySettings.res
// INDEPENDENTLY in plain JS (milli-scaled bounds), so a codegen regression
// surfaces as a differential mismatch.

// Independent re-derivation of the ReScript Math.max(lo, Math.min(hi, v)).
const clampRange = (v, lo, hi) => Math.max(lo, Math.min(hi, v));
const FS_LO = 750, FS_HI = 2000, GS_LO = 250, GS_HI = 1000;

export default {
  affine: "AccessClamp.affine",
  cases: [
    { name: "font_scale_min_milli()", export: "font_scale_min_milli", args: [], oracle: () => 750 },
    { name: "font_scale_max_milli()", export: "font_scale_max_milli", args: [], oracle: () => 2000 },
    { name: "game_speed_min_milli()", export: "game_speed_min_milli", args: [], oracle: () => 250 },
    { name: "game_speed_max_milli()", export: "game_speed_max_milli", args: [], oracle: () => 1000 },
    {
      // Sweep across and well past both bounds, stepping by 1 over the full
      // [0..2500] milli-range would be 2501 cases; a coarser representative sweep
      // over the boundary neighbourhoods plus interior is sufficient and fast.
      name: "clamp_font_scale_milli over boundary + interior",
      export: "clamp_font_scale_milli",
      args: [{ values: [0, 500, 749, 750, 751, 1000, 1375, 1999, 2000, 2001, 2500, -100] }],
      oracle: (v) => clampRange(v, FS_LO, FS_HI),
    },
    {
      name: "clamp_game_speed_milli over boundary + interior",
      export: "clamp_game_speed_milli",
      args: [{ values: [0, 100, 249, 250, 251, 500, 750, 999, 1000, 1001, 1500, -50] }],
      oracle: (v) => clampRange(v, GS_LO, GS_HI),
    },
    {
      name: "font_scale_in_band over boundary neighbourhood",
      export: "font_scale_in_band",
      args: [{ values: [749, 750, 751, 1999, 2000, 2001, 0, 2500] }],
      oracle: (v) => (v >= FS_LO && v <= FS_HI ? 1 : 0),
    },
    {
      name: "game_speed_in_band over boundary neighbourhood",
      export: "game_speed_in_band",
      args: [{ values: [249, 250, 251, 999, 1000, 1001, 0, 1500] }],
      oracle: (v) => (v >= GS_LO && v <= GS_HI ? 1 : 0),
    },
  ],
};
