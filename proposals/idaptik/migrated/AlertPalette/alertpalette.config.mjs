// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for AlertPalette.affine (idaptik accessibility alert
// colour lookup; scalar i32 ABI). The oracle re-derives the alertColor decision
// tables from ColorPalette.res INDEPENDENTLY in plain JS (hex literals, mirroring
// the ReScript source rather than the .affine's decimal encoding), so a codegen
// regression or a transcription error in the .affine surfaces as a differential
// mismatch.

// The catch-all row for level < 0 or > 4 (the ReScript `_` arm), modelled as row 5.
const row = (lvl) => (lvl < 0 || lvl > 4 ? 5 : lvl);

const HC = [0x00ff00, 0xffff00, 0xffaa00, 0xff4400, 0xff0000, 0xff0000];
const NORMAL = [0x00ff00, 0x88ff00, 0xffff00, 0xff8800, 0xff4400, 0xff0000];
const PROTDEUT = [0x0088ff, 0x44aaff, 0xffdd44, 0xffaa00, 0xff8800, 0xff6600];
const TRITAN = [0x00ee88, 0x66dd66, 0xdddd00, 0xee6688, 0xdd2266, 0xcc0044];
const ACHROMA = [0xffffff, 0xcccccc, 0x999999, 0x777777, 0x444444, 0x222222];

const clampMode = (m) => (m < 0 || m > 4 ? 0 : m);

const alertColour = (hc, mode, level) => {
  const r = row(level);
  if (hc !== 0) return HC[r];
  const m = clampMode(mode);
  if (m === 0) return NORMAL[r];
  if (m === 1 || m === 2) return PROTDEUT[r];
  if (m === 3) return TRITAN[r];
  return ACHROMA[r];
};

export default {
  affine: "AlertPalette.affine",
  cases: [
    {
      name: "clamp_mode over [-3..8]",
      export: "clamp_mode",
      args: [[-3, 8]],
      oracle: (m) => clampMode(m),
    },
    {
      // Full cartesian over the high-contrast path: hc=1, every mode, every level
      // (incl. out-of-band level to exercise the `_` row).
      name: "alert_colour high-contrast path (hc=1) over modes x levels",
      export: "alert_colour",
      args: [{ values: [1, 2, 7] }, [0, 4], [-2, 6]],
      oracle: (hc, mode, level) => alertColour(hc, mode, level),
    },
    {
      // Full cartesian over the normal-path: hc=0, every mode 0..4 (+ out-of-band),
      // every level -2..6.
      name: "alert_colour mode-selected path (hc=0) over modes x levels",
      export: "alert_colour",
      args: [0, [-1, 6], [-2, 6]],
      oracle: (hc, mode, level) => alertColour(hc, mode, level),
    },
  ],
};
