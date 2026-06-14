// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Logo.affine (idaptik bouncing-logo direction
// selection; scalar i32 ABI). The oracle re-derives directionFromInt's `mod(i,4)`
// fold INDEPENDENTLY in plain JS so a codegen regression or a transcription error
// in the .affine surfaces as a differential mismatch. The oracle uses the same
// negative-fold convention the .affine documents (JS `%` is truncated, matching
// ReScript `mod`), so the band is total over the tested [-6..9] range.
const dir = (i) => {
  const r = i % 4;
  return r < 0 ? r + 4 : r;
};
export default {
  affine: "Logo.affine",
  cases: [
    {
      name: "logo_direction_count()",
      export: "logo_direction_count",
      args: [],
      oracle: () => 4,
    },
    {
      name: "logo_direction_from_int over [-6..9]",
      export: "logo_direction_from_int",
      args: [[-6, 9]],
      oracle: (i) => dir(i),
    },
  ],
};
