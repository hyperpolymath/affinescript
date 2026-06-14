// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for CharacterSelectScreen.affine (idaptik character-select
// description-truncation + card-grid placement; scalar i32 ABI). The oracle
// re-derives each function from the ORIGINAL CharacterSelectScreen.res semantics
// (NOT from the .affine) so a codegen regression surfaces as a differential
// mismatch.
//
// Truncation, from CharacterSelectScreen.res lines 282-287:
//   let shortDesc = if String.length(desc) > 45 {
//     String.slice(desc, ~start=0, ~end=42) ++ "..."
//   } else { desc }
// The host keeps `desc`; the brain sees only its length and returns the slice
// end + ellipsis flag. ReScript String.slice with start=0 end=42 yields the
// first 42 chars; the non-truncated branch uses the whole string, i.e. an end
// index equal to its length.
//
// Grid placement, from CharacterSelectScreen.res lines 245-246:
//   let col = mod(idx, 3)
//   let row = idx / 3
// ReScript `mod` and integer `/` on non-negative dense array indices.

// Independent re-derivation of the truncation decision from the .res.
const THRESHOLD = 45;
const CUT = 42;
const needsEllipsis = (len) => (len > THRESHOLD ? 1 : 0);
const sliceEnd = (len) => (len > THRESHOLD ? CUT : len);

// Independent re-derivation of the grid placement from the .res.
// ReScript mod/(/) truncate toward zero; over the swept non-negative domain
// this coincides with JS % and Math.trunc of the quotient.
const cardCol = (idx) => idx % 3;
const cardRow = (idx) => Math.trunc(idx / 3);

export default {
  affine: "CharacterSelectScreen.affine",
  cases: [
    {
      name: "csel_trunc_threshold()",
      export: "csel_trunc_threshold",
      args: [],
      oracle: () => THRESHOLD,
    },
    {
      name: "csel_trunc_cut()",
      export: "csel_trunc_cut",
      args: [],
      oracle: () => CUT,
    },
    {
      name: "csel_needs_ellipsis over len[0..60]",
      export: "csel_needs_ellipsis",
      args: [[0, 60]],
      oracle: (len) => needsEllipsis(len),
    },
    {
      name: "csel_needs_ellipsis boundary 44/45/46 + negative",
      export: "csel_needs_ellipsis",
      args: [{ values: [-3, 0, 44, 45, 46, 100] }],
      oracle: (len) => needsEllipsis(len),
    },
    {
      name: "csel_slice_end over len[0..60]",
      export: "csel_slice_end",
      args: [[0, 60]],
      oracle: (len) => sliceEnd(len),
    },
    {
      name: "csel_slice_end boundary 44/45/46 + negative",
      export: "csel_slice_end",
      args: [{ values: [-3, 0, 44, 45, 46, 100] }],
      oracle: (len) => sliceEnd(len),
    },
    {
      name: "csel_grid_cols()",
      export: "csel_grid_cols",
      args: [],
      oracle: () => 3,
    },
    {
      name: "csel_card_col over idx[0..11]",
      export: "csel_card_col",
      args: [[0, 11]],
      oracle: (idx) => cardCol(idx),
    },
    {
      name: "csel_card_row over idx[0..11]",
      export: "csel_card_row",
      args: [[0, 11]],
      oracle: (idx) => cardRow(idx),
    },
  ],
};
