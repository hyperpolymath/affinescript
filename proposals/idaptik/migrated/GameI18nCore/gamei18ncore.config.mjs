// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for GameI18nCore.affine (idaptik localisation numeric
// core; scalar i32 ABI). The oracle re-derives the 0..4 language band, its cycle
// and the singular/plural rule in plain JS, independently of the .affine, so a
// codegen regression surfaces as a differential mismatch.

const validLang = (c) => c >= 0 && c <= 4;
// Independent re-derivation of language_clamp: fold unknown onto EN (0).
const clampLang = (c) => (validLang(c) ? c : 0);
// Independent re-derivation of next_language: cycle EN->ES->FR->DE->JA->EN.
const nextLang = (c) => {
  const k = clampLang(c);
  return k === 4 ? 0 : k + 1;
};
// Independent re-derivation of prev_language: reverse cycle.
const prevLang = (c) => {
  const k = clampLang(c);
  return k === 0 ? 4 : k - 1;
};

export default {
  affine: "GameI18nCore.affine",
  cases: [
    { name: "language_count()", export: "language_count", args: [], oracle: () => 5 },
    {
      name: "is_valid_language over [-3..8]",
      export: "is_valid_language",
      args: [[-3, 8]],
      oracle: (c) => (validLang(c) ? 1 : 0),
    },
    {
      name: "language_clamp over [-3..8]",
      export: "language_clamp",
      args: [[-3, 8]],
      oracle: clampLang,
    },
    {
      name: "next_language over [-3..8]",
      export: "next_language",
      args: [[-3, 8]],
      oracle: nextLang,
    },
    {
      name: "prev_language over [-3..8]",
      export: "prev_language",
      args: [[-3, 8]],
      oracle: prevLang,
    },
    {
      name: "select_plural_form over [-2..6]",
      export: "select_plural_form",
      args: [[-2, 6]],
      oracle: (count) => (count === 1 ? 0 : 1),
    },
  ],
};
