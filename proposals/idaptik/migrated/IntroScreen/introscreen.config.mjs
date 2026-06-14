// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for IntroScreen.affine (idaptik tutorial paginator brain;
// scalar i32 ABI). The oracle re-derives each function from the ORIGINAL
// IntroScreen.res page-flip semantics (NOT from the .affine) so a codegen
// regression surfaces as a differential mismatch.
//
// From IntroScreen.res Next handler (lines 240-249):
//   if currentPage.contents < Array.length(pages) - 1 {
//     currentPage := currentPage.contents + 1; renderPage()
//   } else { goToWorldMap() }     // last page: start the game, index unchanged
// From the indicator (line 213): `${currentPage + 1} / ${length}`
// From the last-page test (line 217): currentPage == Array.length(pages) - 1
// The page count is host data (feature-filtered array), passed in as `count`.

// Independent re-derivation of the paginator validity (count >= 1, in-range).
const posValid = (cur, count) => (count >= 1 && cur >= 0 && cur < count ? 1 : 0);

// Independent re-derivation of the last-page test from the .res.
const isLast = (cur, count) => {
  if (!posValid(cur, count)) return 0;
  return cur === count - 1 ? 1 : 0;
};

// Independent re-derivation of the Next-button index transition from the .res.
const nextPage = (cur, count) => {
  if (!posValid(cur, count)) return -1;
  return cur < count - 1 ? cur + 1 : cur; // last page: index stays put
};

// Independent re-derivation of the 1-based indicator number from the .res.
const pageLabel = (cur) => (cur < 0 ? -1 : cur + 1);

export default {
  affine: "IntroScreen.affine",
  cases: [
    {
      name: "intro_pos_valid over (current[-2..8], count{0..6})",
      export: "intro_pos_valid",
      args: [[-2, 8], { values: [0, 1, 2, 3, 4, 5, 6] }],
      oracle: (cur, count) => posValid(cur, count),
    },
    {
      name: "intro_is_last_page over (current[-2..8], count{0..6})",
      export: "intro_is_last_page",
      args: [[-2, 8], { values: [0, 1, 2, 3, 4, 5, 6] }],
      oracle: (cur, count) => isLast(cur, count),
    },
    {
      name: "intro_next_page over (current[-2..8], count{0..6})",
      export: "intro_next_page",
      args: [[-2, 8], { values: [0, 1, 2, 3, 4, 5, 6] }],
      oracle: (cur, count) => nextPage(cur, count),
    },
    {
      // The two real page counts the feature flag toggles between (4 core + VM = 5).
      name: "intro_next_page over real counts {4,5}",
      export: "intro_next_page",
      args: [{ values: [0, 1, 2, 3, 4 ] }, { values: [4, 5] }],
      oracle: (cur, count) => nextPage(cur, count),
    },
    {
      name: "intro_page_label over current[-2..10]",
      export: "intro_page_label",
      args: [[-2, 10]],
      oracle: (cur) => pageLabel(cur),
    },
  ],
};
