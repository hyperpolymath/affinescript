// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for MainHubScreen.affine (idaptik sidebar-navigation
// dispatch; scalar i32 ABI). The oracle re-derives the nav-button handler +
// showPanel dispatch from MainHubScreen.res INDEPENDENTLY in plain JS (the
// navItem 0..10 encoding, the Website/Quit special-cases, and the `id == item`
// content-panel test), so a codegen regression or a transcription error in the
// .affine surfaces as a differential mismatch.

// hub_nav_action: 2 = Website (open external), 1 = Quit (show quit panel),
// 0 = content panel (codes 0..8), -1 = out of band.
const navAction = (nav) => {
  if (nav < 0 || nav > 10) return -1;
  if (nav === 9) return 2; // Website
  if (nav === 10) return 1; // Quit
  return 0; // content panel
};

// hub_panel_visible: showPanel reveals the content panel whose id == the selected
// item; only the 0..8 content band carries a panel.
const panelVisible = (nav, panel) => {
  if (nav < 0 || nav > 8) return 0;
  if (panel < 0 || panel > 8) return 0;
  return nav === panel ? 1 : 0;
};

// hub_quit_panel_visible: only Quit (10) reveals the quit panel.
const quitVisible = (nav) => (nav === 10 ? 1 : 0);

export default {
  affine: "MainHubScreen.affine",
  cases: [
    { name: "hub_nav_item_count()", export: "hub_nav_item_count", args: [], oracle: () => 11 },
    { name: "hub_content_panel_count()", export: "hub_content_panel_count", args: [], oracle: () => 9 },
    {
      name: "hub_nav_action over [-2..13]",
      export: "hub_nav_action",
      args: [[-2, 13]],
      oracle: (nav) => navAction(nav),
    },
    {
      name: "hub_panel_visible over nav x panel ([-1..10] each)",
      export: "hub_panel_visible",
      args: [[-1, 10], [-1, 10]],
      oracle: (nav, panel) => panelVisible(nav, panel),
    },
    {
      name: "hub_quit_panel_visible over [-1..12]",
      export: "hub_quit_panel_visible",
      args: [[-1, 12]],
      oracle: (nav) => quitVisible(nav),
    },
  ],
};
