// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Terminal.affine (idaptik terminal command-processor
// integer brain; scalar i32 ABI). Each oracle is an INDEPENDENT JS
// reimplementation of the corresponding integer logic in
// idaptik/src/app/devices/Terminal.res, so a codegen regression surfaces as a
// differential mismatch.
//
// Source-of-truth cross-references (Terminal.res line numbers):
//   command taxonomy ...... handleBuiltInCommand switch (213-918) + the
//                           FeaturePacks.isInvertibleProgrammingEnabled guards
//                           at 592/599/609/619/627/698/902; codes 0..16 ungated,
//                           17..23 the invertible-programming pack.
//   ssh_* ................. getActiveState (199-209), ssh push (418), exit
//                           pop (478-510).
//   output_evict_count .... addOutput trim loop (55-62).
//   max_lines ............. make (1063-1064): Float.toInt((h-30)/16).
//   cursor_* .............. update (1108-1114): blink+delta > 0.5 toggles+resets.
//   backspace_len ......... handleKeyInput (964-969): len-1 on Backspace.

// --- independent oracle helpers (re-derived, not imported from the brain) ---

const validCmd = (c) => c >= 0 && c <= 23;
const gatedCmd = (c) => validCmd(c) && c >= 17;

export default {
  affine: "Terminal.affine",
  cases: [
    // --- command taxonomy ---
    {
      name: "ungated_command_count()",
      export: "ungated_command_count",
      args: [],
      oracle: () => 17,
    },
    {
      name: "command_count()",
      export: "command_count",
      args: [],
      oracle: () => 24,
    },
    {
      name: "is_valid_command over [-3..27]",
      export: "is_valid_command",
      args: [[-3, 27]],
      oracle: (c) => (validCmd(c) ? 1 : 0),
    },
    {
      name: "is_gated_command over [-3..27]",
      export: "is_gated_command",
      args: [[-3, 27]],
      oracle: (c) => (gatedCmd(c) ? 1 : 0),
    },
    {
      name: "is_command_available over [-3..27] x {off,on}",
      export: "is_command_available",
      args: [[-3, 27], { values: [0, 1] }],
      oracle: (c, on) => {
        if (!validCmd(c)) return 0;
        if (!gatedCmd(c)) return 1; // ungated: always runs
        return on === 1 ? 1 : 0; // gated: only when feature flag is on
      },
    },
    {
      name: "canonicalise_command over [-3..27]",
      export: "canonicalise_command",
      args: [[-3, 27]],
      oracle: (c) => (validCmd(c) ? c : -1),
    },

    // --- SSH session-stack bounds ---
    {
      name: "ssh_active_index over depth [-2..8]",
      export: "ssh_active_index",
      args: [[-2, 8]],
      oracle: (d) => (d > 0 ? d - 1 : -1),
    },
    {
      name: "ssh_is_remote over depth [-2..8]",
      export: "ssh_is_remote",
      args: [[-2, 8]],
      oracle: (d) => (d > 0 ? 1 : 0),
    },
    {
      name: "ssh_push_depth over depth [-2..8]",
      export: "ssh_push_depth",
      args: [[-2, 8]],
      oracle: (d) => (d < 0 ? 1 : d + 1),
    },
    {
      name: "ssh_pop_depth over depth [-2..8]",
      export: "ssh_pop_depth",
      args: [[-2, 8]],
      oracle: (d) => (d > 0 ? d - 1 : 0),
    },
    {
      name: "ssh_prompt_is_remote_after_exit over depth [-2..8]",
      export: "ssh_prompt_is_remote_after_exit",
      args: [[-2, 8]],
      oracle: (d) => {
        const post = d > 0 ? d - 1 : 0; // ssh_pop_depth
        return post > 0 ? 1 : 0;
      },
    },

    // --- output ring buffer ---
    {
      name: "output_evict_count over count [0..40] x max {-1,0,10,30}",
      export: "output_evict_count",
      args: [[0, 40], { values: [-1, 0, 10, 30] }],
      oracle: (n, max) => {
        const cap = max < 0 ? 0 : max;
        return n > cap ? n - cap : 0;
      },
    },

    // --- maxLines geometry (milli-pixels) ---
    {
      name: "max_lines over height_mpx {0,29999,30000,46000,400000,720000}",
      export: "max_lines",
      args: [{ values: [0, 29999, 30000, 46000, 400000, 720000] }],
      oracle: (h) => {
        const usable = h - 30000;
        if (usable <= 0) return 0;
        return Math.trunc(usable / 16000);
      },
    },

    // --- cursor-blink state machine (milli-seconds) ---
    {
      name: "cursor_blink_next over blink {0,100,400,500} x delta {16,33,200}",
      export: "cursor_blink_next",
      args: [{ values: [0, 100, 400, 500] }, { values: [16, 33, 200] }],
      oracle: (b, d) => {
        const adv = b + d;
        return adv > 500 ? 0 : adv;
      },
    },
    {
      name: "cursor_should_toggle over blink {0,100,400,500} x delta {16,33,200}",
      export: "cursor_should_toggle",
      args: [{ values: [0, 100, 400, 500] }, { values: [16, 33, 200] }],
      oracle: (b, d) => (b + d > 500 ? 1 : 0),
    },

    // --- backspace length clamp ---
    {
      name: "backspace_len over len [-2..12]",
      export: "backspace_len",
      args: [[-2, 12]],
      oracle: (n) => (n > 0 ? n - 1 : 0),
    },
  ],
};
