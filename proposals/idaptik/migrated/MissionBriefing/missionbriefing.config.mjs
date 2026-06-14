// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for MissionBriefing.affine (idaptik mission-progression
// co-processor; scalar i32 ABI). The oracles re-derive each closed band / count
// in plain JS -- independently of the .affine source -- so a codegen regression
// surfaces as a differential mismatch. The contract is the bridge's
// (MissionBriefingCoprocessorBridge.js) integer surface: difficulty 1..5,
// objective-state 0..3, 1-based mission index 1..5.

// Independent re-derivations of the per-mission tables (missions array order).
const PAR = { 1: 300, 2: 480, 3: 600, 4: 900, 5: 1200 };
const MAXS = { 1: 0, 2: 1, 3: 2, 4: 1, 5: 0 };
// Independent re-derivation of the LevelConfig guard-count tuning.
const GUARDS = { 1: 1, 2: 2, 3: 2, 4: 3, 5: 2 };

export default {
  affine: "MissionBriefing.affine",
  cases: [
    {
      name: "mission_count()",
      export: "mission_count",
      args: [],
      oracle: () => 5,
    },
    {
      name: "difficulty_value over [-2..8]",
      export: "difficulty_value",
      args: [[-2, 8]],
      oracle: (d) => (d >= 1 && d <= 5 ? d : 0),
    },
    {
      name: "guard_count_for_difficulty over [-2..8]",
      export: "guard_count_for_difficulty",
      args: [[-2, 8]],
      oracle: (d) => GUARDS[d] ?? 0,
    },
    {
      name: "difficulty_meets over [1..5] x [1..5]",
      export: "difficulty_meets",
      args: [[1, 5], [1, 5]],
      oracle: (d, min) => (d >= min ? 1 : 0),
    },
    {
      name: "is_completed over [-1..5]",
      export: "is_completed",
      args: [[-1, 5]],
      oracle: (s) => (s === 2 ? 1 : 0),
    },
    {
      name: "is_terminal over [-1..5]",
      export: "is_terminal",
      args: [[-1, 5]],
      oracle: (s) => (s === 2 || s === 3 ? 1 : 0),
    },
    {
      name: "is_active over [-1..5]",
      export: "is_active",
      args: [[-1, 5]],
      oracle: (s) => (s === 0 || s === 1 ? 1 : 0),
    },
    {
      name: "is_complete over [0..7] completed x [-1..7] total",
      export: "is_complete",
      args: [[0, 7], [-1, 7]],
      oracle: (completed, total) => (total <= 0 ? 0 : completed >= total ? 1 : 0),
    },
    {
      name: "progress_percent over [0..7] completed x [-1..7] total",
      export: "progress_percent",
      args: [[0, 7], [-1, 7]],
      oracle: (completed, total) => (total <= 0 ? 0 : Math.floor((completed * 100) / total)),
    },
    {
      name: "par_time_sec over [-1..7]",
      export: "par_time_sec",
      args: [[-1, 7]],
      oracle: (i) => PAR[i] ?? 0,
    },
    {
      name: "max_alert_for_s over [-1..7]",
      export: "max_alert_for_s",
      args: [[-1, 7]],
      oracle: (i) => (i in MAXS ? MAXS[i] : -1),
    },
    {
      name: "qualifies_s_alert over [-1..7] index x [-1..4] peak",
      export: "qualifies_s_alert",
      args: [[-1, 7], [-1, 4]],
      oracle: (i, peak) => {
        const budget = i in MAXS ? MAXS[i] : -1;
        if (budget < 0) return 0;
        return peak <= budget ? 1 : 0;
      },
    },
  ],
};
