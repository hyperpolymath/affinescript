// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for DualAlert.affine.
// Oracles re-derive from DualAlert.res semantics in plain JS.
//
// Level ordinal: 0=Clear 1=Guarded 2=Elevated 3=High 4=Severe.
// OOB clamped: < 0 -> 0, > 4 -> 4 (via Invalid(v) arm).
// All parameters and results are Int. Progress/severity use milli-units (x1000).

const COLOURS = [4521796, 11206468, 16755268, 16729156, 16711680];

function clamp(level) {
  if (level < 0) return 0;
  if (level > 4) return 4;
  return level;
}

export default {
  affine: "DualAlert.affine",
  cases: [
    {
      name: "level_value(level -1..5)",
      export: "level_value",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (level) => clamp(level),
    },
    {
      name: "level_colour(level -1..5)",
      export: "level_colour",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (level) => COLOURS[clamp(level)],
    },
    {
      name: "escalate_level(level -1..5): saturating +1",
      export: "escalate_level",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (level) => { const o = clamp(level); return o >= 4 ? 4 : o + 1; },
    },
    {
      name: "scrub_step(level -1..5): saturating -1",
      export: "scrub_step",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (level) => { const o = clamp(level); return o <= 0 ? 0 : o - 1; },
    },
    {
      name: "trigger_escalates(level, prog_m, sev_m)",
      export: "trigger_escalates",
      args: [
        { values: [-1, 0, 1, 2, 3, 4, 5] },
        { values: [0, 200, 500, 800, 999, 1000, 1200] },
        { values: [0, 200, 500, 800, 999, 1000, 1200] },
      ],
      oracle: (level, prog_m, sev_m) => {
        const combined = prog_m + sev_m;
        if (combined < 1000) return 0;
        return clamp(level) >= 4 ? 0 : 1;
      },
    },
    {
      name: "trigger_progress(level, prog_m, sev_m)",
      export: "trigger_progress",
      args: [
        { values: [-1, 0, 2, 4, 5] },
        { values: [0, 500, 800, 1000] },
        { values: [0, 200, 500, 1000] },
      ],
      oracle: (level, prog_m, sev_m) => {
        const combined = prog_m + sev_m;
        if (combined < 1000) return combined;
        return clamp(level) >= 4 ? 1000 : combined - 1000;
      },
    },
    {
      name: "trigger_level(level, prog_m, sev_m)",
      export: "trigger_level",
      args: [
        { values: [-1, 0, 1, 2, 3, 4, 5] },
        { values: [0, 500, 1000] },
        { values: [0, 500, 1000] },
      ],
      oracle: (level, prog_m, sev_m) => {
        const o = clamp(level);
        const combined = prog_m + sev_m;
        if (combined < 1000) return o;
        if (o >= 4) return o;
        return o + 1;
      },
    },
    {
      name: "cooldown_progress(prog_m 0..1200, decay_m 0..1200)",
      export: "cooldown_progress",
      args: [
        { values: [0, 200, 500, 800, 1000, 1200] },
        { values: [0, 100, 200, 500, 1000, 1200] },
      ],
      oracle: (prog_m, decay_m) => Math.max(0, prog_m - decay_m),
    },
    {
      name: "overall_threat(physical -1..5, cyber -1..5)",
      export: "overall_threat",
      args: [
        { values: [-1, 0, 1, 2, 3, 4, 5] },
        { values: [-1, 0, 1, 2, 3, 4, 5] },
      ],
      oracle: (physical, cyber) => {
        const p = clamp(physical);
        const c = clamp(cyber);
        return p >= c ? p : c;
      },
    },
    {
      name: "should_dispatch_antihacker(cyber -1..5)",
      export: "should_dispatch_antihacker",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (cyber) => clamp(cyber) >= 2 ? 1 : 0,
    },
    {
      name: "guards_checking_terminals(physical -1..5)",
      export: "guards_checking_terminals",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (physical) => clamp(physical) >= 3 ? 1 : 0,
    },
  ],
};
