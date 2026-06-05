// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for SecurityAi.affine (Int-only parity subset).
// Points at SecurityAiInt.affine which exposes only the Int-returning exports.
// Oracles re-derive from SecurityAI.res semantics in plain JS.

function clamp_phase(phase) {
  if (phase < 0) return 0;
  if (phase > 4) return 4;
  return phase;
}

export default {
  affine: "SecurityAiInt.affine",
  cases: [
    {
      name: "phase_value(phase -1..5)",
      export: "phase_value",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (phase) => clamp_phase(phase),
    },
    {
      name: "phase_for_alert(alert_level 0..5)",
      export: "phase_for_alert",
      args: [{ values: [0, 1, 2, 3, 4, 5] }],
      oracle: (alert_level) => {
        if (alert_level <= 1) return 0;
        if (alert_level === 2) return 1;
        if (alert_level === 3) return 2;
        if (alert_level === 4) return 3;
        return 4;
      },
    },
    {
      name: "timer_ready(scan_timer, scan_interval): Int params",
      export: "timer_ready",
      args: [
        { values: [0, 1, 5, 9, 10, 11] },
        { values: [1, 5, 10] },
      ],
      oracle: (scan_timer, scan_interval) => scan_timer >= scan_interval ? 1 : 0,
    },
    {
      name: "request_dispatch_phase(phase -1..5)",
      export: "request_dispatch_phase",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (phase) => { const o = clamp_phase(phase); return o === 0 ? 1 : o; },
    },
    {
      name: "is_active(phase -1..5)",
      export: "is_active",
      args: [{ values: [-1, 0, 1, 2, 3, 4, 5] }],
      oracle: (phase) => clamp_phase(phase) === 0 ? 0 : 1,
    },
    {
      name: "increment_counter(count 0..20)",
      export: "increment_counter",
      args: [[0, 20]],
      oracle: (count) => count + 1,
    },
    {
      name: "dormant_phase()",
      export: "dormant_phase",
      args: [],
      oracle: () => 0,
    },
  ],
};
