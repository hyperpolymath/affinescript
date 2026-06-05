// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for Detection.affine (Int-only parity subset).
// Points at DetectionInt.affine which exposes only alert_level (Int return).
// Oracles re-derive from DetectionSystem.res semantics in plain JS.

export default {
  affine: "DetectionInt.affine",
  compile: false,
  cases: [
    {
      name: "alert_level(score) threshold ladder 0..5",
      export: "alert_level",
      args: [{ values: [0, 5, 14, 15, 34, 35, 59, 60, 84, 85, 99, 100, 110, 120] }],
      oracle: (score) => {
        if (score >= 100) return 5;
        if (score >= 85)  return 4;
        if (score >= 60)  return 3;
        if (score >= 35)  return 2;
        if (score >= 15)  return 1;
        return 0;
      },
    },
  ],
};
