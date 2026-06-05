// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for CriticalRoll.affine (Int-native, milli-units).
// Oracles re-derive from CriticalRollCoprocessor.res semantics in plain JS.
//
// All probabilities and roll values are milli-units (×1000 of the original
// Float). Stats (primaryStat, wil) cross as raw integers.
//
// jessica_crit_success_milli: min(250, 50 + (stat - 100))
//   ReScript equivalent: min(0.25, 0.05 + (stat-100)/1000.0) * 1000
// jessica_crit_fail_milli: max(10, 200 - wil)
//   ReScript equivalent: max(0.01, 0.10 + (100-wil)/1000.0) * 1000
// classify_outcome(roll_milli, cs_milli, cf_milli):
//   roll_milli < cs_milli -> 0, roll_milli > 1000-cf_milli -> 2, else 1

function jCritSuccessMilli(stat) {
  // Original: min(0.25, 0.05 + (stat - 100) / 1000.0)
  const v = 0.05 + (stat - 100) / 1000.0;
  const clamped = Math.min(0.25, v);
  return Math.round(clamped * 1000);
}

function jCritFailMilli(wil) {
  // Original: max(0.01, 0.10 + (100 - wil) / 1000.0)
  const v = 0.10 + (100 - wil) / 1000.0;
  const clamped = Math.max(0.01, v);
  return Math.round(clamped * 1000);
}

function classifyOutcome(rollMilli, csMilli, cfMilli) {
  // Original: roll < critSuccess -> 0; roll > 1 - critFail -> 2; else 1
  if (rollMilli < csMilli) return 0;
  if (rollMilli > (1000 - cfMilli)) return 2;
  return 1;
}

// Stat range: 0 to 200 (game stats). Roll range: 0..1000 milli.
const STAT_VALUES = [0, 50, 80, 100, 120, 150, 200];
const WIL_VALUES = [0, 50, 80, 100, 120, 150, 200];
const ROLL_VALUES = [0, 10, 50, 100, 200, 250, 499, 500, 501, 750, 900, 990, 1000];

export default {
  affine: "CriticalRoll.affine",
  compile: false,
  cases: [
    {
      name: "jessica_crit_success_milli(stat 0..200): min(250, stat-50)",
      export: "jessica_crit_success_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (stat) => jCritSuccessMilli(stat),
    },
    {
      name: "jessica_crit_fail_milli(wil 0..200): max(10, 200-wil)",
      export: "jessica_crit_fail_milli",
      args: [{ values: WIL_VALUES }],
      oracle: (wil) => jCritFailMilli(wil),
    },
    {
      name: "classify_outcome(roll_milli, cs_milli, cf_milli) stat=100",
      export: "classify_outcome",
      args: [
        { values: ROLL_VALUES },
        { values: [jCritSuccessMilli(100)] },  // 50 milli (stat=100 baseline)
        { values: [jCritFailMilli(100)] },     // 100 milli (wil=100 baseline)
      ],
      oracle: (r, cs, cf) => classifyOutcome(r, cs, cf),
    },
    {
      name: "classify_outcome boundary: roll exactly on cs threshold",
      export: "classify_outcome",
      args: [
        // roll == crit_success_milli -> Normal (strict <)
        { values: [50, 51, 49] },
        { values: [50] },
        { values: [100] },
      ],
      oracle: (r, cs, cf) => classifyOutcome(r, cs, cf),
    },
    {
      name: "classify_outcome boundary: roll exactly on 1000-cf threshold",
      export: "classify_outcome",
      args: [
        // cf=100, threshold = 1000-100=900; roll==900 -> Normal (strict >)
        { values: [899, 900, 901] },
        { values: [50] },
        { values: [100] },
      ],
      oracle: (r, cs, cf) => classifyOutcome(r, cs, cf),
    },
    {
      name: "classify_outcome wide sweep (stat=80, wil=150)",
      export: "classify_outcome",
      args: [
        { values: ROLL_VALUES },
        { values: [jCritSuccessMilli(80)] },
        { values: [jCritFailMilli(150)] },
      ],
      oracle: (r, cs, cf) => classifyOutcome(r, cs, cf),
    },
    {
      name: "classify_outcome wide sweep (stat=150, wil=50)",
      export: "classify_outcome",
      args: [
        { values: ROLL_VALUES },
        { values: [jCritSuccessMilli(150)] },
        { values: [jCritFailMilli(50)] },
      ],
      oracle: (r, cs, cf) => classifyOutcome(r, cs, cf),
    },
  ],
};
