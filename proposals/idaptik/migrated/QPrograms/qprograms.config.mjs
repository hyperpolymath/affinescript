// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for QPrograms.affine (Int-native, milli-units).
// Oracles re-derive from QProgramsCoprocessor.res semantics in plain JS.
//
// Durations and multipliers are milli-units (×1000 of the original Float).
// Program/tier/bg/eq ordinals are raw integers (unchanged from ReScript).
//
// Encoding:
//   base_duration(program, tier) -> milli-seconds (e.g. 5.0s -> 5000)
//   background_synergy(program, bg) -> milli (1.25 -> 1250, 1.0 -> 1000)
//   equipment_bonus(program, eq) -> milli
//   combined_multiplier(program, bg, eq) -> bg_milli * eq_milli / 1000
//   effective_duration(program, tier, bg, eq) ->
//       base_m * bg_m * eq_m / 1_000_000  (two sequential /1000)
//   bonus_percent(program, bg, eq) -> (combined_milli - 1000) / 10
//   round_one_decimal(dur_milli) -> (dur_milli / 100) * 100
//   deck_is_full(slot_count) -> 1 if >= 4, else 0

// JS oracle helpers matching each ReScript function:

function baseDurationMs(program, tier) {
  const cameraLoop = { 1: 5000, 2: 10000, 3: 20000, 4: 30000, 5: 60000 };
  const firewallBypass = { 1: 8000, 2: 15000, 3: 30000, 4: 60000, 5: 120000 };
  const signalJam = { 1: 5000, 2: 10000, 3: 15000, 4: 25000, 5: 40000 };
  if (program === 5) return cameraLoop[tier] || 0;
  if (program === 6) return firewallBypass[tier] || 0;
  if (program === 12) return signalJam[tier] || 0;
  return 0;
}

function backgroundSynergyMilli(program, bg) {
  const pairs = [[5,2],[9,4],[10,4],[6,3],[7,3],[11,2]];
  for (const [p, b] of pairs) {
    if (program === p && bg === b) return 1250;
  }
  return 1000;
}

function equipmentBonusMilli(program, eq) {
  if (eq === 0) return 1000;
  const pairs = [[9,3,1300],[6,7,1200],[4,1,1150],[8,2,1100],[5,5,1100]];
  for (const [p, e, v] of pairs) {
    if (program === p && eq === e) return v;
  }
  return 1000;
}

function combinedMultiplierMilli(program, bg, eq) {
  return Math.trunc(backgroundSynergyMilli(program, bg) * equipmentBonusMilli(program, eq) / 1000);
}

function effectiveDurationMilli(program, tier, bg, eq) {
  const base = baseDurationMs(program, tier);
  const bgm = backgroundSynergyMilli(program, bg);
  const eqm = equipmentBonusMilli(program, eq);
  // Match the two-step integer multiply/divide in the kernel
  const step1 = Math.trunc(base * bgm / 1000);
  return Math.trunc(step1 * eqm / 1000);
}

const ALL_PROGRAMS = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];
const TIMED_PROGRAMS = [5, 6, 12];
const TIERS = [1, 2, 3, 4, 5];
const BACKGROUNDS = [1, 2, 3, 4, 5, 6];
const EQUIPMENT = [0, 1, 2, 3, 4, 5, 6, 7, 8];

export default {
  affine: "QPrograms.affine",
  compile: false,
  cases: [
    {
      name: "camera_loop_base(tier 0..6)",
      export: "camera_loop_base",
      args: [{ values: [0, 1, 2, 3, 4, 5, 6] }],
      oracle: (tier) => baseDurationMs(5, tier),
    },
    {
      name: "signal_jam_base(tier 0..6)",
      export: "signal_jam_base",
      args: [{ values: [0, 1, 2, 3, 4, 5, 6] }],
      oracle: (tier) => baseDurationMs(12, tier),
    },
    {
      name: "firewall_bypass_base(tier 0..6)",
      export: "firewall_bypass_base",
      args: [{ values: [0, 1, 2, 3, 4, 5, 6] }],
      oracle: (tier) => baseDurationMs(6, tier),
    },
    {
      name: "base_duration(all programs, tiers 0..6)",
      export: "base_duration",
      args: [
        { values: ALL_PROGRAMS },
        { values: [0, 1, 2, 3, 4, 5, 6] },
      ],
      oracle: (program, tier) => baseDurationMs(program, tier),
    },
    {
      name: "background_synergy(all programs, all backgrounds)",
      export: "background_synergy",
      args: [
        { values: ALL_PROGRAMS },
        { values: BACKGROUNDS },
      ],
      oracle: (program, bg) => backgroundSynergyMilli(program, bg),
    },
    {
      name: "equipment_bonus(all programs, all equipment incl 0)",
      export: "equipment_bonus",
      args: [
        { values: ALL_PROGRAMS },
        { values: EQUIPMENT },
      ],
      oracle: (program, eq) => equipmentBonusMilli(program, eq),
    },
    {
      name: "combined_multiplier(all programs, backgrounds, equipment)",
      export: "combined_multiplier",
      args: [
        { values: ALL_PROGRAMS },
        { values: BACKGROUNDS },
        { values: EQUIPMENT },
      ],
      oracle: (program, bg, eq) => combinedMultiplierMilli(program, bg, eq),
    },
    {
      name: "effective_duration(timed programs, tiers, bg, eq)",
      export: "effective_duration",
      args: [
        { values: TIMED_PROGRAMS },
        { values: TIERS },
        { values: BACKGROUNDS },
        { values: EQUIPMENT },
      ],
      oracle: (program, tier, bg, eq) => effectiveDurationMilli(program, tier, bg, eq),
    },
    {
      name: "effective_duration(untimed programs -> 0)",
      export: "effective_duration",
      args: [
        { values: [1, 2, 3, 4, 7, 8, 9, 10, 11] },
        { values: TIERS },
        { values: [1, 3] },
        { values: [0, 1] },
      ],
      oracle: (program, tier, bg, eq) => effectiveDurationMilli(program, tier, bg, eq),
    },
    {
      name: "bonus_percent(all programs, bg, eq)",
      export: "bonus_percent",
      args: [
        { values: ALL_PROGRAMS },
        { values: BACKGROUNDS },
        { values: EQUIPMENT },
      ],
      oracle: (program, bg, eq) => Math.trunc((combinedMultiplierMilli(program, bg, eq) - 1000) / 10),
    },
    {
      name: "round_one_decimal(dur_milli 0..130000)",
      export: "round_one_decimal",
      args: [{ values: [0, 99, 100, 101, 1000, 5000, 5050, 5099, 5100, 10000, 25000, 60000, 120000, 129999, 130000] }],
      oracle: (durMilli) => Math.trunc(durMilli / 100) * 100,
    },
    {
      name: "deck_is_full(slot_count 0..6)",
      export: "deck_is_full",
      args: [{ values: [0, 1, 2, 3, 4, 5, 6] }],
      oracle: (n) => n >= 4 ? 1 : 0,
    },
  ],
};
