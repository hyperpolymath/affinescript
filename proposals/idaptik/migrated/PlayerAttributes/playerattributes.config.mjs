// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for PlayerAttributes.affine (Int-native, milli-units).
// Oracles re-derive from PlayerAttributesCoprocessor.res semantics in plain JS.
//
// Game stats (str, dex, con, int, wil, cha) cross as raw integers.
// All results are milli-units (×1000 of the original Float).
//
// Formulae (stat = raw integer, result = ×1000):
//   speed_multiplier_milli(dex) = dex * 10         (dex/100 * 1000)
//   max_jump_milli(str)         = str * 5000        (5*str * 1000)
//   jump_acceleration_milli(dex)= dex               (0.1*dex * 1000)
//   trajectory_divisor_milli(base_m, intel, len, dflt_m) =
//       len==0 ? dflt_m : (base_m * intel) / len
//   tech_task_speed_milli(intel, wil) = (intel*600 + wil*400) / 100
//   critical_failure_threshold_milli(wil) = max(10, 200 - wil)
//   critical_success_chance_milli(stat)   = min(250, 50 + (stat-100))
//   distraction_effectiveness_milli(cha)  = cha * 10
//   moletaire_obedience_milli(cha)        = 800 + cha * 2
//   fall_damage_resistance_milli(con)     = max(300, 1000 - (con-100)*5)
//   lockpick_speed_milli(dex)             = dex * 10
//   melee_damage_milli(str)               = str * 10

const STAT_VALUES = [0, 50, 80, 100, 120, 150, 200];

export default {
  affine: "PlayerAttributes.affine",
  compile: false,
  cases: [
    {
      name: "speed_multiplier_milli(dex): dex*10",
      export: "speed_multiplier_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (dex) => Math.round((dex / 100.0) * 1000),
    },
    {
      name: "max_jump_milli(str): str*5000",
      export: "max_jump_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (str) => Math.round(5.0 * str * 1000),
    },
    {
      name: "jump_acceleration_milli(dex): dex",
      export: "jump_acceleration_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (dex) => Math.round(0.1 * dex * 1000),
    },
    {
      name: "trajectory_divisor_milli(base_m, intel, len, dflt_m)",
      export: "trajectory_divisor_milli",
      args: [
        { values: [1000, 2000, 5000] },  // base_milli
        { values: [80, 100, 120] },      // intel (raw int)
        { values: [0, 10, 50, 100] },   // len (raw int, 0 = zero-guard)
        { values: [1000] },              // dflt_milli
      ],
      oracle: (baseMilli, intel, len, dfltMilli) => {
        if (len === 0) return dfltMilli;
        // (base_milli * intel) / len -- integer division (truncate)
        return Math.trunc((baseMilli * intel) / len);
      },
    },
    {
      name: "tech_task_speed_milli(intel, wil): (int*600+wil*400)/100",
      export: "tech_task_speed_milli",
      args: [
        { values: STAT_VALUES },
        { values: STAT_VALUES },
      ],
      oracle: (intel, wil) => Math.trunc((intel * 600 + wil * 400) / 100),
    },
    {
      name: "critical_failure_threshold_milli(wil): max(10, 200-wil)",
      export: "critical_failure_threshold_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (wil) => {
        const v = 0.10 + (100 - wil) / 1000.0;
        return Math.round(Math.max(0.01, v) * 1000);
      },
    },
    {
      name: "critical_success_chance_milli(stat): min(250, stat-50)",
      export: "critical_success_chance_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (stat) => {
        const v = 0.05 + (stat - 100) / 1000.0;
        return Math.round(Math.min(0.25, v) * 1000);
      },
    },
    {
      name: "distraction_effectiveness_milli(cha): cha*10",
      export: "distraction_effectiveness_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (cha) => Math.round((cha / 100.0) * 1000),
    },
    {
      name: "moletaire_obedience_milli(cha): 800+cha*2",
      export: "moletaire_obedience_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (cha) => Math.round((0.8 + cha / 500.0) * 1000),
    },
    {
      name: "fall_damage_resistance_milli(con): max(300, 1000-(con-100)*5)",
      export: "fall_damage_resistance_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (con) => {
        const v = 1.0 - (con - 100) / 200.0;
        return Math.round(Math.max(0.3, v) * 1000);
      },
    },
    {
      name: "lockpick_speed_milli(dex): dex*10",
      export: "lockpick_speed_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (dex) => Math.round((dex / 100.0) * 1000),
    },
    {
      name: "melee_damage_milli(str): str*10",
      export: "melee_damage_milli",
      args: [{ values: STAT_VALUES }],
      oracle: (str) => Math.round((str / 100.0) * 1000),
    },
  ],
};
