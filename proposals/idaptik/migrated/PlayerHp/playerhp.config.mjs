// SPDX-License-Identifier: AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for PlayerHp.affine.
// Oracles re-derive from PlayerHP.res semantics in plain JS.
//
// HP stored in tenths (1000 = 100.0 HP). Timers in milliseconds.
// Velocities in units/s as integers.

export default {
  affine: "PlayerHp.affine",
  cases: [
    {
      name: "knockback_speed()",
      export: "knockback_speed",
      args: [],
      oracle: () => 200,
    },
    {
      name: "knockback_duration_ms()",
      export: "knockback_duration_ms",
      args: [],
      oracle: () => 300,
    },
    {
      name: "iframe_duration_ms()",
      export: "iframe_duration_ms",
      args: [],
      oracle: () => 1000,
    },
    {
      name: "knockback_pop_y()",
      export: "knockback_pop_y",
      args: [],
      oracle: () => -80,
    },
    {
      name: "max_hp(con 0..200)",
      export: "max_hp",
      args: [[0, 200]],
      oracle: (con) => 800 + 2 * con,
    },
    {
      name: "apply_damage(hp, amount)",
      export: "apply_damage",
      args: [
        { values: [0, 100, 500, 1000, 1500] },
        { values: [0, 50, 100, 500, 1000, 2000] },
      ],
      oracle: (current_hp, amount) => Math.max(0, current_hp - amount),
    },
    {
      name: "knockback_vel_x(player_x, source_x)",
      export: "knockback_vel_x",
      args: [
        { values: [-100, 0, 50, 100, 200] },
        { values: [-100, 0, 50, 100, 200] },
      ],
      oracle: (player_x, source_x) => player_x >= source_x ? 200 : -200,
    },
    {
      name: "decay_timer(timer_ms, dt_ms)",
      export: "decay_timer",
      args: [
        { values: [0, 100, 300, 500, 1000] },
        { values: [0, 16, 50, 100, 300, 1000, 2000] },
      ],
      oracle: (timer_ms, dt_ms) => Math.max(0, timer_ms - dt_ms),
    },
  ],
};
