// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for CombatFx.affine (Int-only parity subset).
// Points at CombatFxInt.affine which exposes only the Int-returning exports.
// Oracles re-derive from CombatFxLogicCoprocessor.res semantics in plain JS.

export default {
  affine: "CombatFxInt.affine",
  compile: false,
  // Integer-input variants: timer/period as whole-number integers.
  // Integer division timer/period == floor(timer/period) for non-negative inputs,
  // matching the Float floor_pos logic for these test cases.
  cases: [
    {
      name: "flash_cycle_int(timer, period): integer division = floor(float/float)",
      export: "flash_cycle_int",
      args: [
        { values: [0, 1, 2, 3, 4, 5, 6, 8, 10] },
        { values: [1, 2] },
      ],
      oracle: (timer, period) => Math.floor(timer / period),
    },
    {
      name: "flash_on_phase_int(timer, period, on_frac): 1 within on-window",
      export: "flash_on_phase_int",
      args: [
        { values: [0, 1, 2, 3, 4, 5, 6, 8, 10] },
        { values: [1, 2] },
        { values: [0, 1, 2] },
      ],
      oracle: (timer, period, on_frac) => {
        const cycles = Math.floor(timer / period);
        const remainder = timer - cycles * period;
        return remainder < on_frac ? 1 : 0;
      },
    },
    {
      name: "flash_is_even_cycle_int(timer, period): 1 if even cycle",
      export: "flash_is_even_cycle_int",
      args: [
        { values: [0, 1, 2, 3, 4, 5, 6, 8, 10] },
        { values: [1, 2] },
      ],
      oracle: (timer, period) => Math.floor(timer / period) % 2 === 0 ? 1 : 0,
    },
  ],
};
