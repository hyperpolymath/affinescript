// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for DifficultyScale.affine.
// Oracle re-derives from Distraction.res applyDifficultyScaling semantics.
//
// scale_use_count(difficulty, base_uses, item_kind):
//   difficulty ladder: 0=Tutorial 1=Easy 2=Normal 3=Hard 4=Expert
//   item_kind: 0=pizza 1=prank 2=maintenance 3=police 4+=other/unknown
//   pizza/prank:       difficulty >= 3 -> 1, else base_uses
//   maintenance/police: difficulty >= 4 -> 0, else base_uses
//   other/unknown:     base_uses unchanged

export default {
  affine: "DifficultyScale.affine",
  cases: [
    {
      name: "scale_use_count(diff 0..5, base 0..5, kind 0..6)",
      export: "scale_use_count",
      args: [
        { values: [0, 1, 2, 3, 4, 5] },
        { values: [0, 1, 2, 3, 4, 5] },
        { values: [0, 1, 2, 3, 4, 5, 6] },
      ],
      oracle: (difficulty, base_uses, item_kind) => {
        if (item_kind === 0 || item_kind === 1) return difficulty >= 3 ? 1 : base_uses;
        if (item_kind === 2 || item_kind === 3) return difficulty >= 4 ? 0 : base_uses;
        return base_uses;
      },
    },
  ],
};
