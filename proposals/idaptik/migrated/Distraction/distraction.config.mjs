// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-parity config for Distraction.affine.
// Oracles re-derive from DistractionCoprocessor.res / Distraction.res semantics.
//
// use_item(count, kind): kind 0..4 known -> max(0, count-1); kind 5+ -> -1 sentinel
// register_responder(count): count + 1
// unregister_responder(count): max(0, count - 1)
// apply_difficulty(difficulty, base_uses, item_kind):
//   pizza/prank (0,1): difficulty >= 3 -> 1, else base_uses
//   maintenance/police (2,3): difficulty >= 4 -> 0, else base_uses
//   other (4+): base_uses unchanged

export default {
  affine: "Distraction.affine",
  cases: [
    {
      name: "use_item(uses 0..5, kind 0..5): pure decrement, kind is unused by kernel",
      export: "use_item",
      args: [{ values: [0, 1, 2, 3, 4, 5] }, { values: [0, 1, 2, 3, 4, 5] }],
      // The kernel simply decrements: if uses <= 0 return uses, else uses - 1.
      // kind is a parameter but is not consulted by this kernel (host gates on it).
      oracle: (uses, kind) => uses <= 0 ? uses : uses - 1,
    },
    {
      name: "register_responder(count 0..10)",
      export: "register_responder",
      args: [[0, 10]],
      oracle: (count) => count + 1,
    },
    {
      name: "unregister_responder(count 0..10)",
      export: "unregister_responder",
      args: [[0, 10]],
      oracle: (count) => Math.max(0, count - 1),
    },
    {
      // Note: kernel signature is apply_difficulty(base, difficulty, kind)
      // i.e. base is first, difficulty second, kind third.
      name: "apply_difficulty(base 0..5, diff 0..5, kind 0..6)",
      export: "apply_difficulty",
      args: [
        { values: [0, 1, 2, 3, 4, 5] },   // base (first param)
        { values: [0, 1, 2, 3, 4, 5] },   // difficulty (second param)
        { values: [0, 1, 2, 3, 4, 5, 6] }, // kind (third param)
      ],
      oracle: (base, difficulty, item_kind) => {
        if (item_kind === 0 || item_kind === 1) return difficulty >= 3 ? 1 : base;
        if (item_kind === 2 || item_kind === 3) return difficulty >= 4 ? 0 : base;
        return base;
      },
    },
  ],
};
