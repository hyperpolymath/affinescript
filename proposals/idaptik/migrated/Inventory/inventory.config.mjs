// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Inventory.affine (idaptik slot-and-weight co-processor;
// scalar i32 ABI). The oracles are an INDEPENDENT JS reimplementation derived
// from src/shared/Inventory.res -- NOT from the .affine -- so a codegen
// regression surfaces as a differential mismatch.
//
// Slot state is a base-3 word, low digit = slot 0 (EMPTY=0, FILLED=1, LOCKED=2),
// mirroring InventoryCoprocessor.res's SLOT alphabet. Weights are floored
// milli-kilograms (Int, the host's ×1000 floor of the f64 kg in Inventory.res).
//
// Independent oracle helpers (re-derived from the .res semantics):

// The base-3 digit at slot index k of a packed slot word (no shared code with
// the .affine extraction: plain Math here).
const digitAt = (packed, k) => Math.floor(packed / 3 ** k) % 3;

// slotCount: Inventory.make always builds 6 slots; an out-of-band tier has none.
const slotCount = (tier) => (tier >= 0 && tier <= 2 ? 6 : 0);

// isSlotLocked: from Inventory.make's per-tier slot arrays. Accessible (0) locks
// nothing; Realistic (1) and Hardcore (2) lock the last two slots (idx 4, 5).
const isSlotLocked = (tier, idx) => {
  if (tier < 0 || tier > 2) return 0;
  if (idx < 0 || idx > 5) return 0;
  if (tier === 0) return 0;
  return idx >= 4 ? 1 : 0;
};

// findEmptySlot: first EMPTY (digit 0) slot in 0..count-1, else -1 (mirrors
// findEmptySlot's first None-and-unlocked search; a LOCKED or FILLED digit is
// skipped just as a locked or occupied ReScript slot is).
const findEmptySlot = (packed, count) => {
  for (let k = 0; k < count; k++) {
    if (digitAt(packed, k) === 0) return k;
  }
  return -1;
};

// filledCount: digits equal to FILLED (getFilledCount = filter Option.isSome).
const filledCount = (packed, count) => {
  let n = 0;
  for (let k = 0; k < count; k++) if (digitAt(packed, k) === 1) n++;
  return n;
};

// unlockedCount: digits not equal to LOCKED (getUnlockedCount = filter !locked;
// EMPTY and FILLED are both unlocked).
const unlockedCount = (packed, count) => {
  let n = 0;
  for (let k = 0; k < count; k++) if (digitAt(packed, k) !== 2) n++;
  return n;
};

// fitsWeight: negation of Inventory.addItem's tooHeavy test
// (totalWeight +. item.weight > maxWeight), in milli-kg.
const fitsWeight = (total, item, max) => (total + item > max ? 0 : 1);

// canAdd: full addItem eligibility = fits by weight AND an empty slot exists.
const canAdd = (packed, count, total, item, max) =>
  fitsWeight(total, item, max) === 1 && findEmptySlot(packed, count) !== -1 ? 1 : 0;

export default {
  affine: "Inventory.affine",
  cases: [
    {
      name: "slot_count over tier [-2..5]",
      export: "slot_count",
      args: [[-2, 5]],
      oracle: (tier) => slotCount(tier),
    },
    {
      name: "is_slot_locked over tier [-1..4] x idx [-1..7]",
      export: "is_slot_locked",
      args: [[-1, 4], [-1, 7]],
      oracle: (tier, idx) => isSlotLocked(tier, idx),
    },
    {
      name: "find_empty_slot over packed [0..120] x count [0..6]",
      export: "find_empty_slot",
      args: [[0, 120], [0, 6]],
      oracle: (packed, count) => findEmptySlot(packed, count),
    },
    {
      name: "filled_count over packed [0..120] x count [0..6]",
      export: "filled_count",
      args: [[0, 120], [0, 6]],
      oracle: (packed, count) => filledCount(packed, count),
    },
    {
      name: "unlocked_count over packed [0..120] x count [0..6]",
      export: "unlocked_count",
      args: [[0, 120], [0, 6]],
      oracle: (packed, count) => unlockedCount(packed, count),
    },
    {
      name: "remaining_capacity over max [0..3000 step] x total [0..3000 step]",
      export: "remaining_capacity",
      args: [{ values: [0, 500, 1500, 3000, 3200] }, { values: [0, 150, 1500, 3000, 3500] }],
      oracle: (max, total) => max - total,
    },
    {
      name: "add_weight over acc [0..3000] x item [0..500]",
      export: "add_weight",
      args: [{ values: [0, 150, 1500, 2850, 3000] }, { values: [0, 20, 100, 150, 500] }],
      oracle: (acc, item) => acc + item,
    },
    {
      name: "fits_weight over total x item x max (milli-kg)",
      export: "fits_weight",
      args: [
        { values: [0, 1500, 2850, 3000, 3001] },
        { values: [0, 20, 150, 200, 1000] },
        { values: [3000] },
      ],
      oracle: (total, item, max) => fitsWeight(total, item, max),
    },
    {
      name: "can_add over packed x count x total x item x max",
      export: "can_add",
      args: [
        { values: [0, 1, 2, 13, 26, 121] },
        { values: [0, 3, 6] },
        { values: [0, 2850, 3000] },
        { values: [0, 150, 200] },
        { values: [3000] },
      ],
      oracle: (packed, count, total, item, max) => canAdd(packed, count, total, item, max),
    },
  ],
};
