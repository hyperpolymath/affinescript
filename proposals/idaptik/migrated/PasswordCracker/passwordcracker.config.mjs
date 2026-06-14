// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PasswordCracker.affine (idaptik crack-scoring kernel;
// scalar i32 ABI). The oracles re-derive the djb2 recurrence, the low-16 mask,
// the difficulty tables, and the brute-force verdict independently from
// PasswordCracker.res in plain JS, so a codegen regression surfaces as a
// differential mismatch.
//
// i32 NOTE: the wasm `*` is i32.mul (== Math.imul) and `+` wraps at i32, so the
// oracle's hash_step uses Math.imul and `| 0` to model the same 32-bit truncation
// the kernel performs. (The ReScript simpleHash uses unbounded JS numbers; the
// bridge's hash_step is the i32 source of truth, and that is what the kernel
// reproduces and what this oracle checks.)
//
// Difficulty bands (securityLevelOrdinal 0 Open .. 3 Strong):
//   max_attempts:     5 / 50 / 500 / 5000
//   success_chance:   MILLI-units of the .res Float 0.95/0.80/0.40/0.05 = 950/800/400/50
//   time_per_attempt: whole ms of 100.0/200.0/500.0/1000.0 = 100/200/500/1000
//   off-band -> 0 for every field.
// Verdict crack_succeeds(roll_milli, level): the .res `roll < successChance*0.3`
// in milli, scaled by 10 to stay integral: roll_milli*10 < sc_milli*3.

// --- independent reimplementations from PasswordCracker.res ---

function hashSeed() {
  return 5381; // .res: ref(5381)
}

function hashStep(acc, c) {
  // .res: hash := hash*33 + c. The kernel does this with i32.mul + i32 wrap, so
  // model it with Math.imul and final | 0.
  return (Math.imul(acc, 33) + c) | 0;
}

function maskLow16(h) {
  // .res: Int.Bitwise.land(targetHash, 0xFFFF)
  return (h | 0) & 0xffff;
}

function maxAttempts(level) {
  switch (level) {
    case 0: return 5;
    case 1: return 50;
    case 2: return 500;
    case 3: return 5000;
    default: return 0;
  }
}

function successChanceMilli(level) {
  // .res success chances 0.95/0.80/0.40/0.05 -> milli.
  switch (level) {
    case 0: return Math.round(0.95 * 1000); // 950
    case 1: return Math.round(0.80 * 1000); // 800
    case 2: return Math.round(0.40 * 1000); // 400
    case 3: return Math.round(0.05 * 1000); // 50
    default: return 0;
  }
}

function timePerAttempt(level) {
  switch (level) {
    case 0: return 100;
    case 1: return 200;
    case 2: return 500;
    case 3: return 1000;
    default: return 0;
  }
}

function crackSucceeds(rollMilli, level) {
  // .res: roll < successChance *. 0.3. Scale by 10 to keep integral:
  // roll_milli*10 < sc_milli*3.
  const sc = successChanceMilli(level);
  return rollMilli * 10 < sc * 3 ? 1 : 0;
}

// Representative inputs. Char codes span the printable-ASCII band passwords use
// (plus a couple of extremes); accumulators include values that force i32 wrap.
const CHAR_CODES = [0, 32, 45, 48, 57, 65, 90, 97, 122, 126, 255, 1000, 65535];
const ACC_VALUES = [5381, 0, 1, 12345, 2000000000, -2000000000, 178956970];
// roll in [0,1) -> milli draws 0..999, plus the band edges that straddle each
// scaled threshold (sc*0.3 = 285 / 240 / 120 / 15).
const ROLL_VALUES = [
  0, 14, 15, 16, 119, 120, 121, 239, 240, 241, 284, 285, 286, 500, 950, 999,
];
const LEVELS = [-1, 0, 1, 2, 3, 4];

export default {
  affine: "PasswordCracker.affine",
  cases: [
    {
      name: "hash_seed()",
      export: "hash_seed",
      args: [],
      oracle: () => hashSeed(),
    },
    {
      name: "hash_step(acc, c) over i32-wrapping accs x char codes",
      export: "hash_step",
      args: [{ values: ACC_VALUES }, { values: CHAR_CODES }],
      oracle: (acc, c) => hashStep(acc, c),
    },
    {
      name: "mask_low16(h) over a spread of i32 hashes",
      export: "mask_low16",
      args: [{ values: [0, 1, 0xffff, 0x10000, 0x12345, -1, -65536, 2000000000, -2000000000] }],
      oracle: (h) => maskLow16(h),
    },
    {
      name: "max_attempts over levels [-1..4]",
      export: "max_attempts",
      args: [{ values: LEVELS }],
      oracle: (l) => maxAttempts(l),
    },
    {
      name: "success_chance (milli) over levels [-1..4]",
      export: "success_chance",
      args: [{ values: LEVELS }],
      oracle: (l) => successChanceMilli(l),
    },
    {
      name: "time_per_attempt over levels [-1..4]",
      export: "time_per_attempt",
      args: [{ values: LEVELS }],
      oracle: (l) => timePerAttempt(l),
    },
    {
      name: "crack_succeeds(roll_milli, level) across thresholds",
      export: "crack_succeeds",
      args: [{ values: ROLL_VALUES }, { values: LEVELS }],
      oracle: (r, l) => crackSucceeds(r, l),
    },
  ],
};
