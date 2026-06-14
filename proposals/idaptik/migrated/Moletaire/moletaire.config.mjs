// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Moletaire.affine (the robotic-mole behaviour brain
// re-decomposed from src/app/companions/Moletaire.res + MoletaireCoprocessors.res
// + MoletaireHunger.res). The brain is Int-only, so parity runs directly against
// the deliverable.
//
// Every oracle is an INDEPENDENT re-derivation of the ReScript semantics in
// plain JS -- the taxonomies, the Tuning constants, the per-coprocessor effect
// ladders, the hunger thresholds / behaviour classifier / rate formula / gravity
// pull, the order gates and the 11-state machine -- NOT a copy of the .affine. A
// codegen regression surfaces as a differential mismatch. Units mirror the
// brain: depths/hunger/factors in milli (1000 == 1.0), speeds/distances in
// milli-pixels, timers in milliseconds, hunger rate in micro-units.

const trunc = Math.trunc;

// --- taxonomies -------------------------------------------------------------
function validState(s) { return s >= 0 && s <= 10; }
function validLevel(l) { return l >= 0 && l <= 3; }

// --- carry capacity (Moletaire.res getCarryCapacity) ------------------------
function carryCapacity(equipped) {
  return equipped === 4 ? 3 : 1; // Rucksack == 4
}
function canGiveItem(alive, carried, equipped) {
  if (alive === 0) return 0;
  return carried < carryCapacity(equipped) ? 1 : 0;
}

// --- movement speed selection (Moletaire.res updateMovement) ----------------
const UNDERGROUND = 200000, ABOVEGROUND = 25000, SKATEBOARD = 55000;
function movementSpeed(state, equipped, underground, pathMult) {
  if (state === 1) return trunc((UNDERGROUND * pathMult) / 1000);
  if (state === 2) return equipped === 0 ? SKATEBOARD : ABOVEGROUND;
  if (state === 5) {
    if (underground !== 0) {
      const base = trunc((UNDERGROUND * 7) / 10);
      return trunc((base * pathMult) / 1000);
    }
    return ABOVEGROUND;
  }
  return 0;
}

// --- facing / direction signs ----------------------------------------------
function facingForDelta(dx) { return dx > 0 ? 1 : 0; }
function directionForDelta(dx) {
  if (dx > 0) return 1;
  if (dx < 0) return -1;
  return 0;
}

// --- order gates (Moletaire.res command guards) -----------------------------
function canDig(alive, state, depth) {
  if (alive === 0) return 0;
  if (state === 10) return 0;
  return depth > 100 ? 1 : 0;
}
function orderMoveState(alive, state, undergroundReq) {
  if (alive === 0) return -1;
  if (state === 10) return -1;
  return undergroundReq !== 0 ? 1 : 2;
}
function canLaunchGlider(alive, equipped, depth) {
  if (alive === 0) return 0;
  if (equipped !== 1) return 0; // Glider == 1
  return depth < 50 ? 1 : 0; // surfaceThreshold == 50
}
function canUseFlash(alive, equipped) {
  if (alive === 0) return 0;
  return equipped === 5 ? 1 : 0; // FlashCamera == 5
}
function canDistractByWire(alive, state, wireDist) {
  if (alive === 0) return 0;
  if (state === 10 || state === 3 || state === 4) return 0;
  return wireDist < 200000 ? 1 : 0;
}

// --- 11-state machine (Moletaire.res updateState) ---------------------------
function nextState(state, actionTimer, glideProgress, distractTimer) {
  if (!validState(state)) return -1;
  if (state === 3) return actionTimer <= 0 ? 0 : 3;
  if (state === 4) return actionTimer <= 0 ? 0 : 4;
  if (state === 6) return distractTimer <= 0 ? 0 : 6;
  if (state === 7) return glideProgress >= 1000 ? 0 : 7;
  if (state === 8) return 10;
  if (state === 9) return 10;
  return state; // Idle / moving / carrying / Dead hold
}
function itemEaten(roll, stabMult) {
  const eatChance = trunc((50 * stabMult) / 1000);
  return roll < eatChance ? 1 : 0;
}

// --- coprocessor effect ladders (MoletaireCoprocessors.res) ------------------
function audioSoundCount(l) {
  if (!validLevel(l)) return -1;
  return [0, 3, 6, 10][l];
}
function canMimicVoice(l) { return l === 3 ? 1 : 0; }
function pathEfficiency(l) {
  if (!validLevel(l)) return -1;
  return [1000, 1150, 1300, 1500][l];
}
function sensorRange(l) {
  if (!validLevel(l)) return -1;
  return [1, 3, 5, 8][l];
}
function canDetectVault(l) { return l === 3 ? 1 : 0; }
function vibrationBand(l) {
  if (!validLevel(l)) return -1;
  return l;
}
function eatChanceMult(l) {
  if (!validLevel(l)) return -1;
  return [1000, 600, 200, 50][l];
}
function canCarryFragile(l) { return l >= 2 ? 1 : 0; }
function scanAhead(alive, underground, level) {
  if (alive === 0) return 0;
  if (underground === 0) return 0;
  return sensorRange(level);
}
function canPlaySound(alive, maxSounds, idx) {
  if (alive === 0) return 0;
  if (maxSounds <= 0) return 0;
  if (idx < 0) return 0;
  if (idx >= maxSounds) return 0;
  return 1;
}

// --- hunger thresholds / behaviour (MoletaireHunger.res) ---------------------
const PECKISH = 400, HUNGRY = 400, STARVING = 800, OBJ_EAT = 900;
function hungerBehaviour(hunger, hasNearby) {
  if (hunger > STARVING) return hasNearby !== 0 ? 2 : 3;
  if (hunger > HUNGRY) return 1;
  return 0;
}
function willEatObjective(h) { return h > OBJ_EAT ? 1 : 0; }
function objectiveAtRisk(h) { return h > STARVING ? 1 : 0; }
function hungerResists(h) { return h > HUNGRY ? 1 : 0; }
function hungerColorBand(h) {
  if (h < PECKISH) return 0;
  if (h < 600) return 1;
  if (h < STARVING) return 2;
  return 3;
}
function hungerDisplayBand(h) {
  if (h < 100) return 0;
  if (h < PECKISH) return 1;
  if (h < 600) return 2;
  if (h < STARVING) return 3;
  if (h < OBJ_EAT) return 4;
  return 5;
}

// --- hunger rate (MoletaireHunger.calculateHungerRate), micro-units ---------
// rate = base * movement * accel * coproc * level, every float operand passed
// as a milli-factor; result returned scaled by 1_000_000 (micro). movementMult
// = 1500/500; accel = 1000 + hunger/2. Because the brain's Int is i32, the
// product is folded with a /1000 after each milli-factor past the second; the
// per-step truncation IS the contract, so the oracle reproduces the same chain
// step for step (a single big division would round differently).
function hungerRateMicro(baseRate, isMoving, hunger, coprocMult, levelMult) {
  const movement = isMoving !== 0 ? 1500 : 500;
  const accel = 1000 + trunc(hunger / 2);
  const a1 = baseRate * movement;
  const a2 = trunc((a1 * accel) / 1000);
  const a3 = trunc((a2 * coprocMult) / 1000);
  return trunc((a3 * levelMult) / 1000);
}

// --- gravity pull (MoletaireHunger.calculatePull) ---------------------------
// piecewise hungerMult in milli (0..3000).
function hungerMultMilli(h) {
  if (h < PECKISH) return trunc(h / 2);
  if (h < STARVING) return 200 + (h - 400) * 2;
  return 1000 + (h - 800) * 10;
}
// signed pull in milli-pixels/s; magnitude_milli = G(5000)*mult/distSq_px capped
// at 200000. distSq crosses as PLAIN px^2 (milli-px^2 would overflow i32 for any
// realistic distance); the host clamps distSq >= 100 px^2 before calling.
function hungerPull(direction, multMilli, distSqPx) {
  if (distSqPx <= 0) return 0;
  const magnitude = trunc((5000 * multMilli) / distSqPx);
  const capped = magnitude > 200000 ? 200000 : magnitude;
  if (direction > 0) return capped;
  if (direction < 0) return -capped;
  return 0;
}

// --- scan / query predicates ------------------------------------------------
function scanRuns(state) { return state === 1 || state === 3 ? 1 : 0; }
function isUnderground(depth) { return depth > 50 ? 1 : 0; }
function isDigging(state) { return state === 3 || state === 4 ? 1 : 0; }
function isDead(state) {
  if (!validState(state)) return 0;
  return state >= 8 ? 1 : 0;
}

export default {
  affine: "Moletaire.affine",
  cases: [
    // --- taxonomy counts ---
    { name: "state_count()", export: "state_count", args: [], oracle: () => 11 },
    { name: "equipment_count()", export: "equipment_count", args: [], oracle: () => 7 },
    { name: "level_count()", export: "level_count", args: [], oracle: () => 4 },
    { name: "vibration_count()", export: "vibration_count", args: [], oracle: () => 4 },
    { name: "behaviour_count()", export: "behaviour_count", args: [], oracle: () => 4 },

    // --- Tuning constants (nullary) ---
    { name: "underground_speed_mu()", export: "underground_speed_mu", args: [], oracle: () => 200000 },
    { name: "above_ground_speed_mu()", export: "above_ground_speed_mu", args: [], oracle: () => 25000 },
    { name: "skateboard_speed_mu()", export: "skateboard_speed_mu", args: [], oracle: () => 55000 },
    { name: "surface_threshold_mu()", export: "surface_threshold_mu", args: [], oracle: () => 50 },
    { name: "dog_detection_depth_mu()", export: "dog_detection_depth_mu", args: [], oracle: () => 300 },
    { name: "trap_dig_ms()", export: "trap_dig_ms", args: [], oracle: () => 4000 },
    { name: "cable_sabotage_ms()", export: "cable_sabotage_ms", args: [], oracle: () => 3000 },
    { name: "distraction_ms()", export: "distraction_ms", args: [], oracle: () => 5000 },
    { name: "base_carry_capacity()", export: "base_carry_capacity", args: [], oracle: () => 1 },
    { name: "rucksack_carry_capacity()", export: "rucksack_carry_capacity", args: [], oracle: () => 3 },
    { name: "base_eat_chance_milli()", export: "base_eat_chance_milli", args: [], oracle: () => 50 },

    // --- carry capacity / give ---
    { name: "carry_capacity over equipped [-1..7]", export: "carry_capacity", args: [[-1, 7]], oracle: (e) => carryCapacity(e) },
    {
      name: "can_give_item: alive x carried x equipped",
      export: "can_give_item",
      args: [[0, 1], { values: [0, 1, 2, 3, 4] }, { values: [-1, 0, 4] }],
      oracle: (a, c, e) => canGiveItem(a, c, e),
    },

    // --- movement speed selection ---
    {
      name: "movement_speed_mu: state x equipped x underground x pathMult",
      export: "movement_speed_mu",
      args: [
        [-1, 11],                              // state incl. out-of-band shoulders
        { values: [-1, 0, 1, 4] },             // empty / skateboard / glider / rucksack
        [0, 1],                                // underground flag
        { values: [1000, 1150, 1300, 1500] },  // path efficiency milli
      ],
      oracle: (s, e, u, p) => movementSpeed(s, e, u, p),
    },

    // --- facing / direction ---
    { name: "facing_for_delta over [-5..5]", export: "facing_for_delta", args: [[-5, 5]], oracle: (d) => facingForDelta(d) },
    { name: "direction_for_delta over [-5..5]", export: "direction_for_delta", args: [[-5, 5]], oracle: (d) => directionForDelta(d) },

    // --- order gates ---
    {
      name: "can_dig: alive x state x depth",
      export: "can_dig",
      args: [[0, 1], { values: [0, 3, 10] }, { values: [0, 99, 100, 101, 500, 1000] }],
      oracle: (a, s, d) => canDig(a, s, d),
    },
    {
      name: "order_move_state: alive x state x undergroundReq",
      export: "order_move_state",
      args: [[0, 1], { values: [0, 5, 10] }, [0, 1]],
      oracle: (a, s, u) => orderMoveState(a, s, u),
    },
    {
      name: "can_launch_glider: alive x equipped x depth",
      export: "can_launch_glider",
      args: [[0, 1], { values: [0, 1, 5] }, { values: [0, 49, 50, 51, 300] }],
      oracle: (a, e, d) => canLaunchGlider(a, e, d),
    },
    {
      name: "can_use_flash: alive x equipped",
      export: "can_use_flash",
      args: [[0, 1], [-1, 6]],
      oracle: (a, e) => canUseFlash(a, e),
    },
    {
      name: "can_distract_by_wire: alive x state x wireDist",
      export: "can_distract_by_wire",
      args: [[0, 1], { values: [0, 1, 3, 4, 6, 10] }, { values: [0, 199999, 200000, 200001, 250000] }],
      oracle: (a, s, w) => canDistractByWire(a, s, w),
    },

    // --- state machine ---
    {
      name: "next_state: full table x timers x glideProgress",
      export: "next_state",
      args: [
        [-1, 11],                              // state incl. out-of-band shoulders
        { values: [-100, 0, 1, 4000] },        // action timer ms
        { values: [0, 999, 1000, 1001] },      // glide progress milli
        { values: [-100, 0, 1, 5000] },        // distraction timer ms
      ],
      oracle: (s, at, gp, dt) => nextState(s, at, gp, dt),
    },
    {
      name: "item_eaten: roll x stabilisationMult",
      export: "item_eaten",
      args: [
        { values: [0, 24, 25, 29, 30, 49, 50, 51, 999] },
        { values: [50, 200, 600, 1000] },      // MK-III .. Stock multipliers
      ],
      oracle: (r, m) => itemEaten(r, m),
    },

    // --- coprocessor ladders ---
    { name: "audio_sound_count over level [-1..4]", export: "audio_sound_count", args: [[-1, 4]], oracle: (l) => audioSoundCount(l) },
    { name: "can_mimic_voice over level [-1..4]", export: "can_mimic_voice", args: [[-1, 4]], oracle: (l) => canMimicVoice(l) },
    { name: "path_efficiency_milli over level [-1..4]", export: "path_efficiency_milli", args: [[-1, 4]], oracle: (l) => pathEfficiency(l) },
    { name: "sensor_range over level [-1..4]", export: "sensor_range", args: [[-1, 4]], oracle: (l) => sensorRange(l) },
    { name: "can_detect_vault_weak_points over level [-1..4]", export: "can_detect_vault_weak_points", args: [[-1, 4]], oracle: (l) => canDetectVault(l) },
    { name: "vibration_band over level [-1..4]", export: "vibration_band", args: [[-1, 4]], oracle: (l) => vibrationBand(l) },
    { name: "eat_chance_multiplier_milli over level [-1..4]", export: "eat_chance_multiplier_milli", args: [[-1, 4]], oracle: (l) => eatChanceMult(l) },
    { name: "can_carry_fragile over level [-1..4]", export: "can_carry_fragile", args: [[-1, 4]], oracle: (l) => canCarryFragile(l) },
    {
      name: "scan_ahead: alive x underground x level",
      export: "scan_ahead",
      args: [[0, 1], [0, 1], [-1, 4]],
      oracle: (a, u, l) => scanAhead(a, u, l),
    },
    {
      name: "can_play_sound: alive x maxSounds x index",
      export: "can_play_sound",
      args: [[0, 1], { values: [0, 1, 3, 6, 10] }, { values: [-1, 0, 2, 5, 9, 10] }],
      oracle: (a, m, i) => canPlaySound(a, m, i),
    },

    // --- hunger thresholds (nullary) ---
    { name: "peckish_threshold_milli()", export: "peckish_threshold_milli", args: [], oracle: () => 400 },
    { name: "hungry_threshold_milli()", export: "hungry_threshold_milli", args: [], oracle: () => 400 },
    { name: "starving_threshold_milli()", export: "starving_threshold_milli", args: [], oracle: () => 800 },
    { name: "objective_eat_threshold_milli()", export: "objective_eat_threshold_milli", args: [], oracle: () => 900 },

    // --- hunger behaviour / risk ---
    {
      name: "hunger_behaviour: hunger x hasNearby",
      export: "hunger_behaviour",
      args: [{ values: [0, 400, 401, 800, 801, 1000] }, [0, 1]],
      oracle: (h, n) => hungerBehaviour(h, n),
    },
    { name: "will_eat_objective over hunger sweep", export: "will_eat_objective", args: [{ values: [0, 800, 899, 900, 901, 1000] }], oracle: (h) => willEatObjective(h) },
    { name: "objective_at_risk over hunger sweep", export: "objective_at_risk", args: [{ values: [0, 799, 800, 801, 1000] }], oracle: (h) => objectiveAtRisk(h) },
    { name: "hunger_resists over hunger sweep", export: "hunger_resists", args: [{ values: [0, 399, 400, 401, 1000] }], oracle: (h) => hungerResists(h) },
    { name: "hunger_color_band over [0..1000 step]", export: "hunger_color_band", args: [{ values: [0, 399, 400, 599, 600, 799, 800, 1000] }], oracle: (h) => hungerColorBand(h) },
    { name: "hunger_display_band over [0..1000 step]", export: "hunger_display_band", args: [{ values: [0, 99, 100, 399, 400, 599, 600, 799, 800, 899, 900, 1000] }], oracle: (h) => hungerDisplayBand(h) },

    // --- hunger rate (micro-units) ---
    {
      name: "hunger_rate_micro: base x moving x hunger x coproc x level",
      export: "hunger_rate_micro",
      args: [
        { values: [15, 35] },                  // base hunger rate (0.015), training (0.035)
        [0, 1],
        { values: [0, 400, 800, 1000] },
        { values: [1000, 600, 50] },
        { values: [1000, 1500, 3000] },
      ],
      oracle: (b, m, h, c, l) => hungerRateMicro(b, m, h, c, l),
    },

    // --- gravity pull ---
    { name: "hunger_mult_milli over hunger sweep", export: "hunger_mult_milli", args: [{ values: [0, 200, 399, 400, 600, 799, 800, 900, 1000] }], oracle: (h) => hungerMultMilli(h) },
    {
      name: "hunger_pull_mu: direction x multMilli x distSqPx",
      export: "hunger_pull_mu",
      args: [
        { values: [-1, 0, 1] },
        { values: [0, 100, 500, 1000, 3000] },
        { values: [0, 100, 2500, 250000, 10000000, 2000000000] }, // px^2: clamp..near-i32-max
      ],
      oracle: (d, m, q) => hungerPull(d, m, q),
    },

    // --- scan / query predicates ---
    { name: "scan_runs over state [-1..11]", export: "scan_runs", args: [[-1, 11]], oracle: (s) => scanRuns(s) },
    { name: "is_underground over depth sweep", export: "is_underground", args: [{ values: [0, 49, 50, 51, 1000] }], oracle: (d) => isUnderground(d) },
    { name: "is_idle over state [-1..11]", export: "is_idle", args: [[-1, 11]], oracle: (s) => (s === 0 ? 1 : 0) },
    { name: "is_digging over state [-1..11]", export: "is_digging", args: [[-1, 11]], oracle: (s) => isDigging(s) },
    { name: "is_distracted over state [-1..11]", export: "is_distracted", args: [[-1, 11]], oracle: (s) => (s === 6 ? 1 : 0) },
    { name: "is_gliding over state [-1..11]", export: "is_gliding", args: [[-1, 11]], oracle: (s) => (s === 7 ? 1 : 0) },
    { name: "is_dead over state [-1..11]", export: "is_dead", args: [[-1, 11]], oracle: (s) => isDead(s) },
    { name: "crush_state()", export: "crush_state", args: [], oracle: () => 8 },
    { name: "caught_by_dog_state()", export: "caught_by_dog_state", args: [], oracle: () => 9 },
  ],
};
