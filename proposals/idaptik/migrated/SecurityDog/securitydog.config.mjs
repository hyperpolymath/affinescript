// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for SecurityDog.affine (the guard-dog / robo-dog decision
// core extracted from src/app/enemies/SecurityDog.res). The brain is Int-only,
// so parity runs directly against the deliverable.
//
// Every oracle is an INDEPENDENT re-derivation of the SecurityDog.res semantics
// in plain JS (the detection classifier, the 8-state machine transition table,
// the interaction gates and the sign helpers) -- not a copy of the .affine. A
// codegen regression surfaces as a differential mismatch. Units mirror the
// brain: distances/ranges in milli-pixels, angles in milli-radians, timers in
// milliseconds, scent strength in milli-units (0..1000).

// --- detection-model range table (detectionModel records) ------------------
function validVariant(v) { return v === 0 || v === 1; }
function validState(s) { return s >= 0 && s <= 7; }

function scentRange(v) {
  if (!validVariant(v)) return -1;
  return v === 0 ? 180000 : 0;
}
function cameraRange(v) {
  if (!validVariant(v)) return -1;
  return v === 1 ? 200000 : 0;
}
function cameraHalfAngle(v) {
  if (!validVariant(v)) return -1;
  return v === 1 ? 500 : 0;
}
function irRange(v) {
  if (!validVariant(v)) return -1;
  return v === 1 ? 120000 : 0;
}
function hearingRange(v) {
  if (!validVariant(v)) return -1;
  return v === 0 ? 200000 : 150000;
}
function speed(v) {
  if (!validVariant(v)) return -1;
  return v === 0 ? 75000 : 55000;
}

// --- scent strength: 1.0 - distance/scentRange, milli-units ----------------
function scentStrength(distance, range) {
  if (range <= 0) return 0;
  if (distance < 0) return 1000;
  if (distance >= range) return 0;
  const s = 1000 - Math.trunc((distance * 1000) / range);
  if (s < 0) return 0;
  if (s > 1000) return 1000;
  return s;
}

// --- angle normalisation: fold into [0, pi], milli-radians -----------------
function angleNorm(diff, pi, twoPi) {
  if (diff < 0) return 0;
  if (diff > pi) return twoPi - diff;
  return diff;
}
function effectiveCameraRange(camRange, crouching) {
  if (camRange < 0) return -1;
  if (crouching !== 0) return Math.trunc(camRange / 2);
  return camRange;
}

// --- detectPlayer classifier -----------------------------------------------
// 0 NotDetected, 1 ScentDetected, 2 VisualDetected, 3 HeardMovement.
function detectClass(state, variant, distance, sprinting, normAngle, effRange) {
  if (!validVariant(variant)) return 0;
  if (state === 6 || state === 7 || state === 3) return 0; // Disabled/Stunned/Distracted
  if (variant === 0) {
    const scent = scentRange(0);
    const hearing = hearingRange(0);
    if (distance < scent) return 1;
    if (sprinting !== 0 && distance < hearing) return 3;
    return 0;
  }
  const half = cameraHalfAngle(1);
  const ir = irRange(1);
  const hearing = hearingRange(1);
  if (normAngle < half && distance < effRange) return 2;
  if (distance < ir) return 2;
  if (sprinting !== 0 && distance < hearing) return 3;
  return 0;
}

// --- updateState transition table ------------------------------------------
function nextState(state, detection, strength, distToTarget, barkTimer, pauseTimer, hasTarget) {
  if (!validState(state)) return -1;
  switch (state) {
    case 0: // Patrolling
      if (detection === 1) return strength > 300 ? 1 : 0;
      if (detection === 2) return 2;
      if (detection === 3) return 4;
      return 0;
    case 1: // Tracking
      if (hasTarget === 0) return 5;
      if (distToTarget < 30000) return 2;
      return 1;
    case 2: // Barking
      if (barkTimer >= 4000) return 1;
      return 2;
    case 3: // Distracted
      return 3;
    case 4: // Investigating
      if (hasTarget === 0) return 5;
      if (detection === 1 && strength > 400) return 1;
      if (detection === 2) return 2;
      if (distToTarget < 10000 && pauseTimer >= 2500) return 5;
      return 4;
    case 5: // Returning
      if (distToTarget < 5000) return 0;
      return 5;
    default: // Disabled(6) / Stunned(7) absorbing
      return state;
  }
}

function distractionExpired(state, timer) {
  if (state !== 3) return state;
  if (timer <= 0) return 5;
  return 3;
}

function robodogEarlyExit(variant, empActive, empJustExpired, hacked, batteryEmpty) {
  if (variant !== 1) return -1;
  if (empActive !== 0) return empJustExpired !== 0 ? 5 : 7;
  if (hacked !== 0) return 6;
  if (batteryEmpty !== 0) return 6;
  return -1;
}

// --- interaction gates ------------------------------------------------------
function canDistract(variant, state, foodDist) {
  if (variant !== 0) return 0;
  if (state === 6 || state === 7) return 0;
  return foodDist < scentRange(0) ? 1 : 0;
}
function canEmp(variant, state) {
  if (variant !== 1) return 0;
  if (state === 6) return 0;
  return 1;
}
function hackComplete(variant, hacked, progress) {
  if (variant !== 1) return 0;
  if (hacked !== 0) return 0;
  return progress >= 1000 ? 1 : 0;
}
function canRespondDistraction(state, dist, range) {
  if (state !== 0) return 0;
  return dist < range ? 1 : 0;
}

// --- combat / query helpers -------------------------------------------------
function isAggressive(state) {
  if (!validState(state)) return 0;
  return state === 1 || state === 2 ? 1 : 0;
}
function facingForDelta(dx) { return dx > 0 ? 1 : 0; }
function directionForDelta(dx) { return dx > 0 ? 1 : -1; }

export default {
  affine: "SecurityDog.affine",
  cases: [
    { name: "variant_count()", export: "variant_count", args: [], oracle: () => 2 },
    { name: "state_count()", export: "state_count", args: [], oracle: () => 8 },
    { name: "detection_count()", export: "detection_count", args: [], oracle: () => 4 },

    { name: "scent_range_mu over variant [-1..3]", export: "scent_range_mu", args: [[-1, 3]], oracle: (v) => scentRange(v) },
    { name: "camera_range_mu over variant [-1..3]", export: "camera_range_mu", args: [[-1, 3]], oracle: (v) => cameraRange(v) },
    { name: "camera_half_angle_mu over variant [-1..3]", export: "camera_half_angle_mu", args: [[-1, 3]], oracle: (v) => cameraHalfAngle(v) },
    { name: "ir_range_mu over variant [-1..3]", export: "ir_range_mu", args: [[-1, 3]], oracle: (v) => irRange(v) },
    { name: "hearing_range_mu over variant [-1..3]", export: "hearing_range_mu", args: [[-1, 3]], oracle: (v) => hearingRange(v) },
    { name: "speed_mu over variant [-1..3]", export: "speed_mu", args: [[-1, 3]], oracle: (v) => speed(v) },

    {
      name: "scent_strength_milli: distance x range (edges + out-of-band)",
      export: "scent_strength_milli",
      args: [
        { values: [-1, 0, 1000, 45000, 90000, 135000, 179999, 180000, 200000] },
        { values: [0, 180000, 200000] },
      ],
      oracle: (d, r) => scentStrength(d, r),
    },
    {
      name: "angle_norm_mu: diff folded into [0,pi] (pi=3141, 2pi=6283)",
      export: "angle_norm_mu",
      args: [
        { values: [-100, 0, 500, 3141, 3142, 4000, 6283] },
        { values: [3141] },
        { values: [6283] },
      ],
      oracle: (diff, pi, twoPi) => angleNorm(diff, pi, twoPi),
    },
    {
      name: "effective_camera_range_mu: camRange x crouch (0/1) + oob",
      export: "effective_camera_range_mu",
      args: [{ values: [-1, 0, 120000, 200000] }, [0, 1]],
      oracle: (c, crouch) => effectiveCameraRange(c, crouch),
    },

    {
      name: "detect_class GuardDog: state x distance x sprint",
      export: "detect_class",
      args: [
        { values: [0, 1, 3, 6, 7] },          // representative states incl. early-outs
        { values: [0] },                       // GuardDog
        { values: [0, 90000, 179999, 180000, 199999, 200000, 250000] },
        [0, 1],                                // sprinting
        { values: [0] },                       // norm angle (ignored for GuardDog)
        { values: [0] },                       // eff range (ignored for GuardDog)
      ],
      oracle: (s, v, d, sp, na, er) => detectClass(s, v, d, sp, na, er),
    },
    {
      name: "detect_class RoboDog: state x distance x sprint x angle x effRange",
      export: "detect_class",
      args: [
        { values: [0, 1, 3, 6, 7] },
        { values: [1] },                       // RoboDog
        { values: [0, 100000, 119999, 120000, 150000, 199999, 200000, 250000] },
        [0, 1],
        { values: [0, 499, 500, 1000] },       // folded cone angle vs half=500
        { values: [100000, 200000] },          // effective range
      ],
      oracle: (s, v, d, sp, na, er) => detectClass(s, v, d, sp, na, er),
    },

    {
      name: "next_state: full table x detection x strength x dist x timers x hasTarget",
      export: "next_state",
      args: [
        [-1, 8],                               // state incl. out-of-band shoulders
        { values: [0, 1, 2, 3] },              // detection class
        { values: [0, 300, 301, 400, 401, 1000] },
        { values: [0, 4999, 5000, 9999, 10000, 29999, 30000] },
        { values: [0, 3999, 4000] },
        { values: [0, 2499, 2500] },
        [0, 1],
      ],
      oracle: (st, det, str, dt, bt, pt, ht) => nextState(st, det, str, dt, bt, pt, ht),
    },
    {
      name: "distraction_expired: state x timer",
      export: "distraction_expired",
      args: [[-1, 8], { values: [-100, 0, 1, 5000] }],
      oracle: (s, t) => distractionExpired(s, t),
    },
    {
      name: "robodog_early_exit: variant x emp x empExpired x hacked x battery",
      export: "robodog_early_exit",
      args: [[-1, 2], [0, 1], [0, 1], [0, 1], [0, 1]],
      oracle: (v, e, ee, h, b) => robodogEarlyExit(v, e, ee, h, b),
    },

    {
      name: "can_distract: variant x state x foodDist",
      export: "can_distract",
      args: [[-1, 2], [-1, 8], { values: [0, 90000, 179999, 180000, 200000] }],
      oracle: (v, s, d) => canDistract(v, s, d),
    },
    {
      name: "can_emp: variant x state",
      export: "can_emp",
      args: [[-1, 2], [-1, 8]],
      oracle: (v, s) => canEmp(v, s),
    },
    {
      name: "hack_complete: variant x hacked x progress",
      export: "hack_complete",
      args: [[-1, 2], [0, 1], { values: [0, 500, 999, 1000, 1500] }],
      oracle: (v, h, p) => hackComplete(v, h, p),
    },
    {
      name: "can_respond_distraction: state x dist x range",
      export: "can_respond_distraction",
      args: [[-1, 8], { values: [0, 50000, 99999, 100000, 150000] }, { values: [100000] }],
      oracle: (s, d, r) => canRespondDistraction(s, d, r),
    },

    { name: "head_stomp_state()", export: "head_stomp_state", args: [], oracle: () => 6 },
    { name: "body_stomp_state()", export: "body_stomp_state", args: [], oracle: () => 7 },
    { name: "is_aggressive over state [-1..8]", export: "is_aggressive", args: [[-1, 8]], oracle: (s) => isAggressive(s) },
    { name: "is_barking over state [-1..8]", export: "is_barking", args: [[-1, 8]], oracle: (s) => (s === 2 ? 1 : 0) },
    { name: "is_disabled over state [-1..8]", export: "is_disabled", args: [[-1, 8]], oracle: (s) => (s === 6 ? 1 : 0) },
    { name: "is_distracted over state [-1..8]", export: "is_distracted", args: [[-1, 8]], oracle: (s) => (s === 3 ? 1 : 0) },
    { name: "is_tracking over state [-1..8]", export: "is_tracking", args: [[-1, 8]], oracle: (s) => (s === 1 ? 1 : 0) },
    { name: "facing_for_delta over [-5..5]", export: "facing_for_delta", args: [[-5, 5]], oracle: (dx) => facingForDelta(dx) },
    { name: "direction_for_delta over [-5..5]", export: "direction_for_delta", args: [[-5, 5]], oracle: (dx) => directionForDelta(dx) },
  ],
};
