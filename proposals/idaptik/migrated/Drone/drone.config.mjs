// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Drone.affine (idaptik aerial-drone behaviour brain;
// scalar i32 ABI). Every oracle below is an INDEPENDENT JS reimplementation
// derived from src/app/enemies/Drone.res semantics -- NOT a copy of the .affine
// logic -- so the differential sweep is a genuine cross-check. Floats in the .res
// cross in milli-units (x1000); the oracles work in the same scaled integers.
//
// State ordinals: 0 Patrolling 1 Hovering 2 Tracking 3 Spotlighting 4 Returning
//   5 Charging 6 Jammed 7 Disabled 8 Delivering 9 Rescuing 10 Repairing
//   11 Reviving 12 OpeningDoor.  Variant ordinals: 0 Recon 1 Pursuit 2 EMP_Drone.
//   Detection class: 0 NotDetected 1 InDetectionZone 2 SpotlightLock.

// --- shared oracle helpers (re-derived from Drone.res, kept independent) ---

const validVariant = (c) => c >= 0 && c <= 2;
const validState = (c) => c >= 0 && c <= 12;
// canonicalisers: identity on band, -1 sentinel off it (Drone.res variant/state).
const vv = (c) => (validVariant(c) ? c : -1);
const sv = (c) => (validState(c) ? c : -1);

export default {
  affine: "Drone.affine",
  cases: [
    // -- band validity & canonicalisation --
    {
      name: "is_valid_variant over [-3..6]",
      export: "is_valid_variant",
      args: [[-3, 6]],
      oracle: (c) => (validVariant(c) ? 1 : 0),
    },
    {
      name: "variant_value over [-3..6]",
      export: "variant_value",
      args: [[-3, 6]],
      oracle: (c) => vv(c),
    },
    {
      name: "is_valid_state over [-3..16]",
      export: "is_valid_state",
      args: [[-3, 16]],
      oracle: (c) => (validState(c) ? 1 : 0),
    },
    {
      name: "state_value over [-3..16]",
      export: "state_value",
      args: [[-3, 16]],
      oracle: (c) => sv(c),
    },

    // -- per-variant stat table (Drone.make switch; -1 for bad variant) --
    {
      name: "variant_speed over [-1..4]",
      export: "variant_speed",
      args: [[-1, 4]],
      // Recon 60, Pursuit 100, EMP 35; else -1.
      oracle: (c) => (c === 0 ? 60 : c === 1 ? 100 : c === 2 ? 35 : -1),
    },
    {
      name: "variant_detection_radius over [-1..4]",
      export: "variant_detection_radius",
      args: [[-1, 4]],
      oracle: (c) => (c === 0 ? 120 : c === 1 ? 80 : c === 2 ? 100 : -1),
    },
    {
      name: "variant_noise_radius over [-1..4]",
      export: "variant_noise_radius",
      args: [[-1, 4]],
      oracle: (c) => (c === 0 ? 100 : c === 1 ? 180 : c === 2 ? 80 : -1),
    },
    {
      name: "variant_drain_milli over [-1..4]",
      export: "variant_drain_milli",
      args: [[-1, 4]],
      // 0.008/0.015/0.012 -> 8/15/12 milli; else -1.
      oracle: (c) => (c === 0 ? 8 : c === 1 ? 15 : c === 2 ? 12 : -1),
    },

    // -- detection --
    {
      name: "can_detect over states [-1..13]",
      export: "can_detect",
      args: [[-1, 13]],
      // NotDetected while Disabled(7)/Charging(5)/Jammed(6) or out-of-band; else 1.
      oracle: (s) => {
        if (!validState(s)) return 0;
        if (s === 7 || s === 5 || s === 6) return 0;
        return 1;
      },
    },
    {
      name: "effective_radius_milli(radius, crouch)",
      export: "effective_radius_milli",
      args: [{ values: [0, 80, 100, 120, 250] }, { values: [0, 1] }],
      // crouch shrinks radius to 0.6; output in milli (px*1000 or px*600).
      oracle: (r, crouch) => (crouch === 0 ? r * 1000 : r * 600),
    },
    {
      name: "proximity_milli(dist, radius) -- 1 - dist/radius in milli",
      export: "proximity_milli",
      args: [
        { values: [0, 1000, 5000, 40000, 80000, 119000, 120000, 200000] },
        { values: [0, 48000, 72000, 80000, 120000] },
      ],
      oracle: (dist, radius) => {
        if (radius <= 0) return 0;
        if (dist >= radius) return 0;
        let p = 1000 - Math.trunc((dist * 1000) / radius);
        if (p < 0) return 0;
        if (p > 1000) return 1000;
        return p;
      },
    },
    {
      name: "detection_class(state, dist, radius, spotlight)",
      export: "detection_class",
      args: [
        { values: [0, 1, 2, 5, 6, 7, 13] }, // states incl. blind & out-of-band
        { values: [0, 5000, 40000, 80000, 119000, 120000] },
        { values: [0, 120000] },
        { values: [0, 1] },
      ],
      oracle: (s, dist, radius, spot) => {
        // gate
        if (!validState(s)) return 0;
        if (s === 7 || s === 5 || s === 6) return 0;
        if (radius <= 0) return 0;
        if (dist >= radius) return 0;
        let prox = 1000 - Math.trunc((dist * 1000) / radius);
        if (prox < 0) prox = 0;
        if (prox > 1000) prox = 1000;
        if (spot === 0) return 1;
        return prox > 300 ? 2 : 1;
      },
    },

    // -- battery --
    {
      name: "battery_dead over [-50..1050] step via values",
      export: "battery_dead",
      args: [{ values: [-50, -1, 0, 1, 150, 500, 1000] }],
      oracle: (b) => (b <= 0 ? 1 : 0),
    },
    {
      name: "battery_low(battery, threshold=150) sweep battery",
      export: "battery_low",
      args: [{ values: [0, 100, 149, 150, 151, 500, 1000] }, 150],
      oracle: (b, t) => (b <= t ? 1 : 0),
    },
    {
      name: "battery_low varying threshold too",
      export: "battery_low",
      args: [{ values: [0, 200, 300] }, { values: [150, 200, 250] }],
      oracle: (b, t) => (b <= t ? 1 : 0),
    },
    {
      name: "charge_complete (>=950)",
      export: "charge_complete",
      args: [{ values: [0, 500, 949, 950, 951, 1000] }],
      oracle: (b) => (b >= 950 ? 1 : 0),
    },

    // -- transitions --
    {
      name: "arrived (|dx| < 5000 milli = 5px)",
      export: "arrived",
      args: [{ values: [0, 4999, 5000, 5001, 50000] }],
      oracle: (d) => (d < 5000 ? 1 : 0),
    },
    {
      name: "patrol_next(variant, alert, class, proximity)",
      export: "patrol_next",
      args: [
        { values: [0, 1, 2, 7] }, // variant incl. out-of-band 7
        { values: [0, 2, 3, 5] }, // alert level
        { values: [0, 1, 2] }, // detection class
        { values: [0, 400, 401, 1000] }, // proximity milli
      ],
      oracle: (variant, alert, cls, prox) => {
        // Patrolling arm: only InDetectionZone(1) with proximity>0.4 escalates.
        if (cls !== 1) return 0;
        if (prox <= 400) return 0;
        if (alert >= 3) return 2; // -> Tracking
        if (vv(variant) === 1) return 2; // Pursuit -> Tracking
        return 1; // -> Hovering
      },
    },
    {
      name: "hover_next(class, proximity, suspicion, hoverTimer)",
      export: "hover_next",
      args: [
        { values: [0, 1, 2] }, // class
        { values: [0, 300, 301, 1000] }, // proximity milli
        { values: [0, 700, 701, 1000] }, // suspicion milli
        { values: [0, 4999, 5000, 6000] }, // hover timer milli
      ],
      oracle: (cls, prox, susp, timer) => {
        if (cls === 2) return 2; // SpotlightLock -> Tracking
        if (cls === 1) {
          if (prox > 300) {
            if (susp > 700) return 2; // -> Tracking
            return 1; // stay Hovering
          }
        }
        if (timer >= 5000) return 0; // window expired -> Patrolling
        return 1;
      },
    },
    {
      name: "tracking_next(variant, alert, class, payload, dist, timer)",
      export: "tracking_next",
      args: [
        { values: [0, 1, 2] }, // variant
        { values: [1, 2] }, // alert
        { values: [0, 1, 2] }, // class
        { values: [0, 1] }, // payload
        { values: [39999, 40000] }, // dist milli
        { values: [0, 8000] }, // timer milli
      ],
      oracle: (variant, alert, cls, payload, dist, timer) => {
        if (cls === 0) {
          if (timer >= 8000) return 0; // lost visual long enough -> Patrolling
          return 2; // keep Tracking
        }
        const v = vv(variant);
        if (v === 1) {
          if (alert >= 2) return 3; // Pursuit spotlight -> Spotlighting
        }
        if (v === 2) {
          if (payload === 1) {
            if (dist < 40000) return 8; // EMP deliver -> Delivering
          }
        }
        return 2; // stay Tracking
      },
    },
    {
      name: "spotlight_next(class, intensity)",
      export: "spotlight_next",
      args: [{ values: [0, 1, 2] }, { values: [-10, 0, 1, 1000] }],
      oracle: (cls, inten) => {
        if (cls === 0) {
          if (inten <= 0) return 2; // faded -> Tracking
          return 3; // still fading, hold Spotlighting
        }
        return 3; // live sighting, stay Spotlighting
      },
    },
    {
      name: "delivering_next(timer) -- complete at 1500 milli (1.5s)",
      export: "delivering_next",
      args: [{ values: [0, 1499, 1500, 1501, 3000] }],
      oracle: (t) => (t >= 1500 ? 4 : 8),
    },
    {
      name: "returning_next(arrived)",
      export: "returning_next",
      args: [{ values: [0, 1] }],
      oracle: (a) => (a === 1 ? 5 : 4),
    },
    {
      name: "charging_next(charge_done)",
      export: "charging_next",
      args: [{ values: [0, 1] }],
      oracle: (d) => (d === 1 ? 0 : 5),
    },
    {
      name: "helper_task_next(state, hasTarget, arrived, timer)",
      export: "helper_task_next",
      args: [
        { values: [9, 10, 11, 12, 0, 13] }, // current state incl. out-of-band 13
        { values: [0, 1] }, // has target
        { values: [0, 1] }, // arrived
        { values: [-5, 0, 5] }, // task timer milli
      ],
      oracle: (state, hasTarget, arrived, timer) => {
        if (hasTarget === 0) return 0; // abort -> Patrolling
        if (arrived === 1) {
          if (timer <= 0) return 0; // task complete -> Patrolling
        }
        return sv(state); // stay in current helper state (canonicalised)
      },
    },

    // -- interactions & orders --
    {
      name: "apply_emp_next(state) -- Jammed(6) unless Disabled(7)",
      export: "apply_emp_next",
      args: [{ values: [0, 2, 5, 6, 7, 12] }],
      oracle: (s) => (sv(s) === 7 ? 7 : 6),
    },
    {
      name: "hack_complete(progress) -- >=1000 milli (1.0)",
      export: "hack_complete",
      args: [{ values: [0, 500, 999, 1000, 1001] }],
      oracle: (p) => (p >= 1000 ? 1 : 0),
    },
    {
      name: "is_audible(state, dist, noise)",
      export: "is_audible",
      args: [
        { values: [0, 2, 5, 7, 13] }, // states incl. silent & out-of-band
        { values: [0, 79999, 80000, 100000] }, // dist milli
        { values: [80000, 100000] }, // noise milli
      ],
      oracle: (s, dist, noise) => {
        const c = sv(s);
        if (c === 7 || c === 5 || c < 0) return 0;
        return dist < noise ? 1 : 0;
      },
    },
    {
      name: "helper_order_state(variant, state, task_kind) -- .res precedence",
      export: "helper_order_state",
      args: [
        { values: [0, 1, 2, 7] }, // variant incl. out-of-band 7
        { values: [0, 1, 2, 5, 13] }, // state incl. out-of-band 13
        { values: [0, 1, 2, 3, 4] }, // task kind incl. out-of-band 4
      ],
      oracle: (variant, state, kind) => {
        const v = vv(variant);
        const s = sv(state);
        // .res guard, operator precedence preserved:
        //   (v==EMP_Drone(2) && s==Patrolling(0)) || s==Hovering(1)
        const eligible = (v === 2 && s === 0) || s === 1;
        if (!eligible) return -1;
        if (kind === 0) return 9;
        if (kind === 1) return 10;
        if (kind === 2) return 11;
        if (kind === 3) return 12;
        return -1; // bad task_kind
      },
    },
    {
      name: "distraction_next(state, dist, range)",
      export: "distraction_next",
      args: [
        { values: [0, 1, 2, 5, 13] }, // state incl. out-of-band 13
        { values: [0, 49999, 50000] }, // dist milli
        { values: [50000, 60000] }, // range milli
      ],
      oracle: (state, dist, range) => {
        const s = sv(state);
        if (dist >= range) return s; // out of range -> unchanged
        if (s === 0 || s === 1) return 2; // Patrolling/Hovering -> Tracking
        return s; // other states unchanged
      },
    },

    // -- queries --
    {
      name: "is_active over states [-1..13]",
      export: "is_active",
      args: [[-1, 13]],
      oracle: (s) => {
        const c = sv(s);
        if (c < 0) return 0;
        if (c === 7 || c === 5) return 0; // Disabled/Charging inert
        return 1;
      },
    },
    {
      name: "is_helper_busy over states [-1..14]",
      export: "is_helper_busy",
      args: [[-1, 14]],
      oracle: (s) => {
        const c = sv(s);
        if (c < 9) return 0;
        if (c > 12) return 0;
        return 1;
      },
    },
    {
      name: "has_emp_payload(variant, available)",
      export: "has_emp_payload",
      args: [{ values: [0, 1, 2, 7] }, { values: [0, 1] }],
      oracle: (variant, avail) => {
        if (vv(variant) !== 2) return 0;
        return avail === 1 ? 1 : 0;
      },
    },
  ],
};
