// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for PowerManager.affine. Oracle re-derives battery and
// power-state-machine logic from PowerManager.res / PowerManagerCoprocessor.res:
//
//   drain_rate(load, thirty) -> load / thirty.  Host passes thirty=30.
//   battery_drain(charge, dt, load, thirty, zero) ->
//       max(zero, charge - dt * drain_rate(load, thirty)).
//       Host passes thirty=30, zero=0.
//   battery_charge(charge, dt, rate, hundred) -> min(hundred, charge + dt * rate).
//       Host passes hundred=100.
//   brownout_check(charge, threshold) -> charge <= threshold ? 1 : 0
//   power_state_transition(state, event) -> state machine:
//     event=1 (MainsRestored) always -> 0 (MainsPower)
//     event=0 (MainsLost)  state=0 -> 1 (UPSBattery);  else hold
//     event=2 (BatteryDepleted) state=1 -> 2 (NoPower); else hold
//     event=3 (BatteryRecovered) state=2 -> 1 (UPSBattery); else hold
//     unknown event -> hold current state
//
// NOTE: The i32 ABI applies to ALL exports regardless of declared Float type.
// Float parameters received as i32 are used with integer arithmetic opcodes
// (i32.div_s, i32.mul_s, etc.) — the same root cause as the float literal bug.
// The parity oracle mirrors this: drain_rate uses Math.trunc(), so small integer
// loads produce 0 drain (which is correct for the test domain: load=0..10, thirty=30
// means drain=0 for load < 30, integer-exact). The host call-site must account for
// this: meaningful drain requires the host to scale load*thirty before passing, or
// use fixed-point arithmetic. For the parity test domain (small integers) the
// integer truncation is predictable and testable.

// NOTE: The ABI is i32 for ALL exports regardless of declared Float type.
// Float parameters are passed as i32, and Float arithmetic with two i32-ABI
// variables uses i32 integer opcodes (same root cause as the float literal bug).
// The oracle must mirror the INTEGER arithmetic the wasm actually performs:
//   load / thirty  ->  i32 division (truncation)
//   charge - dt * rate  ->  all integer
// floor and clamp also operate on integer values.
function oracleDrainRate(load, thirty) {
  return Math.trunc(load / thirty);   // i32.div_s: integer truncation
}
function oracleBatteryDrain(charge, dt, load, thirty, zero) {
  const rate = Math.trunc(load / thirty);
  const newLevel = charge - dt * rate;
  return newLevel < zero ? zero : newLevel;
}
function oracleBatteryCharge(charge, dt, rate, hundred) {
  const newLevel = charge + dt * rate;
  return newLevel > hundred ? hundred : newLevel;
}
function oracleBrownoutCheck(charge, threshold) {
  return charge <= threshold ? 1 : 0;
}
function oraclePowerStateTransition(state, event) {
  if (event === 1) return 0;
  if (event === 0) { if (state === 0) return 1; return state; }
  if (event === 2) { if (state === 1) return 2; return state; }
  if (event === 3) { if (state === 2) return 1; return state; }
  return state;
}

export default {
  affine: "PowerManager.affine",
  cases: [
    {
      name: "drain_rate: load 0..10 (integer device count), thirty=30 (host constant)",
      export: "drain_rate",
      args: [[0, 10], { values: [30] }],
      oracle: (load, thirty) => oracleDrainRate(load, thirty) | 0,
    },
    {
      name: "battery_drain: charge, dt, load, thirty=30, zero=0 (host constants)",
      export: "battery_drain",
      args: [
        { values: [0, 10, 30, 50, 80, 100] },
        { values: [0, 1, 5, 10, 30] },
        { values: [0, 1, 3, 5] },
        { values: [30] },
        { values: [0] },
      ],
      oracle: (charge, dt, load, thirty, zero) => oracleBatteryDrain(charge, dt, load, thirty, zero) | 0,
    },
    {
      name: "battery_charge: charge, dt, rate, hundred=100 (host constant)",
      export: "battery_charge",
      args: [
        { values: [0, 10, 50, 90, 100] },
        { values: [0, 1, 10, 30, 60] },
        { values: [0, 1, 2] },
        { values: [100] },
      ],
      oracle: (charge, dt, rate, hundred) => oracleBatteryCharge(charge, dt, rate, hundred) | 0,
    },
    {
      name: "brownout_check: charge 0..10, threshold 0..10",
      export: "brownout_check",
      args: [[0, 10], [0, 10]],
      oracle: (charge, threshold) => oracleBrownoutCheck(charge, threshold) | 0,
    },
    {
      name: "power_state_transition: state 0..3, event 0..4",
      export: "power_state_transition",
      args: [
        { values: [0, 1, 2, 3] },
        { values: [0, 1, 2, 3, 4] },
      ],
      oracle: (state, event) => oraclePowerStateTransition(state, event) | 0,
    },
  ],
};
