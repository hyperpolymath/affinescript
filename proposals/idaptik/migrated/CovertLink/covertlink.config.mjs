// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for CovertLink.affine. Oracle re-derives the stats,
// is_monitored, state_color, latency_ticks, and TTL logic from the original
// CovertLink.res / CovertLinkCoprocessor.res source semantics.
//
// defaultStats from CovertLink.res (connectionType enum order, 0..6):
//   DarkFibre(0):       bw=5, lat=5, stealth=5, dur=5
//   LegacyTrunk(1):     bw=4, lat=4, stealth=3, dur=2
//   OOBManagement(2):   bw=2, lat=3, stealth=4, dur=4
//   WirelessBridge(3):  bw=3, lat=2, stealth=1, dur=3
//   CrossConnect(4):    bw=4, lat=4, stealth=3, dur=3
//   MaintenanceVLAN(5): bw=3, lat=3, stealth=2, dur=5
//   ImprovisedLink(6):  bw=1, lat=1, stealth=3, dur=1
//   Out-of-band:        bw=0, lat=0, stealth=0, dur=0

const STATS = [
  { bw: 5, lat: 5, st: 5, dur: 5 }, // 0 DarkFibre
  { bw: 4, lat: 4, st: 3, dur: 2 }, // 1 LegacyTrunk
  { bw: 2, lat: 3, st: 4, dur: 4 }, // 2 OOBManagement
  { bw: 3, lat: 2, st: 1, dur: 3 }, // 3 WirelessBridge
  { bw: 4, lat: 4, st: 3, dur: 3 }, // 4 CrossConnect
  { bw: 3, lat: 3, st: 2, dur: 5 }, // 5 MaintenanceVLAN
  { bw: 1, lat: 1, st: 3, dur: 1 }, // 6 ImprovisedLink
];
const OOB_STATS = { bw: 0, lat: 0, st: 0, dur: 0 };
function getStat(ct, field) {
  const s = (ct >= 0 && ct <= 6) ? STATS[ct] : OOB_STATS;
  return s[field];
}

// isMonitored: WirelessBridge(3) and MaintenanceVLAN(5) only
function oracleIsMonitored(ct) {
  return (ct === 3 || ct === 5) ? 1 : 0;
}

// stateToColor: 0=Unknown=0x444444, 1=Discovered=0xFFAA00,
//               2=Active=0x00FF88, 3=Dead=0xFF2222, else=0x444444
function oracleStateColor(st) {
  if (st === 0) return 0x444444;
  if (st === 1) return 0xFFAA00;
  if (st === 2) return 0x00FF88;
  if (st === 3) return 0xFF2222;
  return 0x444444;
}

// ttl_step(remaining, dt, zero): max(zero, remaining - dt). Host passes zero=0.
// NOTE: parity uses i32 ABI. Float tests use integer inputs so i32 cast is exact.
// ttl_expired(remaining, dt): dt >= remaining -> 1, else 0.
//   (equivalent to remaining - dt <= 0.0, but avoids float literal in binary op)
function oracleTtlStep(remaining, dt) {
  // zero=0 is the host default; oracle computes max(0, remaining-dt)
  const next = remaining - dt;
  return next < 0 ? 0 : next;
}
function oracleTtlExpired(remaining, dt) {
  return (remaining - dt) <= 0 ? 1 : 0;
}

export default {
  affine: "CovertLink.affine",
  cases: [
    {
      name: "type_bandwidth over ct [-1..8]",
      export: "type_bandwidth",
      args: [[-1, 8]],
      oracle: (ct) => getStat(ct, "bw") | 0,
    },
    {
      name: "type_latency over ct [-1..8]",
      export: "type_latency",
      args: [[-1, 8]],
      oracle: (ct) => getStat(ct, "lat") | 0,
    },
    {
      name: "type_stealth over ct [-1..8]",
      export: "type_stealth",
      args: [[-1, 8]],
      oracle: (ct) => getStat(ct, "st") | 0,
    },
    {
      name: "type_durability over ct [-1..8]",
      export: "type_durability",
      args: [[-1, 8]],
      oracle: (ct) => getStat(ct, "dur") | 0,
    },
    {
      name: "is_monitored over ct [-1..8]",
      export: "is_monitored",
      args: [[-1, 8]],
      oracle: (ct) => oracleIsMonitored(ct) | 0,
    },
    {
      name: "state_color over st [-1..5]",
      export: "state_color",
      args: [[-1, 5]],
      oracle: (st) => oracleStateColor(st) | 0,
    },
    {
      name: "latency_ticks (nullary)",
      export: "latency_ticks",
      args: [],
      oracle: () => 1,
    },
    {
      name: "ttl_step: remaining 0..10, dt 0..10, zero=0 (host constant)",
      export: "ttl_step",
      args: [[0, 10], [0, 10], { values: [0] }],
      oracle: (r, d, _z) => oracleTtlStep(r, d) | 0,
    },
    {
      name: "ttl_expired: remaining 0..10, dt 0..10 (integer steps)",
      export: "ttl_expired",
      args: [[0, 10], [0, 10]],
      oracle: (r, d) => oracleTtlExpired(r, d) | 0,
    },
  ],
};
