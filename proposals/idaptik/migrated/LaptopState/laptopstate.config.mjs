// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for LaptopState.affine. Oracle re-derives the service
// bitmask, port-to-service, CPU, and storage kernels from the original
// LaptopState.res / LaptopStateCoprocessor.res source semantics.
//
// Service indices: SSH=0, HTTP=1, RDP=2, Desktop=3, Security=4
// initMask: server -> bits 0,1,4 set = 1+2+16=19; laptop -> bits 3,4 = 8+16=24
// service_for_port: 22->0(SSH), 80->1(HTTP), 3389->2(RDP), else -1
// service_running(mask, index): (mask >> index) & 1 if 0<=index<=4, else 0
// service_set(mask, index, running): set/clear bit index if valid, else mask
// port_open(mask, port): service_running(mask, service_for_port(port))

function validIndex(index) {
  return index >= 0 && index <= 4;
}
function bitOf(index) {
  let v = 1;
  for (let k = 0; k < index; k++) v *= 2;
  return v;
}
function oracleServiceRunning(mask, index) {
  if (!validIndex(index)) return 0;
  return ((mask >> index) & 1) !== 0 ? 1 : 0;
}
function oracleServiceSet(mask, index, running) {
  if (!validIndex(index)) return mask & 0xffffffff;
  const bit = bitOf(index);
  const isSet = ((mask >> index) & 1) !== 0;
  if (running !== 0) {
    return isSet ? mask & 0xffffffff : (mask + bit) & 0xffffffff;
  } else {
    return isSet ? (mask - bit) & 0xffffffff : mask & 0xffffffff;
  }
}
function oracleInitMask(isServer) {
  return isServer !== 0 ? 19 : 24;
}
function oracleServiceForPort(port) {
  if (port === 22) return 0;
  if (port === 80) return 1;
  if (port === 3389) return 2;
  return -1;
}
function oraclePortOpen(mask, port) {
  const idx = oracleServiceForPort(port);
  if (idx < 0) return 0;
  return oracleServiceRunning(mask, idx);
}

// cpu_spike_add(spike, amount, spike_cap): min(spike+amount, spike_cap).
//   Host passes spike_cap=50.
// cpu_spike_decay(spike, dt, decay_two, threshold):
//   decayed = spike - spike*dt*decay_two; decayed<=threshold -> threshold else decayed.
//   Host passes decay_two=2, threshold=0.
//   NOTE: original ReScript threshold was 0.1, but i32 ABI passes 0 as integer;
//   oracle uses 0 (threshold=0) matching the integer-input parity test.
// cpu_usage(base, spike, cores, hundred): min(base+spike, cores*hundred).
//   Host passes hundred=100.
// file_mb_to_gq(file_usage_mb: Int, hundred_mb: Int): max(1, floor(mb/hundred_mb)).
//   Host passes hundred_mb=100. Pure integer division (i32/i32).
//
// NOTE: parity harness normalises to i32. Float kernels only match when the
// computed float, cast to i32, equals the oracle cast to i32. We sweep integer
// inputs so both sides compute exactly the same IEEE-754 result.

function oracleCpuSpikeAdd(spike, amount, spike_cap) {
  const next = spike + amount;
  return next > spike_cap ? spike_cap : next;
}
function oracleCpuSpikeDecay(spike, dt, decay_two, threshold) {
  // decayed = spike - spike*dt*decay_two; if decayed<=threshold then threshold
  const decayed = spike - spike * dt * decay_two;
  return decayed <= threshold ? threshold : decayed;
}
function oracleCpuUsage(base, spike, cores, hundred) {
  const used = base + spike;
  const max = cores * hundred;
  return used < max ? used : max;
}
function oracleFileMbToGq(file_usage_mb, hundred_mb) {
  // Integer division (truncation), then floor to minimum 1
  const gq = Math.trunc(file_usage_mb / hundred_mb);
  return gq < 1 ? 1 : gq;
}

export default {
  affine: "LaptopState.affine",
  cases: [
    {
      name: "service_running: all (mask,index) pairs, mask 0..31, index -1..5",
      export: "service_running",
      args: [[0, 31], [-1, 5]],
      oracle: (mask, index) => oracleServiceRunning(mask, index) | 0,
    },
    {
      name: "service_set: mask 0..31, index -1..5, running 0..1",
      export: "service_set",
      args: [[0, 31], [-1, 5], [0, 1]],
      oracle: (mask, index, running) => oracleServiceSet(mask, index, running) | 0,
    },
    {
      name: "init_mask: is_server 0..1",
      export: "init_mask",
      args: [[0, 1]],
      oracle: (isServer) => oracleInitMask(isServer) | 0,
    },
    {
      name: "service_for_port: standard ports + boundaries",
      export: "service_for_port",
      args: [{ values: [-1, 0, 21, 22, 23, 79, 80, 81, 3388, 3389, 3390, 9999] }],
      oracle: (port) => oracleServiceForPort(port) | 0,
    },
    {
      name: "port_open: mask 0..31, port (22,80,3389,9999)",
      export: "port_open",
      args: [[0, 31], { values: [22, 80, 3389, 9999] }],
      oracle: (mask, port) => oraclePortOpen(mask, port) | 0,
    },
    {
      name: "cpu_spike_add: spike 0..50, amount 0..10, spike_cap=50 (host constant)",
      export: "cpu_spike_add",
      args: [[0, 50], [0, 10], { values: [50] }],
      oracle: (spike, amount, spike_cap) => oracleCpuSpikeAdd(spike, amount, spike_cap) | 0,
    },
    {
      name: "cpu_spike_decay: spike 0..50, dt 0..5, decay_two=2, threshold=0 (host constants)",
      export: "cpu_spike_decay",
      args: [
        { values: [0, 1, 5, 10, 20, 50] },
        { values: [0, 1, 2, 5] },
        { values: [2] },
        { values: [0] },
      ],
      oracle: (spike, dt, decay_two, threshold) => oracleCpuSpikeDecay(spike, dt, decay_two, threshold) | 0,
    },
    {
      name: "file_mb_to_gq: file_usage_mb (integer), hundred_mb=100 (host constant)",
      export: "file_mb_to_gq",
      args: [{ values: [0, 1, 50, 99, 100, 101, 200, 500, 1000] }, { values: [100] }],
      oracle: (mb, hundred_mb) => oracleFileMbToGq(mb, hundred_mb) | 0,
    },
    {
      name: "cpu_usage: base 0..100, spike 0..50, cores 1..4, hundred=100 (host constant)",
      export: "cpu_usage",
      args: [[0, 100], [0, 50], [1, 4], { values: [100] }],
      oracle: (base, spike, cores, hundred) => oracleCpuUsage(base, spike, cores, hundred) | 0,
    },
  ],
};
