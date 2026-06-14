// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for ResourceAccounting.affine (idaptik per-device
// resource-accounting core; scalar i32 ABI). Every oracle is an INDEPENDENT JS
// reimplementation derived from src/shared/ResourceAccounting.res semantics (the
// cumulative folds, the memory PEAK, the 95% saturation cutoff, the checkQuota
// three-way fit) -- NOT a copy of the .affine logic -- so the differential sweep
// is a genuine cross-check. Energy is integer milli-Joules (the host's J->mJ
// scaling) on both sides.

const iabs = (n) => (n < 0 ? -n : n);

const oRecordCompute = (t, u) => t + u;
const oRecordMemoryPeak = (peak, bytes) => (bytes > peak ? bytes : peak);
const oRecordEnergy = (t, m) => t + m;
const oEndCall = (a) => (a <= 0 ? 0 : a - 1);
const oPct = (used, avail) => (avail <= 0 ? 0 : Math.trunc((iabs(used) * 100) / avail));
const oHasCapacity = (a, b) => (a < 95 && b < 95 ? 1 : 0);
const oCheckQuota = (uc, oc, lc, om, lm, ue, oe, le) => {
  const computeOk = uc + oc <= lc;
  const memOk = om <= lm;
  const energyOk = ue + oe <= le;
  return computeOk && memOk && energyOk ? 1 : 0;
};

export default {
  affine: "ResourceAccounting.affine",
  cases: [
    {
      name: "record_compute over total in {0,500,9999}, units in [0..40]",
      export: "record_compute",
      args: [{ values: [0, 500, 9999] }, [0, 40]],
      oracle: oRecordCompute,
    },
    {
      name: "record_memory_peak over peak in {0,1024,65536}, bytes in {0,512,1024,2048,131072}",
      export: "record_memory_peak",
      args: [{ values: [0, 1024, 65536] }, { values: [0, 512, 1024, 2048, 131072] }],
      oracle: oRecordMemoryPeak,
    },
    {
      name: "record_energy over total in {0,1000,50000}, op in [0..30]",
      export: "record_energy",
      args: [{ values: [0, 1000, 50000] }, [0, 30]],
      oracle: oRecordEnergy,
    },
    {
      name: "end_call over [-2..2000]",
      export: "end_call",
      args: [[-2, 2000]],
      oracle: oEndCall,
    },
    {
      name: "pct over used in {0,47,95,100,9500,10001}, avail in {0,-5,100,10000}",
      export: "pct",
      args: [{ values: [0, 47, 95, 100, 9500, 10001] }, { values: [0, -5, 100, 10000] }],
      oracle: oPct,
    },
    {
      name: "has_capacity over a,b in {0,50,94,95,96,100}^2",
      export: "has_capacity",
      args: [{ values: [0, 50, 94, 95, 96, 100] }, { values: [0, 50, 94, 95, 96, 100] }],
      oracle: oHasCapacity,
    },
    {
      name: "check_quota over a representative fit/overflow grid",
      export: "check_quota",
      args: [
        { values: [0, 5000, 9999] }, // used_compute
        { values: [1, 100, 5000] }, // op_compute
        { values: [10000] }, // limit_compute
        { values: [512, 1048576, 2000000] }, // op_memory
        { values: [1048576] }, // limit_memory
        { values: [0, 50000, 99999] }, // used_energy_mj
        { values: [1, 1000, 60000] }, // op_energy_mj
        { values: [100000] }, // limit_energy_mj
      ],
      oracle: oCheckQuota,
    },
  ],
};
