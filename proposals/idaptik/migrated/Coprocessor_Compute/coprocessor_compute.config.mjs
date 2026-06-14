// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
//
// affine-parity config for Coprocessor_Compute.affine (idaptik compute
// co-processor scalar kernels; scalar i32 ABI). Every oracle is an INDEPENDENT
// JS reimplementation derived from src/shared/Coprocessor_Compute.res semantics
// (the `| None =>` ReScript fallback arithmetic) -- NOT a copy of the .affine
// logic -- so the differential sweep is a genuine cross-check. The .res frames
// these as array<int> -> executionResult; here only the scalar result crosses.
//
// Note: `is_prime`'s .res used floor(sqrt n) as the trial bound; this oracle
// uses an independent `for d=3; d*d<=n` loop, so a codegen regression that broke
// the .affine `d*d<=n` rephrasing would surface as a mismatch.

// --- independent oracle helpers (re-derived from Coprocessor_Compute.res) ---

const INT_MAX = 1073741823; // Maths.intMax = 2^30 - 1
const iabs = (n) => (n < 0 ? -n : n);

// gcd: euclidGcd on abs values, gcd(0,0)=0 (cmdGcd).
const oGcd = (a, b) => {
  let x = iabs(a);
  let y = iabs(b);
  while (y !== 0) {
    const r = x % y;
    x = y;
    y = r;
  }
  return x;
};

// non-negative modulo (cmdMod | None); host guards divisor==0 -> kernel sees 0.
const oMod = (a, b) => {
  if (b === 0) return 0;
  const raw = a % b;
  return raw < 0 ? raw + iabs(b) : raw;
};

// primality by independent trial division (isPrime, integer bound).
const oPrime = (n) => {
  if (n < 2) return 0;
  if (n === 2) return 1;
  if (n % 2 === 0) return 0;
  for (let d = 3; d * d <= n; d += 2) {
    if (n % d === 0) return 0;
  }
  return 1;
};

// iterative Fibonacci clamped to 0..46 (cmdFib).
const oFib = (n) => {
  let k = n;
  if (k < 0) k = 0;
  if (k > 46) k = 46;
  if (k === 0) return 0;
  if (k === 1) return 1;
  let a = 0;
  let b = 1;
  for (let i = 2; i <= k; i++) {
    const next = a + b;
    a = b;
    b = next;
  }
  return b;
};

// integer pow, clamped to [0, INT_MAX] (cmdPow | None semantics).
// Independent route: accumulate in JS double (exact for these small ranges),
// then clamp -- mirrors `raw = Math.pow(base, exp); if raw > intMax ...`.
const oPow = (base, exp) => {
  let e = exp;
  if (e < 0) e = 0;
  const raw = Math.pow(base, e);
  if (raw > INT_MAX) return INT_MAX;
  if (raw < 0) return 0;
  return Math.floor(raw);
};

// Physics integer kernels (| None branches).
const oSignal = (power, distance, atten) => {
  const loss = Math.trunc((iabs(distance) * iabs(atten)) / 100);
  return power > loss ? power - loss : 0;
};
const oThermal = (watts, duration, heatCap) => {
  if (heatCap === 0) return 0;
  return Math.trunc((iabs(watts) * iabs(duration)) / heatCap);
};
const oCablesag = (length, weight, tension) => {
  if (tension === 0) return 0;
  const l = iabs(length);
  return Math.trunc((iabs(weight) * l * l) / (8 * tension));
};
const oPowerWatts = (v, i) => v * i;
const oPowerSafe = (w) => (w < 1500 ? 1 : 0);

export default {
  affine: "Coprocessor_Compute.affine",
  cases: [
    // --- Maths kernels ---
    {
      name: "gcd over a,b in [0..30]^2",
      export: "gcd",
      args: [[0, 30], [0, 30]],
      oracle: oGcd,
    },
    {
      name: "gcd over negatives a,b in {-30,-12,-7,0,7,12,30}^2",
      export: "gcd",
      args: [{ values: [-30, -12, -7, 0, 7, 12, 30] }, { values: [-30, -12, -7, 0, 7, 12, 30] }],
      oracle: oGcd,
    },
    {
      name: "mod_nonneg over a in {-17,-5,-1,0,7,13,40}, b in {-5,-3,1,4,7}",
      export: "mod_nonneg",
      args: [{ values: [-17, -5, -1, 0, 7, 13, 40] }, { values: [-5, -3, 1, 4, 7] }],
      oracle: oMod,
    },
    {
      name: "is_prime over [-2..120]",
      export: "is_prime",
      args: [[-2, 120]],
      oracle: oPrime,
    },
    {
      name: "fib over [-3..50]",
      export: "fib",
      args: [[-3, 50]],
      oracle: oFib,
    },
    {
      name: "pow_clamped over base in [-4..12], exp in [-2..10]",
      export: "pow_clamped",
      args: [[-4, 12], [-2, 10]],
      oracle: oPow,
    },
    {
      name: "pow_clamped overflow band: base in {2,3,10,100,1000}, exp in {0..12}",
      export: "pow_clamped",
      args: [{ values: [2, 3, 10, 100, 1000] }, [0, 12]],
      oracle: oPow,
    },
    // --- Physics kernels ---
    {
      name: "signal over power,distance,atten samples",
      export: "signal",
      args: [{ values: [0, 50, 100, 500, 1000] }, { values: [-20, 0, 10, 50, 200] }, { values: [-5, 0, 1, 10, 50] }],
      oracle: oSignal,
    },
    {
      name: "thermal over watts,duration,heatCap samples",
      export: "thermal",
      args: [{ values: [-10, 0, 25, 100, 500] }, { values: [-3, 0, 5, 30, 120] }, { values: [1, 4, 10, 60] }],
      oracle: oThermal,
    },
    {
      name: "cablesag over length,weight,tension samples",
      export: "cablesag",
      args: [{ values: [-8, 0, 10, 40, 100] }, { values: [0, 2, 7, 20] }, { values: [1, 8, 50, 200] }],
      oracle: oCablesag,
    },
    {
      name: "power_watts over voltage,current in {0,5,12,110,240} x {0,1,4,10,20}",
      export: "power_watts",
      args: [{ values: [0, 5, 12, 110, 240] }, { values: [0, 1, 4, 10, 20] }],
      oracle: oPowerWatts,
    },
    {
      name: "power_safe over watts in [0..3000] step (range)",
      export: "power_safe",
      args: [[0, 3000]],
      oracle: oPowerSafe,
    },
  ],
};
