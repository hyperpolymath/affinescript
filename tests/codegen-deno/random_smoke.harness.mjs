// SPDX-License-Identifier: MPL-2.0
// campaign #239 STEP 4-B — Node-ESM harness for random + perf.now bindings.

import assert from "node:assert/strict";

const {
  draw_u32,
  draw_unit,
  draw_in_range,
  perf_tick,
} = await import("./random_smoke.deno.js");

// math_random — every draw is in [0, 1).
for (let i = 0; i < 1000; i++) {
  const x = draw_unit();
  assert.ok(x >= 0 && x < 1, `math_random must be in [0, 1), got ${x}`);
}

// random_u32 — uniform sanity over 10000 draws: at least 1000 distinct
// values + all draws fit in u32. (A degenerate PRNG that always returned
// the same value would fail the distinct-count check; a buggy one that
// over-spilled u32 would fail the range check.)
const u32_draws = new Set();
for (let i = 0; i < 10000; i++) {
  const v = draw_u32();
  assert.ok(v >= 0 && v <= 0xFFFFFFFF, `random_u32 must fit in u32, got ${v}`);
  u32_draws.add(v);
}
assert.ok(u32_draws.size >= 1000, `random_u32 distribution: only ${u32_draws.size} distinct values in 10000`);

// random_in_range — must stay in [lo, hi) for 1000 draws.
for (let i = 0; i < 1000; i++) {
  const v = draw_in_range(0, 100);
  assert.ok(v >= 0 && v < 100, `random_in_range(0, 100) must be in [0, 100), got ${v}`);
}

// random_in_range — non-zero lo / non-100 hi.
const window_draws = new Set();
for (let i = 0; i < 500; i++) {
  const v = draw_in_range(50, 60);
  assert.ok(v >= 50 && v < 60, `random_in_range(50, 60) must be in [50, 60), got ${v}`);
  window_draws.add(v);
}
assert.ok(window_draws.size >= 5, `random_in_range(50, 60) should cover most of [50, 60); got ${window_draws.size} distinct values`);

// performance_now — monotone non-decreasing across consecutive calls.
const t0 = perf_tick();
const t1 = perf_tick();
const t2 = perf_tick();
assert.ok(t1 >= t0, `performance_now monotone: t1 (${t1}) >= t0 (${t0})`);
assert.ok(t2 >= t1, `performance_now monotone: t2 (${t2}) >= t1 (${t1})`);
assert.ok(typeof t0 === "number" && !isNaN(t0), "performance_now returns a finite number");

console.log("random_smoke.harness.mjs OK");
