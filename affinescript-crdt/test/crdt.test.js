// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// Property tests for Crdt.affine (compiled with the Bun-ESM backend):
// convergence of replicas under random operations and gossip, the merge
// laws (commutative, associative, idempotent), and each type's semantics.
// Deterministic seeds; states compare as JSON (entries are kept sorted).

import { beforeAll, expect, test } from "bun:test";
import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const repo = new URL("../../", import.meta.url).pathname;
let C;

beforeAll(async () => {
  const out = join(mkdtempSync(join(tmpdir(), "crdt-")), "crdt.bun.js");
  const r = Bun.spawnSync({
    cmd: [join(repo, "_build/default/bin/main.exe"), "compile", "--bun-esm", "Crdt.affine", "-o", out],
    cwd: join(repo, "affinescript-crdt/src"),
    env: { ...process.env, AFFINESCRIPT_STDLIB: join(repo, "stdlib") },
  });
  if (r.exitCode !== 0) throw new Error(`compile failed:\n${r.stdout}${r.stderr}`);
  C = await import(out);
});

/** Deterministic PRNG (mulberry32). */
function rng(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = Math.imul(a ^ (a >>> 15), a | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}
const pick = (rand, xs) => xs[Math.floor(rand() * xs.length)];
const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);
const unwrap = (o) => (o.tag === "Some" ? o.value : null);

/** Three replicas applying random ops with random partial gossip. */
function simulate(seed, ops, init, apply, merge) {
  const rand = rng(seed);
  const reps = ["r1", "r2", "r3"].map((id) => ({ id, clock: C.clock(id), state: init() }));
  const log = [];
  for (let i = 0; i < ops; i++) {
    const r = pick(rand, reps);
    const [clock, stamp] = C.tick(r.clock);
    r.clock = clock;
    r.state = apply(r, stamp, rand, log);
    if (rand() < 0.2) {
      const other = pick(rand, reps);
      if (other !== r) {
        other.state = merge(other.state, r.state);
        other.clock = C.observe(other.clock, stamp);
      }
    }
  }
  return { reps, log };
}

/** Merge three states in every order; all results must be equal. */
function allOrders(merge, [a, b, c]) {
  const orders = [
    [a, b, c], [a, c, b], [b, a, c], [b, c, a], [c, a, b], [c, b, a],
  ].map(([x, y, z]) => merge(merge(x, y), z));
  orders.push(merge(merge(merge(a, b), merge(b, c)), merge(c, a)));
  return orders;
}

test("Lamport stamps are totally ordered and observe() moves the clock forward", () => {
  const s = (counter, replica) => ({ counter, replica });
  expect(C.after(s(2, "a"), s(1, "z"))).toBe(true);
  expect(C.after(s(1, "b"), s(1, "a"))).toBe(true);
  expect(C.after(s(1, "a"), s(1, "a"))).toBe(false);
  const [, stamp] = C.tick(C.observe(C.clock("x"), s(41, "y")));
  expect(stamp).toEqual({ counter: 42, replica: "x" });
});

test("LWW map: replicas converge and each key holds its latest write", () => {
  for (const seed of [1, 2, 3, 4]) {
    const keys = ["a", "b", "c", "d", "e", "f"];
    const { reps, log } = simulate(
      seed,
      300,
      () => C.map_empty(),
      (r, stamp, rand, log) => {
        const key = pick(rand, keys);
        if (rand() < 0.25) {
          log.push({ key, value: null, stamp });
          return C.map_remove(r.state, key, stamp);
        }
        const value = Math.floor(rand() * 1000);
        log.push({ key, value, stamp });
        return C.map_set(r.state, key, value, stamp);
      },
      C.map_merge,
    );
    const results = allOrders(C.map_merge, reps.map((r) => r.state));
    for (const m of results) expect(same(m, results[0])).toBe(true);
    const final = results[0];
    for (const key of keys) {
      const writes = log.filter((w) => w.key === key);
      if (writes.length === 0) continue;
      const latest = writes.reduce((best, w) => (C.after(w.stamp, best.stamp) ? w : best));
      expect(unwrap(C.map_get(final, key))).toBe(latest.value);
    }
    expect(C.map_keys(final)).toEqual(keys.filter((k) => unwrap(C.map_get(final, k)) !== null));
  }
});

test("LWW register merge keeps the later write", () => {
  const r1 = C.lww("old", { counter: 1, replica: "a" });
  const r2 = C.lww("new", { counter: 2, replica: "a" });
  expect(C.lww_value(C.lww_merge(r1, r2))).toBe("new");
  expect(C.lww_value(C.lww_merge(r2, r1))).toBe("new");
  expect(C.lww_value(C.lww_set(r2, "stale", { counter: 1, replica: "z" }))).toBe("new");
});

test("OR-set: replicas converge; add wins over a concurrent remove", () => {
  for (const seed of [5, 6, 7]) {
    const elems = ["x", "y", "z", "w"];
    const { reps } = simulate(
      seed,
      300,
      () => C.or_empty(),
      (r, stamp, rand) => (rand() < 0.35 ? C.or_remove(r.state, pick(rand, elems)) : C.or_add(r.state, pick(rand, elems), stamp)),
      C.or_merge,
    );
    const results = allOrders(C.or_merge, reps.map((r) => r.state));
    for (const s of results) expect(same(s, results[0])).toBe(true);
  }
  // Concurrent add (unobserved by the remover) survives the merge.
  let a = C.or_add(C.or_empty(), "k", { counter: 1, replica: "a" });
  let b = C.or_merge(C.or_empty(), a);
  b = C.or_remove(b, "k");
  a = C.or_add(a, "k", { counter: 2, replica: "a" });
  expect(C.or_contains(C.or_merge(a, b), "k")).toBe(true);
  // A remove that observed every add deletes the element everywhere.
  const c = C.or_remove(C.or_merge(a, b), "k");
  expect(C.or_contains(C.or_merge(c, a), "k")).toBe(false);
  expect(C.or_elements(C.or_add(C.or_add(C.or_empty(), "b", { counter: 1, replica: "r" }), "a", { counter: 2, replica: "r" }))).toEqual(["a", "b"]);
});

test("PN-counter: replicas converge to the sum of all changes", () => {
  for (const seed of [8, 9]) {
    let total = 0;
    const { reps } = simulate(
      seed,
      300,
      () => C.pn_empty(),
      (r, _stamp, rand) => {
        const n = Math.floor(rand() * 21) - 10;
        total += n;
        return C.pn_add(r.state, r.id, n);
      },
      C.pn_merge,
    );
    const results = allOrders(C.pn_merge, reps.map((r) => r.state));
    for (const s of results) expect(same(s, results[0])).toBe(true);
    expect(C.pn_value(results[0])).toBe(total);
  }
});

test("merge laws: commutative, associative, idempotent", () => {
  const rand = rng(42);
  // Stamps are unique per write (what replica clocks guarantee); counters are
  // shuffled so the generated states disagree about which write is latest.
  let next = 0;
  const stamp = () => ({ counter: 1 + Math.floor(rand() * 50) * 1000 + ++next, replica: pick(rand, ["a", "b", "c"]) });
  const randMap = () => {
    let m = C.map_empty();
    for (let i = 0; i < 20; i++) {
      const k = pick(rand, ["p", "q", "r", "s"]);
      m = rand() < 0.3 ? C.map_remove(m, k, stamp()) : C.map_set(m, k, Math.floor(rand() * 9), stamp());
    }
    return m;
  };
  const randSet = () => {
    let s = C.or_empty();
    for (let i = 0; i < 15; i++) {
      const e = pick(rand, ["e1", "e2", "e3"]);
      s = rand() < 0.3 ? C.or_remove(s, e) : C.or_add(s, e, stamp());
    }
    return s;
  };
  const randPn = () => {
    let c = C.pn_empty();
    for (let i = 0; i < 10; i++) c = C.pn_add(c, pick(rand, ["a", "b", "c"]), Math.floor(rand() * 11) - 5);
    return c;
  };
  for (const [gen, merge] of [[randMap, C.map_merge], [randSet, C.or_merge], [randPn, C.pn_merge]]) {
    for (let i = 0; i < 50; i++) {
      const [x, y, z] = [gen(), gen(), gen()];
      expect(same(merge(x, y), merge(y, x))).toBe(true);
      expect(same(merge(merge(x, y), z), merge(x, merge(y, z)))).toBe(true);
      expect(same(merge(x, x), x)).toBe(true);
    }
  }
});

test("merge cost at 2,000 entries (reported)", () => {
  let a = C.map_empty();
  let b = C.map_empty();
  // Build in key order so construction is linear; measure the merge.
  const keys = Array.from({ length: 2000 }, (_, i) => `k${String(i).padStart(5, "0")}`);
  const ea = [];
  const eb = [];
  keys.forEach((k, i) => {
    ea.push([k, { tag: "Some", value: i }, { counter: i + 1, replica: "a" }]);
    if (i % 2 === 0) eb.push([k, { tag: "Some", value: -i }, { counter: i + 2, replica: "b" }]);
  });
  a = { tag: "Entries", value: ea };
  b = { tag: "Entries", value: eb };
  const t0 = performance.now();
  const m = C.map_merge(a, b);
  const ms = performance.now() - t0;
  console.log(`map_merge 2000 + 1000 entries: ${ms.toFixed(1)} ms`);
  expect(C.map_keys(m).length).toBe(2000);
  expect(unwrap(C.map_get(m, "k00002"))).toBe(-2);
});
