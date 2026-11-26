// SPDX-License-Identifier: MPL-2.0
//
// #643: float_to_string on core WASM must print exactly what Bun's
// String(x) prints (shortest round-trip digits, ECMAScript layout).
// Checks the boundaries (powers of 2 and 10 and their neighbours,
// subnormals, extremes, ties) and a seeded batch of random doubles.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/float_to_string.wasm');
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };
const { exports: ex } = (await WebAssembly.instantiate(buf, imports)).instance;

// The bump heap never frees, and each call keeps its result string.
ex.memory.grow(2000);
const dec = new TextDecoder();
const show = (x) => {
  const p = ex.show(x);
  const len = new DataView(ex.memory.buffer).getInt32(p, true);
  return dec.decode(new Uint8Array(ex.memory.buffer, p + 4, len));
};

const dv = new DataView(new ArrayBuffer(8));
const step = (x, d) => {
  dv.setFloat64(0, x);
  dv.setBigUint64(0, dv.getBigUint64(0) + BigInt(d));
  return dv.getFloat64(0);
};

const cases = [0, -0, NaN, Infinity, -Infinity, 0.1, 0.2, 0.1 + 0.2, 1 / 3,
  100, 1e21, 1e20, 1e-6, 1e-7, 1.5e-7, 5e-324, -5e-324, 2.2250738585072014e-308,
  1.7976931348623157e308, 2 ** 53, 2 ** 53 + 2, 267522186230338.62, 4.35, 12345.678];
for (let i = -1074; i <= 1023; i += 7) cases.push(2 ** i);
for (let i = -323; i <= 308; i += 3) cases.push(Number('1e' + i));
const seeds = cases.filter(Number.isFinite).filter((x) => x !== 0);
for (const s of seeds) for (const d of [-1, 1]) cases.push(step(s, d), -step(s, d));

// xorshift32, fixed seed: random bit patterns, so every exponent is hit
let st = 0x9e3779b9;
const rnd = () => { st ^= st << 13; st ^= st >>> 17; st ^= st << 5; return st >>> 0; };
for (let i = 0; i < 20000; i++) {
  dv.setUint32(0, rnd()); dv.setUint32(4, rnd());
  cases.push(dv.getFloat64(0));
}

let bad = 0;
for (const x of cases) {
  const got = show(x), want = String(x);
  if (got !== want && bad++ < 10) console.error(`float_to_string(${want}) = ${got}`);
}
assert.equal(bad, 0, `${bad} of ${cases.length} doubles differ from String(x)`);
assert.equal(ex.main(), 3);
console.log(`test_float_to_string.mjs OK (${cases.length} doubles)`);
