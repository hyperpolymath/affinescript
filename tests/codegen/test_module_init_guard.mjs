// SPDX-License-Identifier: MPL-2.0
//
// #561 — deferred initialisers run once, in order, from the first call
// into the module; an initialiser that calls an exported function does
// not re-enter the runner; and a failed initialisation leaves the module
// failed rather than running an export with a global still zeroed.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const mod = await WebAssembly.compile(
  await readFile('./tests/codegen/module_init_guard.wasm'));

const instantiate = (tick) => WebAssembly.instantiate(mod, {
  wasi_snapshot_preview1: { fd_write: () => 0 },
  env: { tick },
});

// Every initialiser succeeds: c = 8, a = 1, b = 2.
{
  let n = 0;
  const inst = await instantiate(() => ++n);
  assert.equal(n, 0, 'no initialiser runs during instantiation');
  assert.equal(inst.exports.main(), 812, 'main() after a clean init');
  assert.equal(inst.exports.seven(), 7, 'a second export after init');
  assert.equal(n, 2, `tick ran ${n} times, want 2 (once per initialiser)`);
}

// `tick` throws while `b` is initialised.
{
  let n = 0;
  const boom = new Error('host failure');
  const inst = await instantiate(() => {
    n += 1;
    if (n === 2) throw boom;
    return n;
  });
  assert.throws(() => inst.exports.main(), (e) => e === boom,
    'the host error reaches the first caller');
  assert.throws(() => inst.exports.main(), WebAssembly.RuntimeError,
    'a later call traps instead of reading the zeroed global');
  assert.throws(() => inst.exports.seven(), WebAssembly.RuntimeError,
    'every export traps once initialisation has failed');
  assert.equal(n, 2, `tick ran ${n} times: a completed initialiser re-ran`);
}

console.log('test_module_init_guard.mjs OK');
