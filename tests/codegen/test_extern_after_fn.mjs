// SPDX-License-Identifier: MPL-2.0
//
// #814 review: an `extern fn` declared after a function used to reuse that
// function's index, so calls to it (including from a deferred const
// initialiser) reached the host import instead. tick() returns 99 here;
// 7 (const via seed) + 30 (other) + 99 (tick) = 136.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const mod = await WebAssembly.compile(
  await readFile('./tests/codegen/extern_after_fn.wasm'));
const imports = {};
for (const i of WebAssembly.Module.imports(mod)) {
  if (i.kind !== 'function') continue;
  imports[i.module] ??= {};
  imports[i.module][i.name] = () => 99;
}
const inst = await WebAssembly.instantiate(mod, imports);
const got = inst.exports.main();
assert.equal(got, 136, `extern_after_fn: main()=${got}, want 136`);
console.log('test_extern_after_fn.mjs OK');
