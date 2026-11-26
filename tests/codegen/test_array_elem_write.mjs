// SPDX-License-Identifier: MPL-2.0
//
// #561 review: array element writes skip the length header.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const mod = await WebAssembly.compile(
  await readFile('./tests/codegen/array_elem_write.wasm'));
const imports = {};
for (const i of WebAssembly.Module.imports(mod)) {
  if (i.kind !== 'function') continue;
  imports[i.module] ??= {};
  imports[i.module][i.name] = () => 0;
}
const inst = await WebAssembly.instantiate(mod, imports);
const got = inst.exports.main();
assert.equal(got, 42353, `array_elem_write: main()=${got}, want 42353`);
console.log('test_array_elem_write.mjs OK');
