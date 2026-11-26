// SPDX-License-Identifier: MPL-2.0
//
// #561 review: a const initialiser's local does not leak its record layout.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const mod = await WebAssembly.compile(
  await readFile('./tests/codegen/const_layout_shadow.wasm'));
const imports = {};
for (const i of WebAssembly.Module.imports(mod)) {
  if (i.kind !== 'function') continue;
  imports[i.module] ??= {};
  imports[i.module][i.name] = () => 0;
}
const inst = await WebAssembly.instantiate(mod, imports);
const got = inst.exports.main();
assert.equal(got, 27, `const_layout_shadow: main()=${got}, want 27`);
console.log('test_const_layout_shadow.mjs OK');
