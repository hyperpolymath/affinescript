// SPDX-License-Identifier: MPL-2.0
//
// #817: record literals in a non-declared field order read the right field.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const mod = await WebAssembly.compile(
  await readFile('./tests/codegen/record_field_order.wasm'));
const imports = {};
for (const i of WebAssembly.Module.imports(mod)) {
  if (i.kind !== 'function') continue;
  imports[i.module] ??= {};
  imports[i.module][i.name] = () => 0;
}
const { exports: e } = await WebAssembly.instantiate(mod, imports);
assert.equal(e.reassign(), 7, 'local reassigned in another order');
assert.equal(e.typed_let(), 9, 'typed let passed to a typed param');
assert.equal(e.direct_arg(), 9, 'literal passed directly as an argument');
assert.equal(e.returned(), 902, 'record-returning fn (tail literal)');
assert.equal(e.early_return(), 501, 'record-returning fn (return literal)');
assert.equal(e.same_order(), 902, 'same-order literal still works');
console.log('test_record_field_order.mjs OK');
