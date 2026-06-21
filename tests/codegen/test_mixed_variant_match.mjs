// SPDX-License-Identifier: MPL-2.0
// #607: zero-arg variant (Non) vs args-variant (Som) must match correctly.
// Before the variant-representation fix, `unwrap_or(Non, 99)` returned garbage
// (the raw tag was dereferenced as a pointer), so main() was not 9907.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/mixed_variant_match.wasm');
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };
const inst = (await WebAssembly.instantiate(buf, imports)).instance;

const r = inst.exports.main();
assert.equal(
  r, 9907,
  `unwrap_or(Non,99)*100 + unwrap_or(Som 7,5) should be 9907 (Non->99, Som->7), got ${r}`,
);
console.log('test_mixed_variant_match.mjs OK');
