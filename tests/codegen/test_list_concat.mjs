// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// Regression: `++` list concatenation must actually concatenate.
// Pre-fix this returned 0 (OpConcat was a placeholder I32Add).

import { readFile } from 'fs/promises';

const wasmBuffer = await readFile('./tests/codegen/list_concat.wasm');
const { instance } = await WebAssembly.instantiate(wasmBuffer, {
  wasi_snapshot_preview1: { fd_write: () => 0 },
});
const result = instance.exports.main();

console.log(`Result: ${result}`);
console.log('Expected: 166');
console.log(`Test ${result === 166 ? 'PASSED ✓' : 'FAILED ✗'}`);
process.exit(result === 166 ? 0 : 1);
