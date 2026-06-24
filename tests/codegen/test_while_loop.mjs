// SPDX-License-Identifier: MPL-2.0 OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// #255 regression: while + for-count len + index.

import { readFile } from 'fs/promises';

const wasmBuffer = await readFile('./tests/codegen/while_loop.wasm');
const { instance } = await WebAssembly.instantiate(wasmBuffer, {
  wasi_snapshot_preview1: { fd_write: () => 0 },
});
const result = instance.exports.main();

console.log(`Result: ${result}`);
console.log('Expected: 34');
console.log(`Test ${result === 34 ? 'PASSED ✓' : 'FAILED ✗'}`);
process.exit(result === 34 ? 0 : 1);
