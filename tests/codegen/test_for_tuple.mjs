// SPDX-License-Identifier: MPL-2.0 OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// #255 regression: for over a tuple list + tuple-pattern destructure.

import { readFile } from 'fs/promises';

const wasmBuffer = await readFile('./tests/codegen/for_tuple.wasm');
const { instance } = await WebAssembly.instantiate(wasmBuffer, {
  wasi_snapshot_preview1: { fd_write: () => 0 },
});
const result = instance.exports.main();

console.log(`Result: ${result}`);
console.log('Expected: 21');
console.log(`Test ${result === 21 ? 'PASSED ✓' : 'FAILED ✗'}`);
process.exit(result === 21 ? 0 : 1);
