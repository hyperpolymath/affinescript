// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

import { readFile } from 'fs/promises';

const wasmBuffer = await readFile('./tests/codegen/test_tuple_record_array.wasm');
const imports = {
  wasi_snapshot_preview1: {
    fd_write: () => 0,
  },
};
const wasmModule = await WebAssembly.instantiate(wasmBuffer, imports);
const result = wasmModule.instance.exports.main();

console.log(`Result: ${result}`);
console.log('Expected: 81');
console.log(`Test ${result === 81 ? 'PASSED ✓' : 'FAILED ✗'}`);

process.exit(result === 81 ? 0 : 1);
