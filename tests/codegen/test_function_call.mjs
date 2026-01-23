// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2025 hyperpolymath

import { readFile } from 'fs/promises';

const wasmBuffer = await readFile('./tests/codegen/test_function_call.wasm');
const wasmModule = await WebAssembly.instantiate(wasmBuffer);
const result = wasmModule.instance.exports.main();

console.log(`Result: ${result}`);
console.log(`Expected: 42`);
console.log(`Test ${result === 42 ? 'PASSED ✓' : 'FAILED ✗'}`);

process.exit(result === 42 ? 0 : 1);
