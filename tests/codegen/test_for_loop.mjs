// SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// Regression for #255: `for-in` loop bodies must execute. Before the
// fix, StmtFor read length from arr-4 and elem i from arr+i*4 (the
// canonical layout is length@arr+0, elem i@arr+4+i*4 — ExprArray /
// ExprIndex), so the loop ran zero times and main() returned 0.
// test_for_loop.affine existed but had NO harness, so nothing caught it.

import { readFile } from 'fs/promises';

const wasmBuffer = await readFile('./tests/codegen/test_for_loop.wasm');
const imports = { wasi_snapshot_preview1: { fd_write: () => 0 } };
const { instance } = await WebAssembly.instantiate(wasmBuffer, imports);
const result = instance.exports.main();

console.log(`Result: ${result}`);
console.log('Expected: 15');
console.log(`Test ${result === 15 ? 'PASSED ✓' : 'FAILED ✗'}`);
process.exit(result === 15 ? 0 : 1);
