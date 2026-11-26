// SPDX-License-Identifier: MPL-2.0
//
// #561 — Node-CJS host: an initialiser that prints runs after the host has
// bound memory, and its value is visible to the export (activate() = 42).
// The .cjs is built here because the runner only produces .wasm.
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, rmSync } from 'node:fs';
import { createRequire } from 'node:module';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const compiler = process.env.AFFINESCRIPT_COMPILER
  ?? './_build/default/bin/main.exe';
const dir = mkdtempSync(join(tmpdir(), 'affine-561-'));
try {
  const out = join(dir, 'module_init_print.cjs');
  execFileSync(compiler,
    ['compile', 'tests/codegen/module_init_print.affine', '-o', out]);

  const written = [];
  const origWrite = process.stdout.write.bind(process.stdout);
  process.stdout.write = (chunk, ...rest) => {
    written.push(String(chunk));
    return origWrite(chunk, ...rest);
  };
  let result;
  try {
    result = await createRequire(import.meta.url)(out).activate({});
  } finally {
    process.stdout.write = origWrite;
  }
  assert.equal(result, 42, `activate()=${result}, want 42`);
  assert.ok(written.join('').includes('seeding'),
    `the initialiser's println did not reach stdout: ${JSON.stringify(written)}`);
} finally {
  rmSync(dir, { recursive: true, force: true });
}
console.log('test_module_init_print.mjs OK');
