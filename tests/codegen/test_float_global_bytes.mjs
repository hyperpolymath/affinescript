// SPDX-License-Identifier: MPL-2.0
//
// #561 — Float module globals, checked on the emitted Wasm bytes (the e2e
// tests only inspect the in-memory module):
//   * a same-name `let scale = scale` over a Float const (built by the
//     runner from float_const_shadow.affine);
//   * an imported Float const (test/e2e/fixtures/float_const_caller.affine,
//     compiled here from its own directory so `use FloatRates` resolves).
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';

async function check(bytes, label, want) {
  assert.ok(WebAssembly.validate(bytes), `${label}: emitted wasm fails validation`);
  // Stub every function import (WASI etc.); main() never calls them.
  const mod = new WebAssembly.Module(bytes);
  const imports = {};
  for (const i of WebAssembly.Module.imports(mod)) {
    if (i.kind !== 'function') continue;
    imports[i.module] ??= {};
    imports[i.module][i.name] = () => 0;
  }
  const instance = await WebAssembly.instantiate(mod, imports);
  const got = instance.exports.main();
  assert.equal(got, want, `${label}: main()=${got}, want ${want}`);
}

await check(readFileSync('tests/codegen/float_const_shadow.wasm'),
  'same-name let', 3);

// A bare name (e.g. `affinescript` on PATH) is looked up on PATH; a path is
// made absolute because the compile below runs from the fixtures directory.
const rawCompiler = process.env.AFFINESCRIPT_COMPILER
  ?? './_build/default/bin/main.exe';
const compiler = rawCompiler.includes('/') ? resolve(rawCompiler) : rawCompiler;
const dir = mkdtempSync(join(tmpdir(), 'affine-561-float-'));
try {
  const out = join(dir, 'float_const_caller.wasm');
  execFileSync(compiler, ['compile', 'float_const_caller.affine', '-o', out],
    { cwd: 'test/e2e/fixtures' });
  await check(readFileSync(out), 'imported Float const', 3);
} finally {
  rmSync(dir, { recursive: true, force: true });
}
console.log('test_float_global_bytes.mjs OK');
