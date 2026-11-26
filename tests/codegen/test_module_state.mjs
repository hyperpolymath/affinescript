// SPDX-License-Identifier: MPL-2.0
//
// #561 — module-level mutable state and qualified host calls on core WASM.
// counter is bumped 3 times (300), acc goes 10 -> 15 -> 13, and the
// host clock returns 1000: main() = 300 + 13 + 1000 = 1313.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

const buf = await readFile('./tests/codegen/module_state.wasm');
const mod = await WebAssembly.compile(buf);

const wanted = WebAssembly.Module.imports(mod)
  .filter((i) => i.module !== 'wasi_snapshot_preview1')
  .map((i) => `${i.module}.${i.name}`)
  .sort();
assert.deepEqual(wanted, ['Console.log', 'Dom.domDateNow'],
  `only the host functions the program calls are imported, got ${wanted}`);

let logs = 0;
const imports = {
  wasi_snapshot_preview1: { fd_write: () => 0 },
  Console: { log: () => { logs += 1; } },
  Dom: { domDateNow: () => 1000 },
};
const inst = await WebAssembly.instantiate(mod, imports);
const result = inst.exports.main();

assert.equal(result, 1313, `module_state: wasm main()=${result}, want 1313`);
assert.equal(logs, 1, `Console.log called ${logs} times, want 1`);
console.log('test_module_state.mjs OK');
