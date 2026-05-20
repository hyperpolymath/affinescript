// SPDX-License-Identifier: MPL-2.0
// INT-01 / #178 — cross-module WASM link+execute acceptance harness.
//
// Proves that two SEPARATELY-compiled AffineScript modules link and run
// across the wasm module boundary:
//   callee.wasm  ← module CrossCallee; pub fn consume(own x: Int) -> Int { x }
//   caller.wasm  ← use CrossCallee::{consume}; pub fn main() -> Int { consume(42) }
//
// Usage: deno run --allow-read=<dir> link.mjs <callee.wasm> <caller.wasm>
// Exits 0 and prints PASS iff caller.main() === 42 via the cross-module call.

const [calleePath, callerPath] = Deno.args;
if (!calleePath || !callerPath) {
  console.error("usage: link.mjs <callee.wasm> <caller.wasm>");
  Deno.exit(64);
}

// Minimal WASI shim — the AffineScript wasm imports fd_write for println-style
// codegen; cross-module linking is what is under test, not I/O.
const wasiStub = new Proxy({}, { get: () => () => 0 });

const callee = await WebAssembly.instantiate(
  await Deno.readFile(calleePath),
  { wasi_snapshot_preview1: wasiStub },
);
const consume = callee.instance.exports.consume;
if (typeof consume !== "function") {
  console.error("FAIL: callee does not export a callable `consume`");
  Deno.exit(2);
}

let caller;
try {
  caller = await WebAssembly.instantiate(
    await Deno.readFile(callerPath),
    { wasi_snapshot_preview1: wasiStub, CrossCallee: { consume } },
  );
} catch (e) {
  console.error("FAIL: caller did not link against CrossCallee.consume:", e.message);
  Deno.exit(3);
}

const r = caller.instance.exports.main();
if (r === 42) {
  console.log("PASS: cross-module call CrossCallee.consume(42) === 42");
  Deno.exit(0);
}
console.error(`FAIL: expected 42, got ${r}`);
Deno.exit(5);
