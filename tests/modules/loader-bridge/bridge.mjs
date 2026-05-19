// SPDX-License-Identifier: PMPL-1.0-or-later
// INT-02 / #179 — host-agnostic loader bridge, end-to-end acceptance.
//
// Proves the *actual* INT-02 loader API (packages/affine-js/loader.js)
// drives genuine compiler-emitted, cross-module wasm — not synthetic
// bytes (loader_test.js does the unit level) and not a hand-rolled
// `Deno.readFile` + manual import object (that is the SAT-02 anti-pattern
// the loader exists to replace; the INT-01 xmod-link harness still does it
// by hand — this closes INT-01 ↔ INT-02).
//
//   callee.wasm ← module CrossCallee; pub fn consume(own x: Int) -> Int { x }
//   caller.wasm ← use CrossCallee::{consume}; pub fn main() -> Int { consume(42) }
//
// Usage: deno run --allow-read=<dir> bridge.mjs <callee.wasm> <caller.wasm>
// Exit 0 + PASS iff: readBytes loads both; buildImportObject wires the
// cross-module import; caller.main() === 42; parseOwnershipSection returns
// real entries from the compiler-emitted module.

import {
  buildImportObject,
  parseOwnershipSection,
  readBytes,
} from "../../../packages/affine-js/loader.js";

const [calleePath, callerPath] = Deno.args;
if (!calleePath || !callerPath) {
  console.error("usage: bridge.mjs <callee.wasm> <caller.wasm>");
  Deno.exit(64);
}

// WASI is host-supplied, a catch-all namespace (println-style codegen
// imports fd_write). `buildImportObject` *spreads* module members (to merge
// rather than clobber), so a catch-all Proxy must be attached as a whole
// namespace, not via the spread — mirroring real usage where the host owns
// wasi while the loader owns the affine runtime + cross-module namespaces.
const wasiStub = new Proxy({}, { get: () => () => 0 });
const withWasi = (io) => {
  io.wasi_snapshot_preview1 = wasiStub;
  return io;
};

// 1. readBytes — the host-agnostic reader (the SAT-02 fix).
const calleeBytes = await readBytes(calleePath);
const callerBytes = await readBytes(callerPath);

// 2. buildImportObject — the loader builds the affine import object; the
//    caller's is multi-namespace (NOT env-only): it carries the genuine
//    `CrossCallee` cross-module namespace INT-01/#178 emits.
const callee = await WebAssembly.instantiate(
  calleeBytes,
  withWasi(buildImportObject({})),
);
const consume = callee.instance.exports.consume;
if (typeof consume !== "function") {
  console.error("FAIL: callee does not export a callable `consume`");
  Deno.exit(2);
}

let caller;
try {
  caller = await WebAssembly.instantiate(
    callerBytes,
    withWasi(buildImportObject({}, { modules: { CrossCallee: { consume } } })),
  );
} catch (e) {
  console.error("FAIL: caller did not link via the loader import object:", e.message);
  Deno.exit(3);
}

const got = caller.instance.exports.main();
if (got !== 42) {
  console.error(`FAIL: cross-module call returned ${got}, expected 42`);
  Deno.exit(4);
}

// 3. parseOwnershipSection — the typed-wasm contract carrier, on REAL
//    compiler output (loader_test.js only round-trips a hand-built buffer).
const calleeMod = await WebAssembly.compile(calleeBytes);
const ownership = parseOwnershipSection(calleeMod);
if (!Array.isArray(ownership) || ownership.length === 0) {
  console.error(
    "FAIL: parseOwnershipSection returned no entries for compiler-emitted " +
      "wasm (expected at least CrossCallee.consume's linear param)",
  );
  Deno.exit(5);
}
const consumeOwn = ownership.find((e) => e.paramKinds.includes("linear"));
if (!consumeOwn) {
  console.error(
    "FAIL: no linear param found; `consume(own x: Int)` should be Linear",
  );
  Deno.exit(6);
}

console.log(
  `PASS: loader bridge — readBytes + buildImportObject linked ` +
    `CrossCallee.consume(42) === ${got}; parseOwnershipSection read ` +
    `${ownership.length} entr${ownership.length === 1 ? "y" : "ies"} ` +
    `(consume has a Linear param) from real compiler output`,
);
