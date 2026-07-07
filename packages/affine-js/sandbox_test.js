// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-js/sandbox: capability-restricted import-object tests.
//
// Run:  deno test packages/affine-js/sandbox_test.js
//
// Fixtures are hand-rolled wasm binaries (same approach as loader_test.js's
// wasmWithCustomSection): a type section with one () -> () type, an import
// section declaring exactly the imports under test, and — for the live
// call-through test — a function that calls import 0, exported as "poke".

import { assertEquals, assertThrows } from "jsr:@std/assert@1";
import {
  buildSandboxedImports,
  CapabilityError,
  defineCapabilities,
} from "./sandbox.js";

// ── wasm fixture builders ─────────────────────────────────────────────────────

function uleb(n) {
  const out = [];
  do {
    let b = n & 0x7f;
    n >>>= 7;
    if (n !== 0) b |= 0x80;
    out.push(b);
  } while (n !== 0);
  return out;
}

function section(id, content) {
  return [id, ...uleb(content.length), ...content];
}

function name(s) {
  const b = [...new TextEncoder().encode(s)];
  return [...uleb(b.length), ...b];
}

/** One import entry. kind: "function" | "memory" | "table" | "global". */
function importEntry(mod, nm, kind) {
  const desc = {
    function: [0x00, ...uleb(0)], // type index 0: () -> ()
    table: [0x01, 0x70, 0x00, ...uleb(0)], // funcref, min 0
    memory: [0x02, 0x00, ...uleb(1)], // min 1 page
    global: [0x03, 0x7f, 0x00], // i32 immutable
  }[kind];
  return [...name(mod), ...name(nm), ...desc];
}

/** Module that just declares imports (never needs to instantiate). */
function moduleWithImports(entries) {
  const typeSec = section(1, [0x01, 0x60, 0x00, 0x00]); // 1 type: () -> ()
  const importSec = section(2, [
    ...uleb(entries.length),
    ...entries.flatMap(([m, n, k]) => importEntry(m, n, k)),
  ]);
  return new WebAssembly.Module(new Uint8Array([
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
    ...typeSec, ...importSec,
  ]));
}

/** Module importing fn `mod.nm` and exporting "poke" () that calls it. */
function moduleCallingImport(mod, nm) {
  const typeSec = section(1, [0x01, 0x60, 0x00, 0x00]);
  const importSec = section(2, [0x01, ...importEntry(mod, nm, "function")]);
  const funcSec = section(3, [0x01, 0x00]); // 1 func of type 0
  const exportSec = section(7, [0x01, ...name("poke"), 0x00, ...uleb(1)]);
  const body = [0x00, 0x10, 0x00, 0x0b]; // no locals; call 0; end
  const codeSec = section(10, [0x01, ...uleb(body.length), ...body]);
  return new WebAssembly.Module(new Uint8Array([
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
    ...typeSec, ...importSec, ...funcSec, ...exportSec, ...codeSec,
  ]));
}

// ── shared registry ───────────────────────────────────────────────────────────

const noop = () => {};
const CAPS = defineCapabilities({
  "dom.read": { env: { dom_query_selector: noop, dom_str_eq: noop } },
  "dom.write": { env: { dom_create_element: noop, dom_append_child: noop } },
  "io.stdout": { wasi_snapshot_preview1: { fd_write: noop } },
});

// ── defineCapabilities validation ─────────────────────────────────────────────

Deno.test("defineCapabilities rejects a non-function member", () => {
  assertThrows(
    () => defineCapabilities({ bad: { env: { oops: 42 } } }),
    TypeError,
    "not a function",
  );
});

// ── fail-closed at load ───────────────────────────────────────────────────────

Deno.test("ungranted function import rejects the whole guest at load", () => {
  const mod = moduleWithImports([
    ["env", "dom_query_selector", "function"],
    ["env", "dom_delete_everything", "function"], // never defined anywhere
  ]);
  const err = assertThrows(
    () =>
      buildSandboxedImports(mod, { capabilities: CAPS, grant: ["dom.read"] }),
    CapabilityError,
    "dom_delete_everything",
  );
  assertEquals(err.violations.length, 1);
  assertEquals(err.violations[0].name, "dom_delete_everything");
});

Deno.test("defined-but-ungranted capability still rejects (grant is the gate)", () => {
  const mod = moduleWithImports([["env", "dom_create_element", "function"]]);
  assertThrows(
    () =>
      buildSandboxedImports(mod, { capabilities: CAPS, grant: ["dom.read"] }),
    CapabilityError,
    "dom_create_element",
  );
});

Deno.test("all violations reported at once, not first-only", () => {
  const mod = moduleWithImports([
    ["env", "a", "function"],
    ["env", "b", "function"],
  ]);
  const err = assertThrows(
    () => buildSandboxedImports(mod, { capabilities: CAPS, grant: [] }),
    CapabilityError,
  );
  assertEquals(err.violations.map((v) => v.name), ["a", "b"]);
});

// ── non-function imports ──────────────────────────────────────────────────────

Deno.test("imported memory is refused by default (L13 red flag)", () => {
  const mod = moduleWithImports([["env", "memory", "memory"]]);
  const err = assertThrows(
    () => buildSandboxedImports(mod, { capabilities: CAPS, grant: [] }),
    CapabilityError,
  );
  assertEquals(err.violations[0].kind, "memory");
});

Deno.test("allowMemoryImport waives only the memory refusal", () => {
  const mod = moduleWithImports([["env", "memory", "memory"]]);
  const { requested } = buildSandboxedImports(mod, {
    capabilities: CAPS,
    grant: [],
    allowMemoryImport: true,
  });
  assertEquals(requested.length, 1);
});

Deno.test("imported table/global are refused even with allowMemoryImport", () => {
  const mod = moduleWithImports([
    ["env", "t", "table"],
    ["env", "g", "global"],
  ]);
  const err = assertThrows(
    () =>
      buildSandboxedImports(mod, {
        capabilities: CAPS,
        grant: [],
        allowMemoryImport: true,
      }),
    CapabilityError,
  );
  assertEquals(err.violations.map((v) => v.kind), ["table", "global"]);
});

// ── least privilege + audit ───────────────────────────────────────────────────

Deno.test("import object carries only requested imports; audit names the capability", () => {
  const mod = moduleWithImports([
    ["env", "dom_query_selector", "function"],
    ["wasi_snapshot_preview1", "fd_write", "function"],
  ]);
  const { importObject, audit, granted } = buildSandboxedImports(mod, {
    capabilities: CAPS,
    grant: ["dom.read", "dom.write", "io.stdout"],
  });
  // dom.read also defines dom_str_eq and dom.write is fully unrequested:
  // neither may leak into the instance surface.
  assertEquals(Object.keys(importObject.env), ["dom_query_selector"]);
  assertEquals(Object.keys(importObject.wasi_snapshot_preview1), ["fd_write"]);
  assertEquals(granted, ["dom.read", "dom.write", "io.stdout"]);
  assertEquals(
    audit.map((a) => `${a.module}.${a.name}<-${a.capability}`),
    [
      "env.dom_query_selector<-dom.read",
      "wasi_snapshot_preview1.fd_write<-io.stdout",
    ],
  );
});

// ── registry hygiene ──────────────────────────────────────────────────────────

Deno.test("unknown grant name is a TypeError (authoring error, not guest's fault)", () => {
  const mod = moduleWithImports([]);
  assertThrows(
    () => buildSandboxedImports(mod, { capabilities: CAPS, grant: ["nope"] }),
    TypeError,
    'unknown capability "nope"',
  );
});

Deno.test("two granted capabilities defining the same import differently is loud", () => {
  const mod = moduleWithImports([]);
  const clashing = defineCapabilities({
    a: { env: { f: () => 1 } },
    b: { env: { f: () => 2 } },
  });
  assertThrows(
    () =>
      buildSandboxedImports(mod, { capabilities: clashing, grant: ["a", "b"] }),
    TypeError,
    "different implementations",
  );
});

// ── live call-through ─────────────────────────────────────────────────────────

Deno.test("granted guest instantiates and calls through to the host fn", () => {
  let hits = 0;
  const caps = defineCapabilities({
    score: { env: { game_add_point: () => hits++ } },
  });
  const mod = moduleCallingImport("env", "game_add_point");
  const { importObject } = buildSandboxedImports(mod, {
    capabilities: caps,
    grant: ["score"],
  });
  const inst = new WebAssembly.Instance(mod, importObject);
  inst.exports.poke();
  inst.exports.poke();
  assertEquals(hits, 2);
});

Deno.test("the same guest without the grant never instantiates", () => {
  const mod = moduleCallingImport("env", "game_add_point");
  assertThrows(
    () => buildSandboxedImports(mod, { capabilities: CAPS, grant: [] }),
    CapabilityError,
    "game_add_point",
  );
});
