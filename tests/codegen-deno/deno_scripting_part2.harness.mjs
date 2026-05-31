// SPDX-License-Identifier: MPL-2.0
// campaign #239 STEP 3 part 2 — Node-ESM harness for the second wave
// of Deno-scripting externs (stat predicates, byte accessors, module URL)
// plus the `let _ = X` wildcard-binding codegen fix.

import assert from "node:assert/strict";

// ── In-memory FS stub for statSync + readFileSync ───────────────────
const files = {
  "/etc/hosts":         { kind: "file",   bytes: new Uint8Array([37, 80, 68, 70, 45]) }, // "%PDF-"
  "/var/run":           { kind: "dir" },
  "/empty.bin":         { kind: "file",   bytes: new Uint8Array() },
};

globalThis.Deno = globalThis.Deno || {};
globalThis.Deno.statSync = (path) => {
  const e = files[path];
  if (!e) {
    const err = new Error(`stub: no such path ${path}`);
    err.code = "ENOENT";
    throw err;
  }
  return {
    size: e.kind === "file" ? e.bytes.length : 0,
    isFile: e.kind === "file",
    isDirectory: e.kind === "dir",
  };
};
globalThis.Deno.readFileSync = (path) => {
  const e = files[path];
  if (!e || e.kind !== "file") throw new Error(`stub: not a file ${path}`);
  return e.bytes;
};

const {
  classify_path,
  first_byte,
  header_string,
  module_url_has_scheme,
  discard_chain,
} = await import("./deno_scripting_part2.deno.js");

// statIsFile / statIsDirectory
assert.equal(classify_path("/etc/hosts"), 1, "statIsFile true for a regular file");
assert.equal(classify_path("/var/run"),    2, "statIsDirectory true for a directory");

// bytes accessors
assert.equal(first_byte("/etc/hosts"), 37, "bytesByteAt(0) returns first byte (%)");
assert.equal(first_byte("/empty.bin"), 0,  "bytesByteAt fallback on empty stays 0");

// bytesAsciiSlice on the PDF magic
assert.equal(header_string("/etc/hosts", 5), "%PDF-", "bytesAsciiSlice decodes Latin-1");
assert.equal(header_string("/etc/hosts", 99), "%PDF-", "bytesAsciiSlice clamps to length");
assert.equal(header_string("/empty.bin", 5), "", "bytesAsciiSlice empty when no bytes");

// importMetaUrl — at module top level this is the URL of this very harness's
// imported module; should be a non-empty file:/// or http(s):// string.
assert.equal(module_url_has_scheme(), true, "importMetaUrl returns a non-trivial URL");

// let _ = X chain — three back-to-back discards must not redeclare `_`.
// The harness reaching this assertion at all is the test (a syntax error
// would have thrown during the dynamic import above).
assert.equal(discard_chain(), 42, "let _ = X chain executes without const-redeclaration");

console.log("deno_scripting_part2.harness.mjs OK");
