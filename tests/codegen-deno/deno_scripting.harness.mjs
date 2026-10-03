// SPDX-License-Identifier: MPL-2.0
// issue #122 follow-up — Node-ESM harness for the Deno-era scripting externs,
// now exercised through the Bun-ESM backend.
//
// Stubs only what the surface needs: a tiny in-memory FS (so `walkRecursive`
// traverses it deterministically), `args`, `exit`, and captures
// `console.error`.
//
// IMPORTANT: this corpus is compiled with `--bun-esm`, and the emitted Bun
// prelude resolves `node:fs` lazily via `process.getBuiltinModule()` and reads
// `process.argv` / `process.exit`. Stubbing a `globalThis.Deno` object is
// inert under that backend, so the host is stubbed at the seam the emission
// actually uses.

import assert from "node:assert/strict";

// ── In-memory FS stub for walkRecursive ─────────────────────────────
// Shape: { "/root": ["a.txt", "sub/"], "/root/sub": ["b.txt", "deeper/"],
//          "/root/sub/deeper": ["c.txt"], "/empty": [] }
const fs = {
  "/root":   [{ name: "a.txt",  isFile: true,  isDirectory: false },
              { name: "sub",    isFile: false, isDirectory: true  }],
  "/root/sub": [{ name: "b.txt", isFile: true, isDirectory: false },
                { name: "deeper", isFile: false, isDirectory: true }],
  "/root/sub/deeper": [{ name: "c.txt", isFile: true, isDirectory: false }],
  "/empty": [],
};

const fsStub = {
  readdirSync: (path) => {
    const entries = fs[path];
    if (!entries) {
      const err = new Error(`stub: no such dir ${path}`);
      err.code = "ENOENT";
      throw err;
    }
    return entries.map((e) => ({
      name: e.name,
      isFile: () => e.isFile,
      isDirectory: () => e.isDirectory,
    }));
  },
  mkdirSync: () => {},
  writeFileSync: () => {},
  rmSync: () => {},
  readFileSync: () => new Uint8Array(),
  statSync: (path) => {
    const err = new Error(`stub: no such path ${path}`);
    err.code = "ENOENT";
    throw err;
  },
};

const realGetBuiltinModule = process.getBuiltinModule?.bind(process);
process.getBuiltinModule = (name) => {
  if (name === "node:fs") return fsStub;
  if (!realGetBuiltinModule) {
    throw new Error(`host builtin unavailable: ${name}`);
  }
  return realGetBuiltinModule(name);
};

// args / exit stubs — exit captures the code instead of terminating.
// `args` lowers to process.argv.slice(2), so the leading entries are dropped.
const realArgv = process.argv;
process.argv = ["node", "deno_scripting.harness.mjs", "alpha", "beta", "gamma"];
let lastExit = null;
process.exit = (code) => { lastExit = code; };

// Capture stderr writes from consoleError.
const stderrLog = [];
const origError = console.error;
console.error = (...a) => { stderrLog.push(a.join(" ")); };

const {
  count_walked,
  first_walked,
  arg_count,
  iso_starts_with_year,
  is_pa_code,
  warn_then_zero,
  exit_with,
} = await import("./deno_scripting.bun.js");

// walkRecursive — depth-first across nested dirs.
assert.equal(count_walked("/root"), 3, "walkRecursive finds 3 files (a.txt + b.txt + c.txt)");
assert.equal(first_walked("/root"), "/root/a.txt", "walkRecursive depth-first leading entry");
assert.equal(count_walked("/empty"), 0, "walkRecursive on empty dir returns []");

// Deno.args
assert.equal(arg_count(), 3, "Deno.args lowered as [String]");

// ISO timestamp shape — real new Date(), so just check leading year.
assert.equal(iso_starts_with_year(), true, "dateNowIso starts with 4-digit year");

// Regex
assert.equal(is_pa_code("PA001"), true, "regexMatch matches PA001");
assert.equal(is_pa_code("PA42"),  false, "regexMatch rejects PA42 (needs 3 digits)");
assert.equal(is_pa_code("UnsafeCode"), false, "regexMatch rejects bare category name");

// consoleError captured to stderr
assert.equal(warn_then_zero("test warning"), 0, "consoleError returns 0");
assert.equal(stderrLog.length, 1, "consoleError went to stderr");
assert.equal(stderrLog[0], "test warning", "consoleError payload preserved");

// exit captured (doesn't actually terminate the harness because we stubbed it)
exit_with(2);
assert.equal(lastExit, 2, "Deno.exit lowered correctly");

console.error = origError;
process.argv = realArgv;
console.log("deno_scripting.harness.mjs OK");
