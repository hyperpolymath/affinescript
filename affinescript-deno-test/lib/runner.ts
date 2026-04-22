// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-deno-test: runner.ts
//
// Loads a compiled AffineScript WASM module and wraps every exported function
// whose name starts with `test_` as a Deno.test() case. A test passes when
// the function returns `true`, fails when it returns `false`.
//
// Convention (v0.2.0): each `.affine` file may define multiple tests via
// the `pub fn test_<name>() -> Bool` syntax. Every `pub fn test_*` export
// becomes a separate Deno.test() case. Non-`pub` helpers stay internal to
// the module. This relies on the AffineScript compiler honouring `fd_vis`
// in its WASM-export decision (commit ce324fa, both codegen.ml and
// codegen_gc.ml).
//
// Uses the existing @hyperpolymath/affine-js bridge for WASM loading and
// value marshalling, plus a minimal WASI stub for `fd_write` (AffineScript
// codegen imports this unconditionally even for programs that never print).

import { AffineModule } from "@hyperpolymath/affine-js";

/** Convention: every `pub fn` whose name begins with this prefix is a test. */
export const TEST_PREFIX = "test_";

/** Result shape returned by AffineScript Bool exports (via affine-js). */
interface BoolValue {
  kind: "bool";
  value: boolean;
}

/**
 * Derive the Deno.test() case name from the file basename + export name.
 * For a wasm at `/path/to/math_test.wasm` with export `test_add`, yields
 * `math / add`.
 */
function caseName(wasmPath: string, exportName: string): string {
  const base = wasmPath.split("/").pop() ?? wasmPath;
  const fileStem = base.replace(/\.wasm$/, "").replace(/(_test|\.test)$/, "");
  const caseStem = exportName.replace(/^test_/, "");
  return `${fileStem} / ${caseStem}`;
}

/**
 * Minimal WASI stub. AffineScript's WASM output imports
 * `wasi_snapshot_preview1.fd_write` unconditionally, even for programs that
 * do not use IO. For test modules that only return Bool, we can satisfy this
 * with a no-op that reports `0` bytes written on every call.
 */
function makeWasiStub(): WebAssembly.ModuleImports {
  return {
    fd_write: (
      _fd: number,
      _iovsPtr: number,
      _iovsLen: number,
      _nwrittenPtr: number,
    ): number => 0,
    // Pre-empt future codegen additions with harmless stubs. If the module
    // doesn't import these, they are simply unused.
    proc_exit: (_code: number): void => {},
    fd_close: (_fd: number): number => 0,
  };
}

/**
 * Register a Deno.test() case for every `test_*` export in the WASM module
 * at `wasmPath`. Path should be absolute; relative paths resolve against CWD.
 *
 * Returns the number of tests registered. Throws if no `test_*` exports
 * are found (indicating a misconfigured file — at least one `pub fn test_*`
 * is expected).
 */
export async function registerTestsFromWasm(wasmPath: string): Promise<number> {
  const absolute = wasmPath.startsWith("/")
    ? wasmPath
    : `${Deno.cwd()}/${wasmPath}`;

  const bytes = await Deno.readFile(absolute);
  const wasmMod = await WebAssembly.compile(bytes);
  const neededImports = WebAssembly.Module.imports(wasmMod);
  const needsWasi = neededImports.some((i) => i.module === "wasi_snapshot_preview1");

  // AffineModule.fromBytes only supplies imports under the "env" module key,
  // so when WASI is required we must take the alternative path: raw
  // WebAssembly.instantiate with both env + wasi_snapshot_preview1.
  if (needsWasi) {
    return await registerTestsWithWasi(bytes, wasmPath);
  }

  const mod = await AffineModule.fromBytes(bytes);
  const testExports = mod.functionExports.filter((name: string) =>
    name.startsWith(TEST_PREFIX)
  );

  if (testExports.length === 0) {
    throw new Error(
      `affinescript-deno-test: no '${TEST_PREFIX}*' exports found in ${wasmPath}. ` +
        `Available: [${mod.functionExports.join(", ")}]. ` +
        `Each test must be declared as 'pub fn test_<name>() -> Bool'.`,
    );
  }

  for (const exportName of testExports) {
    Deno.test(caseName(wasmPath, exportName), () => {
      const result = mod.call(exportName, { returnType: "bool" }) as BoolValue;
      if (result.kind !== "bool") {
        throw new Error(
          `test '${exportName}' returned non-bool value: ${JSON.stringify(result)}`,
        );
      }
      if (!result.value) {
        throw new Error(`test '${exportName}' returned false`);
      }
    });
  }
  return testExports.length;
}

/**
 * Alternative instantiation path for WASM modules that import
 * wasi_snapshot_preview1. Bypasses AffineModule because its constructor
 * only accepts imports under the "env" module key.
 */
async function registerTestsWithWasi(
  bytes: Uint8Array,
  wasmPath: string,
): Promise<number> {
  // Copy into a fresh ArrayBuffer-backed Uint8Array so the TS BufferSource
  // overload matches (Deno's Uint8Array default-types to ArrayBufferLike,
  // which the WebAssembly.instantiate overload rejects).
  const buf = new Uint8Array(bytes.byteLength);
  buf.set(bytes);
  const { instance } = await WebAssembly.instantiate(buf.buffer, {
    env: {},
    wasi_snapshot_preview1: makeWasiStub(),
  });

  const testExports = Object.keys(instance.exports).filter(
    (name) =>
      typeof instance.exports[name] === "function" &&
      name.startsWith(TEST_PREFIX),
  );

  if (testExports.length === 0) {
    const available = Object.keys(instance.exports).join(", ");
    throw new Error(
      `affinescript-deno-test: no '${TEST_PREFIX}*' function exports found in ${wasmPath}. ` +
        `Available: [${available}]. ` +
        `Each test must be declared as 'pub fn test_<name>() -> Bool'.`,
    );
  }

  for (const exportName of testExports) {
    const fn = instance.exports[exportName] as () => number;
    Deno.test(caseName(wasmPath, exportName), () => {
      const raw = fn();
      // AffineScript compiles Bool to i32 (0 = false, 1 = true).
      if (raw !== 0 && raw !== 1) {
        throw new Error(
          `test '${exportName}' returned non-bool raw value ${raw}; ` +
            `'${exportName}' must have signature 'fn ${exportName}() -> Bool'`,
        );
      }
      if (raw === 0) {
        throw new Error(`test '${exportName}' returned false`);
      }
    });
  }
  return testExports.length;
}
