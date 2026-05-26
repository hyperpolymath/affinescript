// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-js: JavaScript interop layer for AffineScript WASM modules
//
// AffineScript programs compile to standard WebAssembly (WASM) modules.
// This package provides ergonomic JavaScript bindings for loading and calling
// those modules from Deno or any WASM-capable JS host.
//
// Quick start:
//
//   import { AffineModule, run } from "@hyperpolymath/affine-js";
//
//   // Run a compiled AffineScript program:
//   const result = await run("./hello.wasm");
//
//   // Or load and call individual exports:
//   const mod = await AffineModule.fromFile("./math.wasm");
//   const sum = mod.call("add", { kind: "int", value: 3 }, { kind: "int", value: 4 });
//   console.log(sum); // { kind: "int", value: 7 }
//
// Effect handlers:
//
//   AffineScript programs may declare algebraic effects (IO, State, Exn, etc.).
//   The host must supply a handler for every effect the program may invoke at
//   runtime.  Pass `{ imports: { affine_io_println: myLogger } }` to override
//   the default IO implementation.
//
//   See ./runtime.js for the full list of default handlers and the naming
//   convention used by the codegen.

import { makeRuntimeImports } from "./runtime.js";
import { marshal, unmarshal } from "./marshal.js";
import { buildImportObject, parseOwnershipSection, readBytes } from "./loader.js";

/**
 * An instantiated AffineScript WASM module.
 *
 * Wraps a `WebAssembly.Instance` and provides type-safe access to exported
 * functions via `AffineValue` objects.
 */
export class AffineModule {
  /** @type {WebAssembly.Instance} */
  #instance;

  /** @type {WebAssembly.Memory} */
  #memory;

  /** @type {(n: number) => number} */
  #alloc;

  /** @type {import("./loader.js").OwnershipEntry[]} */
  #ownership;

  /**
   * @param {WebAssembly.Instance} instance
   * @param {WebAssembly.Memory} memory
   * @param {(n: number) => number} alloc
   * @param {import("./loader.js").OwnershipEntry[]} [ownership]
   */
  constructor(instance, memory, alloc, ownership = []) {
    this.#instance = instance;
    this.#memory = memory;
    this.#alloc = alloc;
    this.#ownership = ownership;
  }

  // ── Factory methods ────────────────────────────────────────────────────────

  /**
   * Load a compiled AffineScript `.wasm` file from the local filesystem.
   *
   * Host-agnostic: works on Deno, Node, and in the browser (INT-02, #179).
   * Relative specifiers resolve against `options.base` (default: this
   * module's URL), so callers in another package should pass
   * `{ base: import.meta.url }`.
   *
   * @param {string | URL} path - Path to / URL of the `.wasm` binary
   * @param {LoadOptions} [options]
   * @returns {Promise<AffineModule>}
   *
   * @example
   * const mod = await AffineModule.fromFile("./target/program.wasm");
   */
  static async fromFile(path, options = {}) {
    const bytes = await readBytes(path, {
      base: options.base ?? import.meta.url,
    });
    return AffineModule.fromBytes(bytes, options);
  }

  /**
   * Load a compiled AffineScript module from raw WASM bytes.
   *
   * Use this when you already have the bytes (e.g. fetched from a server,
   * generated in-memory, or embedded as a Uint8Array literal).
   *
   * @param {Uint8Array | ArrayBuffer} bytes - Raw WASM binary
   * @param {LoadOptions} [options]
   * @returns {Promise<AffineModule>}
   *
   * @example
   * const res = await fetch("/api/compiled");
   * const mod = await AffineModule.fromBytes(await res.arrayBuffer());
   */
  static async fromBytes(bytes, options = {}) {
    const runtimeImports = makeRuntimeImports();
    const imports = buildImportObject(runtimeImports, options);

    const wasmBytes = bytes instanceof Uint8Array ? bytes : new Uint8Array(bytes);
    const { instance, module } = await WebAssembly.instantiate(
      wasmBytes,
      imports,
    );
    const ownership = parseOwnershipSection(module);

    const memory = instance.exports.memory;
    if (!(memory instanceof WebAssembly.Memory)) {
      throw new Error(
        "affine-js: AffineScript WASM module must export 'memory'; " +
          "ensure the module was compiled by the AffineScript compiler",
      );
    }

    // Wire memory into the runtime imports so string I/O works.
    runtimeImports.setMemory(memory);

    // affine_alloc: bump-allocator exposed by the runtime.
    const affineAlloc = instance.exports["affine_alloc"];
    const alloc = typeof affineAlloc === "function"
      ? (n) => affineAlloc(n)
      : (_n) => {
        throw new Error(
          "affine-js: module does not export 'affine_alloc'; " +
            "cannot marshal heap-allocated arguments (strings, arrays, records). " +
            "Simple types (int, float, bool, unit) still work.",
        );
      };

    return new AffineModule(instance, memory, alloc, ownership);
  }

  // ── Calling exports ────────────────────────────────────────────────────────

  /**
   * Call a named export, marshaling AffineValues to WASM and back.
   *
   * The `returnType` hint is needed because WebAssembly does not carry
   * high-level type information at runtime.  For simple programs that only
   * use `Int`, `Float`, `Bool`, or `Unit` you can omit it and inspect the
   * raw number.
   *
   * @param {string} name - Export name (as it appears in the AffineScript source)
   * @param {CallOptions} [options]
   * @param {...import("./marshal.js").AffineValue} args - Arguments
   * @returns {import("./marshal.js").AffineValue}
   *
   * @example
   * const sum = mod.call("add", {}, { kind: "int", value: 3 }, { kind: "int", value: 4 });
   */
  call(name, options = {}, ...args) {
    const fn = this.#instance.exports[name];
    if (typeof fn !== "function") {
      const available = this.functionExports.join(", ");
      throw new Error(
        `affine-js: no function export named '${name}'. ` +
          `Available: [${available}]`,
      );
    }

    const rawArgs = args.map((v) => marshal(v, this.#memory, this.#alloc));
    const rawResult = fn(...rawArgs);

    const returnType = options.returnType ?? "int";
    return unmarshal(rawResult, returnType, this.#memory);
  }

  /**
   * Invoke the top-level `main` function of the module.
   *
   * AffineScript programs compile their top-level `fn main() -> T / E { ... }`
   * to a WASM export named `"main"`.
   *
   * @param {CallOptions} [options]
   * @returns {import("./marshal.js").AffineValue}
   */
  runMain(options = {}) {
    return this.call("main", { returnType: "unit", ...options });
  }

  // ── Introspection ──────────────────────────────────────────────────────────

  /**
   * Names of all exported functions.
   * @returns {string[]}
   */
  get functionExports() {
    return Object.keys(this.#instance.exports).filter(
      (k) => typeof this.#instance.exports[k] === "function",
    );
  }

  /**
   * Direct access to the underlying WASM memory buffer.
   * Useful for advanced / low-level interop that bypasses the marshal layer.
   * @type {WebAssembly.Memory}
   */
  get memory() {
    return this.#memory;
  }

  /**
   * Direct access to the underlying `WebAssembly.Instance`.
   * @type {WebAssembly.Instance}
   */
  get instance() {
    return this.#instance;
  }

  /**
   * The parsed `typedwasm.ownership` custom section: per-function
   * parameter/return ownership kinds carrying the typed-wasm discipline
   * (see docs/ECOSYSTEM.adoc — the AffineScript ↔ typed-wasm contract).
   * Empty when the module was compiled without ownership qualifiers.
   * @type {import("./loader.js").OwnershipEntry[]}
   */
  get ownership() {
    return this.#ownership;
  }
}

// ── Convenience helpers ───────────────────────────────────────────────────────

/**
 * Load and run a compiled AffineScript program in one call.
 *
 * Equivalent to `AffineModule.fromFile(path, options).then(m => m.runMain())`.
 *
 * @param {string | URL} path - Path to the `.wasm` binary
 * @param {LoadOptions} [options]
 * @returns {Promise<import("./marshal.js").AffineValue>}
 *
 * @example
 * import { run } from "@hyperpolymath/affine-js";
 * await run("./hello.wasm");
 */
export async function run(path, options = {}) {
  const mod = await AffineModule.fromFile(path, options);
  return mod.runMain(options);
}

// ── Re-exports for convenience ────────────────────────────────────────────────

export { int, float, bool, unit, string, some, none, ok, err, array, record }
  from "./marshal.js";
export { marshal, unmarshal } from "./marshal.js";
export { AFFINE_TAG, AFFINE_SIZE, makeRuntimeImports } from "./runtime.js";

// ── JSDoc typedefs ────────────────────────────────────────────────────────────

/**
 * @typedef {Object} LoadOptions
 * @property {Record<string, Function>} [imports]
 *   Additional WASM imports merged into the `env` namespace alongside the
 *   default runtime.  Keys must match the import names generated by the
 *   AffineScript codegen for your declared effects.
 *   Example: `{ affine_io_println: myLog }`.
 * @property {Record<string, Record<string, Function>>} [modules]
 *   Per-namespace imports for cross-module WASM imports (INT-01, #178):
 *   `use Mod::{fn}` lowers to an import in the `Mod` namespace, so supply
 *   `{ Mod: { fn: impl } }`.  Merged, not clobbered, against `env`.
 * @property {string | URL} [base]
 *   Base URL for resolving a relative `fromFile`/`run` specifier.
 *   Defaults to the affine-js module URL; pass `import.meta.url` from the
 *   calling module when loading a `.wasm` relative to *your* file.
 */

/**
 * @typedef {Object} CallOptions
 * @property {"int"|"float"|"bool"|"unit"|"string"|"option"|"result"|"array"|"record"} [returnType="int"]
 *   Hint for the unmarshaler so it knows how to interpret the raw WASM return value.
 */
