// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-js/loader: host-agnostic loader bridge (INT-02, issue #179).
//
// Prior to this module `AffineModule.fromFile` was Deno-only: it called
// `Deno.readFile(url.pathname)`.  `url.pathname` is *not* a filesystem path
// (it is percent-encoded, drops the Windows drive letter, and is meaningless
// for non-`file:` URLs), and `Deno.readFile` does not exist on Node or in a
// browser.  That was SAT-02.
//
// This module provides the four pieces INT-02 requires:
//
//   1. relative URL resolution that is correct on every host;
//   2. a host-agnostic byte reader (Deno / Node / browser parity);
//   3. a full import-object builder (multi-namespace, for the cross-module
//      WASM imports INT-01/#178 emits — not `env`-only);
//   4. an accessor for the `typedwasm.ownership` custom section (the
//      typed-wasm contract carrier — see docs/ECOSYSTEM.adoc).
//
// It has no dependency on `mod.js`; `mod.js` consumes it.

/**
 * The JavaScript host we are running under.
 * @typedef {"deno"|"node"|"browser"|"unknown"} Host
 */

/**
 * Detect the current JavaScript host by feature, not by user agent.
 * @returns {Host}
 */
export function detectHost() {
  if (typeof Deno !== "undefined" && Deno?.version?.deno) return "deno";
  if (
    typeof process !== "undefined" &&
    process?.versions?.node &&
    // A bundled-for-browser build can shim `process`; require real fs too.
    typeof globalThis.WebAssembly !== "undefined"
  ) {
    return "node";
  }
  if (
    typeof globalThis.fetch === "function" &&
    (typeof window !== "undefined" || typeof self !== "undefined")
  ) {
    return "browser";
  }
  return "unknown";
}

/**
 * Resolve a module specifier to an absolute URL.
 *
 * Accepts a `URL`, an absolute URL string, a `file:`-relative specifier, or a
 * filesystem path (POSIX or Windows).  `base` is required for relative
 * specifiers; callers pass their own `import.meta.url`.
 *
 * @param {string | URL} spec
 * @param {string | URL} [base]
 * @returns {URL}
 */
export function resolveUrl(spec, base) {
  if (spec instanceof URL) return spec;
  if (typeof spec !== "string") {
    throw new TypeError(
      `affine-js/loader: specifier must be a string or URL, got ${typeof spec}`,
    );
  }
  // Absolute URL (has a scheme like file:, http:, https:, data:, blob:).
  if (/^[a-zA-Z][a-zA-Z0-9+.-]*:/.test(spec) && !/^[a-zA-Z]:[\\/]/.test(spec)) {
    return new URL(spec);
  }
  // Windows absolute path, e.g. C:\dir\x.wasm  ->  file:///C:/dir/x.wasm
  if (/^[a-zA-Z]:[\\/]/.test(spec)) {
    return new URL(`file:///${spec.replace(/\\/g, "/")}`);
  }
  // POSIX absolute path.
  if (spec.startsWith("/")) return new URL(`file://${spec}`);
  // Relative specifier — needs a base.
  if (base === undefined) {
    throw new Error(
      `affine-js/loader: relative specifier ${JSON.stringify(spec)} needs a ` +
        `base URL; pass { base: import.meta.url }`,
    );
  }
  return new URL(spec, base);
}

/**
 * Read a WASM module's bytes from any source, on any host.
 *
 * @param {string | URL | Uint8Array | ArrayBuffer} source
 * @param {{ base?: string | URL }} [options]
 * @returns {Promise<Uint8Array>}
 */
export async function readBytes(source, options = {}) {
  if (source instanceof Uint8Array) return source;
  if (source instanceof ArrayBuffer) return new Uint8Array(source);
  if (ArrayBuffer.isView(source)) {
    return new Uint8Array(source.buffer, source.byteOffset, source.byteLength);
  }

  const url = resolveUrl(source, options.base);

  if (url.protocol === "file:") {
    const host = detectHost();
    if (host === "deno") {
      // Deno.readFile accepts a URL directly — no pathname mangling.
      return await Deno.readFile(url);
    }
    if (host === "node") {
      const { readFile } = await import("node:fs/promises");
      const { fileURLToPath } = await import("node:url");
      const buf = await readFile(fileURLToPath(url));
      return new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength);
    }
    // Browser: a `file:` URL may still be reachable via fetch when the page
    // is itself served from disk; otherwise this throws a clear error.
    try {
      const res = await fetch(url);
      if (!res.ok) throw new Error(`HTTP ${res.status}`);
      return new Uint8Array(await res.arrayBuffer());
    } catch (cause) {
      throw new Error(
        `affine-js/loader: cannot read ${url.href} in a browser host; ` +
          `serve the .wasm over http(s) or pass its bytes directly`,
        { cause },
      );
    }
  }

  // http:, https:, data:, blob: — fetch works on every host that has it.
  if (typeof globalThis.fetch !== "function") {
    throw new Error(
      `affine-js/loader: no fetch available to read ${url.href} on this host`,
    );
  }
  const res = await fetch(url);
  if (!res.ok) {
    throw new Error(
      `affine-js/loader: failed to fetch ${url.href} (HTTP ${res.status})`,
    );
  }
  return new Uint8Array(await res.arrayBuffer());
}

/**
 * Build the full WebAssembly import object.
 *
 * The legacy shape merged everything into a single `env` namespace.  INT-01
 * (#178) emits genuine cross-module imports under the *callee module's* name,
 * so the import object must carry arbitrary namespaces.  This builder keeps
 * `env` backward-compatible (runtime defaults + flat `options.imports`) and
 * adds any `options.modules` namespaces verbatim.
 *
 * @param {Record<string, Function>} runtimeImports
 * @param {{ imports?: Record<string, Function>,
 *           modules?: Record<string, Record<string, Function>> }} [options]
 * @returns {WebAssembly.Imports}
 */
export function buildImportObject(runtimeImports, options = {}) {
  /** @type {WebAssembly.Imports} */
  const importObject = {
    env: {
      ...runtimeImports,
      ...(options.imports ?? {}),
    },
  };
  for (const [ns, members] of Object.entries(options.modules ?? {})) {
    if (ns === "env") {
      // Merge rather than clobber the runtime namespace.
      Object.assign(importObject.env, members);
    } else {
      importObject[ns] = { ...(importObject[ns] ?? {}), ...members };
    }
  }
  return importObject;
}

/** @typedef {"unrestricted"|"linear"|"sharedBorrow"|"exclBorrow"} OwnershipKind */

const OWNERSHIP_KINDS = /** @type {const} */ ([
  "unrestricted",
  "linear",
  "sharedBorrow",
  "exclBorrow",
]);

/**
 * One per-function ownership annotation.
 * @typedef {Object} OwnershipEntry
 * @property {number} funcIdx
 * @property {OwnershipKind[]} paramKinds
 * @property {OwnershipKind} retKind
 */

/**
 * Parse the `typedwasm.ownership` custom section.
 *
 * Binary encoding (must match `Codegen.build_ownership_section` /
 * `Tw_verify.parse_ownership_section_payload` in the compiler):
 *
 *   u32le count
 *   for each entry:
 *     u32le func_idx
 *     u8    n_params
 *     u8[n] param_kinds   (0=Unrestricted,1=Linear,2=SharedBorrow,3=ExclBorrow)
 *     u8    ret_kind
 *
 * @param {WebAssembly.Module} wasmModule
 * @returns {OwnershipEntry[]}
 */
export function parseOwnershipSection(wasmModule) {
  const sections = WebAssembly.Module.customSections(
    wasmModule,
    "typedwasm.ownership",
  );
  if (sections.length === 0) return [];
  const view = new DataView(sections[0]);
  let pos = 0;
  const u32 = () => {
    const v = view.getUint32(pos, /* littleEndian */ true);
    pos += 4;
    return v;
  };
  const u8 = () => {
    const v = view.getUint8(pos);
    pos += 1;
    return v;
  };
  const kind = (b) => OWNERSHIP_KINDS[b] ?? "unrestricted";

  const count = u32();
  /** @type {OwnershipEntry[]} */
  const entries = [];
  for (let i = 0; i < count; i++) {
    const funcIdx = u32();
    const nParams = u8();
    const paramKinds = [];
    for (let p = 0; p < nParams; p++) paramKinds.push(kind(u8()));
    const retKind = kind(u8());
    entries.push({ funcIdx, paramKinds, retKind });
  }
  return entries;
}
