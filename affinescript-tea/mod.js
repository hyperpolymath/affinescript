// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-tea: the host-side TEA (The Elm Architecture) runtime + run
// loop for AffineScript modules (INT-07, issue #182).
//
// The compiler-internal `lib/tea_bridge.ml` defines the TEA runtime ABI a
// conforming WASM module exposes:
//
//   exports:
//     affinescript_init()                  -> ()    write the initial model
//     affinescript_update(msg: i32)        -> ()    msg is LINEAR (consumed
//                                                   exactly once per cycle)
//     affinescript_get_<field>() -> i32                (optional getters)
//     affinescript_set_screen(w, h)        -> ()       (optional)
//     memory
//   custom sections:
//     affinescript.tea_layout    model field layout (parsed here, generically)
//     affinescript.ownership     per-fn ownership kinds (the Linear-msg proof)
//
// This runtime is GENERIC: it discovers the model from `tea_layout` rather
// than hard-coding any one model (the bridge's TitleModel was only a demo).
// It reuses the INT-02 host-agnostic loader for Deno/Node/browser parity.

import {
  parseOwnershipSection,
  readBytes,
} from "@hyperpolymath/affine-js/loader";

/**
 * One model field, decoded from the `affinescript.tea_layout` section.
 * @typedef {Object} TeaField
 * @property {string} name
 * @property {number} offset   byte offset relative to the model base
 * @property {"i32"} type
 */

/**
 * @typedef {Object} TeaLayout
 * @property {number} version
 * @property {number} baseAddr
 * @property {TeaField[]} fields
 */

/**
 * Parse the `affinescript.tea_layout` custom section.
 *
 * Binary format (must match `Tea_bridge.build_tea_layout_section`):
 *   u8  version
 *   u8  base_addr
 *   u8  field_count
 *   per field: u8 name_len, name_bytes, u8 offset, u8 type_tag (0x49='i32')
 *
 * @param {WebAssembly.Module} wasmModule
 * @returns {TeaLayout | null} null when the section is absent
 */
export function parseTeaLayout(wasmModule) {
  const secs = WebAssembly.Module.customSections(
    wasmModule,
    "affinescript.tea_layout",
  );
  if (secs.length === 0) return null;
  const b = new Uint8Array(secs[0]);
  let p = 0;
  const version = b[p++];
  const baseAddr = b[p++];
  const count = b[p++];
  const dec = new TextDecoder("utf-8");
  /** @type {TeaField[]} */
  const fields = [];
  for (let i = 0; i < count; i++) {
    const nameLen = b[p++];
    const name = dec.decode(b.subarray(p, p + nameLen));
    p += nameLen;
    const offset = b[p++];
    const typeTag = b[p++];
    fields.push({
      name,
      offset,
      type: typeTag === 0x49 ? "i32" : `tag:0x${typeTag.toString(16)}`,
    });
  }
  return { version, baseAddr, fields };
}

/**
 * A loaded, running TEA application.
 *
 * Lifecycle: `await TeaApp.load(src)` → `app.init()` → `app.dispatch(msg)`
 * (each call drives one TEA update cycle) → `app.model()`. Or hand it to
 * `app.run(...)` for a managed loop.
 */
export class TeaApp {
  /** @type {WebAssembly.Instance} */ #instance;
  /** @type {WebAssembly.Memory} */ #memory;
  /** @type {TeaLayout} */ #layout;
  /** @type {import("@hyperpolymath/affine-js/loader").OwnershipEntry[]} */ #ownership;
  /** Re-entrancy guard enforcing the Linear-msg invariant. */
  #inCycle = false;

  /**
   * @param {WebAssembly.Instance} instance
   * @param {TeaLayout} layout
   * @param {import("@hyperpolymath/affine-js/loader").OwnershipEntry[]} ownership
   */
  constructor(instance, layout, ownership) {
    this.#instance = instance;
    const mem = instance.exports.memory;
    if (!(mem instanceof WebAssembly.Memory)) {
      throw new Error(
        "affinescript-tea: module must export 'memory' (TEA ABI)",
      );
    }
    this.#memory = mem;
    if (!layout) {
      throw new Error(
        "affinescript-tea: module has no 'affinescript.tea_layout' custom " +
          "section — not a TEA-conformant module",
      );
    }
    this.#layout = layout;
    this.#ownership = ownership;
    for (const fn of ["affinescript_init", "affinescript_update"]) {
      if (typeof instance.exports[fn] !== "function") {
        throw new Error(`affinescript-tea: module must export '${fn}' (TEA ABI)`);
      }
    }
  }

  /**
   * Load + instantiate a TEA-conformant module from any source/host.
   * @param {string | URL | Uint8Array | ArrayBuffer} source
   * @param {{ base?: string | URL, imports?: Record<string, Function> }} [options]
   * @returns {Promise<TeaApp>}
   */
  static async load(source, options = {}) {
    const bytes = await readBytes(source, { base: options.base });
    const { instance, module } = await WebAssembly.instantiate(bytes, {
      env: { ...(options.imports ?? {}) },
    });
    const layout = parseTeaLayout(module);
    const ownership = parseOwnershipSection(module);
    return new TeaApp(instance, layout, ownership);
  }

  /** The parsed model layout (from `affinescript.tea_layout`). */
  get layout() {
    return this.#layout;
  }

  /**
   * The ownership annotations. `affinescript_update`'s `msg` parameter is
   * Linear (kind `"linear"`) — the host-visible proof that a message is
   * consumed exactly once per update cycle.
   * @type {import("@hyperpolymath/affine-js/loader").OwnershipEntry[]}
   */
  get ownership() {
    return this.#ownership;
  }

  /** Read the current model as a plain object (generic, layout-driven). */
  model() {
    const dv = new DataView(this.#memory.buffer);
    /** @type {Record<string, number>} */
    const m = {};
    for (const f of this.#layout.fields) {
      m[f.name] = dv.getInt32(this.#layout.baseAddr + f.offset, true);
    }
    return m;
  }

  /** Run `affinescript_init()`; returns the initial model. */
  init() {
    this.#instance.exports.affinescript_init();
    return this.model();
  }

  /**
   * Drive one TEA update cycle with `msg`, then return the new model.
   *
   * Enforces the Linear-msg invariant host-side: `affinescript_update` is
   * invoked exactly once, and the call is non-re-entrant — dispatching
   * again from within a view/effect triggered by this cycle throws (that
   * would consume a second message inside one cycle, violating the
   * linearity the `affinescript.ownership` section asserts).
   *
   * @param {number} msg
   * @returns {Record<string, number>} the model after the update
   */
  dispatch(msg) {
    if (!Number.isInteger(msg)) {
      throw new TypeError(`affinescript-tea: msg must be an i32, got ${msg}`);
    }
    if (this.#inCycle) {
      throw new Error(
        "affinescript-tea: re-entrant dispatch — a message must be consumed " +
          "exactly once per update cycle (Linear-msg invariant)",
      );
    }
    this.#inCycle = true;
    try {
      this.#instance.exports.affinescript_update(msg);
    } finally {
      this.#inCycle = false;
    }
    return this.model();
  }

  /** Optional resize hook (present iff the module exports it). */
  setScreen(w, h) {
    const fn = this.#instance.exports.affinescript_set_screen;
    if (typeof fn !== "function") {
      throw new Error(
        "affinescript-tea: module does not export 'affinescript_set_screen'",
      );
    }
    fn(w, h);
    return this.model();
  }

  /**
   * The managed run loop. Generic over the message source: `messages` is
   * any (async) iterable of i32 msgs (DOM events, a channel, a test array,
   * a timer — all adapt to this). `view(model)` is called once after
   * `init()` and once after every dispatched message.
   *
   * @param {{ messages: Iterable<number> | AsyncIterable<number>,
   *           view: (model: Record<string, number>) => void }} driver
   * @returns {Promise<Record<string, number>>} the final model
   */
  async run({ messages, view }) {
    if (typeof view !== "function") {
      throw new TypeError("affinescript-tea: run() needs a view(model) fn");
    }
    let model = this.init();
    view(model);
    for await (const msg of messages) {
      model = this.dispatch(msg);
      view(model);
    }
    return model;
  }
}
