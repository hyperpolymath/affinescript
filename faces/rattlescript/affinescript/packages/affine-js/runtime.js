// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-js: AffineScript WASM runtime support
//
// Provides the host-side imports that AffineScript WASM modules expect.
// AffineScript programs declare algebraic effects (e.g. `effect IO { ... }`)
// and compile to WASM modules that import the handlers from the host.  The
// host is responsible for providing implementations of every declared effect
// operation.
//
// The defaults below implement `IO` (print / println / read_line) using the
// Deno standard streams.  Pass a custom `imports` object to `AffineModule`
// to override handlers or to implement user-defined effects.
//
// Memory layout constants (AffineScript 0.1.x WASM codegen):
//
//   TAG_NONE   = 0x00  (Option::None sentinel, also Unit sentinel)
//   TAG_SOME   = 0x01  (Option::Some(v) — followed by payload)
//   TAG_OK     = 0x02  (Result::Ok(v) — followed by payload)
//   TAG_ERR    = 0x03  (Result::Err(e) — followed by payload)
//
// String layout in linear memory:
//   [length: i32 LE][...utf-8 bytes]
//
// Array layout in linear memory:
//   [length: i32 LE][...elements (element-size bytes each)]
//
// Record layout: field values stored consecutively in declaration order,
// each aligned to its natural size (4B for i32, 8B for f64).
//
// These constants should be kept in sync with lib/codegen.ml and
// lib/codegen_gc.ml as the compiler evolves.

/** Tag byte values used in AffineScript's WASM memory encoding. */
export const AFFINE_TAG = Object.freeze({
  /** Unit / Option::None sentinel */
  NONE: 0x00,
  /** Option::Some payload header */
  SOME: 0x01,
  /** Result::Ok payload header */
  OK: 0x02,
  /** Result::Err payload header */
  ERR: 0x03,
});

/** Byte size of each WASM value category. */
export const AFFINE_SIZE = Object.freeze({
  INT: 4,   // i32
  FLOAT: 8, // f64
  PTR: 4,   // pointer into linear memory (i32)
  TAG: 4,   // tag word prefix (i32)
  LEN: 4,   // length prefix (i32)
});

/**
 * Build the default host import object for AffineScript WASM modules.
 *
 * AffineScript's `effect IO` compiles to WASM imports under the `env`
 * namespace.  This function returns a set of implementations that write to
 * Deno's stdout/stderr and read from stdin.
 *
 * @param {WebAssembly.Memory} [memory] - Provided lazily because the memory
 *   object is not available until after instantiation.  The host should call
 *   `setMemory(mem)` on the returned object after calling
 *   `WebAssembly.instantiate`.  Alternatively, use `AffineModule.fromBytes`
 *   which handles this automatically.
 * @returns {AffineRuntimeImports}
 */
export function makeRuntimeImports(memory = null) {
  let _memory = memory;

  /** Read a UTF-8 string from WASM linear memory given a pointer. */
  function readString(ptr) {
    if (!_memory) throw new Error("affine-js: memory not yet set");
    const view = new DataView(_memory.buffer);
    const len = view.getInt32(ptr, /* littleEndian = */ true);
    const bytes = new Uint8Array(_memory.buffer, ptr + AFFINE_SIZE.LEN, len);
    return new TextDecoder().decode(bytes);
  }

  /**
   * Write a JS string into WASM linear memory starting at `ptr`.
   * Caller must ensure there is sufficient space.
   * @returns {number} Number of bytes written (including length prefix).
   */
  function writeString(ptr, str) {
    if (!_memory) throw new Error("affine-js: memory not yet set");
    const encoded = new TextEncoder().encode(str);
    const view = new DataView(_memory.buffer);
    view.setInt32(ptr, encoded.byteLength, true);
    new Uint8Array(_memory.buffer).set(encoded, ptr + AFFINE_SIZE.LEN);
    return AFFINE_SIZE.LEN + encoded.byteLength;
  }

  const imports = {
    /**
     * Accept the instantiated memory from the caller.
     * @param {WebAssembly.Memory} mem
     */
    setMemory(mem) {
      _memory = mem;
    },

    // ── IO effect operations ──────────────────────────────────────────────

    /** `IO.print(s: String)` — write to stdout without newline */
    affine_io_print(strPtr) {
      Deno.stdout.writeSync(new TextEncoder().encode(readString(strPtr)));
    },

    /** `IO.println(s: String)` — write to stdout with trailing newline */
    affine_io_println(strPtr) {
      Deno.stdout.writeSync(
        new TextEncoder().encode(readString(strPtr) + "\n"),
      );
    },

    /** `IO.read_line() -> String` — read one line from stdin, return WASM pointer */
    affine_io_read_line(outPtr) {
      const buf = new Uint8Array(4096);
      const n = Deno.stdin.readSync(buf);
      const line = new TextDecoder().decode(buf.subarray(0, n ?? 0)).trimEnd();
      writeString(outPtr, line);
      return outPtr;
    },

    // ── Panic / abort ────────────────────────────────────────────────────

    /** Called by the runtime when an unreachable code path is reached. */
    affine_panic(msgPtr) {
      const msg = _memory ? readString(msgPtr) : "(memory not available)";
      throw new Error(`AffineScript panic: ${msg}`);
    },

    /** Called by the runtime when an unhandled effect is invoked. */
    affine_unhandled_effect(namePtr) {
      const name = _memory ? readString(namePtr) : "(memory not available)";
      throw new Error(
        `AffineScript unhandled effect: '${name}' — provide a handler in the imports object`,
      );
    },
  };

  return imports;
}

/**
 * @typedef {ReturnType<typeof makeRuntimeImports>} AffineRuntimeImports
 */
