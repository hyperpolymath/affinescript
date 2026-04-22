// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-js: Value marshaling between JavaScript and AffineScript WASM
//
// AffineScript values have a typed representation on both sides of the WASM
// boundary.  This module provides:
//
//   marshal(value, memory)    — AffineValue → raw WASM operand (i32 / f64)
//   unmarshal(raw, tag, mem)  — raw WASM operand → AffineValue
//   constructors              — type-safe AffineValue builders
//
// Simple types (Int, Float, Bool, Unit) are passed as WASM value-type
// operands directly.  Complex types (String, Option, Result, Array, Record)
// are heap-allocated in linear memory and passed as i32 pointers.
//
// ── Memory protocol ──────────────────────────────────────────────────────────
//
// The AffineScript WASM runtime allocates from a bump allocator starting at
// the low end of linear memory (above the stack).  Functions that return
// complex values write to an out-pointer supplied by the caller.  The JS
// host uses the same protocol when marshaling arguments into memory.
//
// The exported function `affine_alloc(bytes: i32) -> i32` provides a simple
// allocator interface.  The JS host calls this to reserve space before
// writing strings or records.  The corresponding `affine_dealloc` is a no-op
// in the current bump allocator but exists for forward-compatibility.

import { AFFINE_SIZE, AFFINE_TAG } from "./runtime.js";

// ── AffineValue constructors ──────────────────────────────────────────────────

/**
 * @typedef {
 *   | { kind: "int";    value: number }
 *   | { kind: "float";  value: number }
 *   | { kind: "bool";   value: boolean }
 *   | { kind: "unit" }
 *   | { kind: "string"; value: string }
 *   | { kind: "some";   value: AffineValue }
 *   | { kind: "none" }
 *   | { kind: "ok";     value: AffineValue }
 *   | { kind: "err";    value: AffineValue }
 *   | { kind: "array";  elements: AffineValue[] }
 *   | { kind: "record"; fields: Record<string, AffineValue> }
 * } AffineValue
 */

/** @returns {AffineValue} */
export const int = (value) => ({ kind: "int", value });
/** @returns {AffineValue} */
export const float = (value) => ({ kind: "float", value });
/** @returns {AffineValue} */
export const bool = (value) => ({ kind: "bool", value });
/** @returns {AffineValue} */
export const unit = () => ({ kind: "unit" });
/** @returns {AffineValue} */
export const string = (value) => ({ kind: "string", value });
/** @returns {AffineValue} */
export const some = (value) => ({ kind: "some", value });
/** @returns {AffineValue} */
export const none = () => ({ kind: "none" });
/** @returns {AffineValue} */
export const ok = (value) => ({ kind: "ok", value });
/** @returns {AffineValue} */
export const err = (value) => ({ kind: "err", value });
/** @returns {AffineValue} */
export const array = (elements) => ({ kind: "array", elements });
/** @returns {AffineValue} */
export const record = (fields) => ({ kind: "record", fields });

// ── Marshal: AffineValue → WASM ───────────────────────────────────────────────

/**
 * Marshal an `AffineValue` into the form expected by a WASM import/export.
 *
 * For simple types this returns a JS number directly.  For heap types this
 * allocates in linear memory (via `affine_alloc`) and returns the i32
 * pointer.
 *
 * @param {AffineValue} value
 * @param {WebAssembly.Memory} memory
 * @param {(n: number) => number} alloc - `affine_alloc` export from the module
 * @returns {number} Raw WASM operand (i32 or f64 as JS number)
 */
export function marshal(value, memory, alloc) {
  switch (value.kind) {
    case "int":
      return value.value | 0; // coerce to i32

    case "float":
      return value.value; // f64, passed as JS number

    case "bool":
      return value.value ? 1 : 0; // i32 0/1

    case "unit":
      return 0; // sentinel i32

    case "string": {
      const encoded = new TextEncoder().encode(value.value);
      const ptr = alloc(AFFINE_SIZE.LEN + encoded.byteLength);
      const view = new DataView(memory.buffer);
      view.setInt32(ptr, encoded.byteLength, true);
      new Uint8Array(memory.buffer).set(encoded, ptr + AFFINE_SIZE.LEN);
      return ptr;
    }

    case "none":
      return 0; // null pointer = None

    case "some": {
      // [TAG_SOME: i32][...marshaled payload]
      // For payload we need its size; for simplicity primitives inline here.
      const payloadSize = sizeOf(value.value);
      const ptr = alloc(AFFINE_SIZE.TAG + payloadSize);
      const view = new DataView(memory.buffer);
      view.setInt32(ptr, AFFINE_TAG.SOME, true);
      writePrimitive(value.value, memory, ptr + AFFINE_SIZE.TAG, alloc);
      return ptr;
    }

    case "ok": {
      const payloadSize = sizeOf(value.value);
      const ptr = alloc(AFFINE_SIZE.TAG + payloadSize);
      const view = new DataView(memory.buffer);
      view.setInt32(ptr, AFFINE_TAG.OK, true);
      writePrimitive(value.value, memory, ptr + AFFINE_SIZE.TAG, alloc);
      return ptr;
    }

    case "err": {
      const payloadSize = sizeOf(value.value);
      const ptr = alloc(AFFINE_SIZE.TAG + payloadSize);
      const view = new DataView(memory.buffer);
      view.setInt32(ptr, AFFINE_TAG.ERR, true);
      writePrimitive(value.value, memory, ptr + AFFINE_SIZE.TAG, alloc);
      return ptr;
    }

    case "array": {
      const ptr = alloc(AFFINE_SIZE.LEN + value.elements.length * AFFINE_SIZE.PTR);
      const view = new DataView(memory.buffer);
      view.setInt32(ptr, value.elements.length, true);
      let off = ptr + AFFINE_SIZE.LEN;
      for (const elem of value.elements) {
        const elemPtr = marshal(elem, memory, alloc);
        view.setInt32(off, elemPtr, true);
        off += AFFINE_SIZE.PTR;
      }
      return ptr;
    }

    case "record": {
      const entries = Object.entries(value.fields);
      const ptr = alloc(entries.length * AFFINE_SIZE.PTR);
      const view = new DataView(memory.buffer);
      let off = ptr;
      for (const [, fieldVal] of entries) {
        const fieldPtr = marshal(fieldVal, memory, alloc);
        view.setInt32(off, fieldPtr, true);
        off += AFFINE_SIZE.PTR;
      }
      return ptr;
    }

    default:
      throw new Error(`affine-js: unknown value kind '${value.kind}'`);
  }
}

// ── Unmarshal: WASM → AffineValue ────────────────────────────────────────────

/**
 * Unmarshal a raw WASM return value into an `AffineValue`.
 *
 * The `valueType` hint tells the unmarshaler how to interpret the raw number
 * (because WASM is untyped from the JS host perspective).
 *
 * @param {number} raw - Raw WASM operand
 * @param {"int"|"float"|"bool"|"unit"|"string"|"option"|"result"|"array"|"record"} valueType
 * @param {WebAssembly.Memory} memory
 * @param {string[]} [fieldNames] - For "record" kind, in declaration order
 * @returns {AffineValue}
 */
export function unmarshal(raw, valueType, memory) {
  const view = new DataView(memory.buffer);
  switch (valueType) {
    case "int":
      return int(raw | 0);

    case "float":
      return float(raw);

    case "bool":
      return bool(raw !== 0);

    case "unit":
      return unit();

    case "string": {
      if (raw === 0) return string(""); // null pointer = empty string
      const len = view.getInt32(raw, true);
      const bytes = new Uint8Array(memory.buffer, raw + AFFINE_SIZE.LEN, len);
      return string(new TextDecoder().decode(bytes));
    }

    case "option": {
      if (raw === 0) return none();
      const tag = view.getInt32(raw, true);
      if (tag !== AFFINE_TAG.SOME) {
        throw new Error(`affine-js: expected Some tag (1), got ${tag}`);
      }
      // Payload is an opaque i32 pointer; caller must further unmarshal
      const payloadPtr = view.getInt32(raw + AFFINE_SIZE.TAG, true);
      return some(int(payloadPtr)); // opaque: caller can re-unmarshal
    }

    case "result": {
      if (raw === 0) throw new Error("affine-js: null Result pointer");
      const tag = view.getInt32(raw, true);
      const payloadPtr = view.getInt32(raw + AFFINE_SIZE.TAG, true);
      if (tag === AFFINE_TAG.OK) return ok(int(payloadPtr));
      if (tag === AFFINE_TAG.ERR) return err(int(payloadPtr));
      throw new Error(`affine-js: unknown Result tag ${tag}`);
    }

    case "array": {
      if (raw === 0) return array([]);
      const len = view.getInt32(raw, true);
      const elements = [];
      let off = raw + AFFINE_SIZE.LEN;
      for (let i = 0; i < len; i++) {
        elements.push(int(view.getInt32(off, true)));
        off += AFFINE_SIZE.PTR;
      }
      return array(elements);
    }

    default:
      // Fallback: treat as opaque int pointer
      return int(raw);
  }
}

// ── Internal helpers ──────────────────────────────────────────────────────────

/**
 * Return the byte size needed to store a marshaled value in-place.
 * For heap types (string, option, result) the caller is responsible for
 * allocating space for the pointee separately; this returns PTR size.
 * @param {AffineValue} value
 * @returns {number}
 */
function sizeOf(value) {
  switch (value.kind) {
    case "float": return AFFINE_SIZE.FLOAT;
    case "int":
    case "bool":
    case "unit":
    case "string":
    case "some":
    case "none":
    case "ok":
    case "err":
    case "array":
    case "record":
      return AFFINE_SIZE.PTR;
    default:
      return AFFINE_SIZE.PTR;
  }
}

/**
 * Write a marshaled value at a given offset in memory, in-place.
 * Primitive values are written directly; heap values are marshaled recursively.
 * @param {AffineValue} value
 * @param {WebAssembly.Memory} memory
 * @param {number} offset
 * @param {(n: number) => number} alloc
 */
function writePrimitive(value, memory, offset, alloc) {
  const view = new DataView(memory.buffer);
  switch (value.kind) {
    case "int":
    case "bool":
    case "unit":
      view.setInt32(offset, value.kind === "bool" ? (value.value ? 1 : 0) : (value.value ?? 0), true);
      break;
    case "float":
      view.setFloat64(offset, value.value, true);
      break;
    default: {
      const ptr = marshal(value, memory, alloc);
      view.setInt32(offset, ptr, true);
    }
  }
}
