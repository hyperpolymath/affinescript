// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-ts: TypeScript-oriented wrapper for affine-js
//
// This package re-exports everything from @hyperpolymath/affine-js and adds
// TypeScript-ergonomic helpers that make it easy to write typed call sites
// without manual `returnType` hints.
//
// Note on TypeScript: this package is implemented in JavaScript with JSDoc
// annotations and ambient `.d.ts` declarations.  No TypeScript source files
// are used.  TypeScript projects consume the package via the declaration
// files; the runtime is plain JavaScript.

export * from "@hyperpolymath/affine-js";
import {
  AffineModule as _AffineModule,
  run as _run,
  unmarshal,
  marshal,
  int,
  float,
  bool,
  unit,
  string,
  some,
  none,
  ok,
  err,
  array,
  record,
} from "@hyperpolymath/affine-js";

// ── Typed call helpers ────────────────────────────────────────────────────────
//
// These helpers wrap AffineModule.call with a fixed `returnType` hint so
// call sites read as typed calls rather than configuration objects.

/**
 * Call an exported function and interpret the result as an Int.
 *
 * @param {_AffineModule} mod
 * @param {string} name
 * @param {...import("@hyperpolymath/affine-js").AffineValue} args
 * @returns {{ kind: "int"; value: number }}
 */
export function callInt(mod, name, ...args) {
  return /** @type {{ kind: "int"; value: number }} */ (
    mod.call(name, { returnType: "int" }, ...args)
  );
}

/**
 * Call an exported function and interpret the result as a Float.
 *
 * @param {_AffineModule} mod
 * @param {string} name
 * @param {...import("@hyperpolymath/affine-js").AffineValue} args
 * @returns {{ kind: "float"; value: number }}
 */
export function callFloat(mod, name, ...args) {
  return /** @type {{ kind: "float"; value: number }} */ (
    mod.call(name, { returnType: "float" }, ...args)
  );
}

/**
 * Call an exported function and interpret the result as a Bool.
 *
 * @param {_AffineModule} mod
 * @param {string} name
 * @param {...import("@hyperpolymath/affine-js").AffineValue} args
 * @returns {{ kind: "bool"; value: boolean }}
 */
export function callBool(mod, name, ...args) {
  return /** @type {{ kind: "bool"; value: boolean }} */ (
    mod.call(name, { returnType: "bool" }, ...args)
  );
}

/**
 * Call an exported function and interpret the result as a String.
 *
 * @param {_AffineModule} mod
 * @param {string} name
 * @param {...import("@hyperpolymath/affine-js").AffineValue} args
 * @returns {{ kind: "string"; value: string }}
 */
export function callString(mod, name, ...args) {
  return /** @type {{ kind: "string"; value: string }} */ (
    mod.call(name, { returnType: "string" }, ...args)
  );
}

/**
 * Call an exported function and interpret the result as an Option.
 *
 * @param {_AffineModule} mod
 * @param {string} name
 * @param {...import("@hyperpolymath/affine-js").AffineValue} args
 * @returns {import("@hyperpolymath/affine-js").AffineSome | import("@hyperpolymath/affine-js").AffineNone}
 */
export function callOption(mod, name, ...args) {
  return mod.call(name, { returnType: "option" }, ...args);
}

/**
 * Call an exported function and interpret the result as a Result.
 *
 * @param {_AffineModule} mod
 * @param {string} name
 * @param {...import("@hyperpolymath/affine-js").AffineValue} args
 * @returns {import("@hyperpolymath/affine-js").AffineOk | import("@hyperpolymath/affine-js").AffineErr}
 */
export function callResult(mod, name, ...args) {
  return mod.call(name, { returnType: "result" }, ...args);
}

// ── Narrowing helpers ─────────────────────────────────────────────────────────

/**
 * Assert that an AffineValue is Some and extract the payload.
 *
 * @param {import("@hyperpolymath/affine-js").AffineValue} value
 * @returns {import("@hyperpolymath/affine-js").AffineValue}
 */
export function expectSome(value) {
  if (value.kind !== "some") {
    throw new Error(`affine-ts: expected Some, got ${value.kind}`);
  }
  return value.value;
}

/**
 * Assert that an AffineValue is Ok and extract the payload.
 *
 * @param {import("@hyperpolymath/affine-js").AffineValue} value
 * @returns {import("@hyperpolymath/affine-js").AffineValue}
 */
export function expectOk(value) {
  if (value.kind !== "ok") {
    const msg = value.kind === "err" && value.value.kind === "string"
      ? value.value.value
      : value.kind;
    throw new Error(`affine-ts: expected Ok, got Err(${msg})`);
  }
  return value.value;
}

/**
 * Check if an AffineValue is None.
 *
 * @param {import("@hyperpolymath/affine-js").AffineValue} value
 * @returns {boolean}
 */
export function isNone(value) {
  return value.kind === "none";
}

/**
 * Check if an AffineValue is Some.
 *
 * @param {import("@hyperpolymath/affine-js").AffineValue} value
 * @returns {value is import("@hyperpolymath/affine-js").AffineSome}
 */
export function isSome(value) {
  return value.kind === "some";
}

/**
 * Check if an AffineValue is Ok.
 *
 * @param {import("@hyperpolymath/affine-js").AffineValue} value
 * @returns {value is import("@hyperpolymath/affine-js").AffineOk}
 */
export function isOk(value) {
  return value.kind === "ok";
}

/**
 * Check if an AffineValue is Err.
 *
 * @param {import("@hyperpolymath/affine-js").AffineValue} value
 * @returns {value is import("@hyperpolymath/affine-js").AffineErr}
 */
export function isErr(value) {
  return value.kind === "err";
}
