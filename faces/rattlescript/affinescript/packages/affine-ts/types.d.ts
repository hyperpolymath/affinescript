// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-ts: Enhanced TypeScript declarations
//
// Re-exports everything from affine-js and adds affine-ts typed helpers.

export * from "@hyperpolymath/affine-js";

import type {
  AffineModule,
  AffineValue,
  AffineSome,
  AffineNone,
  AffineOk,
  AffineErr,
  AffineInt,
  AffineFloat,
  AffineBool,
  AffineString,
} from "@hyperpolymath/affine-js";

// ── Typed call helpers ────────────────────────────────────────────────────────

/** Call an export and coerce the return value to Int. */
export declare function callInt(mod: AffineModule, name: string, ...args: AffineValue[]): AffineInt;

/** Call an export and coerce the return value to Float. */
export declare function callFloat(mod: AffineModule, name: string, ...args: AffineValue[]): AffineFloat;

/** Call an export and coerce the return value to Bool. */
export declare function callBool(mod: AffineModule, name: string, ...args: AffineValue[]): AffineBool;

/** Call an export and coerce the return value to String. */
export declare function callString(mod: AffineModule, name: string, ...args: AffineValue[]): AffineString;

/** Call an export and coerce the return value to Option. */
export declare function callOption(mod: AffineModule, name: string, ...args: AffineValue[]): AffineSome | AffineNone;

/** Call an export and coerce the return value to Result. */
export declare function callResult(mod: AffineModule, name: string, ...args: AffineValue[]): AffineOk | AffineErr;

// ── Narrowing helpers ─────────────────────────────────────────────────────────

/** Assert Some and extract payload; throws on None. */
export declare function expectSome(value: AffineValue): AffineValue;

/** Assert Ok and extract payload; throws on Err with message. */
export declare function expectOk(value: AffineValue): AffineValue;

/** Type-narrowing predicate: is this value None? */
export declare function isNone(value: AffineValue): value is AffineNone;

/** Type-narrowing predicate: is this value Some? */
export declare function isSome(value: AffineValue): value is AffineSome;

/** Type-narrowing predicate: is this value Ok? */
export declare function isOk(value: AffineValue): value is AffineOk;

/** Type-narrowing predicate: is this value Err? */
export declare function isErr(value: AffineValue): value is AffineErr;
