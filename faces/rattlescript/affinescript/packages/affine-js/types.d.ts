// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-js: TypeScript declarations
//
// These are ambient type declarations for the affine-js package.
// They allow TypeScript projects to consume affine-js with full type safety
// without requiring TypeScript source code in this package.
//
// Usage in a TypeScript/tsconfig project:
//   import { AffineModule, run, int, string } from "@hyperpolymath/affine-js";

// ── AffineValue discriminated union ──────────────────────────────────────────

export type AffineInt    = { kind: "int";    value: number };
export type AffineFloat  = { kind: "float";  value: number };
export type AffineBool   = { kind: "bool";   value: boolean };
export type AffineUnit   = { kind: "unit" };
export type AffineString = { kind: "string"; value: string };
export type AffineSome   = { kind: "some";   value: AffineValue };
export type AffineNone   = { kind: "none" };
export type AffineOk     = { kind: "ok";     value: AffineValue };
export type AffineErr    = { kind: "err";    value: AffineValue };
export type AffineArray  = { kind: "array";  elements: AffineValue[] };
export type AffineRecord = { kind: "record"; fields: Record<string, AffineValue> };

export type AffineValue =
  | AffineInt
  | AffineFloat
  | AffineBool
  | AffineUnit
  | AffineString
  | AffineSome
  | AffineNone
  | AffineOk
  | AffineErr
  | AffineArray
  | AffineRecord;

// ── Return type hint ──────────────────────────────────────────────────────────

export type AffineValueType =
  | "int"
  | "float"
  | "bool"
  | "unit"
  | "string"
  | "option"
  | "result"
  | "array"
  | "record";

// ── LoadOptions ───────────────────────────────────────────────────────────────

export interface LoadOptions {
  /**
   * Extra host imports merged with the default AffineScript runtime.
   *
   * Keys must match the import names the AffineScript codegen emits for your
   * declared effects.  The naming convention is `affine_<effect>_<op>`, e.g.
   * `affine_io_println` for the `println` operation of the `IO` effect.
   *
   * @example
   * { imports: { affine_io_println: (ptr: number) => console.log(readString(ptr)) } }
   */
  imports?: Record<string, (...args: number[]) => number | void>;
}

// ── CallOptions ───────────────────────────────────────────────────────────────

export interface CallOptions extends LoadOptions {
  /**
   * Type hint for the return value unmarshaler.
   *
   * Because WASM erases high-level type information at the binary level,
   * the host must be told how to interpret raw return values.
   * @default "int"
   */
  returnType?: AffineValueType;
}

// ── AffineModule ──────────────────────────────────────────────────────────────

export declare class AffineModule {
  /**
   * Load a compiled AffineScript `.wasm` file from the local filesystem.
   */
  static fromFile(path: string | URL, options?: LoadOptions): Promise<AffineModule>;

  /**
   * Load a compiled AffineScript module from raw WASM bytes.
   */
  static fromBytes(bytes: Uint8Array | ArrayBuffer, options?: LoadOptions): Promise<AffineModule>;

  /**
   * Call a named export by name.
   */
  call(name: string, options?: CallOptions, ...args: AffineValue[]): AffineValue;

  /**
   * Invoke the top-level `main` function.
   */
  runMain(options?: CallOptions): AffineValue;

  /** Names of all exported functions. */
  readonly functionExports: string[];

  /** Underlying WASM linear memory. */
  readonly memory: WebAssembly.Memory;

  /** Underlying `WebAssembly.Instance`. */
  readonly instance: WebAssembly.Instance;
}

// ── Top-level functions ───────────────────────────────────────────────────────

/** Load and run a compiled AffineScript program in one call. */
export declare function run(path: string | URL, options?: LoadOptions): Promise<AffineValue>;

/** Marshal an AffineValue to a raw WASM operand. */
export declare function marshal(
  value: AffineValue,
  memory: WebAssembly.Memory,
  alloc: (n: number) => number,
): number;

/** Unmarshal a raw WASM operand to an AffineValue. */
export declare function unmarshal(
  raw: number,
  valueType: AffineValueType,
  memory: WebAssembly.Memory,
): AffineValue;

// ── AffineValue constructors ──────────────────────────────────────────────────

export declare function int(value: number): AffineInt;
export declare function float(value: number): AffineFloat;
export declare function bool(value: boolean): AffineBool;
export declare function unit(): AffineUnit;
export declare function string(value: string): AffineString;
export declare function some(value: AffineValue): AffineSome;
export declare function none(): AffineNone;
export declare function ok(value: AffineValue): AffineOk;
export declare function err(value: AffineValue): AffineErr;
export declare function array(elements: AffineValue[]): AffineArray;
export declare function record(fields: Record<string, AffineValue>): AffineRecord;

// ── Runtime constants ─────────────────────────────────────────────────────────

export declare const AFFINE_TAG: Readonly<{ NONE: 0; SOME: 1; OK: 2; ERR: 3 }>;
export declare const AFFINE_SIZE: Readonly<{ INT: 4; FLOAT: 8; PTR: 4; TAG: 4; LEN: 4 }>;
