// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// AffineScript: ReScript bindings for the affine-js interop layer
//
// This module provides idiomatic ReScript access to compiled AffineScript
// WASM modules.  Under the hood it delegates to @hyperpolymath/affine-js.
//
// Usage:
//
//   open AffineRes
//
//   let main = async () => {
//     let mod = await AffineScript.fromFile("./program.wasm")
//     let result = mod->AffineScript.callInt("compute", [Value.Int(42)])
//     Console.log(result)
//   }

open AffineScriptValue

// ── JS interop layer — bind to affine-js ─────────────────────────────────────

/** Opaque type wrapping a `AffineModule` instance from affine-js. */
type moduleHandle

@module("@hyperpolymath/affine-js") @scope("AffineModule")
external _fromFile: (string, Js.Json.t) => promise<moduleHandle> = "fromFile"

@module("@hyperpolymath/affine-js") @scope("AffineModule")
external _fromBytes: (Js.TypedArray2.Uint8Array.t, Js.Json.t) => promise<moduleHandle> =
  "fromBytes"

@send
external _call: (moduleHandle, string, Js.Json.t, array<Js.Json.t>) => Js.Json.t = "call"

@send
external _runMain: (moduleHandle, Js.Json.t) => Js.Json.t = "runMain"

@send
external _functionExports: moduleHandle => array<string> = "functionExports"

// ── Public API ────────────────────────────────────────────────────────────────

/** An instantiated AffineScript WASM module. */
type t = {
  handle: moduleHandle,
}

/**
 * Load a compiled AffineScript `.wasm` file.
 *
 *   let mod = await AffineScript.fromFile("./hello.wasm")
 */
let fromFile = async (path: string): t => {
  let handle = await _fromFile(path, %raw(`{}`))
  {handle: handle}
}

/**
 * Load a compiled AffineScript module from raw WASM bytes.
 *
 *   let bytes = await Deno.readFile("./hello.wasm")
 *   let mod = await AffineScript.fromBytes(bytes)
 */
let fromBytes = async (bytes: Js.TypedArray2.Uint8Array.t): t => {
  let handle = await _fromBytes(bytes, %raw(`{}`))
  {handle: handle}
}

/**
 * Call a named export and return an AffineScriptValue.t.
 *
 *   let result = mod->AffineScript.call("add", ~returnType="int", [Value.Int(3), Value.Int(4)])
 */
let call = (mod: t, name: string, ~returnType: string="int", args: array<t>): t => {
  let opts = %raw(`{ returnType: returnType }`)
  let jsArgs = args->Belt.Array.map(v => v->Obj.magic->toJs)
  let raw = _call(mod.handle, name, opts, jsArgs)
  ofJs(raw)->Obj.magic
}

/**
 * Run the top-level `main` export.
 *
 *   mod->AffineScript.runMain()
 */
let runMain = (mod: t): t => {
  let raw = _runMain(mod.handle, %raw(`{ returnType: "unit" }`))
  ofJs(raw)->Obj.magic
}

/** Names of all exported functions in this module. */
let functionExports = (mod: t): array<string> =>
  _functionExports(mod.handle)

// ── Typed call shortcuts ──────────────────────────────────────────────────────

/** Call an export and extract the result as a ReScript int. */
let callInt = (mod: t, name: string, args: array<t>): int =>
  mod->call(name, ~returnType="int", args)->expectInt

/** Call an export and extract the result as a ReScript float. */
let callFloat = (mod: t, name: string, args: array<t>): float =>
  mod->call(name, ~returnType="float", args)->expectFloat

/** Call an export and extract the result as a ReScript bool. */
let callBool = (mod: t, name: string, args: array<t>): bool =>
  mod->call(name, ~returnType="bool", args)->expectBool

/** Call an export and extract the result as a ReScript string. */
let callString = (mod: t, name: string, args: array<t>): string =>
  mod->call(name, ~returnType="string", args)->expectString

/** Call an export and return as a Belt option. */
let callOption = (mod: t, name: string, args: array<t>): option<AffineScriptValue.t> =>
  mod->call(name, ~returnType="option", args)->toOption

/** Call an export and unwrap Ok or raise on Err. */
let callOk = (mod: t, name: string, args: array<t>): AffineScriptValue.t =>
  mod->call(name, ~returnType="result", args)->expectOk
