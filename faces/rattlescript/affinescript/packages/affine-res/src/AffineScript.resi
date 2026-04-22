// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// AffineScript.resi: Public interface for the AffineScript ReScript bindings

open AffineScriptValue

/** An instantiated AffineScript WASM module. */
type t

/** Load a compiled `.wasm` file. */
let fromFile: string => promise<t>

/** Load a compiled module from raw bytes. */
let fromBytes: Js.TypedArray2.Uint8Array.t => promise<t>

/** Call a named export, returning a generic AffineScriptValue. */
let call: (t, string, ~returnType: string=?, array<t>) => t

/** Run the top-level `main` export. */
let runMain: t => t

/** Names of all exported functions. */
let functionExports: t => array<string>

/** Call and extract result as int. Raises on wrong type. */
let callInt: (t, string, array<t>) => int

/** Call and extract result as float. Raises on wrong type. */
let callFloat: (t, string, array<t>) => float

/** Call and extract result as bool. Raises on wrong type. */
let callBool: (t, string, array<t>) => bool

/** Call and extract result as string. Raises on wrong type. */
let callString: (t, string, array<t>) => string

/** Call and return as Belt option. Raises if not Option type. */
let callOption: (t, string, array<t>) => option<AffineScriptValue.t>

/** Call and unwrap Ok. Raises on Err with error message. */
let callOk: (t, string, array<t>) => AffineScriptValue.t
