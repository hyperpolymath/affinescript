// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// AffineScriptValue: ReScript type definitions for AffineScript WASM values
//
// Mirrors the AffineValue tagged union from affine-js as a proper ReScript
// variant type, giving ReScript call sites exhaustive pattern matching.

/** A value crossing the JS ↔ AffineScript WASM boundary. */
type t =
  | Int(int)
  | Float(float)
  | Bool(bool)
  | Unit
  | String(string)
  | Some(t)
  | None
  | Ok(t)
  | Err(t)
  | Array(array<t>)
  | Record(Js.Dict.t<t>)

/** Convert an AffineScriptValue.t to the plain JS object shape that affine-js
    expects.  Used internally by AffineScript bindings. */
let rec toJs = (v: t): Js.Json.t =>
  switch v {
  | Int(n) => %raw(`{ kind: "int", value: n }`)
  | Float(f) => %raw(`{ kind: "float", value: f }`)
  | Bool(b) => %raw(`{ kind: "bool", value: b }`)
  | Unit => %raw(`{ kind: "unit" }`)
  | String(s) => %raw(`{ kind: "string", value: s }`)
  | Some(inner) => %raw(`{ kind: "some", value: AffineScriptValue.toJs(inner) }`)
  | None => %raw(`{ kind: "none" }`)
  | Ok(inner) => %raw(`{ kind: "ok", value: AffineScriptValue.toJs(inner) }`)
  | Err(inner) => %raw(`{ kind: "err", value: AffineScriptValue.toJs(inner) }`)
  | Array(elems) =>
    let jsElems = elems->Belt.Array.map(toJs)
    %raw(`{ kind: "array", elements: jsElems }`)
  | Record(fields) =>
    let jsFields = Js.Dict.map((. v) => toJs(v), fields)
    %raw(`{ kind: "record", fields: jsFields }`)
  }

/** Convert the plain JS object shape from affine-js to an AffineScriptValue.t.
    Raises Js.Exn.raiseError on unknown kind tags. */
let rec ofJs = (obj: Js.Json.t): t => {
  let kind = %raw(`obj.kind`)
  switch (kind: string) {
  | "int" => Int(%raw(`obj.value | 0`))
  | "float" => Float(%raw(`obj.value`))
  | "bool" => Bool(%raw(`obj.value`))
  | "unit" => Unit
  | "string" => String(%raw(`obj.value`))
  | "some" => Some(ofJs(%raw(`obj.value`)))
  | "none" => None
  | "ok" => Ok(ofJs(%raw(`obj.value`)))
  | "err" => Err(ofJs(%raw(`obj.value`)))
  | "array" =>
    let elems: array<Js.Json.t> = %raw(`obj.elements`)
    Array(elems->Belt.Array.map(ofJs))
  | "record" =>
    let fields: Js.Dict.t<Js.Json.t> = %raw(`obj.fields`)
    Record(Js.Dict.map((. v) => ofJs(v), fields))
  | other =>
    Js.Exn.raiseError(`affine-res: unknown AffineValue kind '${other}'`)
  }
}

/** Convenience: extract an Int value or raise. */
let expectInt = (v: t): int =>
  switch v {
  | Int(n) => n
  | _ => Js.Exn.raiseError(`affine-res: expected Int, got ${Obj.magic(v).kind}`)
  }

/** Convenience: extract a Float value or raise. */
let expectFloat = (v: t): float =>
  switch v {
  | Float(f) => f
  | _ => Js.Exn.raiseError(`affine-res: expected Float, got ${Obj.magic(v).kind}`)
  }

/** Convenience: extract a Bool value or raise. */
let expectBool = (v: t): bool =>
  switch v {
  | Bool(b) => b
  | _ => Js.Exn.raiseError(`affine-res: expected Bool, got ${Obj.magic(v).kind}`)
  }

/** Convenience: extract a String value or raise. */
let expectString = (v: t): string =>
  switch v {
  | String(s) => s
  | _ => Js.Exn.raiseError(`affine-res: expected String, got ${Obj.magic(v).kind}`)
  }

/** Convenience: extract an Ok payload or raise with Err contents. */
let expectOk = (v: t): t =>
  switch v {
  | Ok(payload) => payload
  | Err(e) =>
    let msg = switch e {
    | String(s) => s
    | _ => "(non-string error)"
    }
    Js.Exn.raiseError(`affine-res: expected Ok, got Err(${msg})`)
  | _ => Js.Exn.raiseError(`affine-res: expected Result, got ${Obj.magic(v).kind}`)
  }

/** Convenience: extract a Some payload or return None as a Belt option. */
let toOption = (v: t): option<t> =>
  switch v {
  | Some(payload) => Stdlib.Some(payload)
  | None => Stdlib.None
  | _ => Js.Exn.raiseError(`affine-res: expected Option, got ${Obj.magic(v).kind}`)
  }
