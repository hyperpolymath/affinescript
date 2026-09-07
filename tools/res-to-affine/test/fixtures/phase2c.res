// SPDX-License-Identifier: MPL-2.0
// Synthetic fixture for the two anti-patterns that were explicitly
// deferred from Phase 1 entirely because they need real AST:
//   1. inline-callback-record — 3+ inline function values in a single
//      record literal or a single call's labelled-argument list
//   2. oversized-function    — function spans more than 50 source rows
// Walker-only by construction; the line-regex scanner does not detect
// either pattern.

open Types

// --- inline-callback-record: a record literal with 4 inline lambdas
let handlers = {
  onMount: () => Js.log("mounted"),
  onUnmount: () => Js.log("unmounted"),
  onClick: e => Js.log(e),
  onHover: e => Js.log(e),
}

// --- inline-callback-record at a call site: 3 labelled-argument lambdas
let _ = Widget.make(
  ~onMount=() => Js.log("mounted"),
  ~onUnmount=() => Js.log("unmounted"),
  ~onClick=e => Js.log(e),
)

// --- oversized-function: 60 source-row span. Body intentionally tedious
//     to surface the row-span proxy the walker uses.
let huge = id => {
  let a01 = id + 1
  let a02 = a01 + 1
  let a03 = a02 + 1
  let a04 = a03 + 1
  let a05 = a04 + 1
  let a06 = a05 + 1
  let a07 = a06 + 1
  let a08 = a07 + 1
  let a09 = a08 + 1
  let a10 = a09 + 1
  let a11 = a10 + 1
  let a12 = a11 + 1
  let a13 = a12 + 1
  let a14 = a13 + 1
  let a15 = a14 + 1
  let a16 = a15 + 1
  let a17 = a16 + 1
  let a18 = a17 + 1
  let a19 = a18 + 1
  let a20 = a19 + 1
  let a21 = a20 + 1
  let a22 = a21 + 1
  let a23 = a22 + 1
  let a24 = a23 + 1
  let a25 = a24 + 1
  let a26 = a25 + 1
  let a27 = a26 + 1
  let a28 = a27 + 1
  let a29 = a28 + 1
  let a30 = a29 + 1
  let a31 = a30 + 1
  let a32 = a31 + 1
  let a33 = a32 + 1
  let a34 = a33 + 1
  let a35 = a34 + 1
  let a36 = a35 + 1
  let a37 = a36 + 1
  let a38 = a37 + 1
  let a39 = a38 + 1
  let a40 = a39 + 1
  let a41 = a40 + 1
  let a42 = a41 + 1
  let a43 = a42 + 1
  let a44 = a43 + 1
  let a45 = a44 + 1
  let a46 = a45 + 1
  let a47 = a46 + 1
  let a48 = a47 + 1
  let a49 = a48 + 1
  let a50 = a49 + 1
  let a51 = a50 + 1
  let a52 = a51 + 1
  a52
}
