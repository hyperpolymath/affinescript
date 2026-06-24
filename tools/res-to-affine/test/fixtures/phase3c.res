// SPDX-License-Identifier: MPL-2.0
// Phase 3 slice 3 fixture: module-level `let <id> = <literal>` -> `const`.
//   let answer = 42       -> const answer: Int = 42;
//   let pi = 3.14         -> const pi: Float = 3.14;
//   let greeting = "hi"   -> const greeting: String = "hi";
//   let enabled = true    -> const enabled: Bool = true;
// Non-literal / ref / destructuring bindings are SKIPPED (not compile-time
// constants, or not a plain identifier).

let answer = 42

let pi = 3.14

let greeting = "hi"

let enabled = true

let disabled = false

// SKIPPED: non-literal body (a call) — not a compile-time constant.
let now = Date.now()

// SKIPPED: ref body is the mutable-global anti-pattern, not a const.
let counter = ref(0)

// SKIPPED: destructuring pattern, not a plain identifier.
let (a, b) = (1, 2)
