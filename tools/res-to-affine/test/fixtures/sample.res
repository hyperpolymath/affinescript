// SPDX-License-Identifier: MIT
// hypatia:ignore cicd_rules/banned_language_file
// Synthetic fixture exercising every Phase-1 anti-pattern. Not a real
// ReScript program; the scanner is line-based so it doesn't care.
// The .res extension is intentional — this fixture is the input to the
// `.res → .affine` migration tool (issue #57); exempted from the banned-
// language policy via the inline pragma above (governance bundle reads
// the first 8 lines for `hypatia:ignore <rule>`).

open Types

// 1. side-effect import (Pixi sound modules)
let _ = Pixi.Sound.register

// 2. raw JS escape hatch
let host = %raw(`globalThis.location.host`)

// 3. mutable global ref + := assignment
let currentUser = ref(None)
currentUser := Some("alice")

// 4. untyped exception path
let fetchUser = id => {
  try {
    Some(GitHub.Users.get(id))
  } catch {
  | Js.Exn.Error(_) => None
  }
}

// 5. Promise.catch — different shape of the same anti-pattern
let load = () =>
  api->Promise.catch(e => Js.log(e))
