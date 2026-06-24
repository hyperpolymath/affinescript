// SPDX-License-Identifier: MPL-2.0
// Synthetic fixture exercising every Phase-1 anti-pattern. Not a real
// ReScript program; the scanner is line-based so it doesn't care.

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
