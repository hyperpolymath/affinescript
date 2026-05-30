// SPDX-License-Identifier: MIT
// Synthetic fixture for Phase 3 slice 1: structural type-declaration
// translation. The first three type declarations are fully structural
// and #228-independent, so the walker's --translate path renders them
// as compilable AffineScript:
//   type userId = int          -> type UserId = Int
//   type color  = Red | ...    -> type Color = | Red | Green | Blue
//   type shape  = Circle(...)  -> type Shape = | Circle(Float) | Rect(Int, Int)
// The trailing `let`/`switch` is NOT a type declaration and must remain a
// TODO island (absent from the translation list). The generic and
// qualified type decls below must also be skipped (slice 1 is monomorphic
// and unqualified).

type userId = int

type color =
  | Red
  | Green
  | Blue

type shape =
  | Circle(float)
  | Rect(int, int)

// Skipped in slice 1: type parameters (generic).
type box<'a> = Box('a)

// Skipped in slice 1: qualified-path RHS.
type theirMap = Belt.Map.t

// Not a type declaration: stays a TODO island.
let area = s =>
  switch s {
  | Circle(r) => r *. r
  | Rect(w, h) => float_of_int(w * h)
  }
