// SPDX-License-Identifier: MPL-2.0
// Synthetic fixture for Phase 3 slice 1: structural type-declaration
// translation. The first three type declarations are fully structural
// and #228-independent, so the walker's --translate path renders them
// as compilable AffineScript:
//   type userId = int          -> type UserId = Int
//   type color  = Red | ...    -> type Color = | Red | Green | Blue
//   type shape  = Circle(...)  -> type Shape = | Circle(Float) | Rect(Int, Int)
// The trailing `let`/`switch` is NOT a type declaration and stays a TODO
// island (absent from the translation list). The generic `box` below now
// translates too (slice 2: type parameters); the qualified-path decl stays
// skipped (qualified-path RHS is deferred — it would parse but not resolve).

type userId = int

type color =
  | Red
  | Green
  | Blue

type shape =
  | Circle(float)
  | Rect(int, int)

// Slice 2: type parameters now translate -> type Box[A] = | Box(A)
type box<'a> = Box('a)

// Skipped: qualified-path RHS (deferred).
type theirMap = Belt.Map.t

// Not a type declaration: stays a TODO island.
let area = s =>
  switch s {
  | Circle(r) => r *. r
  | Rect(w, h) => float_of_int(w * h)
  }
