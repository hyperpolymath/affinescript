// SPDX-License-Identifier: MIT
// Phase 3 slice 2 fixture: record types (-> struct) and generics.
//   type point   = {x: int, y: int}  -> struct Point { x: Int, y: Int }
//   type box<'a> = {value: 'a}       -> struct Box[A] { value: A }
//   type id<'a>  = 'a                 -> type Id[A] = A
// Records with `mutable` or optional `?` fields are SKIPPED (their
// semantics can't be dropped).

type point = {
  x: int,
  y: int,
}

type box<'a> = {
  value: 'a,
}

type id<'a> = 'a

// SKIPPED: mutable field — AffineScript struct fields can't carry it.
type counter = {
  mutable count: int,
}

// SKIPPED: optional field — `?` has no struct equivalent.
type config = {
  verbose?: bool,
}
