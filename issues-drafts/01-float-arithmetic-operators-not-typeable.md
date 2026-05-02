# Float arithmetic operators (`+`/`-`/`*`/`/`) fail to typecheck

**Surfaced by:** IDApTIK migration (Wave 3, 2026-05-02)
**Affected version:** v0.1.0 (`affinescript` compiler at HEAD as of 2026-05-02)
**Severity:** Blocking for ~all .res → .affine translations involving game math, physics, animation, geometry.

## Reproducer

```affinescript
fn pi_plus_pi() -> Float {
  3.14 + 3.14
}
```

```
$ affinescript check pi.affine
Unification error: (Unify.TypeMismatch (Int, Float))
affinescript: Type error
```

Smaller still:

```affinescript
fn add_floats(a: Float, b: Float) -> Float {
  a + b
}
```
Same error.

## What works

- Float literals are recognised and accepted in Float-returning positions:
  ```affinescript
  fn just_pi() -> Float { 3.14 }   // typechecks
  ```
- Stdlib functions returning Float compile fine: `affinescript/stdlib/Math.affine` declares `pub fn pi() -> Float { return 3.14159...; }` and that file typechecks.
- The interpreter implements Float operators (`affinescript/lib/value.ml`: `OpAdd → Ok (VFloat (a +. b))`). So this is a typechecker gap, not a codegen one.

## What does not work

The typechecker treats `+` / `-` / `*` / `/` / `<` / `>` / `<=` / `>=` as Int-typed, with no path to Float. There appears to be no overload resolution, no implicit Int→Float coercion, and no separate `+.` operator (as in OCaml) exposed at the surface.

## Why this matters for the migration

The IDApTIK codebase is a game engine with extensive Float math: collision (`combat/Hitbox.res`), physics, animation easing, screen positioning, particle effects, audio mixing. Without Float-arithmetic operators, every such file requires either:

1. **Int placeholders** with documented precision compromise (works for collision; bad for physics/easing).
2. **Hand-written FFI** to a Float-arithmetic helper (defeats the purpose of using AffineScript).
3. **Waiting on this issue.**

The first IDApTIK migration (`Hitbox.res` → `Hitbox.affine`) used option 1, with a header note flagging the compromise. Most other files that would benefit from translation are blocked on option 3.

## Suggested resolution shape

Either:

- **Polymorphic numeric operators** — `+ : ∀ N. (N, N) -> N` with `N` constrained to a `Numeric` typeclass (or row, or trait dictionary, depending on AffineScript's chosen mechanism). This is the most ergonomic.
- **Distinct float operators** at the surface (`+.`, `-.`, etc., OCaml-style). Less ergonomic, but unambiguous and matches the underlying machinery.
- **Default to `f64` and provide explicit `Int` operators** — Rust-style, but a much bigger semantic shift.

Option 2 is probably the smallest delta from the current state, given the interpreter already has separate Int and Float operations.

## Cross-reference

- `AI.a2ml` directives include "ergonomics first" and "the type system defaults must be sensible so that most code reads like modern JavaScript". A user expecting `3.14 + 3.14` to work is squarely within that target.
- The `frontier-guide.adoc` doesn't yet have a chapter on numerics; whichever resolution lands, that chapter should be written so future translators know what to expect.
