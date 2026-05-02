# Array type syntax `[T]` not parseable in user source code

**Surfaced by:** IDApTIK migration (Wave 3 / Wave 4, 2026-05-02)
**Affected version:** v0.1.0 (`affinescript` compiler at HEAD as of 2026-05-02)
**Severity:** Blocking for the majority of idaptik's `.res / .ts → .affine` translation surface — almost every cross-component data type uses arrays/lists.

## Reproducer

```affinescript
fn first(xs: [Int]) -> Int {
  xs[0]
}
```

```
$ affinescript check first.affine
first.affine:1:14: parse error: Syntax error
affinescript: Parse error
```

Same error for arrays in struct fields:

```affinescript
struct Tags {
  names: [String]
}
```

```
parse error at column 10
```

## What works

The `[T]` syntax appears extensively in **stdlib** files and is treated as the list/array type:

```bash
$ grep -rE "[: \(]\[" affinescript/stdlib/
stdlib/math.affine:fn mean(values: [Float]) -> Float
stdlib/result.affine:fn collect<T, E>(results: [Result<T, E>]) -> Result<[T], E>
stdlib/prelude.affine:fn map<T, U>(arr: [T], f: T -> U) -> [U]
stdlib/collections.affine:fn reverse<T>(list: [T]) -> [T]
```

The stdlib parses (presumably under a different load path or different parser configuration), but freshly-authored user source does not.

## Why this matters for the migration

Arrays/lists are pervasive in the IDApTIK codebase being translated:

- Game state: device lists, enemy patrols, inventory items.
- Cross-component types: `array<(string, int)>` for register snapshots, puzzle hints, cable connections, etc.
- Polymorphic variants with array payloads: `VMStateChanged({registers: array<(string, int)>})`.
- Asset bundles, level configs, world items.

Without user-source array syntax, only files whose types are purely scalar + struct + plain enum are translatable. From the IDApTIK survey, this is a single-digit percentage of the corpus.

## Suspected cause

Stdlib files may be parsed via a special path that allows extra syntax forms (similar to the way OCaml stdlib uses internal-only constructs). If that's the case, the fix is to expose the same syntax to the user-facing parser — likely a one-line change in `lib/parser.mly` to match the same token sequence at module scope.

If the stdlib forms genuinely don't go through the same parser at all, that's a deeper integration question — the stdlib should typecheck via the same parser users do, otherwise the stdlib types are not observable to user code.

## Suggested resolution shape

Either:

- **Make `[T]` a parseable type expression in user source** — minimal change, matches what stdlib already uses.
- **Document an alternative array spelling** (e.g. `Array[T]`, `List<T>`) and update stdlib accordingly. More verbose, but easier to disambiguate from generic-parameter brackets.

Either path lets idaptik's Wave 4 (kernel types preview) and most of Wave 3 proceed.

## Cross-reference

- The `migration-playbook.adoc` ReScript→AffineScript table doesn't yet have a row for arrays — once this lands, that row should be added with the chosen syntax.
