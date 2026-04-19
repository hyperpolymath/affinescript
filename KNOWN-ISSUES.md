<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# Known AffineScript codegen issues

Minimal reproducers for codegen bugs discovered while using AffineScript
downstream. Please open a proper GitHub issue (or link an existing one)
before starting a fix so the fix can be attributed.

> **Status 2026-04-19:** both issues below are FIXED in `lib/codegen.ml`.
> Regression coverage lives in `developer-ecosystem/affinescript-ecosystem/
> affinescript-deno-test/example/codegen_regression_test.affine` and the
> double-track-browser `extension_lifecycle_test.affine` (10/10 green).
> The reproducers are retained for history; workarounds below are no
> longer needed.

---

## Issue 1 — `match` on enum with distinct zero-arity constructors per arm emits invalid WASM

**Discovered:** 2026-04-19 while writing `web-ecosystem/double-track-browser/tests/affine/extension_lifecycle_test.affine`.

**Symptom:** `affinescript compile` succeeds. The emitted `.wasm` is rejected by every WebAssembly validator (tested: V8 via Deno):

```
CompileError: WebAssembly.compile(): Compiling function #1 failed:
  expected 1 elements on the stack for fallthru, found 2
```

**Minimal reproducer:**

```affine
enum Lifecycle {
  A,
  B,
  C(Int)
}

fn step(s: Lifecycle) -> Lifecycle {
  match s {
    A => B(),
    B => B(),
    C(n) => C(n)
  }
}
```

**Workaround:** Downgrade enums to tagged structs when the match must
return distinct zero-arity constructors across arms:

```affine
struct State {
  tag: Int,
  // ...payload fields
}
```

**Root cause:** `gen_pattern` for `PatCon` with args saved the tag-test
boolean via `LocalTee` on a fresh `__match_result` local and then
pushed the same value via `LocalGet` at the end of the pattern code,
leaving the stack with TWO booleans where the enclosing `If` expected
ONE. Bindings between save and restore are stack-neutral by
construction, so the save/restore was redundant.

**Fix:** remove the LocalTee/LocalGet pair, leaving the `I32Eq` result
directly on the stack. (`lib/codegen.ml`, `gen_pattern` / `PatCon` arm.)
Secondary fix in the same commit: `ExprVar` falls back to
`ctx.variant_tags` when `lookup_local` misses, so bare `Initialised`
(parens omitted) is accepted as a zero-arity constructor expression —
the parser accepts the form, but codegen previously failed with
`UnboundVariable`.

---

## Issue 2 — Non-first struct-field read from function parameter returns 0

**Discovered:** 2026-04-19 in the same pilot.

**Symptom:** Reading `s.tag` (field at offset 0) from a function parameter
`s: State` works correctly. Reading `s.profile_id` (field at offset 1) or
`s.activity_count` (field at offset 2) from the same parameter always
returns 0, regardless of the actual value stored in the struct. Reading
the same fields from a let-bound local of the same struct type works
fine.

**Minimal reproducer:**

```affine
struct State {
  tag: Int,
  profile_id: Int,
  activity_count: Int
}

fn build_state(pid: Int) -> State {
  { tag: 2, profile_id: pid, activity_count: 0 }
}

fn read_pid(s: State) -> Int {
  s.profile_id
}

pub fn test_direct_local() -> Bool {
  let s = { tag: 2, profile_id: 42, activity_count: 0 };
  s.profile_id == 42           // PASSES
}

pub fn test_local_from_builder() -> Bool {
  let s = build_state(42);
  s.profile_id == 42           // PASSES
}

pub fn test_via_helper() -> Bool {
  let s = build_state(42);
  read_pid(s) == 42            // FAILS: read_pid returns 0
}
```

**Workaround:** Pass individual scalars instead of a struct, or read
struct fields into local variables at the call site before handing off
to a helper:

```affine
fn read_pid_scalar(_tag: Int, pid: Int, _count: Int) -> Int {
  pid
}

pub fn test_via_scalar_helper() -> Bool {
  let s = build_state(42);
  read_pid_scalar(s.tag, s.profile_id, s.activity_count) == 42
}
```

**Root cause:** the per-variable `field_layouts` map was populated
only by `let`-bindings whose RHS was a record literal. Every other
binding path — function parameters of struct type, let-bindings from
function calls returning struct, type-annotated lets — fell through
to a default offset of 0, so `.field_1_or_later` read the tag byte
instead of the real field.

**Fix:** register struct field layouts globally in
`ctx.struct_layouts` from `TopType(TyStruct)` decls, and propagate
them to (a) function parameters via `p_ty`, (b) call-result lets via
a new `fn_ret_structs` map populated from `fd_ret_ty`, (c) let-bindings
with an explicit type annotation (`sl_ty`), (d) let-bindings whose RHS
is another tracked variable. (`lib/codegen.ml`, `gen_function` /
`StmtLet` / `gen_decl`.)

---

## Tracking

Both issues were unblocking the `affinescript-deno-test` harness
(sibling component at `developer-ecosystem/affinescript-ecosystem/
affinescript-deno-test/`) for idiomatic enum + struct test idioms.
With these fixes the harness now runs the double-track-browser
extension-lifecycle pilot (10/10 green) and is ready to scale beyond
the MVP shape into the estate-wide TypeScript-test migration.

Estate tracker: `~/Desktop/AI-WORK-todo.md §11`.
