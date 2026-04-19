<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
# Known AffineScript codegen issues

Minimal reproducers for codegen bugs discovered while using AffineScript
downstream. Please open a proper GitHub issue (or link an existing one)
before starting a fix so the fix can be attributed.

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

**Likely location in compiler:** `lib/codegen.ml` around match-arm
lowering for enum construction sites.

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

**Likely location in compiler:** `lib/codegen.ml` around struct-field
access codegen for function-parameter locals (likely a wrong local
index or wrong offset calculation when the struct pointer is a function
parameter rather than a let-bound local).

---

## Tracking

Both issues block scaling the `affinescript-deno-test` harness (sibling
component at `developer-ecosystem/affinescript-ecosystem/
affinescript-deno-test/`) from its current 10-test pilot to the
estate-wide TypeScript-test migration in AI-WORK-todo.md §3c.

Estate tracker: `~/Desktop/AI-WORK-todo.md §11`.
