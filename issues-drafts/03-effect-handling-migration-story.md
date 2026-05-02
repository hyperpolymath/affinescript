# Document the migration story for code that needs algebraic effect handling

**Surfaced by:** IDApTIK migration (Wave 3, 2026-05-02)
**Type:** Documentation / scope clarification, not a bug.
**Affected version:** v0.1.0; relevant to any release while effect handling stays out of scope.

## Context

`AI.a2ml` (the "Frontier Programming Practices — AI Edition" scope statement) is unambiguous:

> `algebraic-effect-handlers`
> `(reason "interaction-with-affine-is-unresolved")`
> `(note "Multi-shot resume of continuations that captured affine resources is a soundness hole; handlers are deferred until the design is explicit. Effect TRACKING — declaring what a function can do — is in scope. Effect HANDLING — intercepting and redirecting effects at runtime — is not.")`

This is a sound, deliberate position. **The issue here is not the position; the issue is the lack of guidance for migrators who hit the gap.**

## What the gap actually looks like in practice

A large fraction of any real `-script` codebase is *operations against shared mutable state, executed for their side effects*:

```rescript
// idaptik/src/app/GetEngine.res
let instance: ref<option<Engine.t>> = ref(None)
let get = (): option<Engine.t> => instance.contents
let set = (engine: Engine.t): unit => { instance := Some(engine) }
```

The migration playbook is clear that this should become a `State` effect:

> "If two callers need to see the same mutation, it is no longer local — lift it."

But effect HANDLING is out of scope, so the lifted form **declares** the intent without being able to **execute** it. A faithful migration produces a function signature like `fn get_engine() -> Option[Engine] / {State[EngineRef]}` that compiles, but with no way to actually wire up the State handler at runtime, the program cannot run. Not just the function — the entire program.

This means **a migrator who follows the playbook hits a dead-end on any file that reads or writes shared state.** Idaptik's audit found this is most of `src/app/`: navigation registries, engine singleton, popup/screen constructors, lobby registry, Burble adapter, voice bridge, persistence layers — all blocked at the same gate.

## What would help

A short doc — either a chapter in `frontier-guide.adoc` or a sibling under `docs/guides/effects-migration-stance.adoc` — that answers:

1. **What's the intended runtime model for code that conceptually needs `State`/`IO`/`Async` effects, in the absence of handlers?** Are migrators meant to:
   - (a) Defer translation entirely until handlers are designed?
   - (b) Hand-write FFI shims that wrap the JS-side mutation as opaque foreign primitives?
   - (c) Use linear/affine resources to encode state-passing manually (no shared cell, but threaded)?
   - (d) Something else?
2. **What's the expected timeline / sequencing?** Is handler design a 6-week problem, a 6-month problem, or a sibling-project (Typed WASM) problem? Migrators planning a multi-month effort need to know which.
3. **Are there idaptik-specific or general-pattern examples** showing the recommended workaround for the most common cases — singleton state, IO-emitting helpers, async network calls?

## Why a doc is enough (no code change needed)

The scope statement's reasoning ("multi-shot resume + affine = soundness hole") is the right reasoning. Rushing handlers in to unblock migration would be the wrong move. **The fix is a paragraph telling migrators what to do in the meantime, not a feature.**

Right now the migrator's experience is:

- Read `migration-playbook.adoc` → "lift mutation to a State effect"
- Try it → the function signature compiles
- Realize the program can't run because handlers aren't implemented
- Re-read `AI.a2ml` → "handlers are out of scope"
- ... and now what?

A short doc covering this gap closes the circle.

## Cross-reference

- IDApTIK's STATE.a2ml `[affinescript-v0_1_0-translatable-surface]` section catalogues the language gaps that block migration; this issue is the doc-side counterpart.
