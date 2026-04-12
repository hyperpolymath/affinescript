# Effect Handler Implementation

## Status Sync (2026-04-12)

This document tracks the implementation status of algebraic effects in
AffineScript and is aligned with `.machine_readable/6a2/STATE.a2ml`.

## Syntax (Current Parser Form)

### Effect Declaration

```affinescript
effect Console {
  fn print(s: String) -> ();
  fn read() -> String;
}
```

### Effect Annotation in Function Types

```affinescript
fn greet() -{Console}-> String {
  Console.print("Hello!");
  Console.read()
}
```

### Effect Handlers

```affinescript
handle greet() {
  print(s) => {
    resume("ok")
  },
  read() => {
    resume("Alice")
  },
  return(x) => x
}
```

## Current Implementation

### Implemented

1. Effect declarations and operation signatures.
2. Effect operations as builtins that raise `PerformEffect`.
3. Handler operation matching (`HandlerOp`).
4. Return-arm handling (`HandlerReturn`).
5. `resume` plumbing in the interpreter path.

### Backend Caveats

1. **Interpreter path**: effect handlers are implemented and usable.
2. **WASM 1.0 backend**: handler lowering is partial; advanced handler semantics
   do not fully map to backend continuations.
3. **WASM GC backend**: `handle` / `resume` are currently rejected with
   `UnsupportedFeature` where continuation semantics are required.

## Technical Notes

The interpreter represents effect operations via:

```ocaml
type eval_error =
  | ...
  | PerformEffect of string * value list
```

Handler evaluation catches `PerformEffect`, selects a matching arm, and
evaluates the arm body in the handler context.

## Not Yet Complete

1. Full continuation semantics in Wasm backends without relying on interpreter
   behavior.
2. A backend strategy for multi-resume semantics when needed.
3. End-to-end parity between interpreter and all codegen targets for advanced
   handler/control-flow combinations.

## Testing

See:

- `test/e2e/fixtures/effects.affine`
- `test/test_e2e.ml`

Run:

```bash
dune runtest
```
