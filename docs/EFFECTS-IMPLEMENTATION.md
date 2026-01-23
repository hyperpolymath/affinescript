# Effect Handler Implementation

## Overview

AffineScript supports algebraic effects and handlers, a powerful abstraction for computational effects. This document describes the current implementation status and limitations.

## Syntax

### Effect Declaration

```affinescript
effect Console {
  fn print(s: String) -> ();
  fn read() -> String;
}
```

### Using Effects

```affinescript
fn greet() -> String / Console {
  print("Hello!");
  return read();
}
```

### Effect Handlers

```affinescript
handle greet() {
  print(s) => {
    // Handle print operation
    resume ();
  },
  read() => {
    // Handle read operation
    resume "Alice";
  },
  return x => {
    // Handle return value
    return x;
  }
}
```

## Implementation Status

### ✓ Implemented

1. **Effect Declarations** - Effect types are declared with operation signatures
2. **Effect Operations as Builtins** - Each operation becomes a special builtin function
3. **Basic Handler Matching** - Handlers can match and handle effect operations
4. **HandlerReturn** - Handlers can intercept the final return value
5. **Error Propagation** - Unhandled effects propagate as errors

### ⚠️ Limitations

1. **No Continuations** - The current implementation lacks proper delimited continuations
   - Effects only work at the top level of the handled expression
   - `resume` evaluates to a value but doesn't continue the computation
   - Nested computations with multiple effects don't work correctly

2. **Single-Shot Effects** - Each handler invocation processes one effect operation
   - Computations that perform multiple effects in sequence won't work as expected

### ❌ Not Yet Implemented

1. **Proper Resume** - Resume should continue the suspended computation
2. **Effect Polymorphism** - Generic effect signatures
3. **Effect Subtyping** - Effect hierarchies and refinement
4. **Multi-Resume** - Invoking continuation multiple times

## Examples

### Working Example

```affinescript
effect Ask {
  fn get_value() -> Int;
}

fn main() -> Int {
  return handle get_value() {
    get_value() => {
      return 42;
    }
  };
}
```

This works because `get_value()` is at the top level of the handled expression.

### Not Yet Working

```affinescript
effect State {
  fn get() -> Int;
  fn set(x: Int) -> ();
}

fn increment() -> Int {
  let current = get();  // First effect
  set(current + 1);     // Second effect
  return current + 1;
}

fn main() -> Int {
  return handle increment() {
    get() => { return 0; },
    set(x) => { return (); }
  };
}
```

This doesn't work correctly because:
1. When `get()` is performed, the handler returns `0`
2. But the computation doesn't continue to execute `set(current + 1)`
3. The handler's return value becomes the result of the entire `handle` expression

## Technical Details

### Implementation Approach

The current implementation uses an error-based approach:

1. Effect operations are represented as builtin functions that raise `PerformEffect` errors
2. The `handle` expression catches these errors
3. Handlers are matched against the operation name
4. The handler body is evaluated and its result is returned

### Value Type

```ocaml
type eval_error =
  | ...
  | PerformEffect of string * value list  (* operation name, arguments *)
```

### Handler Evaluation

```ocaml
and eval_handle (env : env) (body : expr) (handlers : handler_arm list) : value result =
  match eval env body with
  | Ok value -> (* Check for HandlerReturn *)
  | Error (PerformEffect (op_name, args)) -> (* Match and invoke handler *)
  | Error other_err -> (* Propagate error *)
```

## Future Work

### Phase 2: Delimited Continuations

To fully support algebraic effects, we need to implement delimited continuations:

1. **Capture Continuation** - When an effect is performed, capture the rest of the computation
2. **Pass to Handler** - Make the continuation available to the handler via `resume`
3. **Invoke Continuation** - `resume(value)` continues the suspended computation with the provided value

### Implementation Options

1. **CPS Transformation** - Convert the entire interpreter to continuation-passing style
   - Pros: Clean semantics, full control
   - Cons: Major refactoring required

2. **Stack-Based Continuations** - Maintain an explicit call stack
   - Pros: More efficient than CPS
   - Cons: Complex state management

3. **Effect Handlers Library** - Use OCaml's effect handlers (OCaml 5.0+)
   - Pros: Efficient, native support
   - Cons: Requires OCaml 5.0+, different semantics

## References

- [Algebraic Effects for Functional Programming](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/08/algeff-tr-2016-v2.pdf)
- [Programming with Algebraic Effects and Handlers](https://arxiv.org/abs/1203.1539)
- [Eff programming language](https://www.eff-lang.org/)

## Testing

See `tests/effects/` for effect handler test cases.

Current test coverage:
- ✓ Basic effect operation and handler
- ✗ Multiple operations in sequence (known limitation)
- ✗ Nested handlers (not yet implemented)
- ✗ Effect polymorphism (not yet implemented)
