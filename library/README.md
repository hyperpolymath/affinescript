# AffineScript Standard Library

This directory contains the AffineScript standard library, organized into two main sections:

## Common Library (`common/`)

Language-agnostic utilities that could be used by any language:

- **prelude.afs** - Core types (Option, Result, Ordering), fundamental traits (Eq, Ord, Clone, Display, Iterator)
- **collections.afs** - Data structures (Vec, HashMap, HashSet, LinkedList, BinaryHeap, Deque)
- **io.afs** - IO effect, file operations, streams, buffered readers/writers
- **async.afs** - Async/await primitives, futures, channels, synchronization
- **string.afs** - String manipulation, character utilities, StringBuilder
- **math.afs** - Mathematical constants and functions, statistics, random numbers
- **time.afs** - Duration, Instant, SystemTime, DateTime, timing utilities
- **sync.afs** - Atomic types, spinlocks, RwLock, barriers, latches

## AffineScript Library (`affinescript/`)

AffineScript-specific features leveraging the type system:

- **linear.afs** - Linear/affine type utilities (LinearBox, Token, session types)
- **effects.afs** - Algebraic effect handlers (Reader, Writer, State, Exception, NonDet)
- **ownership.afs** - Ownership helpers (Box, Rc, Weak, Cow, RefCell)
- **refinements.afs** - Refinement types and dependent types (Positive, NonZero, Vec[N, T], Matrix)

## Usage

```afs
// Import everything from common
use Common::*;

// Import AffineScript prelude
use AffineScript::prelude::*;

// Import specific modules
use Common.Math::{sqrt, PI};
use AffineScript.Linear::LinearBox;
use AffineScript.Refinements::Positive;
```

## Design Philosophy

1. **Type Safety First** - Leverage AffineScript's type system to prevent errors at compile time
2. **Zero-Cost Abstractions** - Abstractions that compile away to efficient code
3. **Effect Tracking** - IO and other effects are explicit in function signatures
4. **Resource Safety** - Linear types ensure resources are properly managed

## Examples

### Option and Result
```afs
let x: Option[Int] = Some(42);
let y = x.map(|n| n * 2).unwrap_or(0);

let result: Result[Int, String] = Ok(10);
let value = result?;  // Propagates error
```

### Linear Resources
```afs
let handle = ResourceHandle::new(file, |f| f.close());
handle.use_with(|f| f.write("hello"));
handle.close()?;  // Must be called exactly once
```

### Effects
```afs
fn computation() -> Int / State[Int], Except[String] {
  let current = State::get();
  if current < 0 {
    Except::throw("negative state");
  }
  State::put(current + 1);
  current
}
```

### Refinement Types
```afs
fn safe_div(a: Int, b: NonZero) -> Int {
  a / b  // Statically guaranteed b != 0
}

let v: Vec[3, Int] = Vec::from_array([1, 2, 3]).unwrap();
let first = v.get::<0>();  // Compile-time bounds check
```
