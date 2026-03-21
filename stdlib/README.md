# AffineScript Standard Library

The AffineScript standard library provides essential utilities and data structures.

## Modules

### Core
Basic utilities and operations.

**Functions:**
- `id[T](x: T) -> T` - Identity function
- `const[A, B](x: A, _y: B) -> A` - Constant function
- `compose[A, B, C](f, g)` - Function composition
- `flip[A, B, C](f)` - Flip function arguments
- `min(a, b)`, `max(a, b)`, `clamp(x, low, high)` - Numeric operations
- `abs(x)`, `sign(x)` - Absolute value and sign
- `not(x)`, `and(a, b)`, `or(a, b)`, `xor(a, b)` - Boolean operations

**Example:**
```affinescript
use Core::{min, max, abs};

let smallest = min(10, 20);      // 10
let largest = max(10, 20);        // 20
let absolute = abs(-42);          // 42
```

### Result
Error handling utilities for `Result[T, E]` type.

**Functions:**
- `is_ok(r)`, `is_err(r)` - Check result status
- `unwrap(r)`, `unwrap_or(r, default)` - Extract value
- `unwrap_err(r)` - Extract error
- `map(r, f)`, `map_err(r, f)` - Transform value or error
- `and_then(r, f)` - Chain operations (flatMap)
- `ok(r)`, `err(r)` - Convert to Option

**Example:**
```affinescript
use Result::{map, unwrap_or};

fn divide(a: Int, b: Int) -> Result[Int, String] {
  return if b == 0 {
    Err("division by zero")
  } else {
    Ok(a / b)
  };
}

let result = divide(10, 2);
let doubled = map(result, |x| { return x * 2; });
let value = unwrap_or(doubled, 0);  // 10
```

### Option
Optional value utilities for `Option[T]` type.

**Functions:**
- `is_some(opt)`, `is_none(opt)` - Check if value exists
- `unwrap(opt)`, `unwrap_or(opt, default)` - Extract value
- `unwrap_or_else(opt, f)` - Extract or compute default
- `map(opt, f)`, `map_or(opt, default, f)` - Transform value
- `and_then(opt, f)` - Chain operations (flatMap)
- `or(opt, other)`, `or_else(opt, f)` - Alternative values
- `filter(opt, pred)` - Filter by predicate
- `ok_or(opt, err)`, `ok_or_else(opt, f)` - Convert to Result

**Example:**
```affinescript
use Option::{map, unwrap_or};

fn find_positive(x: Int) -> Option[Int] {
  return if x > 0 { Some(x) } else { None };
}

let opt = find_positive(42);
let doubled = map(opt, |x| { return x * 2; });
let value = unwrap_or(doubled, 0);  // 84
```

### Math
Mathematical functions and constants.

**Constants:**
- `PI` = 3.14159...
- `E` = 2.71828...
- `TAU` = 6.28318... (2π)

**Integer Functions:**
- `abs(x)`, `min(a, b)`, `max(a, b)`, `clamp(x, low, high)`
- `pow(base, exp)` - Integer exponentiation
- `gcd(a, b)`, `lcm(a, b)` - Greatest common divisor and least common multiple
- `factorial(n)` - Factorial
- `fib(n)` - Fibonacci number
- `is_even(n)`, `is_odd(n)` - Parity checks

**Float Functions:**
- `abs_f(x)`, `min_f(a, b)`, `max_f(a, b)`, `clamp_f(x, low, high)`

**Example:**
```affinescript
use Math::{pow, gcd, factorial};

let squared = pow(5, 2);           // 25
let divisor = gcd(48, 18);         // 6
let perm = factorial(5);           // 120
```

## Usage

Import modules using the `use` statement:

```affinescript
// Import entire module
use Core;
let result = Core.abs(-10);

// Import specific functions
use Core::{min, max};
let smaller = min(5, 10);

// Import with alias
use Math as M;
let circle_area = M.PI * radius * radius;
```

## Built-in Types

The standard library uses these built-in types:

- `Result[T, E]` - Success (Ok) or failure (Err)
- `Option[T]` - Present value (Some) or absent (None)
- `Int` - Integer numbers
- `Float` - Floating-point numbers
- `Bool` - Boolean values (true/false)
- `String` - Text strings

## Status

**Implemented:**
- ✅ Core utilities
- ✅ Result error handling
- ✅ Option optional values
- ✅ Math basic functions

**TODO:**
- String manipulation functions
- Array/List operations
- I/O functions (requires FFI)
- Transcendental math functions (sin, cos, sqrt, etc.)
- Date/Time utilities
- File system operations

## Contributing

To add new stdlib functions:

1. Add function to appropriate module file
2. Document with examples
3. Update this README
4. Add tests in `tests/stdlib/`

## Testing

Test standard library functions:

```bash
affinescript eval tests/stdlib/test_core.as
affinescript eval tests/stdlib/test_result.as
affinescript eval tests/stdlib/test_option.as
affinescript eval tests/stdlib/test_math.as
```
