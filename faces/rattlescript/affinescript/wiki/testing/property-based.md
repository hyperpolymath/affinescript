# Property-Based Testing

Property-based testing generates random test cases to verify that properties hold for all inputs.

## Foundations

Inspired by:
- **QuickCheck** (Haskell) - Original property-based testing
- **Hypothesis** (Python) - Smart shrinking
- **Echidna** (Solidity) - Invariant-based fuzzing

## Core Concepts

### Properties vs Examples

```affine
// Example-based test (specific cases)
#[test]
fn test_reverse_examples() {
  assert_eq([1, 2, 3].reverse(), [3, 2, 1])
  assert_eq([].reverse(), [])
  assert_eq([1].reverse(), [1])
}

// Property-based test (universal property)
#[property]
fn prop_reverse_involutive(xs: Vec[Int]) -> Bool {
  xs.reverse().reverse() == xs
}
```

### Properties

Properties are functions returning Bool that should hold for all inputs:

```affine
// Algebraic properties
#[property]
fn prop_add_commutative(x: Int, y: Int) -> Bool {
  x + y == y + x
}

#[property]
fn prop_add_associative(x: Int, y: Int, z: Int) -> Bool {
  (x + y) + z == x + (y + z)
}

#[property]
fn prop_mul_distributes_over_add(x: Int, y: Int, z: Int) -> Bool {
  x * (y + z) == x * y + x * z
}
```

### Generators

Generators produce random values:

```affine
use test::property::Gen

// Built-in generators
Gen::bool()           // Random Bool
Gen::int()            // Random Int
Gen::int_range(0, 100) // Int in range [0, 100)
Gen::float()          // Random Float64
Gen::char()           // Random Char
Gen::string()         // Random String
Gen::vec(Gen::int())  // Vec of random Ints
Gen::option(Gen::int()) // Option[Int]
```

### Custom Generators

```affine
// Generator combinators
fn gen_point() -> Gen[Point] {
  Gen::map2(
    Gen::int_range(-100, 100),
    Gen::int_range(-100, 100),
    |x, y| Point { x, y }
  )
}

fn gen_email() -> Gen[String] {
  Gen::map3(
    Gen::alpha_string(),
    Gen::one_of(["gmail.com", "example.com", "test.org"]),
    |name, domain| format("{}@{}", name, domain)
  )
}

fn gen_non_empty_vec[T](g: Gen[T]) -> Gen[Vec[T]] {
  Gen::vec(g).filter(|v| !v.is_empty())
}
```

## The Arbitrary Trait

Types implementing `Arbitrary` can be automatically generated:

```affine
trait Arbitrary {
  fn arbitrary() -> Gen[Self]
  fn shrink(self) -> Vec[Self]  // For shrinking on failure
}

// Auto-derived for simple types
#[derive(Arbitrary)]
struct User {
  name: String,
  age: Int,
  active: Bool,
}

// Manual implementation
impl Arbitrary for Point {
  fn arbitrary() -> Gen[Point] {
    Gen::map2(Gen::int(), Gen::int(), |x, y| Point { x, y })
  }

  fn shrink(self) -> Vec[Point] {
    // Shrink towards origin
    let mut shrinks = vec![]

    if self.x != 0 {
      shrinks.push(Point { x: self.x / 2, y: self.y })
    }
    if self.y != 0 {
      shrinks.push(Point { x: self.x, y: self.y / 2 })
    }
    if self.x != 0 && self.y != 0 {
      shrinks.push(Point { x: 0, y: 0 })
    }

    shrinks
  }
}
```

## Shrinking

When a property fails, shrinking finds the minimal failing case:

```affine
#[property]
fn prop_all_positive(xs: Vec[Int]) -> Bool {
  xs.iter().all(|x| x > 0)
}

// Fails with: [3, -5, 7, 2, -1]
// Shrinks to: [-1]  (minimal counterexample)
```

### Custom Shrinking

```affine
fn shrink_tree(tree: Tree) -> Vec[Tree] {
  match tree {
    Leaf(n) -> {
      // Shrink leaf values
      n.shrink().map(Leaf)
    },
    Branch(left, right) -> {
      // Try subtrees, then shrink children
      vec![left.clone(), right.clone()] ++
      left.shrink().map(|l| Branch(l, right.clone())) ++
      right.shrink().map(|r| Branch(left.clone(), r))
    }
  }
}
```

## Advanced Properties

### Conditional Properties

```affine
// Only test when precondition holds
#[property]
fn prop_divide_exact(x: Int, y: Int) -> Property {
  assume(y != 0 && x % y == 0)
  (x / y) * y == x
}

// Alternative syntax
#[property]
fn prop_sorted_head_min(xs: Vec[Int]) -> Bool {
  if xs.is_empty() {
    return true
  }
  xs.sorted().first() == xs.iter().min()
}
```

### Classified Properties

Track distribution of test cases:

```affine
#[property]
fn prop_classified(xs: Vec[Int]) -> Property {
  classify(xs.len() == 0, "empty") |>
  classify(xs.len() < 10, "small") |>
  classify(xs.len() >= 10, "large") |>
  property(xs.reverse().reverse() == xs)
}

// Output:
// OK, passed 100 tests
//   15% empty
//   45% small
//   40% large
```

### Labeled Properties

Better failure messages:

```affine
#[property]
fn prop_labeled(x: Int, y: Int) -> Property {
  label("sum", x + y) |>
  label("product", x * y) |>
  property(x + y >= x || y < 0)
}

// On failure:
// Counterexample: x = 5, y = 3
//   sum = 8
//   product = 15
```

### Property Composition

```affine
fn prop_list_laws[T: Eq](xs: Vec[T]) -> Property {
  prop_all([
    ("reverse involutive", xs.reverse().reverse() == xs),
    ("length preserved", xs.len() == xs.reverse().len()),
    ("append length", (xs.clone() ++ xs.clone()).len() == xs.len() * 2),
  ])
}
```

## State Machine Testing

Test stateful systems by modeling operations:

```affine
// Model a queue
enum QueueOp {
  Push(Int),
  Pop,
  IsEmpty,
}

impl Arbitrary for QueueOp {
  fn arbitrary() -> Gen[QueueOp] {
    Gen::one_of([
      Gen::int().map(Push),
      Gen::const_(Pop),
      Gen::const_(IsEmpty),
    ])
  }
}

#[property]
fn prop_queue_model(ops: Vec[QueueOp]) -> Bool {
  // Reference model (simple list)
  let mut model: Vec[Int] = vec![]

  // System under test
  let mut queue = Queue::new()

  for op in ops {
    match op {
      Push(x) -> {
        model.push(x)
        queue.push(x)
      },
      Pop -> {
        let expected = if model.is_empty() { None } else { Some(model.remove(0)) }
        let actual = queue.pop()
        if expected != actual {
          return false
        }
      },
      IsEmpty -> {
        if model.is_empty() != queue.is_empty() {
          return false
        }
      }
    }
  }

  true
}
```

## Compiler Testing Properties

### Lexer Properties

```affine
#[property]
fn prop_lexer_roundtrip(tokens: Vec[Token]) -> Bool {
  // tokens -> source -> tokens
  let source = tokens.iter().map(|t| t.to_source()).join(" ")
  let re_lexed = lexer::lex(&source)
  tokens == re_lexed
}

#[property]
fn prop_lexer_no_panic(input: String) -> Bool {
  // Lexer should never panic
  let _ = std::panic::catch(|| lexer::lex(&input))
  true
}
```

### Parser Properties

```affine
#[property]
fn prop_parse_pretty_roundtrip(ast: Expr) -> Bool {
  let pretty = ast.pretty_print()
  let re_parsed = parser::parse_expr(&pretty)
  match re_parsed {
    Ok(ast2) => ast.alpha_equiv(&ast2),
    Err(_) => false
  }
}
```

### Type System Properties

```affine
// Type preservation: evaluation preserves types
#[property]
fn prop_type_preservation(expr: WellTypedExpr) -> Property {
  let ty = type_of(expr)
  assume(can_step(expr))
  let expr2 = step(expr)
  property(type_of(expr2) == ty)
}

// Progress: well-typed terms can step or are values
#[property]
fn prop_progress(expr: WellTypedExpr) -> Bool {
  is_value(expr) || can_step(expr)
}
```

## Configuration

```affine
use test::property::Config

#[test]
fn test_with_config() {
  let config = Config {
    num_tests: 1000,
    max_shrinks: 100,
    seed: Some(12345),
    verbose: true,
  }

  check_with_config(config, |xs: Vec[Int]| {
    xs.reverse().reverse() == xs
  })
}
```

## Debugging Failed Properties

```affine
#[property]
#[verbose]                    // Print all test cases
#[replay(seed = 12345)]       // Replay specific seed
fn prop_debug(x: Int) -> Bool {
  x > 0
}
```

---

## See Also

- [Testing Guide](guide.md) - General testing
- [Fuzzing](fuzzing.md) - Coverage-guided fuzzing
- [QuickCheck Paper](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
