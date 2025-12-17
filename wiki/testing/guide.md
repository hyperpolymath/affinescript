# Testing Guide

Comprehensive guide to testing AffineScript programs and the compiler itself.

## Table of Contents

1. [Testing Framework](#testing-framework)
2. [Writing Tests](#writing-tests)
3. [Test Organization](#test-organization)
4. [Property-Based Testing](#property-based-testing)
5. [Fuzzing](#fuzzing)
6. [Compiler Testing](#compiler-testing)
7. [Integration Tests](#integration-tests)
8. [Benchmarking](#benchmarking)

---

## Testing Framework

AffineScript uses a built-in testing framework:

### Basic Test Structure

```affine
#[test]
fn test_addition() {
  assert_eq(2 + 2, 4)
}

#[test]
fn test_string_concat() {
  let s = "hello" ++ " " ++ "world"
  assert_eq(s, "hello world")
}
```

### Running Tests

```bash
# Run all tests
aspm test

# Run specific test file
aspm test tests/math_test.as

# Run tests matching pattern
aspm test --filter "test_add"

# Run with verbose output
aspm test --verbose

# Run tests in parallel
aspm test --jobs 4
```

---

## Writing Tests

### Assertions

```affine
// Equality
assert_eq(actual, expected)
assert_ne(actual, unexpected)

// Boolean
assert(condition)
assert(condition, "custom message")

// Comparisons
assert_lt(a, b)   // a < b
assert_le(a, b)   // a <= b
assert_gt(a, b)   // a > b
assert_ge(a, b)   // a >= b

// Option/Result
assert_some(opt)
assert_none(opt)
assert_ok(result)
assert_err(result)

// Collections
assert_contains(collection, element)
assert_empty(collection)
assert_len(collection, expected_len)
```

### Test Attributes

```affine
// Basic test
#[test]
fn test_basic() { ... }

// Test expected to panic
#[test]
#[should_panic]
fn test_panic() {
  panic("expected panic")
}

// Test expected to panic with specific message
#[test]
#[should_panic(expected = "index out of bounds")]
fn test_bounds() {
  let arr = [1, 2, 3]
  arr[10]  // Panics
}

// Ignore test (skip)
#[test]
#[ignore]
fn test_slow() { ... }

// Ignore with reason
#[test]
#[ignore = "requires network"]
fn test_network() { ... }

// Test with timeout
#[test]
#[timeout(1000)]  // 1000ms
fn test_fast() { ... }
```

### Test Fixtures

```affine
mod tests {
  use super::*

  // Setup function
  fn setup() -> TestContext {
    TestContext::new()
  }

  // Teardown function
  fn teardown(ctx: TestContext) {
    ctx.cleanup()
  }

  #[test]
  fn test_with_fixture() {
    let ctx = setup()

    // Test code...

    teardown(ctx)
  }
}
```

### Test Modules

```affine
// In lib.as
pub fn add(x: Int, y: Int) -> Int {
  x + y
}

#[cfg(test)]
mod tests {
  use super::*

  #[test]
  fn test_add_positive() {
    assert_eq(add(2, 3), 5)
  }

  #[test]
  fn test_add_negative() {
    assert_eq(add(-1, 1), 0)
  }

  #[test]
  fn test_add_zero() {
    assert_eq(add(0, 0), 0)
  }
}
```

---

## Test Organization

### Directory Structure

```
project/
├── src/
│   ├── lib.as
│   └── math/
│       └── mod.as
├── tests/
│   ├── integration_test.as    # Integration tests
│   ├── math_test.as           # Module tests
│   └── fixtures/
│       └── test_data.json
└── benches/
    └── performance_bench.as   # Benchmarks
```

### Unit vs Integration Tests

```affine
// tests/unit_test.as - Unit tests for internal modules
use mylib::internal::parse_number

#[test]
fn test_parse_number() {
  assert_eq(parse_number("42"), Ok(42))
}

// tests/integration_test.as - Integration tests for public API
use mylib

#[test]
fn test_full_workflow() {
  let config = mylib::Config::default()
  let result = mylib::process(config, "input data")
  assert_ok(result)
}
```

---

## Property-Based Testing

Inspired by QuickCheck and Echidna's fuzzing approach.

### Basic Properties

```affine
use test::property::*

#[property]
fn prop_addition_commutative(x: Int, y: Int) -> Bool {
  x + y == y + x
}

#[property]
fn prop_reverse_reverse(xs: Vec[Int]) -> Bool {
  xs.reverse().reverse() == xs
}

#[property]
fn prop_sort_preserves_length[T: Ord](xs: Vec[T]) -> Bool {
  xs.len() == xs.sorted().len()
}
```

### Custom Generators

```affine
use test::property::*

// Custom generator for positive integers
fn gen_positive() -> Gen[Int] {
  Gen::int_range(1, 1000)
}

// Custom generator for non-empty strings
fn gen_non_empty_string() -> Gen[String] {
  Gen::string()
    .filter(|s| s.len() > 0)
}

#[property]
fn prop_positive_sqrt(#[gen(gen_positive)] n: Int) -> Bool {
  let root = (n as Float64).sqrt()
  root >= 0.0
}

// Generator for custom types
struct Point { x: Int, y: Int }

impl Arbitrary for Point {
  fn arbitrary() -> Gen[Point] {
    Gen::map2(Gen::int(), Gen::int(), |x, y| Point { x, y })
  }

  fn shrink(self) -> Vec[Point] {
    // Shrink towards simpler cases
    vec![
      Point { x: 0, y: 0 },
      Point { x: self.x, y: 0 },
      Point { x: 0, y: self.y },
    ]
  }
}
```

### Configuration

```affine
#[property]
#[tests(1000)]        // Run 1000 test cases
#[max_shrinks(100)]   // Try up to 100 shrinks on failure
fn prop_many_tests(x: Int) -> Bool {
  x * 0 == 0
}
```

### Invariants (Echidna-Style)

Define invariants that should always hold:

```affine
struct BankAccount {
  balance: Int,
  withdrawals: Vec[Int],
}

impl BankAccount {
  fn withdraw(self: &mut Self, amount: Int) -> Result[(), Error] {
    if amount > self.balance {
      return Err(InsufficientFunds)
    }
    self.balance -= amount
    self.withdrawals.push(amount)
    Ok(())
  }

  // Invariant: balance should never be negative
  #[invariant]
  fn balance_non_negative(self: &Self) -> Bool {
    self.balance >= 0
  }

  // Invariant: sum of withdrawals + balance = initial balance
  #[invariant]
  fn accounting_correct(self: &Self) -> Bool {
    let total_withdrawn: Int = self.withdrawals.iter().sum()
    // This requires tracking initial balance...
    true
  }
}

#[test]
fn fuzz_bank_account() {
  test::fuzz::run_invariant_tests::<BankAccount>(
    1000,  // iterations
    BankAccount { balance: 1000, withdrawals: vec![] }
  )
}
```

---

## Fuzzing

### Fuzz Testing

```affine
use test::fuzz::*

// Fuzz test for parser
#[fuzz]
fn fuzz_parser(data: &[Byte]) {
  // Should not panic on any input
  let _ = Parser::parse_str(String::from_utf8_lossy(data))
}

// Fuzz test with structured input
#[fuzz]
fn fuzz_json_roundtrip(value: JsonValue) {
  let serialized = value.to_string()
  let parsed = JsonValue::parse(&serialized)
  assert_eq(parsed, Ok(value))
}
```

### Coverage-Guided Fuzzing

```affine
#[fuzz]
#[coverage_guided]
fn fuzz_lexer(input: String) {
  let tokens = lexer::lex(&input)
  // Should handle all inputs gracefully
  for token in tokens {
    let _ = token.to_string()
  }
}
```

### Running Fuzz Tests

```bash
# Run all fuzz tests
aspm fuzz

# Run specific fuzz test
aspm fuzz --target fuzz_parser

# Run for specific duration
aspm fuzz --duration 3600  # 1 hour

# Use specific corpus
aspm fuzz --corpus ./corpus/
```

---

## Compiler Testing

### Lexer Tests

```ocaml
(* test/test_lexer.ml *)
open Alcotest
open Affinescript

let test_keywords () =
  let tokens = Lexer.lex "fn let mut own ref" in
  check (list token_eq) "keywords"
    [FN; LET; MUT; OWN; REF; EOF]
    tokens

let test_string_escapes () =
  let tokens = Lexer.lex {|"hello\n\tworld"|} in
  check (list token_eq) "escapes"
    [STRING_LIT "hello\n\tworld"; EOF]
    tokens

let tests = [
  "keywords", `Quick, test_keywords;
  "string escapes", `Quick, test_string_escapes;
]
```

### Parser Tests

```ocaml
let test_parse_expr () =
  let ast = Parser.parse_expr "1 + 2 * 3" in
  check ast_eq "precedence"
    (Binary (Lit 1, Add, Binary (Lit 2, Mul, Lit 3)))
    ast

let test_parse_function () =
  let ast = Parser.parse "fn add(x: Int, y: Int) -> Int { x + y }" in
  match ast with
  | [FnDecl { name = "add"; params; ret_ty; body }] ->
      check int "param count" 2 (List.length params)
  | _ -> fail "expected function"
```

### Type Checker Tests

```ocaml
(* Positive test - should type check *)
let test_infer_literal () =
  let expr = parse_expr "42" in
  let (ty, _) = Type_check.infer empty_ctx expr in
  check type_eq "int literal" T_Int ty

(* Negative test - should produce error *)
let test_type_mismatch () =
  let program = parse "let x: Int = \"hello\"" in
  match Type_check.check_program program with
  | Error (Type_mismatch _) -> ()
  | _ -> fail "expected type mismatch error"
```

### Golden Tests

Compare output against expected files:

```ocaml
let test_golden name =
  let input = read_file (sprintf "tests/golden/%s.as" name) in
  let expected = read_file (sprintf "tests/golden/%s.expected" name) in
  let actual = compile_and_run input in
  check string name expected actual

let golden_tests = [
  "hello", `Quick, test_golden "hello";
  "factorial", `Quick, test_golden "factorial";
  "fibonacci", `Quick, test_golden "fibonacci";
]
```

---

## Integration Tests

### End-to-End Tests

```affine
// tests/e2e_test.as
use std::process::Command

#[test]
fn test_compile_and_run() {
  // Compile
  let compile_result = Command::new("aspm")
    .args(["build", "examples/hello.as"])
    .output()

  assert!(compile_result.status.success())

  // Run
  let run_result = Command::new("./target/hello.wasm")
    .output()

  assert_eq(run_result.stdout, "Hello, World!\n")
}
```

### Testing with External Dependencies

```affine
#[test]
#[requires(database)]
fn test_database_integration() {
  let db = test::fixtures::setup_test_db()

  db.execute("INSERT INTO users (name) VALUES ('Alice')")

  let users = db.query("SELECT * FROM users")
  assert_len(users, 1)

  test::fixtures::teardown_test_db(db)
}
```

---

## Benchmarking

### Basic Benchmarks

```affine
use test::bench::*

#[bench]
fn bench_vector_push(b: &mut Bencher) {
  b.iter(|| {
    let mut v = Vec::new()
    for i in 0..1000 {
      v.push(i)
    }
    v
  })
}

#[bench]
fn bench_hashmap_insert(b: &mut Bencher) {
  b.iter(|| {
    let mut m = HashMap::new()
    for i in 0..1000 {
      m.insert(i, i * 2)
    }
    m
  })
}
```

### Parameterized Benchmarks

```affine
#[bench]
#[params(size = [10, 100, 1000, 10000])]
fn bench_sort(b: &mut Bencher, size: Int) {
  let data = gen_random_vec(size)

  b.iter(|| {
    let mut copy = data.clone()
    copy.sort()
    copy
  })
}
```

### Running Benchmarks

```bash
# Run all benchmarks
aspm bench

# Run specific benchmark
aspm bench --filter "bench_sort"

# Compare against baseline
aspm bench --baseline main

# Output to file
aspm bench --output results.json
```

---

## Test Coverage

### Generating Coverage Reports

```bash
# Run tests with coverage
aspm test --coverage

# Generate HTML report
aspm coverage --format html --output coverage/

# Generate lcov format
aspm coverage --format lcov --output coverage.lcov
```

### Coverage in CI

```yaml
# .github/workflows/ci.yml
- name: Run tests with coverage
  run: aspm test --coverage

- name: Upload coverage
  uses: codecov/codecov-action@v3
  with:
    file: coverage.lcov
```

---

## See Also

- [Property-Based Testing](property-based.md) - Advanced QuickCheck-style testing
- [Fuzzing](fuzzing.md) - Coverage-guided fuzzing
- [Benchmarking](benchmarks.md) - Performance testing
- [CI/CD](../tooling/ci.md) - Continuous integration
