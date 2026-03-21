# Lesson 10: Building Real Programs

## Project Structure

```
my-project/
├── src/
│   ├── main.as
│   ├── lib.as
│   └── util.as
├── tests/
│   └── test_main.as
└── affinescript.toml
```

## Testing

```affinescript
fn test_addition() -> TestResult {
  let result = add(2, 3);
  assert_eq(result, 5, "2 + 3 should equal 5");
  return Pass;
}
```

## Compilation

```bash
# Check types and borrow checker
affinescript check src/main.as

# Compile to WebAssembly
affinescript compile src/main.as -o output.wasm

# Run with interpreter
affinescript eval src/main.as
```

## Next Steps

- Read the [Language Reference](../reference/index.md)
- Explore [Standard Library](../stdlib/index.md)
- Join the community at github.com/hyperpolymath/affinescript

Congratulations! You've completed the AffineScript tutorial!
