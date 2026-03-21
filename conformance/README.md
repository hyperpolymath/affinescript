# AffineScript Conformance Test Suite

This directory contains the conformance test corpus for AffineScript.
The tests here are **binding** - changes to expected outputs require explicit justification.

## Structure

```
conformance/
├── valid/       # Programs that must parse successfully (exit 0)
│   ├── *.as     # Source files
│   └── *.expected  # Expected parser output
├── invalid/     # Programs that must fail with diagnostics (exit non-zero)
│   ├── *.as     # Source files
│   └── *.expected  # Expected error diagnostics
└── README.md    # This file
```

## Test Methodology

### Valid Programs
- Must parse without error
- CLI command: `affinescript parse <file>`
- Expected exit code: 0
- Output must match `.expected` file exactly

### Invalid Programs
- Must produce a parse/lex error
- CLI command: `affinescript parse <file>` or `affinescript lex <file>`
- Expected exit code: non-zero (1 for parse errors)
- Error diagnostic must match `.expected` file pattern

## Running Tests

```bash
# Run all conformance tests
just conformance

# Or directly with dune
dune runtest conformance
```

## Adding New Tests

1. Add `.as` source file to `valid/` or `invalid/`
2. Run the compiler to generate expected output
3. Review and save as `.expected` file
4. Commit both files together

## Versioning

- Format: `conformance-vN.M`
- Breaking changes (modified .expected): increment major version
- New tests only: increment minor version

## F0 Requirements

Per the scope arrest directive:
- Minimum 10 valid programs
- Minimum 10 invalid programs
- Stable exit-code contract
- Deterministic diagnostics
