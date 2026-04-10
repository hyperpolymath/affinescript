# aLib Conformance Tests

This directory contains conformance tests for the **aggregate-library (aLib)** specification.

## Purpose

These tests verify that AffineScript's standard library operations conform to the language-agnostic specifications defined in the [aggregate-library](https://github.com/hyperpolymath/aggregate-library) project.

## What is aLib?

aggregate-library (aLib) is a **methodology repository** that provides:
- Language-agnostic operation specifications
- Behavioral semantics and properties
- Executable test vectors in YAML format

aLib is NOT a code library - it's a way to specify minimal overlap between diverse programming ecosystems.

## Test Structure

```
tests/conformance/
├── arithmetic/          # Arithmetic operation tests
│   └── add.affine
├── collection/          # Collection operation tests
│   ├── map.affine
│   ├── filter.affine
│   ├── fold.affine
│   └── contains.affine
├── run_all.affine          # Master test runner
└── README.md           # This file
```

## Running Tests

### Run all conformance tests:
```bash
affinescript tests/conformance/run_all.affine
```

### Run specific category:
```bash
affinescript tests/conformance/collection/map.affine
```

### Expected Output:
```
================================================================================
aLib Conformance Report
================================================================================

✓ PASS collection/map: 5/5 tests
✓ PASS collection/filter: 5/5 tests
✓ PASS collection/fold: 6/6 tests
✓ PASS collection/contains: 6/6 tests
✓ PASS arithmetic/add: 5/5 tests

================================================================================
Summary
================================================================================
Total operations tested: 5
Conformant operations: 5/5
Total test cases: 27
Tests passed: 27
Tests failed: 0
Conformance rate: 100%

✓ Excellent aLib conformance (≥95%)
================================================================================
```

## Test Vector Sources

Each test file includes a reference to its source aLib spec:
```affinescript
// Source: aggregate-library/specs/collection/map.md
```

## Conformance Criteria

- **100% conformance**: All test vectors pass
- **≥95% conformance**: Excellent (production-ready)
- **≥80% conformance**: Good (acceptable with documented gaps)
- **<80% conformance**: Needs improvement

## AffineScript-Specific Semantics

AffineScript's conformance tests respect affine type constraints:

### map
- Source collection **moved** (not copied)
- Elements consumed **exactly once**
- Result owned by caller

### filter
- Predicate **borrows** (`&T -> Bool`)
- Source collection **moved**
- Filtered elements automatically **dropped**

### fold
- Accumulator ownership tracked
- Source collection **moved**
- Left-associative evaluation

### contains
- Requires `Eq` trait on element type
- Short-circuit on first match
- Source collection **borrowed** (not moved)

## Adding New Conformance Tests

1. Read the aLib spec from `aggregate-library/specs/`
2. Extract test vectors from YAML section
3. Create `tests/conformance/<category>/<operation>.affine`
4. Translate aLib function expressions to AffineScript syntax
5. Add test to `run_all.affine`

Example:
```affinescript
// SPDX-License-Identifier: PMPL-1.0-or-later
// Source: aggregate-library/specs/collection/map.md

fn test_map_double() -> TestResult {
  let input = [1, 2, 3];
  let result = map(input, fn(x) => x * 2);
  assert_eq(result, [2, 4, 6], "Double each number");
  Pass
}
```

## Integration Strategy

See [docs/ALIB-INTEGRATION.md](../../docs/ALIB-INTEGRATION.md) for the complete aLib integration roadmap.

## Status

**Phase 1: Conformance** ✅ **COMPLETE**
- [x] Collection conformance tests (4/4 specs) ✓
- [x] Arithmetic conformance tests (5/5 specs) ✓
- [x] Comparison conformance tests (6/6 specs) ✓
- [x] Logical conformance tests (3/3 specs) ✓
- [x] String conformance tests (3/3 specs) ✓
- [x] Conditional conformance tests (1/1 specs) ✓

**🏆 Total Progress: 22/22 specs (100% - PERFECT CONFORMANCE)**

AffineScript now validates against all core aLib operations. Phase 1 complete!

## Contributing

When contributing affine-specific notes to aLib upstream:
1. Document ownership semantics
2. Explain move vs borrow decisions
3. Show safety guarantees
4. Provide affine-specific test vectors

## License

PMPL-1.0-or-later (following AffineScript project license)
