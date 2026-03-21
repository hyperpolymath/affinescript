# AffineScript ↔ aLib Integration

This directory contains documentation and tools for integrating AffineScript with the **aggregate-library (aLib)** methodology.

## Quick Links

- **Strategy Document**: [ALIB-INTEGRATION.md](../ALIB-INTEGRATION.md) - Complete integration strategy
- **Conformance Generator**: [../../tools/alib_conformance_gen.jl](../../tools/alib_conformance_gen.jl) - Auto-generate tests from aLib specs
- **aLib Repository**: https://github.com/hyperpolymath/aggregate-library

## What is aLib?

aggregate-library (aLib) is a **methods repository** that demonstrates how to:
- Specify minimal overlap between diverse programming ecosystems
- Express that overlap as stable specs + semantics + conformance tests
- Enable cross-language compatibility without imposing a standard library

aLib is NOT a library to import - it's a methodology to apply.

## Why AffineScript + aLib?

AffineScript brings **unique value** to aLib:

1. **Stress Testing** - Affine types are "extreme constraints" that push specs to their limits
2. **Novel Semantics** - Shows how common operations work under move semantics
3. **Safety Model** - Demonstrates memory-safe, use-after-free-free implementations
4. **Ecosystem Diversity** - Adds functional + affine-typed language to aLib portfolio

## Current Status

### ✅ Completed
- [x] Strategic analysis of aLib integration opportunities
- [x] Documentation of integration approach
- [x] Conformance test generator tool (Julia)

### 🚧 In Progress
- [ ] Run conformance tests against current stdlib
- [ ] Document affine semantics for each aLib spec
- [ ] Create `alib-for-affinescript` ecosystem repo

### 📋 Planned
- [ ] Contribute affine semantics notes to aLib upstream
- [ ] Cross-language benchmarking
- [ ] Interop examples with other aLib-conformant systems

## Quick Start

### Generate Conformance Tests

```bash
# Assuming aggregate-library is cloned alongside affinescript
cd affinescript
julia tools/alib_conformance_gen.jl \
    ../aggregate-library/specs \
    tests/conformance
```

### Run Conformance Tests

```bash
affinescript test tests/conformance/
```

### View Conformance Report

```bash
affinescript test --alib-conformance-report
```

## aLib Spec Categories

AffineScript stdlib aligns with these aLib categories:

| aLib Category | AffineScript Module | Status |
|---------------|---------------------|--------|
| `arithmetic` | `stdlib/math.as` | ✓ Good |
| `collection` | `stdlib/collections.as` + `prelude.as` | ⚠ Needs affine semantics docs |
| `comparison` | `stdlib/prelude.as` | ✓ Good |
| `conditional` | Built-in (if/match) | ✓ Good |
| `logical` | Built-in (&&, \|\|, !) | ✓ Good |
| `string` | `stdlib/string.as` | ⚠ Partial |

## Affine Semantics Examples

### map (Collection → Collection)

**aLib Spec**: `map: Collection[A], Function[A -> B] -> Collection[B]`

**AffineScript Implementation**:
```affinescript
/// Conforms to aLib collection/map
/// Affine: source moved, elements consumed exactly once
fn map<T, U>(arr: [T], f: T -> U) -> [U] {
  let result = [];
  for x in arr {  // arr moved
    result = result ++ [f(x)];  // x consumed by f
  }
  result
}
// arr is no longer accessible here (moved)
```

**Key Difference**: aLib spec doesn't specify ownership. AffineScript adds:
- Source collection **moved** (not copied)
- Each element consumed **exactly once**
- Result collection **owned** by caller

### filter (Collection + Predicate → Collection)

**aLib Spec**: `filter: Collection[A], Function[A -> Bool] -> Collection[A]`

**AffineScript Implementation**:
```affinescript
/// Conforms to aLib collection/filter
/// Affine: predicate borrows, source moved, filtered elements dropped
fn filter<T: affine>(arr: [T], pred: &T -> Bool) -> [T] {
  let result = [];
  for x in arr {
    if pred(&x) {  // predicate borrows (doesn't consume)
      result = result ++ [x];  // x moved into result
    }
    // else: x dropped here (affine allows drop without use)
  }
  result
}
```

**Key Difference**: Predicate **borrows** instead of consuming (allows checking without ownership transfer).

## Contributing

### To AffineScript
1. Implement aLib-conformant operations in stdlib
2. Add conformance test attributes
3. Document affine-specific semantics

### To aLib (upstream)
1. Contribute affine semantics notes
2. Add affine-specific test vectors
3. Document edge cases under move semantics

## Resources

- **aLib Specs**: https://github.com/hyperpolymath/aggregate-library/tree/main/specs
- **Integration Strategy**: [ALIB-INTEGRATION.md](../ALIB-INTEGRATION.md)
- **AffineScript Types**: [../specs/affinescript-spec.md](../specs/affinescript-spec.md)

## License

PMPL-1.0 (following AffineScript project license)
