# AffineScript ↔ aggregate-library Integration Strategy

**Date:** 2026-01-23
**Status:** Proposal / Roadmap
**Goal:** Leverage aLib methodology to strengthen AffineScript's ecosystem position

## Executive Summary

**aggregate-library (aLib)** is a methods repository demonstrating how to specify, test, and stress-test library overlap across wildly different systems. AffineScript, with its **affine types, dependent types, and effect tracking**, represents an extreme case that can:

1. **Stress-test aLib specs** under unique constraints
2. **Contribute novel semantics** for operations under affine ownership
3. **Demonstrate cross-language compatibility** without sacrificing type safety
4. **Build ecosystem credibility** through conformance

## What aLib Provides

From `aggregate-library/`:

### Spec Categories
- **arithmetic**: Basic numeric operations
- **collection**: map, filter, fold, contains
- **comparison**: Ordering and equality
- **conditional**: Boolean logic
- **logical**: AND, OR, NOT operations
- **string**: String manipulation

### Each Spec Includes
1. **Interface signature** (language-agnostic)
2. **Behavioral semantics** (properties, edge cases)
3. **Executable test cases** (YAML conformance vectors)

### Philosophy
- **NOT** a standard library replacement
- **IS** a methodology for cross-ecosystem design
- Enables implementations to share semantics without sharing code
- Emphasizes reversibility and conformance testing

## How AffineScript Benefits

### 1. Conformance Validation

**Action:** Implement aLib conformance test runner for AffineScript

```affinescript
// stdlib/alib_conformance.as
fn run_collection_map_tests() -> TestResult {
  // Load test vectors from aggregate-library/specs/collection/map.md
  // Execute against AffineScript stdlib implementation
  // Report conformance score
}
```

**Benefits:**
- Validates stdlib correctness against language-neutral specs
- Provides regression safety during development
- Demonstrates interoperability guarantees

### 2. Affine Semantics Contribution

**Action:** Contribute AffineScript-specific semantics notes to aLib

**Example - `map` under affine constraints:**

```markdown
# AffineScript Implementation Notes

## Ownership Semantics
```affinescript
fn map<T: affine, U>(list: [T], f: T -> U) -> [U]
```

**Affine Constraint**: Each element `T` consumed exactly once during map.

**Properties under Affine Types:**
- Source collection `list` is moved (cannot be used after map)
- Function `f` must consume its argument
- If `f` panics, remaining elements are dropped safely
- No iterator invalidation (collection moved, not borrowed)

**Safety Guarantees:**
- Memory leak freedom: All elements processed or dropped
- Use-after-free prevention: Source collection inaccessible
- Double-free prevention: Elements consumed exactly once
```

**Impact:** Shows how affine types provide stronger guarantees than aLib's base specs

### 3. Stress-Test Case for aLib

**Action:** Position AffineScript as an "extreme constraint" test case

**Unique Challenges AffineScript Presents:**

| aLib Operation | Affine Challenge | Solution |
|---------------|------------------|----------|
| `map(list, f)` | Source consumed | Move semantics, explicit lifetime |
| `filter(list, pred)` | Predicate can't consume | Predicate must be `&T -> Bool` |
| `fold(list, acc, f)` | Accumulator ownership | Explicit `own` or `&mut` types |
| `contains(list, x)` | Search requires equality | Requires `Eq` trait, borrow semantics |

**Value:** Demonstrates how aLib specs work under memory-safe, move-only semantics

### 4. Cross-Language Benchmarking

**Action:** Create `alib-benchmarks/affinescript/` with performance data

```yaml
# alib-benchmarks/affinescript/collection_map.yml
language: affinescript
implementation: stdlib-0.1.0
spec: collection/map

benchmarks:
  - name: map_int_array_1000
    input_size: 1000
    iterations: 10000
    time_ns: 42350
    memory_bytes: 4000

  - name: map_string_array_100
    input_size: 100
    iterations: 5000
    time_ns: 15200
    memory_bytes: 2400
```

**Compare against:** ReScript, OCaml, Rust, JavaScript implementations

**Insight:** Shows cost/benefit of affine type safety

### 5. Ecosystem Implementation: `alib-for-affinescript`

**Action:** Create separate repo `alib-for-affinescript`

**Structure:**
```
alib-for-affinescript/
├── src/
│   ├── arithmetic.as      # Conformant arithmetic operations
│   ├── collection.as      # Conformant collection operations
│   ├── comparison.as      # Conformant comparison operations
│   ├── string.as          # Conformant string operations
│   └── ...
├── tests/
│   └── conformance/       # aLib test vectors
│       ├── runner.as
│       └── vectors/       # Imported from aggregate-library
├── docs/
│   └── affine-semantics.md  # AffineScript-specific notes
└── README.md
```

**Goal:** Demonstrate aLib method applied to affine-typed language

## Implementation Roadmap

### Phase 1: Conformance (Week 1-2)
- [ ] Import aLib test vectors into AffineScript test suite
- [ ] Implement conformance test runner (YAML → AffineScript tests)
- [ ] Run conformance tests against current stdlib
- [ ] Document conformance gaps

### Phase 2: Semantics Contribution (Week 3-4)
- [ ] Write affine semantics notes for each aLib spec
- [ ] Contribute to `aggregate-library/notes/affine-types.md`
- [ ] Submit PRs with AffineScript edge cases
- [ ] Add affine-specific test vectors

### Phase 3: Ecosystem Repo (Month 2)
- [ ] Create `alib-for-affinescript` repository
- [ ] Implement all aLib specs with affine constraints
- [ ] Comprehensive documentation of ownership semantics
- [ ] Example projects using aLib-conformant API

### Phase 4: Cross-Language Integration (Month 3)
- [ ] Benchmark AffineScript vs other implementations
- [ ] Create interop examples (AffineScript ↔ ReScript/OCaml)
- [ ] Publish comparison study
- [ ] Present findings to aLib community

## Specific Integration Points

### 1. AffineScript Stdlib Alignment

**Current stdlib modules that align with aLib:**

| AffineScript Module | aLib Spec | Conformance Status |
|---------------------|-----------|-------------------|
| `stdlib/prelude.as` | collection/{map,filter,fold} | Partial (needs testing) |
| `stdlib/math.as` | arithmetic/* | Good (basic ops) |
| `stdlib/string.as` | string/* | Partial (missing ops) |
| `stdlib/collections.as` | collection/* | Good (comprehensive) |

**Actions:**
1. Add conformance attributes to stdlib functions:
```affinescript
/// Conforms to aLib collection/map spec v1.0
/// Test vectors: aggregate-library/specs/collection/map.md
fn map<T, U>(arr: [T], f: T -> U) -> [U] { ... }
```

2. Generate conformance report:
```bash
$ affinescript test --alib-conformance
✓ collection/map: 12/12 test vectors pass
✓ collection/filter: 10/10 test vectors pass
✗ collection/fold: 8/12 test vectors pass (4 failures)
  - Accumulator ownership semantics differ
  - See: docs/affine-semantics.md#fold
```

### 2. Novel Affine Operations

**Contribute AffineScript-specific operations to aLib:**

#### `take_ownership`
```markdown
# Operation: take_ownership (Affine-specific)

## Interface Signature
```
take_ownership: Collection[A: affine] -> (A, Collection[A])
```

## Behavioral Semantics
Removes first element from collection, transferring ownership.
Source collection is modified (mutable borrow).

**Affine Guarantee**: Element removed exactly once, caller owns result.
```

#### `partition_consume`
```markdown
# Operation: partition_consume (Affine-specific)

## Interface Signature
```
partition_consume: Collection[A: affine], Function[&A -> Bool]
  -> (Collection[A], Collection[A])
```

## Behavioral Semantics
Partitions collection into two based on predicate.
All elements consumed exactly once.

**Affine Guarantee**: No element duplication, all elements accounted for.
```

### 3. Conformance Test Integration

**Add to AffineScript test suite:**

```affinescript
// tests/alib_conformance_test.as

// Auto-generated from aggregate-library/specs/collection/map.md
fn test_map_conformance() -> TestResult {
  // Test case 1: Double each number
  let input = [1, 2, 3];
  let output = map(input, fn(x) => x * 2);
  assert_eq(output, [2, 4, 6], "map: double numbers");

  // Test case 2: Empty collection
  let empty = [];
  let result = map(empty, fn(x) => x * 2);
  assert_eq(result, [], "map: empty collection");

  // ... more test cases from YAML

  Pass
}
```

**Automation:**
```bash
# Generate conformance tests from aLib specs
$ deno task gen-conformance-tests \
    --alib-repo ../aggregate-library \
    --output tests/conformance/

Generated 47 conformance tests from aLib specs
```

## Strategic Value

### For AffineScript:
1. **Validation** - Proves stdlib correctness against language-neutral specs
2. **Credibility** - Shows serious approach to language design
3. **Interoperability** - Eases integration with other aLib-conformant systems
4. **Documentation** - aLib specs serve as reference documentation

### For aLib:
1. **Stress Testing** - Affine types push specs to extremes
2. **Semantics Enrichment** - Adds ownership/borrowing considerations
3. **Safety Model** - Demonstrates specs under memory-safe constraints
4. **Diversity** - Adds functional + affine-typed language to portfolio

### For Ecosystem:
1. **Pattern Library** - Shows how to handle affine constraints
2. **Interop Guide** - AffineScript ↔ other languages via aLib surface
3. **Research Value** - Novel combination of affine types + standard operations

## Example: Complete Integration Flow

### 1. Start with aLib Spec

From `aggregate-library/specs/collection/filter.md`:
```markdown
filter: Collection[A], Function[A -> Bool] -> Collection[A]
Preserves order, only includes elements where predicate returns true
```

### 2. Implement in AffineScript (Affine Semantics)

```affinescript
// stdlib/collections.as

/// Conforms to aLib collection/filter v1.0
/// Affine semantics: Predicate borrows, source moved
fn filter<T: affine>(arr: [T], pred: &T -> Bool) -> [T] {
  let result = [];
  for x in arr {  // arr moved, each x consumed
    if pred(&x) {  // pred borrows (doesn't consume)
      result = result ++ [x];  // x moved into result
    }
    // else: x dropped here (affine allows drop)
  }
  result  // caller owns result, arr fully consumed
}
```

### 3. Run Conformance Tests

```affinescript
// tests/conformance/collection_filter.as

fn test_alib_filter_conformance() -> TestResult {
  // From aLib test vectors
  let input = [1, 2, 3, 4, 5];
  let evens = filter(input, fn(x) => x % 2 == 0);
  assert_eq(evens, [2, 4], "filter: keep evens");

  // Affine-specific: input no longer accessible
  // Uncommenting next line would be compile error:
  // let _ = len(input);  // ERROR: value moved

  Pass
}
```

### 4. Document Affine Semantics

Contribute to `aggregate-library/notes/affine-types.md`:

```markdown
## AffineScript Implementation: filter

### Ownership Model
- **Source collection**: Moved (consumed)
- **Predicate function**: Borrows element (`&T -> Bool`)
- **Result collection**: Owned by caller
- **Filtered-out elements**: Automatically dropped

### Why Predicate Borrows
```affinescript
// If predicate consumed element:
fn filter_bad<T: affine>(arr: [T], pred: T -> Bool) -> [T]
  // Problem: pred(x) consumes x even if we want to keep it!
  // Can't move x into both pred() and result

// Solution: Predicate borrows
fn filter_good<T: affine>(arr: [T], pred: &T -> Bool) -> [T]
  // pred checks without consuming
  // We decide whether to move into result or drop
```

### Safety Guarantees
- No use-after-filter of source
- No double-free of filtered elements
- All elements either moved to result or properly dropped
```

### 5. Benchmark and Compare

```yaml
# Submitted to aLib benchmarks
spec: collection/filter
implementations:
  - lang: affinescript
    time_ns: 3500
    memory: 0  # No allocation (in-place filtering)
    notes: "Move semantics enable zero-copy filtering"

  - lang: javascript
    time_ns: 4200
    memory: 8192  # Allocates new array
    notes: "GC handles cleanup"

  - lang: rust
    time_ns: 3400
    memory: 0  # Iterator, no allocation
    notes: "Similar to AffineScript (ownership semantics)"
```

## Deliverables

### Short Term (Month 1)
- [ ] Conformance test suite integrated
- [ ] Stdlib alignment assessment document
- [ ] Initial affine semantics notes

### Medium Term (Month 2-3)
- [ ] `alib-for-affinescript` repository created
- [ ] All aLib specs implemented with affine semantics
- [ ] Contribution to aggregate-library (semantics notes)
- [ ] Benchmark comparison published

### Long Term (Month 4-6)
- [ ] AffineScript cited as aLib stress-test case
- [ ] Cross-language interop examples
- [ ] Research paper: "Affine Types Meet Common Library Interfaces"
- [ ] Integration patterns documented

## Success Metrics

1. **Conformance**: ≥95% of aLib test vectors pass
2. **Contribution**: ≥10 semantics notes contributed to aLib
3. **Performance**: Within 20% of Rust implementations (affine-to-affine)
4. **Documentation**: Complete affine semantics guide
5. **Ecosystem**: ≥3 example projects using aLib-conformant API

## Next Steps

**Immediate (This Week):**
1. Import aLib test vectors into AffineScript test suite
2. Audit current stdlib for aLib spec alignment
3. Create conformance test runner prototype

**Follow-up (Next Week):**
1. Run full conformance test suite
2. Begin affine semantics documentation
3. Identify gaps in stdlib coverage

**Strategic (Next Month):**
1. Create `alib-for-affinescript` repository
2. Submit first contributions to aggregate-library
3. Begin cross-language benchmarking

---

**Author:** Claude Sonnet 4.5
**Review Status:** Awaiting user feedback
**Implementation Priority:** High - positions AffineScript in wider ecosystem
