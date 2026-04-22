# TEST-NEEDS.md — affinescriptiser

## CRG Grade: C — ACHIEVED 2026-04-04

## Current Test State

| Category | Count | Notes |
|----------|-------|-------|
| Test directories | 2 | Location(s): /tests, /verification/tests |
| CI workflows | 22 | Running tests on GitHub Actions |
| Unit tests | Built-in | Rust/cargo test framework |
| Integration tests | Configured | Via integration/ directory |

## What's Covered

- [x] Rust unit test suite (cargo test)
- [x] Documentation tests
- [x] Example programs with tests

## Still Missing (for CRG B+)

- [ ] Code coverage reports (codecov integration)
- [ ] Detailed test documentation in CONTRIBUTING.md
- [ ] Integration tests beyond unit tests
- [ ] Performance benchmarking suite

## Run Tests

```bash
cargo test
```
