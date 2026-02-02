# Codegen WASM tests

This directory contains AffineScript codegen tests and their compiled WASM artifacts.

## Run locally

From the repo root:

```
./tools/run_codegen_wasm_tests.sh
```

The script:
- compiles every `tests/codegen/*.as` file to `tests/codegen/*.wasm`
- runs any `tests/codegen/*.mjs` harnesses

## Notes

- WASM artifacts under `tests/codegen/` are intentionally checked in.
- `out.wasm` is also tracked at the repo root for tooling convenience.
