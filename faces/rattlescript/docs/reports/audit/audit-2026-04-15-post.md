# Post-audit Status Report: rattlescript
- **Date:** 2026-04-15
- **Status:** Complete (M5 Sweep)
- **Repo:** /var/mnt/eclipse/repos/rattlescript

## Actions Taken
1. Standard CI/Workflow Sweep: Added blocker workflows (`ts-blocker.yml`, `npm-bun-blocker.yml`) and updated `Justfile`.
2. SCM-to-A2ML Migration: Staged and committed deletions of legacy `.scm` files.
3. Lockfile Sweep: Generated and tracked missing lockfiles where manifests were present.
4. Static Analysis: Verified with `panic-attack assail`.

## Findings Summary
- 14 TODO/FIXME/HACK markers in .machine_readable/contractiles/k9/template-hunt.k9.ncl
- flake.nix declares inputs without narHash, rev pinning, or sibling flake.lock — dependency revision is unpinned in flake.nix
- Hardcoded /tmp/ path without mktemp in tests/e2e.sh
- 14 TODO/FIXME/HACK markers in affinescript/contractiles/k9/template-hunt.k9.ncl
- DOM manipulation (innerHTML/document.write) in affinescript/tools/affine-doc/assets/search.js
- 18 TODO/FIXME/HACK markers in affinescript/tools/affine-pkg/src/main.rs
- 9 unwrap/expect calls in affinescript/tools/affinescript-lsp/src/handlers.rs
- Rust project has test infrastructure but no mutation-test configuration (cargo-mutants/.cargo-mutants.toml) — add `cargo mutants` to verify test suite kills mutations

## Final Grade
- **CRG Grade:** D (Promoted from E/X) - CI and lockfiles are in place.
