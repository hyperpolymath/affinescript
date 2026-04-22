<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) -->
# TOPOLOGY.md — Affinescriptiser Repository Map

## Root

| File | Purpose |
|------|---------|
| `0-AI-MANIFEST.a2ml` | AI agent entry point — canonical file locations and invariants |
| `README.adoc` | Human-facing orientation: what, why, how, architecture |
| `ROADMAP.adoc` | Phased development plan (Phase 0–5) |
| `TOPOLOGY.md` | This file — repository structure map |
| `CONTRIBUTING.adoc` | Contribution guide |
| `SECURITY.md` | Security policy |
| `CHANGELOG.md` | Release history |
| `LICENSE` | PMPL-1.0-or-later full text |
| `Cargo.toml` | Rust crate manifest (clap, serde, toml, handlebars) |
| `Justfile` | Task runner (build, test, lint, fmt, quality, assail) |
| `Containerfile` | OCI container build (Chainguard base) |
| `contractile.just` | Contractile enforcement recipes |
| `flake.nix` | Nix flake for reproducible builds |
| `guix.scm` | Guix package definition |
| `.editorconfig` | Editor formatting rules |
| `.envrc` | direnv environment |
| `.gitignore` | Git ignore rules |
| `.gitattributes` | Git attribute rules |
| `.gitlab-ci.yml` | GitLab CI mirror pipeline |
| `.guix-channel` | Guix channel metadata |
| `.tool-versions` | asdf tool version pins |

## `src/` — Source Code

### Rust CLI and Orchestration

| File | Purpose |
|------|---------|
| `src/main.rs` | CLI entry point — clap subcommands: init, validate, generate, build, run, info |
| `src/lib.rs` | Library facade — re-exports manifest, codegen, abi modules |
| `src/manifest/mod.rs` | TOML manifest parser — `Manifest`, `WorkloadConfig`, `DataConfig`, `Options` |
| `src/codegen/mod.rs` | Code generation stubs — `generate_all()`, `build()`, `run()` (pending implementation) |
| `src/abi/mod.rs` | ABI module declaration (Rust side) |

### Verified Interface Seams (`src/interface/`)

| Path | Language | Purpose |
|------|----------|---------|
| `src/interface/abi/Types.idr` | Idris2 | ABI type definitions: `ResourceKind`, `Linearity`, `Ownership`, platform detection, opaque handles, memory layout proofs |
| `src/interface/abi/Layout.idr` | Idris2 | WASM memory layout proofs: alignment, padding, struct field layout, C ABI compliance |
| `src/interface/abi/Foreign.idr` | Idris2 | FFI declarations: library lifecycle, core operations, string/buffer ops, error handling, version info, callbacks |
| `src/interface/ffi/build.zig` | Zig | Build configuration: shared/static library, C header generation, test/bench steps |
| `src/interface/ffi/src/main.zig` | Zig | FFI implementation: C-ABI functions matching Foreign.idr declarations |
| `src/interface/ffi/test/integration_test.zig` | Zig | Integration tests: lifecycle, operations, strings, errors, memory safety, threading |
| `src/interface/generated/abi/` | (empty) | Target directory for generated C headers from Idris2 ABI |

### Cross-Cutting Modules

| Path | Purpose |
|------|---------|
| `src/aspects/integrity/` | Data integrity aspects (pending) |
| `src/aspects/observability/` | Observability aspects (pending) |
| `src/aspects/security/` | Security aspects (pending) |
| `src/bridges/` | Bridge code between analysis and codegen (pending) |
| `src/contracts/` | Runtime contract enforcement (pending) |
| `src/core/` | Core domain logic (pending) |
| `src/definitions/` | Shared type definitions (pending) |
| `src/errors/` | Error types and diagnostics (pending) |

## `examples/` — Usage Examples

| File | Purpose |
|------|---------|
| `examples/SafeDOMExample.res` | ReScript example: SafeDOM mounting with proven selector/HTML validation |
| `examples/web-project-deno.json` | Deno project configuration for web example |

## `tests/` — Test Suite

Rust integration and unit tests (via `cargo test`).

## `docs/` — Documentation

Technical documentation, architecture diagrams, theory, and practice guides.
Includes `docs/attribution/`, `docs/architecture/`, `docs/theory/`, `docs/practice/`.

## `features/` — Feature Specifications

BDD-style feature specs for affinescriptiser behaviour.

## `verification/` — Verification Artefacts

Formal verification scripts and proof artefacts.

## `container/` — Container Ecosystem

Stapeln container configuration for OCI deployment.

## `.machine_readable/` — Machine-Readable Metadata

All machine-readable metadata lives here (never in root).

| Path | Purpose |
|------|---------|
| `.machine_readable/6a2/STATE.a2ml` | Project state: scaffold phase, 5% complete |
| `.machine_readable/6a2/META.a2ml` | Architecture decisions: iser-pattern, ABI-FFI standard, RSR template |
| `.machine_readable/6a2/ECOSYSTEM.a2ml` | Ecosystem position: -iser family, siblings (typedqliser, chapeliser, verisimiser) |
| `.machine_readable/CLADE.a2ml` | Clade taxonomy classification |
| `.machine_readable/ENSAID_CONFIG.a2ml` | ENSAID integration configuration |
| `.machine_readable/anchors/` | Semantic boundary declarations |
| `.machine_readable/bot_directives/` | Bot-specific instructions (rhodibot, echidnabot, etc.) |
| `.machine_readable/contractiles/` | Policy enforcement contracts (k9, dust, must, trust) |
| `.machine_readable/policies/` | Maintenance axes, checklists, development approach |
| `.machine_readable/configs/` | Tool and CI configuration |
| `.machine_readable/scripts/` | Automation scripts |
| `.machine_readable/integrations/` | External integration configs |
| `.machine_readable/ai/` | AI agent patterns and guides |
| `.machine_readable/compliance/` | Compliance documentation |

## `.github/` — GitHub Configuration

CI/CD workflows (17 total), issue templates, PR templates, CODEOWNERS.

## `.hypatia/` — Hypatia Security Scanner

Neurosymbolic security scanning rules for affinescriptiser.

## `.claude/` — Claude Code Configuration

| File | Purpose |
|------|---------|
| `.claude/CLAUDE.md` | Project-specific AI instructions: architecture, build commands, integration points |

## `.devcontainer/` — Dev Container

VS Code dev container configuration for reproducible development environments.

## `.well-known/` — Well-Known URIs

Standard well-known directory for security.txt and related metadata.
