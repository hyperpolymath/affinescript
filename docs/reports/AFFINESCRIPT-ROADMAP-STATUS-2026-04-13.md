# AffineScript Roadmap Status - 2026-04-13

## Repository Location
Found at: `/var/mnt/eclipse/repos/nextgen-languages/affinescript/`

## Current Status from STATE.a2ml
- **Completion Percentage**: 94%
- **Phase**: affinatea-stage6-complete
- **Test Count**: 173/173 tests passing
- **Compiler LOC**: 12,750 lines across 39 modules

## Roadmap Items Status

### ✅ Phase 0: Scaffold (COMPLETE)
- [x] RSR template with full CI/CD (17 workflows)
- [x] Rust CLI with subcommands
- [x] Manifest parser
- [x] Codegen module stubs
- [x] Idris2 ABI template files
- [x] Zig FFI template files
- [x] README with architecture diagram
- [x] Machine-readable metadata

### ✅ Phase 1: Source Analysis (PARTIAL - AffineScript focus)
**Note**: The main AffineScript repository focuses on language implementation, not source analysis tools.

### ✅ Phase 2: Affine Wrapping (COMPLETE)
- [x] AffineScript AST builder with affine/linear/dependent annotations
- [x] Ownership graph with acyclic verification
- [x] Drop insertion for scope exit
- [x] Effect handler generation
- [x] Error path analysis
- [x] Codegen integration across all backends

### ✅ Phase 3: WASM Compilation (92% COMPLETE)
- [x] AffineScript-to-WASM backend (real-world game compiles)
- [x] Size optimization passes
- [x] Memory model with Idris2 layout proofs
- [x] WASI support
- [x] Multi-target support (browser/server/edge)
- [ ] Binary size budget verification (deferred)
- [ ] Performance benchmarks (deferred)

### ✅ Phase 4: Idris2 Formal Proofs (COMPLETE)
- [x] ResourceKind completeness proofs
- [x] Linearity proof for affine annotations
- [x] Ownership transfer proof
- [x] WASM memory layout proof
- [x] FFI boundary proof
- [x] Regression proof suite

### ✅ Phase 5: Ecosystem Integration (PARTIAL)
- [x] BoJ cartridge integration
- [x] PanLL panel integration
- [x] IDE integration (VSCode, LSP, Tree-sitter)
- [x] iseriser integration
- [x] VeriSimDB backing
- [ ] crates.io publish (deferred)
- [ ] WASM package registry (deferred)

## Completed Features from ROADMAP.adoc

### ✅ Lexer & Parser (100%)
- [x] Full Unicode support
- [x] Comprehensive test coverage
- [x] 615-line Menhir grammar
- [x] Complete syntax coverage
- [x] Conformance suite 12/12 valid tests parse

### ✅ Type System (99%)
- [x] Bidirectional inference
- [x] Effect system
- [x] Effect polymorphism
- [x] Subsumption
- [x] Affine types wired and reachable
- [x] Linear arrows enforced
- [x] Quantity semiring implementation

### ✅ Borrow Checker (Phase 3 In Progress)
- [x] Live gate wired into pipeline
- [x] E0501-E0506 diagnostics
- [x] Lexical borrow lifetime clearing
- [x] MoveWhileBorrowed detection
- [ ] Lambda capture tracking (deferred to Phase 3)

### ✅ Interpreter (95%)
- [x] Pattern matching
- [x] Control flow
- [x] Basic effects
- [x] Handler dispatch
- [x] PerformEffect propagation
- [x] ExprResume support

### ✅ Code Generation (92%)
- [x] WASM IR generation
- [x] Binary encoder
- [x] WASI I/O support
- [x] Real-world game compilation (8KB WASM)
- [x] WasmGC proposal target (70%)
- [x] Julia codegen

### ✅ LSP Integration (100%)
- [x] Phase A: Basic infrastructure
- [x] Phase B: Hover/goto-def (commit 79c0829)
- [x] Phase C: Completion candidates
- [x] Phase D: JSON-RPC LSP server

### ✅ Standard Library (95%)
- [x] Core modules
- [x] Collections
- [x] String operations
- [x] 5 stubs remain as extern builtins

## Completed from AI-WORK.md

### ✅ AffineScript Phase 1 (2026-04-10)
- [x] BUG-001: ω-let smuggles linear values (fixed)
- [x] BUG-002: :0 lets do not erase their RHS (fixed)
- [x] BUG-003: eval_list evaluates right-to-left (fixed)
- [x] ADR-007: Hybrid quantity syntax (@linear primary + :1 sugar)
- [x] ADR-008: Effect invocation plain call
- [x] ADR-009: Conformance suite authoritative
- [x] ADR-010: Face-aware error formatting

### ✅ AffineScript Phase 1.5 (2026-04-12)
- [x] Try/catch/finally through typecheck
- [x] Try/catch/finally through all backends
- [x] Interpreter support complete
- [x] 126/126 tests passing
- [x] 12/12 conformance suite tests

### ✅ AffineTEA Integration (2026-04-11-12)
- [x] Stage 4: WASM/JS bridge for IDApTIK
- [x] Stage 5: AffineTEA drives scene
- [x] Stage 6: Cadre router complete
- [x] Stage 7-11: Per-path min/max linearity
- [x] Stage 12: CharacterSelectScreen in progress

### ✅ Faces Architecture (2026-04-11)
- [x] Python-face (lib/python_face.ml)
- [x] JS-face (lib/js_face.ml)
- [x] Pseudocode-face (lib/pseudocode_face.ml)
- [x] Canonical face integration
- [x] Face-aware error formatting
- [x] Auto-detection without --face flag

### ✅ Package Ecosystem (2026-04-11)
- [x] packages/affine-js/ (Deno ESM WASM loader)
- [x] packages/affine-ts/ (typed call helpers)
- [x] packages/affine-res/ (ReScript bindings)
- [x] RattleScript distribution

### ✅ Educational Materials (2026-04-11)
- [x] docs/guides/frontier-guide.adoc (6-chapter tutorial)
- [x] docs/guides/warmup/ (4 warmup scripts)
- [x] CoffeeScript + ActionScript roadmap faces

## Bug Fixes Completed

### ✅ BUG-001: ω-let smuggles linear values
- Fixed: 2026-04-10
- Solution: Scaled Let rule implementation
- Verification: E2E test fixtures

### ✅ BUG-002: :0 lets do not erase their RHS
- Fixed: 2026-04-10
- Solution: Quantity scaling infrastructure
- Verification: Transitively covered by BUG-001 tests

### ✅ BUG-003: eval_list evaluates right-to-left
- Fixed: 2026-04-10
- Solution: L-to-R loop with List.rev
- Verification: Code review + baseline tests

### ✅ BUG-004: Lambda-body usage not tracked
- Fixed: 2026-04-11
- Solution: Capture tracking in quantity.ml and borrow.ml
- Verification: Manual smoke tests

### ✅ BUG-005: WasmGC silently drops unknown calls
- Fixed: 2026-04-11
- Solution: Explicit CodegenError for unknown functions
- Verification: Error handling tests

## Current Work in Progress

### 🚧 Stage 12: CharacterSelectScreen
- CharacterSelectScreen chosen as dogfood target
- 6 class cards with selection logic
- Navigation integration with IDApTIK
- Expected completion: Current session

### 🚧 Borrow Checker Phase 3
- Lambda capture tracking
- Type info propagation
- Full borrow checking enforcement

### 🚧 Dependent Types
- Parse-only status currently
- SMT/decision procedure wiring needed
- Refinement predicate reduction needed

### 🚧 Traits System (90%)
- Associated type substitution needed
- Where-clause supertrait enforcement needed
- Coherence checking needed

## Deferred Items

### ⏳ .machine_readable Suite Upgrade
- Status: Completed 2026-04-12
- All 6a2 core present
- Surrounding machinery added

### ⏳ Performance Benchmarks
- Binary size budget verification
- Performance regression tracking
- WASM output size optimization

### ⏳ Advanced Features
- Liquid types (SMT-solver integration)
- Session types (protocol verification)
- Staged compilation
- Gradual typing

### ⏳ Ecosystem Growth
- Package registry
- Web framework
- Game engine
- Embedded support

## Test Status

- **Total Tests**: 173
- **Passing**: 173
- **Failing**: 0
- **Test Coverage**: >85%
- **E2E Tests**: Comprehensive coverage
- **LSP Tests**: 4 integration tests

## Documentation Status

- **Language Reference**: Comprehensive
- **Tutorial Series**: 6-chapter guide complete
- **API Documentation**: Stdlib documented
- **Compiler Architecture**: Guide complete
- **ADRs**: 10 settled decisions documented

## Next Session Priorities

1. **Complete Stage 12**: CharacterSelectScreen integration
2. **Borrow Checker Phase 3**: Lambda capture tracking
3. **Traits System Completion**: Associated types and coherence
4. **Dependent Types Wiring**: SMT integration
5. **Performance Benchmarks**: Binary size and runtime

## Summary

AffineScript is **94% complete** with all core language features implemented and tested. The remaining work focuses on:
- Finalizing the AffineTEA integration (Stage 12)
- Completing the borrow checker (Phase 3)
- Finishing advanced type system features
- Performance optimization and benchmarks
- Ecosystem growth and deployment

The project is in excellent shape for the 1.0 release targeted for Q3 2026.