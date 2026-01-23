;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; ROADMAP.f0.scm - Quarantined Future Work for AffineScript
;;
;; This file defines the development phases and quarantines future work
;; that is NOT part of the f0 control pass. Implementation of items here
;; requires explicit phase advancement.

(define roadmap
  '((schema . "hyperpolymath.roadmap/1")
    (repo . "hyperpolymath/affinescript")
    (updated . "2026-01-01")

    ;; =========================================================================
    ;; CURRENT PHASE: F0 (Control Pass)
    ;; =========================================================================
    (current-phase
      . ((id . "f0")
         (name . "Control Pass")
         (goal . "Establish stable lexer/parser, CLI behavior, and conformance baseline")
         (status . "active")
         (rsr-tier . "bronze-now")

         (deliverables
           . ((completed
                . ("Lexer implementation (sedlex)"
                   "Parser implementation (Menhir)"
                   "AST definitions"
                   "CLI with lex/parse commands"
                   "Basic test infrastructure"
                   "License coherence"
                   "Machine-readable control files"))
              (in-progress
                . ("Conformance test suite (10+ valid, 10+ invalid)"
                   "Golden test outputs"
                   "SPDX header compliance"))
              (not-started-f0
                . ())))

         (success-criteria
           . ("dune build succeeds on clean OCaml 5.1+ machine"
              "dune runtest passes all tests"
              "CLI lex/parse commands work on examples"
              ">=10 valid conformance tests pass"
              ">=10 invalid conformance tests produce expected diagnostics"
              "All license files agree"
              "All source files have SPDX headers"))))

    ;; =========================================================================
    ;; PHASE F1: Type System (QUARANTINED)
    ;; =========================================================================
    (phase-f1
      . ((id . "f1")
         (name . "Type System")
         (goal . "Complete bidirectional type checker with inference")
         (status . "quarantined")
         (prerequisite . "f0-complete")
         (rsr-tier . "silver-after-f1")

         (deliverables
           . ((type-inference . "Hindley-Milner style inference with bidirectional checking")
              (type-unification . "Complete unification with occurs check")
              (row-polymorphism . "Row variable unification and extension")
              (effect-types . "Effect tracking in function types")
              (dependent-types . "Basic dependent type support (Nat indices)")
              (refinement-types . "Predicate refinements on types")))

         (implementation-notes
           . ("lib/typecheck.ml has skeleton - complete TODOs"
              "lib/unify.ml has basic structure - implement row/effect unification"
              "Must not break f0 conformance tests"
              "Add type-error conformance tests"))

         (exit-criteria
           . ("affinescript check <file> type-checks all valid conformance programs"
              "Type errors produce expected diagnostics for invalid programs"
              ">=20 additional type-focused conformance tests"))))

    ;; =========================================================================
    ;; PHASE F2: Ownership & Effects (QUARANTINED)
    ;; =========================================================================
    (phase-f2
      . ((id . "f2")
         (name . "Ownership & Effects")
         (goal . "Implement borrow checking and effect verification")
         (status . "quarantined")
         (prerequisite . "f1-complete")

         (deliverables
           . ((borrow-checker . "Rust-style ownership and borrow analysis")
              (affine-enforcement . "Ensure affine values used at most once")
              (quantity-checker . "QTT quantity tracking (0, 1, omega)")
              (effect-checker . "Verify effect annotations match usage")
              (handler-verification . "Check effect handlers are well-formed")))

         (implementation-notes
           . ("lib/borrow.ml has skeleton - implement full analysis"
              "lib/quantity.ml has framework - implement checking"
              "Model after Rust's MIR-based borrow checker"
              "Effects use evidence-passing (Koka-style)"))

         (exit-criteria
           . ("Borrow errors caught for use-after-move, double free"
              "Quantity errors caught for linear value violations"
              "Effect errors caught for unhandled effects"
              ">=30 ownership/effect conformance tests"))))

    ;; =========================================================================
    ;; PHASE F3: Code Generation (QUARANTINED)
    ;; =========================================================================
    (phase-f3
      . ((id . "f3")
         (name . "Code Generation")
         (goal . "Compile AffineScript to WASM")
         (status . "quarantined")
         (prerequisite . "f2-complete")

         (deliverables
           . ((ir . "Intermediate representation (post-typing)")
              (wasm-codegen . "Generate valid WASM modules")
              (runtime-linking . "Link against runtime/")
              (optimization . "Basic optimizations (dead code, inlining)")))

         (implementation-notes
           . ("New module lib/codegen.ml"
              "Use wasm crate or generate WAT text"
              "Runtime in runtime/ is quarantined helper"
              "No GC - rely on affine types for memory"))

         (exit-criteria
           . ("affinescript compile examples/hello.as -o hello.wasm works"
              "Generated WASM passes validation"
              "Simple programs execute correctly in WASM runtime"))))

    ;; =========================================================================
    ;; PHASE F4: Tooling (QUARANTINED)
    ;; =========================================================================
    (phase-f4
      . ((id . "f4")
         (name . "Tooling")
         (goal . "Developer tooling for productive AffineScript development")
         (status . "quarantined")
         (prerequisite . "f3-complete")

         (deliverables
           . ((lsp . "Language Server Protocol implementation (tools/affinescript-lsp)")
              (package-manager . "Package manager (tools/affine-pkg)")
              (doc-generator . "Documentation generator (tools/affine-doc)")
              (repl . "Read-eval-print loop for experimentation")
              (formatter . "Code formatter (affinescript fmt)")))

         (implementation-notes
           . ("LSP skeleton exists in tools/affinescript-lsp"
              "Package manager skeleton in tools/affine-pkg"
              "May implement in OCaml or Rust"))))

    ;; =========================================================================
    ;; NON-GOALS (Explicitly Out of Scope)
    ;; =========================================================================
    (non-goals
      . ((out-of-scope
           . ("Framework functionality"
              "Standard library (separate repo)"
              "Package registry infrastructure"
              "IDE/editor plugins (beyond LSP)"
              "Multiple backend targets (LLVM initially out)"))
         (explicitly-forbidden
           . ("Second authoritative implementation"
              "TypeScript in compiler"
              "Network-required builds"
              "Breaking f0 behavior"))))

    ;; =========================================================================
    ;; PHASE ADVANCEMENT RULES
    ;; =========================================================================
    (advancement-rules
      . ((to-advance-phase
           . ("All exit criteria for current phase met"
              "All conformance tests pass"
              "Documentation updated"
              "ROADMAP.f0.scm updated to reflect new current phase"))
         (breaking-changes
           . ("Require explicit anchor update"
              "Must preserve backwards compatibility for conformance"
              "Document migration path"))
         (authority
           . ("Phase advancement requires repo superintendent approval"
              "Conformance corpus changes require explicit justification"))))

    ;; =========================================================================
    ;; IMPLEMENTATION STATUS SNAPSHOT (F0)
    ;; =========================================================================
    (f0-status-snapshot
      . ((lexer . "90% complete - handles all tokens")
         (parser . "70% complete - core grammar working")
         (ast . "95% complete - all constructs represented")
         (cli . "lex/parse working, check/compile stubbed")
         (tests . "lexer/parser/golden tests exist")
         (conformance . "directory created, needs population")))

    ;; =========================================================================
    ;; TASK AUTHORITY
    ;; =========================================================================
    (task-authority
      . ((local-tasks . "just")
         (description . "All local operations invoked via just <recipe>")
         (recipes
           . ((build . "dune build")
              (test . "dune runtest")
              (lint . "dune fmt --check")
              (fmt . "dune fmt")
              (clean . "dune clean")))))))

;; Export for machine consumption
(define (get-roadmap) roadmap)
