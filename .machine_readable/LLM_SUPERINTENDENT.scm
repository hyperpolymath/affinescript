;; SPDX-License-Identifier: PMPL-1.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; LLM_SUPERINTENDENT.scm - AI/LLM Interaction Guidelines for AffineScript
;;
;; This file provides machine-readable guidance for AI assistants working
;; on this repository. It defines constraints, priorities, and behavioral
;; expectations aligned with the scope arrest directive.

(define llm-superintendent
  '((schema . "hyperpolymath.llm-superintendent/1")
    (repo . "hyperpolymath/affinescript")
    (updated . "2026-01-01")

    ;; =========================================================================
    ;; IDENTITY & SCOPE
    ;; =========================================================================
    (identity
      . ((project . "AffineScript")
         (type . "programming-language-compiler")
         (current-phase . "f0-control-pass")
         (one-sentence . "OCaml/Dune compiler frontend for AffineScript, a language with affine types, dependent types, row polymorphism, and extensible effects targeting WASM.")))

    ;; =========================================================================
    ;; SCOPE ARREST - WHAT THIS REPO IS AND IS NOT
    ;; =========================================================================
    (scope
      . ((is
           . ("A programming language specification"
              "An OCaml/Dune compiler frontend (lexer, parser, type checker)"
              "A CLI tool with lex/parse/check/compile subcommands"
              "A conformance test suite"
              "Documentation for the language"))
         (is-not
           . ("A framework"
              "A standard library (stdlib is separate)"
              "A package registry"
              "An IDE or editor"
              "A build system (beyond the compiler itself)"))))

    ;; =========================================================================
    ;; F0 CONTROL PASS - AUTHORITATIVE BEHAVIOR
    ;; =========================================================================
    (f0-authority
      . ((primary-authority . "OCaml/Dune implementation behavior")
         (binding-artifacts
           . ("CLI exit codes and output format"
              "Diagnostic message structure"
              "Conformance test corpus"
              "Golden test outputs"))
         (non-binding-in-f0
           . ("Type system implementation details (many TODOs)"
              "Borrow checker implementation (skeleton only)"
              "Code generation (not implemented)"
              "Runtime behavior (Rust runtime is quarantined)"))))

    ;; =========================================================================
    ;; ALLOWED MODIFICATIONS
    ;; =========================================================================
    (allowed-modifications
      . ((encouraged
           . ("Bug fixes to lexer/parser"
              "Completing TODO items in type checker"
              "Adding conformance tests"
              "Improving error messages"
              "Adding SPDX headers where missing"
              "Documentation improvements"))
         (permitted-with-justification
           . ("New CLI subcommands"
              "Parser grammar extensions"
              "Type system feature implementation"))
         (requires-explicit-approval
           . ("Changing the language semantics"
              "Adding new language keywords"
              "Changing diagnostic formats (breaks conformance)"
              "Modifying golden test expected outputs"))
         (forbidden
           . ("Adding a second authoritative implementation"
              "Moving semantic authority away from OCaml reference"
              "Introducing TypeScript as implementation language"
              "Network-required builds/tests"
              "Feature expansion not required for golden path"
              "Adding frameworks or unnecessary dependencies"))))

    ;; =========================================================================
    ;; IMPLEMENTATION CONSTRAINTS
    ;; =========================================================================
    (implementation
      . ((primary-language . "OCaml")
         (build-system . "Dune 3.14 + Menhir 3.0")
         (required-ocaml-version . ">= 5.1")
         (allowed-languages
           . (("OCaml" . "Compiler implementation")
              ("Scheme" . "Machine-readable control plane files")
              ("Just" . "Task runner glue")))
         (quarantined
           . (("Rust" . "WASM runtime - not authoritative in f0")))
         (forbidden
           . ("TypeScript" "Go" "Python (except SaltStack)"))))

    ;; =========================================================================
    ;; GOLDEN PATH REQUIREMENTS
    ;; =========================================================================
    (golden-path
      . ((commands
           . ("dune build"
              "dune runtest"
              "dune exec affinescript -- lex examples/hello.as"
              "dune exec affinescript -- parse examples/ownership.as"))
         (success-criteria
           . ("Build succeeds on clean machine with OCaml toolchain"
              "All tests pass"
              "CLI commands work on shipped examples"
              "Invalid programs produce deterministic diagnostics"))
         (exit-codes
           . ((success . 0)
              (parse-error . 1)
              (type-error . 2)
              (internal-error . 255)))))

    ;; =========================================================================
    ;; CONFORMANCE REQUIREMENTS
    ;; =========================================================================
    (conformance
      . ((directory . "conformance/")
         (structure
           . ((valid-programs . "conformance/valid/")
              (invalid-programs . "conformance/invalid/")
              (golden-outputs . "conformance/*.expected")))
         (minimum-counts
           . ((valid-programs . 10)
              (invalid-programs . 10)))
         (test-requirements
           . ("Each program has .expected file with golden output"
              "Exit codes are deterministic"
              "Diagnostic messages match expected patterns"))))

    ;; =========================================================================
    ;; LICENSING
    ;; =========================================================================
    (licensing
      . ((spdx-identifier . "MIT OR AGPL-3.0-or-later")
         (canonical-file . "./LICENSE")
         (header-format
           . ";; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later\n;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath")
         (enforcement
           . ("All source files MUST have SPDX headers"
              "All license declarations MUST agree"
              "No contradictory license files"))))

    ;; =========================================================================
    ;; BEHAVIORAL GUIDELINES FOR AI ASSISTANTS
    ;; =========================================================================
    (llm-behavior
      . ((always
           . ("Read existing code before modifying"
              "Check conformance impact of changes"
              "Preserve existing behavior unless explicitly changing it"
              "Add tests for new functionality"
              "Use consistent code style with existing codebase"
              "Check for TODO comments and address them appropriately"))
         (never
           . ("Guess at language semantics"
              "Modify golden test outputs without justification"
              "Add dependencies not strictly necessary"
              "Introduce TypeScript or forbidden languages"
              "Over-engineer solutions"
              "Add features beyond what was requested"))
         (prefer
           . ("Minimal changes that achieve the goal"
              "Reusing existing patterns in the codebase"
              "Clear error messages over clever code"
              "Completing existing TODOs over new features"))))

    ;; =========================================================================
    ;; KEY FILES
    ;; =========================================================================
    (key-files
      . ((implementation
           . ("lib/lexer.ml"       ;; Sedlex lexer
              "lib/parser.mly"     ;; Menhir grammar
              "lib/parse_driver.ml" ;; Parser integration
              "lib/ast.ml"         ;; AST definitions
              "lib/token.ml"       ;; Token types
              "lib/span.ml"        ;; Source locations
              "lib/error.ml"       ;; Diagnostics
              "lib/typecheck.ml"   ;; Type checker (WIP)
              "lib/unify.ml"       ;; Type unification (WIP)
              "lib/borrow.ml"      ;; Borrow checker (stub)
              "bin/main.ml"))      ;; CLI
         (specification
           . ("affinescript-spec.md"  ;; Language spec
              ".machine_read/SPEC.core.scm"))  ;; Formal core
         (machine-readable
           . (".machine_read/LLM_SUPERINTENDENT.scm"
              ".machine_read/SPEC.core.scm"
              ".machine_read/ROADMAP.f0.scm"))
         (tests
           . ("test/test_lexer.ml"
              "test/test_parser.ml"
              "test/test_golden.ml"
              "conformance/"))))

    ;; =========================================================================
    ;; FUTURE PHASES (NOT F0)
    ;; =========================================================================
    (future-phases
      . ((f1 . "Complete type checker + borrow checker")
         (f2 . "WASM code generation")
         (f3 . "Standard library")
         (f4 . "Tooling (LSP, package manager)")))))

;; Export for machine consumption
(define (get-superintendent) llm-superintendent)
