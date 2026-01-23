;; SPDX-License-Identifier: PMPL-1.0
;; STATE.scm - Project state for AffineScript

(state
  (metadata
    (version "0.3.0")
    (schema-version "1.0")
    (created "2024-01-01")
    (updated "2026-01-23")
    (project "affinescript")
    (repo "hyperpolymath/affinescript"))

  (project-context
    (name "AffineScript")
    (tagline "Affine types + dependent types + effects, compiling to WebAssembly")
    (tech-stack ("ocaml" "dune" "menhir")))

  (current-position
    (phase "alpha")
    (overall-completion 50)
    (components
      ((lexer . 90)
       (parser . 75)  ; Fixed block/record ambiguity (no implicit returns)
       (ast . 100)
       (borrow-checker . 95)  ; Working! Detects use-after-move
       (type-checker . 40)  ; Fixed parameter inference via global symbol lookup
       (codegen-wasm . 0)
       (stdlib . 10)
       (tooling . 30)))
    (working-features
      ("Lexical analysis with token spans"
       "Parser with explicit return statements"
       "AST representation"
       "Error reporting with source locations"
       "Basic type definitions"
       "Affine type checking with ownership tracking"
       "Borrow checker detects use-after-move errors"
       "Function parameter type inference")))

  (route-to-mvp
    (milestones
      ((name "Frontend")
       (status "in-progress")
       (items
         ("Lexer" "Parser" "AST" "Error handling")))
      ((name "Type System")
       (status "pending")
       (items
         ("Affine type checking"
          "Dependent types"
          "Row polymorphism"
          "Effect inference")))
      ((name "Backend")
       (status "pending")
       (items
         ("WASM codegen"
          "Runtime"
          "FFI")))))

  (blockers-and-issues
    (critical ())
    (high
      (("Lambda Syntax Mismatch" . "Test files use 'fn(x) => expr' syntax but parser only supports '|x| expr' pipe syntax.")
       ("No Implicit Returns" . "Parser requires explicit 'return' statements. Rust-style implicit returns not supported due to grammar ambiguity with records.")))
    (medium
      (("Type Checker Coverage" . "Only basic type checking implemented. Missing: dependent types, row polymorphism, effect inference.")
       ("Interpreter Incomplete" . "Only 25% complete. Missing: pattern matching, effects, most control flow.")))
    (low
      (("Parser Conflicts" . "63 shift/reduce conflicts and 5 reduce/reduce conflicts remain in grammar."))))

  (critical-next-actions
    (immediate
      ("Complete interpreter implementation (effects, pattern matching, control flow)"
       "Add simple module/import system for code organization"))
    (this-week
      ("Implement WebAssembly code generation"
       "Create standard library with basic I/O and data structures"
       "Add more borrow checker tests (mut borrows, field access, lifetimes)"))
    (this-month
      ("Dependent type checking implementation"
       "Row polymorphism for extensible records"
       "Effect inference and checking"
       "IDE tooling (LSP, syntax highlighting)")))

  (session-history
    ((date "2026-01-23T18:00")
     (accomplishments
       ("FIXED: Parser block/record ambiguity by removing implicit returns from grammar"
        "FIXED: Type checker parameter inference by adding global symbol table lookup"
        "FIXED: Block return type handling for functions with explicit return statements"
        "FIXED: Borrow checker symbol lookup to search all_symbols instead of current scope"
        "VERIFIED: Borrow checker correctly detects use-after-move errors"
        "VERIFIED: Borrow checker allows valid moves and shared borrows"
        "Created official test files: tests/borrow/use_after_move.as and valid_move.as"
        "Reduced parser conflicts from 138 to 63 shift/reduce")))
    ((date "2026-01-23T12:00")
     (accomplishments
       ("Completed borrow checker implementation with function signature tracking"
        "Integrated borrow checker into compiler pipeline after type checking"
        "Added context-based ownership enforcement at call sites"
        "Discovered and documented parser block/record ambiguity bug"
        "Discovered and documented type checker parameter inference bug"
        "Identified that blocks with 'return' statements work, implicit returns don't"
        "Found that all test files use unsupported 'fn() =>' lambda syntax")))))
