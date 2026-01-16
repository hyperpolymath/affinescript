;; SPDX-License-Identifier: PMPL-1.0
;; STATE.scm - Project state for AffineScript

(state
  (metadata
    (version "0.3.0")
    (schema-version "1.0")
    (created "2024-01-01")
    (updated "2025-01-16")
    (project "affinescript")
    (repo "hyperpolymath/affinescript"))

  (project-context
    (name "AffineScript")
    (tagline "Affine types + dependent types + effects, compiling to WebAssembly")
    (tech-stack ("ocaml" "dune" "menhir")))

  (current-position
    (phase "alpha")
    (overall-completion 35)
    (components
      ((lexer . 90)
       (parser . 80)
       (ast . 100)
       (type-checker . 20)
       (codegen-wasm . 0)
       (stdlib . 10)
       (tooling . 30)))
    (working-features
      ("Lexical analysis with token spans"
       "Menhir-based parser"
       "AST representation"
       "Error reporting with source locations"
       "Basic type definitions")))

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
      (("Type Checker" . "Core type checking needs implementation")))
    (medium
      (("Parser Completeness" . "Some syntax forms not yet parsed")))
    (low ()))

  (critical-next-actions
    (immediate
      ("Complete parser for all syntax forms"))
    (this-week
      ("Begin affine type checker"))
    (this-month
      ("Implement dependent type checking"
       "Add row polymorphism"))))
