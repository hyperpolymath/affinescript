;; SPDX-License-Identifier: AGPL-3.0-or-later
;; AGENTIC.scm - AI agent interaction patterns for affinescript

(define agentic-config
  `((version . "1.0.0")
    (claude-code
      ((model . "claude-opus-4-5-20251101")
       (tools . ("read" "edit" "bash" "grep" "glob"))
       (permissions . "read-all")))
    (patterns
      ((code-review
        (style . "thorough")
        (focus . ("type-safety" "ownership" "effect-tracking" "totality")))
       (refactoring
        (style . "conservative")
        (preserve . ("type-annotations" "effect-signatures" "ownership-modifiers")))
       (testing
        (style . "comprehensive")
        (coverage . ("lexer" "parser" "type-checker" "borrow-checker")))))
    (constraints
      ((allowed-languages . ("ocaml" "bash" "affinescript"))
       (banned . ("typescript" "go" "python" "makefile" "javascript"))
       (build-system . "dune")
       (test-framework . "alcotest")))
    (code-generation
      ((ocaml
        (style . "functional")
        (prefer . ("pattern-matching" "algebraic-data-types" "modules"))
        (avoid . ("mutable-state" "exceptions-for-control-flow")))
       (affinescript
        (style . "ownership-aware")
        (prefer . ("explicit-effects" "linear-types" "row-polymorphism"))
        (require . ("effect-annotations" "ownership-modifiers")))))
    (compiler-phases
      ((lexer . "lib/lexer.ml")
       (parser . "lib/parser.mly")
       (ast . "lib/ast.ml")
       (type-checker . "lib/typecheck.ml")
       (borrow-checker . "lib/borrow.ml")
       (name-resolution . "lib/resolve.ml")))
    (documentation
      ((style . "ocamldoc")
       (format . "asciidoc")
       (generate . "dune build @doc")))))
