;; SPDX-License-Identifier: PMPL-1.0-or-later
;; AffineScript Testing Report - Machine-Readable Format
;; Generated: 2025-12-29

(testing-report
  (metadata
    (version "1.0.0")
    (project "affinescript")
    (project-version "0.1.0")
    (test-date "2025-12-29")
    (generated-by "automated-testing"))

  (environment
    (language "OCaml")
    (ocaml-version "5.x")
    (ocaml-required ">= 5.1")
    (dune-version "3.20.2")
    (menhir-version ">= 20231231")
    (platform "linux")
    (architecture "x86_64"))

  (build-summary
    (status pass)
    (duration-seconds 45)
    (issues-fixed 5)
    (warnings 0))

  (test-summary
    (total-tests 74)
    (passed 47)
    (failed 27)
    (skipped 0)
    (pass-rate 63.5))

  (test-suites
    (suite
      (name "Lexer")
      (total 16)
      (passed 16)
      (failed 0)
      (tests
        (test (name "keywords") (status pass))
        (test (name "identifiers") (status pass))
        (test (name "literals") (status pass))
        (test (name "string-literal") (status pass))
        (test (name "string-escapes") (status pass))
        (test (name "operators") (status pass))
        (test (name "punctuation") (status pass))
        (test (name "row-variable") (status pass))
        (test (name "line-comment") (status pass))
        (test (name "block-comment") (status pass))
        (test (name "nested-comments") (status pass))
        (test (name "hex-literal") (status pass))
        (test (name "binary-literal") (status pass))
        (test (name "function-decl") (status pass))
        (test (name "total-function") (status pass))
        (test (name "type-decl") (status pass))))

    (suite
      (name "Parser")
      (total 58)
      (passed 31)
      (failed 27)
      (tests
        ;; Passing tests
        (test (name "literal-int") (status pass))
        (test (name "literal-float") (status pass))
        (test (name "literal-string") (status pass))
        (test (name "literal-bool") (status pass))
        (test (name "literal-unit") (status pass))
        (test (name "variable") (status pass))
        (test (name "binary-add") (status pass))
        (test (name "binary-precedence") (status pass))
        (test (name "binary-associativity") (status pass))
        (test (name "comparison") (status pass))
        (test (name "logical") (status pass))
        (test (name "unary-neg") (status pass))
        (test (name "unary-not") (status pass))
        (test (name "function-call") (status pass))
        (test (name "field-access") (status pass))
        (test (name "index-access") (status pass))
        (test (name "tuple") (status pass))
        (test (name "array") (status pass))
        (test (name "record") (status pass))
        (test (name "match-expr") (status pass))
        (test (name "lambda") (status pass))
        (test (name "let-stmt") (status pass))
        (test (name "let-mut-stmt") (status pass))
        (test (name "while-stmt") (status pass))
        (test (name "for-stmt") (status pass))
        (test (name "fn-decl-effect") (status pass))
        (test (name "struct-decl") (status pass))
        (test (name "enum-decl") (status pass))
        (test (name "type-alias") (status pass))
        (test (name "trait-decl") (status pass))
        (test (name "effect-decl") (status pass))
        ;; Failing tests
        (test (name "if-expr") (status fail)
          (issue "block-trailing-expression-conflict"))
        (test (name "block") (status fail)
          (issue "block-trailing-expression-conflict"))
        (test (name "pattern-wildcard") (status fail)
          (issue "block-parsing-conflict"))
        (test (name "pattern-variable") (status fail)
          (issue "block-parsing-conflict"))
        (test (name "pattern-constructor") (status fail)
          (issue "block-parsing-conflict"))
        (test (name "pattern-tuple") (status fail)
          (issue "block-parsing-conflict"))
        (test (name "pattern-or") (status fail)
          (issue "block-parsing-conflict"))
        (test (name "fn-decl-simple") (status fail)
          (issue "return-type-parsing"))
        (test (name "fn-decl-total") (status fail)
          (issue "return-type-parsing"))
        (test (name "fn-decl-generic") (status fail)
          (issue "return-type-parsing"))
        (test (name "impl-block") (status fail)
          (issue "impl-type-context"))
        (test (name "impl-trait") (status fail)
          (issue "impl-type-context"))
        (test (name "type-simple") (status fail)
          (issue "standalone-type-parsing"))
        (test (name "type-generic") (status fail)
          (issue "standalone-type-parsing"))
        (test (name "type-tuple") (status fail)
          (issue "standalone-type-parsing"))
        (test (name "type-function") (status fail)
          (issue "standalone-type-parsing"))
        (test (name "type-record") (status fail)
          (issue "standalone-type-parsing"))
        (test (name "type-row-poly") (status fail)
          (issue "standalone-type-parsing"))
        (test (name "type-ownership") (status fail)
          (issue "standalone-type-parsing"))
        (test (name "import-simple") (status fail)
          (issue "import-parsing"))
        (test (name "import-alias") (status fail)
          (issue "import-parsing"))
        (test (name "import-list") (status fail)
          (issue "import-parsing"))
        (test (name "import-glob") (status fail)
          (issue "import-parsing"))
        (test (name "fibonacci") (status fail)
          (issue "block-expression-conflict"))
        (test (name "linked-list") (status fail)
          (issue "multiple-issues"))
        (test (name "effect-handler") (status fail)
          (issue "handler-syntax"))
        (test (name "trait-bounds") (status fail)
          (issue "where-clause-parsing")))))

  (issues-fixed
    (issue
      (id "BUILD-001")
      (severity error)
      (type build)
      (file "lib/dune")
      (description "Menhir --explain flag deprecated in newer versions")
      (fix "Removed --explain from menhir flags"))

    (issue
      (id "BUILD-002")
      (severity error)
      (type type-error)
      (file "lib/parser.mly")
      (line 371)
      (description "Double option wrapping in impl_trait_ref rule")
      (fix "Removed Some wrapper from rule actions"))

    (issue
      (id "BUILD-003")
      (severity error)
      (type unused-variable)
      (file "lib/parser.mly")
      (line 324)
      (description "Unused variable where_clause in trait_decl")
      (fix "Added trd_where field to AST and used variable"))

    (issue
      (id "BUILD-004")
      (severity error)
      (type unbound-constructor)
      (file "lib/parse.ml")
      (line 94)
      (description "Reference to undefined Token.WITH constructor")
      (fix "Removed stray token mapping"))

    (issue
      (id "BUILD-005")
      (severity error)
      (type pattern-match)
      (file "test/test_parser.ml")
      (description "Incomplete record patterns and non-returning code")
      (fix "Added wildcard to patterns, parenthesized expressions")))

  (parser-conflicts
    (shift-reduce-states 23)
    (reduce-reduce-states 2)
    (shift-reduce-resolved 132)
    (reduce-reduce-resolved 5)
    (root-cause
      "Block grammar allows list(stmt) followed by optional expr, "
      "but expressions can start statements, causing ambiguity"))

  (cli-testing
    (command "affinescript --help" (status pass))
    (command "affinescript lex examples/hello.as" (status pass))
    (command "affinescript parse examples/hello.as" (status pass)
      (note "After fixing effect syntax")))

  (example-files
    (file
      (name "hello.as")
      (status fixed)
      (issue "Effect syntax mismatch")
      (fix "Changed `-> () / IO` to `-{IO}-> ()`"))

    (file
      (name "effects.as")
      (status needs-update)
      (issues
        "Uses aspirational syntax not yet implemented"))

    (file
      (name "ownership.as")
      (status needs-update)
      (issues
        "Uses aspirational syntax not yet implemented"))

    (file
      (name "traits.as")
      (status needs-update)
      (issues
        "Uses aspirational syntax not yet implemented"))

    (file
      (name "refinements.as")
      (status needs-update)
      (issues
        "Uses aspirational syntax not yet implemented"))

    (file
      (name "rows.as")
      (status needs-update)
      (issues
        "Uses aspirational syntax not yet implemented"))

    (file
      (name "vectors.as")
      (status needs-update)
      (issues
        "Uses aspirational syntax not yet implemented")))

  (recommendations
    (critical
      (item
        (priority 1)
        (description "Fix block trailing expression parsing")
        (approach "Refactor grammar to use lookahead or explicit return")))

    (improvement
      (item
        (priority 2)
        (description "Update example files to match current syntax"))

      (item
        (priority 3)
        (description "Improve parser error messages"))

      (item
        (priority 4)
        (description "Add more comprehensive test coverage"))))

  (files-modified
    (file "lib/dune"
      (changes "Removed --explain flag from menhir"))
    (file "lib/parser.mly"
      (changes "Fixed trait_ref, added trd_where usage"))
    (file "lib/ast.ml"
      (changes "Added trd_where field to trait_decl"))
    (file "lib/parse.ml"
      (changes "Removed WITH token mapping"))
    (file "test/test_parser.ml"
      (changes "Fixed pattern matching issues"))
    (file "examples/hello.as"
      (changes "Fixed effect annotation syntax"))))
