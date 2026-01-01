;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2024-2025 hyperpolymath
;;
;; SPEC.core.scm - Minimal Formal Core Specification for AffineScript
;;
;; This file defines the minimal formal semantics surface for f0 (control pass).
;; It is reducible to: syntax + static rules + dynamic core + error taxonomy.
;; The conformance corpus in conformance/ is binding.

(define spec-core
  '((schema . "hyperpolymath.spec-core/1")
    (version . "f0.2026-01-01")
    (status . "f0-control-pass")

    ;; =========================================================================
    ;; PART 1: LEXICAL GRAMMAR (SYNTAX)
    ;; =========================================================================
    (lexical
      . ((character-classes
           . ((letter . "[a-zA-Z]")
              (digit . "[0-9]")
              (alpha-num . "[a-zA-Z0-9_]")
              (hex-digit . "[0-9a-fA-F]")
              (bin-digit . "[01]")
              (oct-digit . "[0-7]")))

         (whitespace
           . ((space . "' '")
              (tab . "'\\t'")
              (newline . "'\\n'")
              (carriage-return . "'\\r'")))

         (comments
           . ((line-comment . "// ... until newline")
              (block-comment . "/* ... */")))

         (identifiers
           . ((lower-ident . "[a-z][a-zA-Z0-9_]*")
              (upper-ident . "[A-Z][a-zA-Z0-9_]*")
              (row-var . "\\.\\.[a-z][a-zA-Z0-9_]*")
              (type-var . "[a-z][a-zA-Z0-9_]*'?")))

         (keywords
           . ("fn" "let" "mut" "own" "ref"
              "type" "struct" "enum" "trait" "impl"
              "effect" "handle" "resume" "handler"
              "match" "if" "else" "while" "for"
              "return" "break" "continue" "in"
              "true" "false"
              "where" "total"
              "module" "use" "pub" "as"
              "unsafe" "assume" "transmute" "forget"
              ;; Built-in types
              "Nat" "Int" "Bool" "Float" "String" "Type" "Row"))

         (literals
           . ((integer . "decimal | 0x hex | 0b binary | 0o octal")
              (float . "digits.digits [exponent]")
              (char . "'char' | '\\escape'")
              (string . "\"chars\"")
              (bool . "true | false")
              (unit . "()")))

         (quantity-annotations
           . ((erased . "0")
              (linear . "1")
              (unrestricted . ("omega" "ω"))))

         (operators
           . ((arithmetic . ("+" "-" "*" "/" "%"))
              (comparison . ("==" "!=" "<" ">" "<=" ">="))
              (logical . ("&&" "||" "!"))
              (bitwise . ("&" "|" "^" "~" "<<" ">>"))
              (assignment . ("=" "+=" "-=" "*=" "/="))
              (type-level . ("->" "=>" ":" "/" "\\"))))))

    ;; =========================================================================
    ;; PART 2: SYNTACTIC GRAMMAR (ABSTRACT SYNTAX)
    ;; =========================================================================
    (syntactic
      . ((program
           . "(module-decl? import-decl* top-level*)")

         (top-level
           . "(type-decl | fn-decl | trait-decl | impl-block | effect-decl | const-decl)")

         (type-expr
           . "(type-atom | fn-type | dependent-fn-type | type-app | refined-type | row-type | owned-type | ref-type)")

         (type-atom
           . "(Nat | Int | Bool | Float | String | Type | upper-ident | type-var | tuple)")

         (fn-type
           . "(type-expr -> type-expr [/ effects])")

         (ownership-modifiers
           . ((own . "Owned value, must be consumed exactly once")
              (ref . "Immutable borrow, no ownership transfer")
              (mut . "Mutable borrow, exclusive access")))

         (quantity-semantics
           . ((0 . "Erased at runtime, compile-time only")
              (1 . "Linear, must be used exactly once")
              (omega . "Unrestricted, can be used any number of times")))

         (expression
           . "(literal | variable | binary-op | unary-op | call | field-access | method-call | if-expr | match-expr | block | lambda | let-binding | return)")

         (pattern
           . "(wildcard | variable | literal | constructor | tuple | record | or-pattern | guard)")))

    ;; =========================================================================
    ;; PART 3: STATIC SEMANTICS (TYPE RULES)
    ;; =========================================================================
    (static-rules
      . ((typing-judgment
           . "Gamma |- e : T / E  (expression e has type T with effect E in context Gamma)")

         (key-rules
           . ((var . "Gamma, x:T |- x : T / pure")
              (lambda . "Gamma, x:A |- e : B / E  =>  Gamma |- (fn(x:A) e) : A -> B / E")
              (application . "Gamma |- e1 : A -> B / E1, Gamma |- e2 : A / E2  =>  Gamma |- e1(e2) : B / E1 + E2")
              (let . "Gamma |- e1 : A / E1, Gamma,x:A |- e2 : B / E2  =>  Gamma |- let x = e1 in e2 : B / E1 + E2")))

         (subtyping
           . ((reflexivity . "T <: T")
              (transitivity . "S <: T, T <: U  =>  S <: U")
              (row-extension . "{..r, x:A} <: {..r}  when x not in r")
              (effect-subsumption . "E1 <: E2  when E1 is subset of E2")))

         (ownership-rules
           . ((linear-use . "Linear values must be used exactly once")
              (affine-drop . "Affine values may be dropped but not duplicated")
              (borrow-exclusive . "Mutable borrows are exclusive")
              (borrow-shared . "Immutable borrows can coexist")))

         (quantity-rules
           . ((0-erasure . "0-quantity bindings erased at runtime")
              (1-linearity . "1-quantity bindings used exactly once")
              (omega-unrestricted . "omega-quantity bindings unrestricted")))

         (effect-rules
           . ((pure . "No effects, safe to inline/optimize")
              (effect-tracking . "Effects propagate through function calls")
              (handler-discharge . "Handlers discharge their handled effects")))))

    ;; =========================================================================
    ;; PART 4: DYNAMIC CORE (EVALUATION)
    ;; =========================================================================
    (dynamic-core
      . ((evaluation-strategy . "strict (call-by-value)")

         (reduction-rules
           . ((beta . "(fn(x) e) v  -->  e[x := v]")
              (let . "let x = v in e  -->  e[x := v]")
              (if-true . "if true { e1 } else { e2 }  -->  e1")
              (if-false . "if false { e1 } else { e2 }  -->  e2")
              (match . "match v { p => e, ... }  -->  e[bindings(p, v)]  when v matches p")))

         (values
           . ((literals . "integers, floats, bools, strings, unit")
              (closures . "fn(x) e with captured environment")
              (constructors . "C(v1, ..., vn)")
              (records . "{ f1: v1, ..., fn: vn }")))

         (effects-runtime
           . ((effect-invocation . "perform Op(v) suspends to nearest handler")
              (handler-semantics . "handle { e } with { Op(x) => k => ... }")
              (resumption . "resume(v) continues suspended computation with v")))))

    ;; =========================================================================
    ;; PART 5: ERROR TAXONOMY
    ;; =========================================================================
    (error-taxonomy
      . ((categories
           . ((lexical-error . "Malformed tokens, invalid characters")
              (syntax-error . "Grammar violations, unexpected tokens")
              (name-error . "Unbound variables, undefined types")
              (type-error . "Type mismatch, constraint violation")
              (ownership-error . "Use after move, double free, borrow violation")
              (quantity-error . "Linear value not used, dropped incorrectly")
              (effect-error . "Unhandled effect, handler mismatch")))

         (exit-codes
           . ((success . 0)
              (lex-error . 1)
              (parse-error . 1)
              (name-error . 2)
              (type-error . 2)
              (ownership-error . 2)
              (quantity-error . 2)
              (effect-error . 2)
              (internal-error . 255)))

         (diagnostic-format
           . ((structure . "error[CODE]: message\n  --> file:line:col\n   |\nNN | source line\n   | ^^^^ annotation")
              (codes
                . ((E0001 . "Lexical error")
                   (E0002 . "Parse error")
                   (E0100 . "Unbound variable")
                   (E0101 . "Undefined type")
                   (E0200 . "Type mismatch")
                   (E0201 . "Cannot unify types")
                   (E0300 . "Use after move")
                   (E0301 . "Borrow violation")
                   (E0400 . "Unhandled effect")
                   (E0500 . "Quantity violation")))))))

    ;; =========================================================================
    ;; PART 6: F0 DECLARED HOOKS (FUTURE EXTENSIONS)
    ;; =========================================================================
    (future-hooks
      . ((declared-not-implemented
           . ((typecheck-full . "Complete bidirectional type inference")
              (borrow-check . "Rust-style borrow checking")
              (quantity-check . "QTT linear/affine enforcement")
              (effect-check . "Effect system verification")
              (codegen-wasm . "WASM code generation")
              (codegen-llvm . "Optional LLVM backend")))

         (extension-points
           . ((new-keywords . "Reserved for future language extensions")
              (new-operators . "May add operators with proper precedence")
              (stdlib-hooks . "Standard library integration points")
              (ffi-hooks . "Foreign function interface")))

         (compatibility-promise
           . "f0 behavior is authoritative. Future phases MUST NOT break f0 conformance tests.")))

    ;; =========================================================================
    ;; PART 7: CONFORMANCE
    ;; =========================================================================
    (conformance
      . ((binding-artifacts
           . ("conformance/valid/*.as with .expected"
              "conformance/invalid/*.as with .expected"
              "test/golden/*.as with .expected"))

         (test-methodology
           . ((valid-programs . "Must parse without error, exit 0")
              (invalid-programs . "Must produce expected diagnostic, exit non-zero")
              (golden-comparison . "Output must match .expected byte-for-byte")))

         (versioning
           . ((format . "conformance-vN.M")
              (breaking-change . "Increment major version if any golden output changes")
              (additive-change . "Increment minor version for new tests")))))))

;; Export for machine consumption
(define (get-spec-core) spec-core)
