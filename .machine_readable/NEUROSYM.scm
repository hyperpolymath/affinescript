;; SPDX-License-Identifier: AGPL-3.0-or-later
;; NEUROSYM.scm - Neurosymbolic integration config for affinescript

(define neurosym-config
  `((version . "1.0.0")
    (symbolic-layer
      ((type . "scheme")
       (reasoning . "deductive")
       (verification . "formal")
       (domains
         ((type-theory
           (description . "Bidirectional type inference, unification, constraint solving")
           (formalization . "typed lambda calculus with effects"))
          (ownership
           (description . "Affine type tracking, borrow checking")
           (formalization . "linear logic with quantities"))
          (effects
           (description . "Algebraic effect inference and handler checking")
           (formalization . "effect algebras with row polymorphism"))))))
    (neural-layer
      ((embeddings . false)
       (fine-tuning . false)
       (code-completion
         (enabled . true)
         (context . "type-aware")
         (constraints . ("respect-ownership" "infer-effects")))
       (error-explanation
         (enabled . true)
         (style . "pedagogical")
         (include . ("type-errors" "borrow-errors" "effect-errors")))))
    (integration
      ((smt-solver
        (name . "z3")
        (purpose . "refinement-type-checking")
        (optional . true))
       (static-analysis
        (tools . ("ocaml-lsp" "merlin"))
        (integration . "editor"))
       (symbolic-execution
        (enabled . false)
        (future . "for-verification"))))))
