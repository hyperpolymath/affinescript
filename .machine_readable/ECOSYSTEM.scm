;; SPDX-License-Identifier: AGPL-3.0-or-later
;; ECOSYSTEM.scm - Ecosystem position for affinescript
;; Media-Type: application/vnd.ecosystem+scm

(ecosystem
  (version "1.0")
  (name "AffineScript")
  (type "programming-language")
  (purpose "A systems programming language combining affine types, dependent types, row polymorphism, and extensible effects, compiling to WebAssembly")

  (position-in-ecosystem
    (category "Programming Languages")
    (subcategory "Systems Programming / Type-Safe Languages")
    (unique-value
      ("First language to combine affine types, dependent types, row polymorphism, and algebraic effects"
       "No garbage collector - ownership model handles memory"
       "WebAssembly-first compilation target"
       "Practical dependent types without full theorem prover complexity")))

  (related-projects
    ((rust
      (relationship "inspiration")
      (description "Ownership model, borrowing, no GC design inspired by Rust"))
     (idris
      (relationship "inspiration")
      (description "Dependent types and totality checking concepts from Idris/Agda"))
     (koka
      (relationship "inspiration")
      (description "Algebraic effects and evidence-passing compilation from Koka"))
     (purescript
      (relationship "inspiration")
      (description "Row polymorphism concepts from PureScript"))
     (linear-haskell
      (relationship "inspiration")
      (description "Quantitative type theory from Linear Haskell"))
     (januskey
      (relationship "sibling-standard")
      (description "Identity framework that could use AffineScript for verified implementations"))
     (bunsenite
      (relationship "potential-consumer")
      (description "Configuration management could benefit from type-safe config validation"))))

  (what-this-is
    ("A programming language with compile-time resource tracking"
     "A compiler targeting WebAssembly"
     "A type system combining multiple advanced features"
     "A practical tool for writing safe, efficient code"
     "An OCaml-based compiler implementation"))

  (what-this-is-not
    ("Not a general-purpose scripting language"
     "Not a dynamic language"
     "Not a JVM/CLR target language"
     "Not a full theorem prover"
     "Not a garbage-collected runtime")))