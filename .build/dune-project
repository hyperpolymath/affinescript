(lang dune 3.14)

(using menhir 3.0)

(name affinescript)

(version 0.1.0)

(generate_opam_files true)

(source
 (github hyperpolymath/affinescript))

(authors "hyperpolymath")

(maintainers "hyperpolymath")

(license "MIT OR AGPL-3.0-or-later")

(documentation https://github.com/hyperpolymath/affinescript)

(package
 (name affinescript)
 (synopsis "A programming language with affine types, dependent types, row polymorphism, and extensible effects")
 (description
  "AffineScript combines Rust-style ownership (affine types), compile-time size verification (dependent types), extensible records (row polymorphism), and trackable side effects (extensible effects). It compiles to WebAssembly for efficient, portable execution.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.14))
  (menhir (>= 20231231))
  (sedlex (>= 3.2))
  (ppx_deriving (>= 5.2))
  (ppx_sexp_conv (>= 0.16))
  (sexplib0 (>= 0.16))
  (fmt (>= 0.9))
  (cmdliner (>= 1.2))
  (alcotest (and (>= 1.7) :with-test))
  (odoc (and (>= 2.4) :with-doc)))
 (tags
  (programming-language compiler webassembly affine-types dependent-types row-polymorphism effects ocaml)))
