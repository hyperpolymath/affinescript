;; SPDX-License-Identifier: PMPL-1.0-or-later
;; PLAYBOOK.scm - Operational runbook for affinescript

(define playbook
  `((version . "1.0.0")
    (procedures
      ((build
        (steps
          (("Install OCaml 5.1+" . "opam switch create . ocaml-base-compiler.5.1.0")
           ("Install dependencies" . "opam install . --deps-only --with-test --with-doc")
           ("Build compiler" . "dune build")
           ("Run tests" . "dune runtest")
           ("Build docs" . "dune build @doc")))
        (artifacts . ("_build/default/bin/main.exe")))
       (test
        (commands
          (("Unit tests" . "dune runtest")
           ("Lexer tests" . "dune runtest test/test_lexer.ml")
           ("Parser tests" . "dune runtest test/test_parser.ml")
           ("Golden tests" . "dune runtest test/test_golden.ml")
           ("Conformance tests" . "dune runtest conformance")))
        (coverage . "bisect-ppx"))
       (lint
        (commands
          (("Format check" . "dune fmt --check")
           ("Format fix" . "dune fmt"))))
       (release
        (steps
          (("Update version" . "edit dune-project")
           ("Update changelog" . "edit RELEASE.md")
           ("Build release" . "dune build --release")
           ("Tag release" . "git tag vX.Y.Z")
           ("Push tags" . "git push --tags"))))
       (debug
        (tools
          (("OCaml debugger" . "ocamldebug")
           ("Trace execution" . "OCAMLRUNPARAM=b dune exec affinescript")
           ("Profile" . "perf record dune exec affinescript")))
        (common-issues
          (("Parser conflicts" . "Check parser.mly for shift/reduce conflicts")
           ("Type errors" . "Use merlin for type information")
           ("Build failures" . "Run opam reinstall . for dependency issues"))))))
    (alerts
      ((ci-failure
        (severity . "high")
        (action . "Check GitHub Actions logs, fix failing tests"))
       (security-vulnerability
        (severity . "critical")
        (action . "Update dependencies, run security audit"))))
    (contacts
      ((maintainer . "hyperpolymath")
       (issues . "github.com/hyperpolymath/affinescript/issues")))
    (commands
      ((just-build . "just build")
       (just-test . "just test")
       (just-lint . "just lint")
       (just-fmt . "just fmt")
       (just-doc . "just doc")
       (just-lex . "just lex <FILE>")
       (just-parse . "just parse <FILE>")
       (just-golden-path . "just golden-path")))))
