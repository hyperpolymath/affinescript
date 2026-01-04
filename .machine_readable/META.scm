;; SPDX-License-Identifier: AGPL-3.0-or-later
;; META.scm - Meta-level information for affinescript
;; Media-Type: application/meta+scheme

(meta
  (architecture-decisions
    ((adr-001
      (title "Runtime Model: Minimal runtime, rely on host")
      (status "accepted")
      (date "2024-01-01")
      (context "Need to decide on runtime model for WASM target")
      (decision "Minimal runtime focused on AffineScript-specific needs. No GC for most data. Host provides I/O, networking, filesystem.")
      (consequences "Keeps WASM binaries small, maximizes portability"))
     (adr-002
      (title "Effect Compilation: Evidence-passing (Koka-style)")
      (status "accepted")
      (date "2024-01-01")
      (context "Need to choose effect compilation strategy")
      (decision "Effects compiled via evidence-passing transformation. Handlers install evidence at runtime.")
      (consequences "Better performance than CPS, proven in Koka"))
     (adr-003
      (title "WASM Target: Core + WASI with Component Model readiness")
      (status "accepted")
      (date "2024-01-01")
      (context "Need to define WASM feature requirements")
      (decision "WASM core required, WASM GC not required, WASI for CLI/server, Component Model for future")
      (consequences "Broad compatibility now, future-proofed"))
     (adr-004
      (title "SMT Solver: Z3 as optional external dependency")
      (status "accepted")
      (date "2024-01-01")
      (context "Need SMT for refinement type checking")
      (decision "Z3 bindings for refinement type checking. SMT is optional.")
      (consequences "Powerful static verification when available"))
     (adr-005
      (title "Package Manager: Workspace-aware, Cargo-inspired")
      (status "accepted")
      (date "2024-01-01")
      (context "Need package management solution")
      (decision "Single affine.toml manifest. Workspace support. Lock file. Content-addressed storage. Written in Rust.")
      (consequences "Familiar to Rust developers"))))

  (development-practices
    (code-style
      ((language "OCaml")
       (formatter "ocamlformat")
       (style-guide "Standard OCaml conventions")))
    (security
      (principle "Defense in depth")
      (practices
        ("Memory safety via affine types"
         "Effect tracking prevents implicit side effects"
         "Refinement types for runtime invariants")))
    (testing
      ((framework "Alcotest")
       (coverage-target 80)
       (test-types ("unit" "integration" "golden" "conformance"))))
    (versioning "SemVer with edition system")
    (documentation "AsciiDoc for prose, odoc for API docs")
    (branching "main for stable, feature branches for development"))

  (design-rationale
    ((why-affine-types "Track resource usage with quantities for memory safety without GC")
     (why-dependent-types "Types that depend on values enable compile-time verification")
     (why-row-polymorphism "Extensible records for flexible data structures without subtyping")
     (why-effects "User-defined effects make side effects composable and visible")
     (why-ocaml "Mature ecosystem for compiler development")
     (why-wasm "Portable, high-performance execution target"))))