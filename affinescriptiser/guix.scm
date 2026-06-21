;; SPDX-License-Identifier: MPL-2.0
;; Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
;;
;; Guix package definition for affinescriptiser
;;
;; Usage:
;;   guix shell -D -f guix.scm    # Enter development shell
;;   guix build -f guix.scm       # Build package
;;
;; affinescriptiser is a Rust CLI (cargo build --release). The build phases
;; below are the RSR scaffold default (install README only); wiring the real
;; cargo build is tracked separately.
;; See: https://guix.gnu.org/manual/en/html_node/Defining-Packages.html

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix build-system gnu)
             (guix licenses)
             (gnu packages base))

(package
  (name "affinescriptiser")
  (version "0.1.0")
  (source (local-file "." "source"
                       #:recursive? #t
                       #:select? (lambda (file stat)
                                   (not (string-contains file ".git")))))
  (build-system gnu-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       ;; TODO: wire the real Rust build, e.g.
       ;;   (replace 'build (lambda _ (invoke "cargo" "build" "--release")))
       ;;   (replace 'check (lambda _ (invoke "cargo" "test")))
       (delete 'configure)
       (delete 'build)
       (delete 'check)
       (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (mkdir-p (string-append out "/share/doc"))
             (copy-file "README.adoc"
                        (string-append out "/share/doc/README.adoc"))))))))
  (native-inputs
   (list
    ;; TODO: add build-time dependencies (Rust toolchain) once the cargo
    ;; build phase above is wired.
    ))
  (inputs
   (list
    ;; TODO: add runtime dependencies
    ))
  (home-page "https://github.com/hyperpolymath/affinescriptiser")
  (synopsis "Wrap code in affine + dependent types targeting WASM")
  (description "RSR-compliant project. See README.adoc for details.")
  (license mpl2.0))
