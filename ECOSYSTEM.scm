;; SPDX-License-Identifier: AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;; ECOSYSTEM.scm — affinescript

(ecosystem
  (version "1.0.0")
  (name "affinescript")
  (type "project")
  (purpose "**AffineScript** is a programming language that combines:")

  (position-in-ecosystem
    "Part of hyperpolymath ecosystem. Follows RSR guidelines.")

  (related-projects
    (project (name "rhodium-standard-repositories")
             (url "https://github.com/hyperpolymath/rhodium-standard-repositories")
             (relationship "standard")))

  (what-this-is "**AffineScript** is a programming language that combines:")
  (what-this-is-not "- NOT exempt from RSR compliance"))
