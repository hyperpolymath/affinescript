;; SPDX-License-Identifier: MPL-2.0
;; AffineScript Testing Report (retired)
;;
;; This file has been retired.  It carried a 2025-12-29 snapshot
;; (47 PASS / 27 FAIL) that the live CI gate has contradicted since
;; early 2026 (gate is now >250 tests / 0 failures).  Continuing to
;; ship the snapshot would be a DOC-09 over-claim hazard.
;;
;; Authoritative sources:
;;   - docs/CAPABILITY-MATRIX.adoc          (live readiness)
;;   - docs/standards/TESTING.adoc          (test taxonomy + gates)
;;   - docs/TECH-DEBT.adoc                  (ledger / critical path)
;;   - docs/ECOSYSTEM.adoc                  (spine + contract)
;;
;; The companion docs/guides/TESTING-REPORT.adoc carries the
;; human-readable retirement notice and the redirect table.

(testing-report-retired
  (replaced-by
    (capability-matrix "docs/CAPABILITY-MATRIX.adoc")
    (testing-standards "docs/standards/TESTING.adoc")
    (tech-debt-ledger  "docs/TECH-DEBT.adoc")
    (ecosystem-spine   "docs/ECOSYSTEM.adoc"))
  (rationale "stale snapshot contradicted by live gate; retained as redirect to preserve inbound links")
  (retired-on "2026-05-23"))
