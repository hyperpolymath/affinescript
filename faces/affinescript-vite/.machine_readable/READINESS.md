<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk> -->

# affinescript-vite Component Readiness Assessment

**Standard:** [Component Readiness Grades (CRG) v2.2](https://github.com/hyperpolymath/standards/tree/main/component-readiness-grades)
**Current Grade:** C
**Assessed:** 2026-04-06
**Assessor:** Jonathan D.A. Jewell

---

## Summary

| Component           | Grade | Release Stage | Evidence Summary                          |
|---------------------|-------|---------------|-------------------------------------------|
| Primary component   | C     | Alpha-stable  | Dogfooded on own project; CI passing      |

**Overall:** Grade C — dogfooding confirmed, CI passing, deep annotation in place.

---

## Grade C Evidence

- Deployed and dogfooded on the affinescript-vite project itself
- CI passing (dogfood-gate, hypatia-scan, static-analysis-gate)
- TEST-NEEDS.md documents test matrix
- No home failures
- Deep code and folder annotation in place per CRG v2 requirements

---

## Promotion Path to Grade B

Grade B requires: **6+ diverse external targets tested, issues fed back**.

Diversity means: different languages, different architectures, different use cases.

To reach B:
1. Deploy on at least 6 external projects that differ meaningfully from each other
2. Confirm it works in each (or document failures)
3. Feed back any issues found (GitHub issues or PRs)
4. Update this file with the evidence

---

## Concerns and Maintenance Notes

*Document any known limitations, demotion risks, or maintenance concerns here.*

---

## Run `just crg-badge` to generate the shields.io badge for your README.
