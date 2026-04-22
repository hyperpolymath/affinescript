<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- Last updated: 2026-04-04 -->

# Architecture Topology

## System Overview

RSR (Rhodium Standard Repository) template provides the canonical scaffold for all hyperpolymath projects, with integrated CI/CD, documentation, and service discovery patterns.

## Component Overview

| Component | Language | Purpose |
|-----------|----------|---------|
| dogfood-gate workflow | YAML | Quality checks (CRG, security, linting) |
| eclexiaiser-validate job | YAML | Resource cost awareness scoring |
| Groove discovery | JSON | Service endpoint registration |

## Data Flow

```
[Code Push] → [GitHub Actions] → [hypatia scan] → [eclexiaiser validate] → [Results]
```

## Integration Points

- **Upstream**: Hypatia (neurosymbolic CI/CD), eclexiaiser (resource scoring)
- **Downstream**: All RSR-based repositories (500+ instances)

## Deployment

- Container: Stapeln Six ecosystem
- CI/CD: GitHub Actions → Hypatia scan → eclexiaiser-validate (6 scorecard dimensions) → Mirror
- Service Discovery: Groove protocol (.well-known/groove/manifest.json)
