# Session Bindings (Thin Local Layer)

This directory provides local integration for central session-management standards.

Authoritative protocols live in:

- `../standards/session-management-standards/` (or `$SESSION_STANDARDS_DIR`)

This repo keeps only thin bindings:

- `dispatch.sh` maps canonical commands to central protocol paths.
- `custom-checks.k9` defines repo-local policy checks.
- `local-hooks.sh` provides optional repo-specific hook behavior.

## Canonical Commands

- `intake repo <path>`
- `checkpoint change <path>`
- `verify maintenance <path>`
- `verify substantial <path>`
- `verify release <path>`
- `close planned <path>`
- `close urgent <path>`
- `recover repo <path>`
- `handover full <path>`
- `handover split <path>`
- `handover model <path>`
- `handover human <path>`

## Justfile Aliases

Run `just session-help` to list aliases, then use recipes such as:

- `just intake-repo path=.`
- `just checkpoint-change path=.`
- `just verify-maintenance path=.`
- `just close-planned path=.`
- `just handover-model path=.`

## Runtime Artifacts

Runtime files are generated per repository in `.session/` and are not canonical standards text.
