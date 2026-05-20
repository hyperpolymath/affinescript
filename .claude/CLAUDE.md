## Machine-Readable Artefacts

The following files in `.machine_readable/` contain structured project metadata:

- `.machine_readable/6a2/STATE.a2ml` - Current project state and progress
- `.machine_readable/6a2/META.a2ml` - Architecture decisions and development practices
- `.machine_readable/6a2/ECOSYSTEM.a2ml` - Position in the ecosystem and related projects
- `.machine_readable/6a2/AGENTIC.a2ml` - AI agent interaction patterns
- `.machine_readable/6a2/NEUROSYM.a2ml` - Neurosymbolic integration config
- `.machine_readable/6a2/PLAYBOOK.a2ml` - Operational runbook

---

# CLAUDE.md - AI Assistant Instructions

## Language Policy (Hyperpolymath Standard)

### ALLOWED Languages & Tools

| Language/Tool | Use Case | Notes |
|---------------|----------|-------|
| **ReScript** | Primary application code | Compiles to JS, type-safe |
| **Deno** | Runtime & package management | Replaces Node/npm/bun |
| **Rust** | Performance-critical, systems, WASM | Preferred for CLI tools |
| **Tauri 2.0+** | Mobile apps (iOS/Android) | Rust backend + web UI |
| **Dioxus** | Mobile apps (native UI) | Pure Rust, React-like |
| **Gleam** | Backend services | Runs on BEAM or compiles to JS |
| **Bash/POSIX Shell** | Scripts, automation | Keep minimal |
| **JavaScript** | Only where ReScript cannot | MCP protocol glue, Deno APIs |
| **Python** | SaltStack only | No other Python permitted |
| **Nickel** | Configuration language | For complex configs |
| **A2ML** | State/meta files | .machine_readable/6a2/STATE.a2ml, .machine_readable/6a2/META.a2ml, .machine_readable/6a2/ECOSYSTEM.a2ml |
| **Julia** | Batch scripts, data processing | Per RSR |
| **OCaml** | AffineScript compiler | Language-specific |
| **Ada** | Safety-critical systems | Where required |

### BANNED - Do Not Use

| Banned | Replacement |
|--------|-------------|
| TypeScript | ReScript |
| Node.js | Deno |
| npm | Deno |
| Bun | Deno |
| pnpm/yarn | Deno |
| Go | Rust |
| Python (general) | ReScript/Rust |
| Java/Kotlin | Rust/Tauri/Dioxus |
| Swift | Tauri/Dioxus |
| React Native | Tauri/Dioxus |
| Flutter/Dart | Tauri/Dioxus |

### Mobile Development

**No exceptions for Kotlin/Swift** - use Rust-first approach:

1. **Tauri 2.0+** - Web UI (ReScript) + Rust backend, MIT/Apache-2.0
2. **Dioxus** - Pure Rust native UI, MIT/Apache-2.0

Both are FOSS with independent governance (no Big Tech).

### Enforcement Rules

1. **No new TypeScript files** - Convert existing TS to ReScript
2. **No package.json for runtime deps** - Use deno.json imports
3. **No node_modules in production** - Deno caches deps automatically
4. **No Go code** - Use Rust instead
5. **Python only for SaltStack** - All other Python must be rewritten
6. **No Kotlin/Swift for mobile** - Use Tauri 2.0+ or Dioxus

### TypeScript Exemptions (Approved)

The "no new TypeScript" rule has seven approved exemptions in this repo. These paths are *not* policy violations — they are documented carve-outs because the file format or downstream consumer requires TypeScript:

| Path | Files | Rationale | Unblock condition |
|---|---|---|---|
| `packages/affine-js/types.d.ts` | 1 | TypeScript declaration file — the public API contract by which JS callers consume AffineScript-compiled artefacts. `.d.ts` is TS by definition. | Generate from canonical compiler output (issue: see ROADMAP). |
| `packages/affinescript-cli/mod.d.ts` | 1 | TypeScript declaration file for the JSR shim's public API. Required by JSR's "fast type-check" — without it, the published package emits a `unsupported-javascript-entrypoint` warning and consumers get no editor types. `.d.ts` is TS by definition. | Same as `affine-js`: generate or rewrite the shim entry once stdlib + bindings exist. |
| `affinescript-deno-test/*.ts` | 6 | Deno-based test harness for AffineScript itself: `cli.ts`, `mod.ts`, `lib/{compile,discover,runner}.ts`, `example/smoke_driver.ts`. Deno test runner is TS-native. | AffineScript stdlib + Deno bindings (no scheduled issue). |

Adding to this list requires explicit user approval and an unblock condition. New TypeScript files outside this list are still banned per the policy table above.

**Closed exemptions:**
- `editors/vscode/src/extension.ts` — closed 2026-05-03 by issue #35 Phase 3 (Node-target codegen + Vscode bindings landed). Source of truth is now `editors/vscode/src/extension.affine`, compiled to `out/extension.cjs`. Re-introducing the .ts file is blocked by `tools/check-no-extension-ts.sh` (wired into `just check` and `.github/workflows/ci.yml`).
- `packages/affine-ts/` — removed 2026-05-11 (issue #66 closed won't-do). TypeScript is not a first-class consumer; TS callers should use `@hyperpolymath/affine-js` directly and supply their own typings.

The 5 external references to `affinescript-deno-test/` (CI workflow, status docs, history docs) and the references to `packages/affine-js/` (status docs, Deno config) are why physical relocation into a `vendor/` subtree was rejected — the relocation cost exceeded the visibility benefit when the directories are already named clearly.

### Runtime Exemptions (Approved)

The "no Node.js / no Bun" rules in the language policy table have one approved exemption in this repo. Adding to this list requires explicit user approval — same gate as the TypeScript exemptions above.

| Path | Banned thing(s) used | Rationale | Unblock condition |
|---|---|---|---|
| `packages/affinescript-cli/mod.js` | `process.platform`/`process.arch`/`process.env`, `node:fs/promises`, `node:child_process`, `Bun.spawn`, `Bun.file`, `Bun.write` | The shim is the **compiler-distribution front door**. Its consumers — LSP installers, IDE extensions, CI scripts wiring AffineScript into a build pipeline — overwhelmingly live in Node and Bun ecosystems, not Deno. Forcing them to install Deno solely to fetch+verify+exec a binary defeats the shim's "ergonomic install" purpose. The branches are guarded by single-line runtime detection at module load; nothing else in the repo depends on this pattern. | None — this is the intended steady state. The shim's whole job is to be runtime-agnostic. |

Browsers and Cloudflare Workers are NOT supported and never will be (the shim's purpose — fetch, save to disk, exec a native binary — cannot be done in a sandboxed JS runtime). The JSR runtime-compatibility checkboxes for this package should be: Deno ✅, Bun ✅, Node ✅, Workers ❌, Browsers ❌.

### Package Management

- **Primary**: Guix (guix.scm)
- **Fallback**: Nix (flake.nix)
- **JS deps**: Deno (deno.json imports)

### Security Requirements

- No MD5/SHA1 for security (use SHA256+)
- HTTPS only (no HTTP URLs)
- No hardcoded secrets
- SHA-pinned dependencies
- SPDX license headers on all files

## Hypatia and gitbot-fleet standing rules

Standing governance rules for the Hypatia review bot and the gitbot-fleet
auto-merge bot. These are estate-wide policies; this section is the canonical
copy that bots read at PR-review time. Keep this section standalone so it can
be read cold without requiring the rest of CLAUDE.md as context.

### DOC-FORMAT
Rule: All documentation files in docs/ must use .adoc extension.
Exception: GitHub community-health files must use .md by name (CONTRIBUTING.md, CODE_OF_CONDUCT.md, SECURITY.md, CHANGELOG.md, README.md).
Action (Hypatia): On any PR that adds or renames a docs/ file with .md extension (excluding the above exceptions), post a review comment requesting rename to .adoc and request-changes.
Action (gitbot): Reject auto-merge for any PR with a docs/ .md file that is not a community-health file name.

### DOC-DEDUP
Rule: Before creating a new docs/specs/*.adoc or adding a major new section to SPEC.adoc, search existing specs for overlapping coverage.
Action (Hypatia): On any PR that closes the same issue number as another open PR, post a comment listing both PRs and ask the author to confirm which is canonical.
Action (gitbot): Block merge if two open PRs share a "Closes #N" reference to the same issue.

### STACK-SIGNAL
Rule: A docs PR that documents a feature implemented in a sibling feature PR must declare the dependency in its PR body.
Format: Add a line "Depends on: #NNN" near the top of the PR body.
Action (Hypatia): On any docs PR whose title or body references types/grammar/syntax added in an open feat PR, check whether "Depends on:" is present; if missing, request it.
Action (gitbot): Do not auto-merge a PR with "Depends on: #NNN" until #NNN is merged.

### ISSUE-CLOSURE
Rule: An issue is closed only by explicit merge of a PR with "Closes #N" OR by explicit human comment agreement in the issue thread. bots may not self-close issues.
Action (Hypatia): When asked to close an issue, confirm via a reply on the issue thread before doing so.
Action (gitbot): Never use GitHub's "close issue" API directly; only close via PR merge with "Closes #N" keywords.

