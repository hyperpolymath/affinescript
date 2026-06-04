<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
## 🚨 Disambiguation (read first)

**This repo is `hyperpolymath/affinescript`.** It is **NOT** `hyperpolymath/ephapax`.

| | This repo | NOT this repo |
|---|---|---|
| Name | **AffineScript** | Ephapax |
| Path | `hyperpolymath/affinescript` | `hyperpolymath/ephapax` |
| File extension | `.affine` (plus face dialects) | `.eph` |
| Build | `dune-project` at root | `Cargo.toml` at root |
| Type checker | `lib/borrow.ml` (OCaml) | `ephapax-linear/src/{linear,affine}.rs` (Rust) |
| Proofs | None mechanized; soundness arguments live in `lib/borrow.ml` + `docs/CAPABILITY-MATRIX.adoc` + issue #177 (CORE-01) | `formal/Semantics.v` (Coq), `src/abi/Ephapax/…` (Idris2) |

**The trap.** Ephapax is internally dyadic — it contains `ephapax-linear` and `ephapax-affine` *sublanguages* inside one Rust crate. **The `ephapax-affine` sublanguage is NOT AffineScript.** The word `affine` is shared because both type systems happen to be substructural-logic-family — that's a logic-family fact, not a project relationship.

**Rule for agents:** before applying any prior-session lesson, memory entry, or snippet, check whether it was about *AffineScript* or about *ephapax*. They share zero AST / typing / borrow-checker / codegen. The only shared surface is the compile target (`hyperpolymath/typed-wasm`) and the shared Rust verifier crate (`crates/typed-wasm-verify/` *in* that repo).

When in doubt: state the context shift explicitly ("switching from ephapax context to AffineScript context") so the user sees the boundary respected.

**Canonical disambiguation doc** (single source of truth):
https://github.com/hyperpolymath/nextgen-languages/blob/main/docs/disambiguation/ephapax-vs-affinescript.md

**Companion memory entry** (in user auto-memory):
`feedback_affinescript_ephapax_siblings_not_impl_proof.md`

---

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

> **Policy refresh 2026-05-25**: AffineScript is now the primary application language across the estate (this is its home repo). ReScript / TypeScript / meaningfully-migrated JavaScript are banned going forward — write new code in AffineScript. Existing ReScript stays in place as legacy until the `.res → .affine` migration assistant (#488) walks the corpus. MPL-1.0 / MPL-1.0-or-later are banned; rewrite to MPL-2.0 wherever encountered (code + docs). **Jekyll is banned**; the canonical estate SSG is `hyperpolymath/casket-ssg` (Haskell). This repo already migrated via `.github/workflows/casket-pages.yml`.

### ALLOWED Languages & Tools

| Language/Tool | Use Case | Notes |
|---------------|----------|-------|
| **AffineScript** (`.affine`) | Primary application code | Affine types, dependent types, row polymorphism, extensible effects; compiles to Wasm |
| **Deno** | Runtime & package management | Replaces Node/npm/bun |
| **Rust** | Performance-critical, systems, WASM | Preferred for CLI tools |
| **Tauri 2.0+** | Mobile apps (iOS/Android) | Rust backend + web UI |
| **Dioxus** | Mobile apps (native UI) | Pure Rust, React-like |
| **Gleam** | Backend services | Runs on BEAM or compiles to JS |
| **Bash/POSIX Shell** | Scripts, automation | Keep minimal |
| **Python** | SaltStack only | No other Python permitted |
| **Nickel** | Configuration language | For complex configs |
| **A2ML** | State/meta files | .machine_readable/6a2/STATE.a2ml, .machine_readable/6a2/META.a2ml, .machine_readable/6a2/ECOSYSTEM.a2ml |
| **Julia** | Batch scripts, data processing | Per RSR |
| **OCaml** | AffineScript compiler | Language-specific (compiler-host tooling lives here) |
| **Ada** | Safety-critical systems | Where required |

### LEGACY — exists in tree, but write no new occurrences

| Language | Status | Disposition |
|----------|--------|-------------|
| **ReScript** (`.res`, `.resi`) | Legacy — pre-2026-05-25 | Migrate via `tools/res-to-affine/` (#488) when touching adjacent code; do not add new `.res` files. |
| **JavaScript** (`.js`, `.cjs`, `.mjs`) | Legacy / carve-outs only | Approved runtime-exemption carve-outs (see below) remain; net-new JS in a project already migrated to AffineScript is banned. |

### BANNED — Do Not Use (write zero new occurrences)

| Banned | Replacement |
|--------|-------------|
| TypeScript | **AffineScript** |
| ReScript (new files) | **AffineScript** (migration via #488) |
| JavaScript (where the project has been meaningfully migrated to AffineScript) | **AffineScript** |
| Node.js | Deno |
| npm | Deno |
| Bun | Deno |
| pnpm/yarn | Deno |
| Go | Rust |
| Python (general) | **AffineScript** / Rust / Julia |
| Java/Kotlin | Rust / Tauri / Dioxus |
| Swift | Tauri / Dioxus |
| React Native | Tauri / Dioxus |
| Flutter/Dart | Tauri / Dioxus |
| **MPL-1.0** | **MPL-2.0** (rewrite SPDX headers + LICENSE files wherever encountered, code AND docs) |
| **MPL-1.0-or-later** | **MPL-2.0** (same) |
| **Jekyll** (`jekyll.yml`, `jekyll-gh-pages.yml`, `_config.yml`, `Gemfile`) | **`hyperpolymath/casket-ssg`** (Haskell SSG, owner's own tool). This repo already migrated — see `.github/workflows/casket-pages.yml` for the canonical pattern. Hypatia flags Jekyll filenames as `jekyll_workflow_detected` / `jekyll_gh_pages_workflow_detected` / `jekyll_config_detected` / `gemfile_detected`. |

### Mobile Development

**No exceptions for Kotlin/Swift** - use Rust-first approach:

1. **Tauri 2.0+** - Web UI (AffineScript) + Rust backend, MIT/Apache-2.0
2. **Dioxus** - Pure Rust native UI, MIT/Apache-2.0

Both are FOSS with independent governance (no Big Tech).

### Enforcement Rules

1. **No new TypeScript files** - Write new code in AffineScript (closed exemptions table below covers the residual `.d.ts` / Deno-test cases).
2. **No new ReScript files** - As of 2026-05-25 policy refresh; AffineScript is the go-forward. Existing `.res` files stay until migrated via #488.
3. **No package.json - use deno.json deps** - Use deno.json imports.
4. **No node_modules in production** - Deno caches deps automatically.
5. **No Go code** - Use Rust instead.
6. **Python only for SaltStack** - All other Python must be rewritten.
7. **No Kotlin/Swift for mobile** - Use Tauri 2.0+ or Dioxus.
8. **MPL-1.0 / MPL-1.0-or-later are non-conforming** - Rewrite to MPL-2.0 in SPDX headers and LICENSE files when encountered (Hypatia's `validate_license` flags both).

### TypeScript Exemptions (Approved)

The "no new TypeScript" / "no new JavaScript" rules have approved exemptions in this repo. These paths are *not* policy violations — they are documented carve-outs because the file format or downstream consumer requires the source language. They are honoured by Hypatia's scanner via path-based exemption + the per-repo CLAUDE.md exemption tables.

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

The "no Node.js / no Bun" rules in the language policy table have two approved exemptions in this repo. Adding to this list requires explicit user approval — same gate as the TypeScript exemptions above.

| Path | Banned thing(s) used | Rationale | Unblock condition |
|---|---|---|---|
| `packages/affinescript-cli/mod.js` | `process.platform`/`process.arch`/`process.env`, `node:fs/promises`, `node:child_process`, `Bun.spawn`, `Bun.file`, `Bun.write` | The shim is the **compiler-distribution front door**. Its consumers — LSP installers, IDE extensions, CI scripts wiring AffineScript into a build pipeline — overwhelmingly live in Node and Bun ecosystems, not Deno. Forcing them to install Deno solely to fetch+verify+exec a binary defeats the shim's "ergonomic install" purpose. The branches are guarded by single-line runtime detection at module load; nothing else in the repo depends on this pattern. | None — this is the intended steady state. The shim's whole job is to be runtime-agnostic. |
| `editors/vscode/test/**/*.js` | `node:*`, Mocha, `@vscode/test-electron` (Electron-based VS Code download + launch) | The **in-editor smoke harness** for issue #139 — loads the compiled `out/extension.cjs` in a real VS Code extension host and asserts activation, command registration, `restartLsp` cycling, and `deactivate` teardown. The VS Code extension host is npm/Node-native; `@vscode/test-electron` (the official runner) downloads a real Electron VS Code and launches it under `xvfb-run`. No Deno equivalent exists, and the test cannot be expressed in any other runtime. Scope is strictly `editors/vscode/test/` — no production code uses Node. | None — this is the intended steady state, paralleling the `affine-vscode-publish.yml` workflow that already uses npm at publish time (#104). |

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

## Agent operations notes

Practical guidance for agents (Claude / other) operating in this repo,
captured from parallel-bot session experience. Read once; saves turns.

### CI signal reliability

**"PR merged" does NOT mean "build green".** Auto-merge on this repo
currently fires even when `build` or `lint` is failing — multiple
recently-merged PRs (#334, #335, #336, #344) landed with `build` red,
and the red persisted until PR #346's `FnExtern` interp fix. If you
inherit a session reasoning about a recently-merged PR, do not assume
its CI was green; check `mcp__github__pull_request_read` with method
`get_check_runs` for the actual statuses, and check whether `main`
itself is currently red before treating a build failure on your own
PR as something *you* introduced.

### Reading CI logs

`WebFetch` against the GitHub Actions UI returns the React skeleton,
not the log content. The fast paths for an agent are:

* `mcp__github__pull_request_read` with method `get_check_runs` —
  per-job status (queued / in_progress / success / failure) with
  `details_url`. Sufficient for "did the build pass".
* `mcp__github__pull_request_read` with method `get_status` —
  combined commit status.
* For actual log lines on a failed run, hand back to the user with
  `gh run view --log-failed <run-id>`; do not loop trying to scrape
  the UI.

### Known-failing baseline checks

These checks currently fail on *every* PR for repo-wide reasons, not
because of any individual PR's changes. Do not waste turns
investigating them on a per-PR basis:

* `vscode-smoke` — npm 404 on `@hyperpolymath/affine-vscode` (the
  in-editor harness depends on a not-yet-published npm package).
* `migration-assistant` — was fixed by #342, but any branch created
  from a base older than #342 will still see it red until rebased.
* `governance / Language / package anti-pattern policy` — flags the
  approved TypeScript exemptions (`affinescript-deno-test/*.ts`,
  `editors/vscode/test/*.js`, etc., all documented in this file's
  exemptions tables); the check has no allowlist for them.
* The Hypatia security-scan bot comment — 143 findings; the bulk are
  the same TypeScript exemption hits + pre-existing root files. A
  real new finding will show as a *delta* in the count; otherwise
  ignore.

If a check from this list *changes status* on a PR (e.g.
`vscode-smoke` suddenly passes, or Hypatia surfaces a new class of
finding), that's signal worth investigating.

### Branching discipline with concurrent merges

When multiple agents are spawned in parallel, branch-creation time
can lag `main` by hours and a stale base will silently revert other
agents' work at merge time. **Before pushing any branch, run:**

```
git fetch origin main
git rebase origin/main
```

Not just at branch-creation; immediately before push, after any
in-session work. This guards against parallel-merge drift. Claude 1's
STDLIB-04c branch (#337) accidentally reverted #334 and #335 because
its base was stale; force-rebased to fix. Cheap to prevent, expensive
to clean up.

### Post-squash-merge branch divergence

When a PR is squash-merged, the squashed commit on `main` gets a
*new* SHA, distinct from any of the source-branch commits. If you
then reset your local branch to `main` (or simply re-resolve it),
`git status` reports N "ahead of origin/branch" — but those N
commits are just the main-side commits the obsolete remote
branch-tip never saw, not unpushed work.

Recognising the situation:

* The branch was already merged (PR closed, `merged: true`).
* The local working tree matches `main`.
* The remote branch still points at the *pre-merge* tip
  (`origin/<branch>` is an old SHA, not the squashed one).
* `git log origin/<branch>..HEAD` lists commits that look like
  other people's work.

Safe fix — pick the one matching intent:

```
git push origin --delete <branch>            # done with the branch
git push --force-with-lease origin <branch>  # align the remote to main
```

`--force-with-lease` is safe here because nothing on the remote
branch is unmerged work; force-push without `--lease` only matters
if someone else pushed concurrently, which is irrelevant for an
already-merged branch you're cleaning up.

### Test-fixture hygiene for latent bug surfaces

When you add a stdlib `extern fn` (or any other new declaration
shape), add a test that feeds it to *every* downstream consumer
(parse, resolve, typecheck, interp, every codegen target that
shouldn't reject it). The PR #346 `FnExtern` interp bug had survived
since the interpreter was written because no test had ever fed an
inline `extern fn` to `Interp.eval_program` — STDLIB-04a's tests
were the first, and only then did the missing match arm fire.

Treat "first user of an existing-but-untested declaration shape" as a
class-level surface, not a single test case.

