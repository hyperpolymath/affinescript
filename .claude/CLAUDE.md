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

The "no new TypeScript" rule has nine approved exemptions in this repo. These paths are *not* policy violations — they are documented carve-outs because the file format or downstream consumer requires TypeScript:

| Path | Files | Rationale | Unblock condition |
|---|---|---|---|
| `packages/affine-js/types.d.ts` | 1 | TypeScript declaration file — the public API contract by which JS callers consume AffineScript-compiled artefacts. `.d.ts` is TS by definition. | Generate from canonical compiler output (issue: see ROADMAP). |
| `packages/affine-ts/types.d.ts` | 1 | Same, for TS callers. | Same as above. |
| `editors/vscode/src/extension.ts` | 1 | VS Code extension entry point. Path pinned by `package.json`'s `main` field. | AffineScript issue #35 (Node-target codegen). |
| `affinescript-deno-test/*.ts` | 6 | Deno-based test harness for AffineScript itself: `cli.ts`, `mod.ts`, `lib/{compile,discover,runner}.ts`, `example/smoke_driver.ts`. Deno test runner is TS-native. | AffineScript stdlib + Deno bindings (no scheduled issue). |

Adding to this list requires explicit user approval and an unblock condition. New TypeScript files outside this list are still banned per the policy table above.

The 5 external references to `affinescript-deno-test/` (CI workflow, status docs, history docs) and the 3 references to `packages/affine-{js,ts}/` (status docs, Deno config) are why physical relocation into a `vendor/` subtree was rejected — the relocation cost exceeded the visibility benefit when the directories are already named clearly.

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

