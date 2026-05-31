<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `affinescript` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(res-to-affine): partial-port mode #488 slice 3 — `--partial` now translates array literals (`[a, b]`) and record literals (`{x, y}` → `Rec #{ x: x, y: y }`, with a nominal placeholder type + field-punning expansion) (Refs #488)
- feat(res-to-affine): partial-port mode #488 slice 2 — `--partial` now desugars ReScript pipe-first `->` (`a->f(b)` → `f(a, b)`, chained left-to-right), and translates `if`/`else` and blocks with `let` statements (Refs #488)
- feat(res-to-affine): partial-port mode (#488) — new `--partial` flag renders module-top-level functions as AffineScript `fn` skeletons with `switch`→`match` and best-effort expression translation (literals / idents / calls / binary ops with float-op + identity-equality normalisation / `++` / member + qualified access / ternary / variant + tuple + literal patterns); un-translatable forms become `() /* TODO */` / `_ /* TODO */` holes. Output deliberately does NOT type-check but parses (verified). Distinct model from `--translate` (Refs #488)
- feat(res-to-affine): Phase 3 slice 3 — `--translate` now also lowers module-level `let <id> = <literal>` (int/float/string/bool) to a typed `const name: T = value;`; call / `ref(...)` / destructuring bindings are skipped (not compile-time constants); every emitted form verified compilable via `main.exe check`. `switch`→`match` and qualified-path resolution remain out of the standalone-type-check scope (Refs #57)
- feat(res-to-affine): Phase 3 slice 2 — `--translate` now also handles record types (→ `struct`) and generics (type params `'a` → `[A]`) across aliases / sums / records; `mutable`/optional-`?` records, qualified paths, and nested generics are still skipped (never guessed); every emitted form verified compilable via `main.exe check` (Refs #57)
- feat(res-to-affine): Phase 3 slice 1 — `--translate` renders fully-structural type declarations (primitive aliases + simple sum types) into compilable AffineScript; conservative (generics / qualified paths / records / non-primitive payloads are skipped, never guessed); walker-only (Refs #57)
- feat(stdlib/Http): RSR rewire — surface `hpm-http-rsr` Zig FFI (10 server-side externs: listen / port / free / accept / method / path / header / body / respond / request-free) + opaque `HpmHttpServer` + `HpmHttpRequest` types; native-only (#425)
- feat(stdlib/json): v0.3 — RSR rewire to `hpm-json-rsr` Zig FFI (11 externs + opaque `HpmJsonValue` + `parse` / `to_json`), Deno-ESM lowering via `__as_hpmJson*` shims (#421)
- feat(parser): trailing-comma in fn params and expr lists (Refs gitbot-fleet#148) (#370)
- feat(lexer): underscore-prefix idents `_key`/`_unused` (Refs gitbot-fleet#148) (#373)
- feat(parser): record-update spread at start `#{ ..base, f: v }` (Refs gitbot-fleet#148) (#376)
- feat(parser): fn-type with effect arrow in type position (Refs gitbot-fleet#148)
- feat(borrow): CFG-join for ExprHandle + ExprTry catch arms (CORE-01 pt3 Slice C-light, Refs #177) (#358)
- feat(tw_verify): v2-parse support for affinescript.ownership (ADR-020) (#352)
- feat(wasi): #180 ADR-015 S6b — sockets on-ramp via net_shutdown
- feat(stdlib): STDLIB-04e — wire `string_to_int` alias + lock pure-extern semantics (Closes #332) (#338)
- feat(stdlib): STDLIB-04b — wire Throws extern `error&lt;T&gt;` (Closes #329) (#340)
- feat(wasm): byte-level load/store IR + env_at/arg_at (ADR-015 S5) (#339)

### Fixed

- fix(vscode-smoke): SKIP cleanly when @hyperpolymath/affine-vscode is unpublished (#381)
- fix(governance): rename CLAUDE.md TypeScript exemptions heading to match workflow regex
- fix(stdlib): wire env_at / arg_at surface — codegen lowers via gen_str_at_via_get (ADR-015 S5, #180) (#364)
- fix(ci): unblock the PR queue — bench/dune + adapter-load + .res-fixture exemption (#361)
- fix(interp): wire missing string_length builtin (Refs #332, #329) (#362)
- fix(borrow): escape `(*r` inside doc-comment examples (unblocks main … (#349)
- fix(interp): `eval_decl` handles `FnExtern` (#328 build-failure root cause) (#346)
- fix(version): single source of truth via lib/version.ml + tag-time bake (#297) (#300)
- fix(shim): relicense the JSR shim package to MPL-2.0 (#299)
- fix(release): scope checksums-job gh calls with --repo to avoid the git probe (#294)

### Changed

- refactor(codegen): extract affinescript.ownership emission to lib/tw_… (#347)

### Documentation

- docs(claude): refresh language-policy tables for 2026-05-25 estate policy (#363)
- docs: restore ADR-020 + ADR-021 + coordination ledger (lost in #344 squash) (#350)
- docs(CLAUDE.md): agent operations notes from parallel-bot session exp… (#348)
- docs(adr-015): settle the S4 / S5 / S6c numbering drift (#180)
- docs(adr): CORE-02 / #234 / ADR-016 — truth ledger to "DELIVERED" (#336)
- docs(tech-debt): split STDLIB-04 into 04a–04e per per-extern audit (Refs #175) (#333)
- docs(res-to-affine): corpus run + regex precision fixes (Refs #57) (#319)
- docs: README internal-drift fix + DOC-04/05/06 done-in-tree (Refs #176, Refs #175) (#315)
- docs: post-#303 catch-up — #297/#300/#301/#302/#304 + repos-monorepo retirement (#305)
- docs: catch up TECH-DEBT, PACKAGING + STATE.a2ml with the 2026-05-20 JSR publish (#303)

### CI

- ci(migration-assistant): fix smoke-parse for tree-sitter-cli 0.25 (#342)
- ci: bump github/codeql-action from 4.32.6 to 4.36.0 (#323)
- ci: bump actions/upload-artifact from 4.6.2 to 7.0.1 (#324)
- ci: bump actions/github-script from 8.0.0 to 9.0.0 (#325)
- ci: bump actions/checkout from 4 to 6 (#326)

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->
