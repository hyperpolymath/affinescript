<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
<!-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell -->

# tree-sitter-rescript (vendoring manifest)

This directory is a **manifest-only vendoring** of the canonical
[`rescript-lang/tree-sitter-rescript`][upstream] grammar. The grammar
itself is not copied into this repository — `package.json` declares it
as a dependency, and `scripts/install.sh` fetches and builds it at the
pinned commit.

The grammar is consumed by `tools/res-to-affine/`, the `.res → .affine`
migration assistant (`affinescript#57`). It is **not** an editor binding
for AffineScript; for that, see `editors/tree-sitter-affinescript/`.

## Pinned upstream

- **Repository:** <https://github.com/rescript-lang/tree-sitter-rescript>
- **Commit:** `990214a83f25801dfe0226bd7e92bb71bba1970f`
- **Version:** 6.0.0
- **License:** MIT (preserved upstream; compatible with this repo's MPL-2.0)

When updating the pin, regenerate `tools/res-to-affine/test/expected/`
snapshots, since AST shapes may shift.

## Install

From the repo root:

```sh
just install-grammar          # justfile recipe
# or directly:
./editors/tree-sitter-rescript/scripts/install.sh
```

This writes a `tree-sitter-rescript` directory under `tools/vendor/`
(gitignored — same convention as the WASI adapter pinning), containing
the generated parser. Requires `git` and the `tree-sitter` CLI on PATH.

The `tree-sitter` CLI can be installed either way:

```sh
cargo install tree-sitter-cli          # Rust-native, repo-preferred
npm install -g tree-sitter-cli         # Node-based, also fine
```

CI installs via `npm` for speed (`tree-sitter-cli` from npm is a pre-built
binary, ~5 s install). The `cargo` path builds from source (~5 min on a
cold cache) and is the recommended local install because it keeps the
contributor toolchain centred on Rust rather than Node. The
`package.json` in this directory pins the version range; bump it in
sync when the upstream grammar pin moves.

## Continuous integration

The `migration-assistant` job in `.github/workflows/ci.yml` runs `just
install-grammar` on every PR, then smoke-parses
`tools/res-to-affine/test/fixtures/sample.res`. If the pinned commit
stops building cleanly, this job is the first signal.

## Why manifest, not copy

The upstream grammar is ~10k lines of JS plus generated C. Copying it
into this MPL-2.0 repo would (a) bloat the tree, (b) create an ongoing
sync burden, and (c) duplicate MIT-licensed code we have no business
modifying. The manifest+install approach keeps the dependency explicit
and pinned without absorbing the source.

[upstream]: https://github.com/rescript-lang/tree-sitter-rescript
