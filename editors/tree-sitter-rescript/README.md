<!-- SPDX-License-Identifier: MPL-2.0 -->
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

```sh
./scripts/install.sh
```

This writes a `tree-sitter-rescript` directory under `tools/vendor/`
(gitignored — same convention as the WASI adapter pinning), containing
the generated parser. Requires `git` and `tree-sitter` CLI on PATH.

## Why manifest, not copy

The upstream grammar is ~10k lines of JS plus generated C. Copying it
into this MPL-2.0 repo would (a) bloat the tree, (b) create an ongoing
sync burden, and (c) duplicate MIT-licensed code we have no business
modifying. The manifest+install approach keeps the dependency explicit
and pinned without absorbing the source.

[upstream]: https://github.com/rescript-lang/tree-sitter-rescript
