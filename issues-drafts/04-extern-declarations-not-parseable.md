<!--
SPDX-License-Identifier: MPL-2.0
Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->
# `extern type` / `extern fn` declarations not parseable in user source

**STATUS: CLOSED 2026-05-03** — `extern fn name(...) -> Ret;` and
`extern type Name;` both parse, resolve, typecheck, and emit
`(import "env" "<name>" (func ...))` in the WASM target. New `EXTERN`
keyword in lexer/token/parse_driver, new `FnExtern` / `TyExtern` AST
variants, new parser rules in `lib/parser.mly`. See `STATE.a2ml`
`session-note-2026-05-03-c` and the `E2E Externs` test suite. Vscode
bindings (`stdlib/Vscode.affine`) are the first real consumer. Original
issue text preserved below for historical context.

---

**Surfaced by:** IDApTIK migration / `affinescript-pixijs` integration attempt (2026-05-02)
**Affected version:** v0.1.0
**Severity:** Blocking for `@affinescript/pixijs` and any other connector package that uses FFI-style imports. The package's own `src/pixi.as` cannot be compiled by today's toolchain.

## Reproducer

The shape used by `affinescript/affinescript-pixijs/src/pixi.as`:

```affinescript
extern type Application;
extern fn createApplication(width: Int, height: Int) -> Application;

pub fn init_pixi(width: Int, height: Int) -> Application {
  createApplication(width, height)
}
```

```
$ affinescript check pixi.affine
pixi.affine:1:1: parse error: Syntax error
affinescript: Parse error
```

`extern` is rejected at the very first character — it's not a recognised keyword in the user-source grammar.

## Why this matters

`affinescript-pixijs/src/pixi.as` (the canonical PixiJS connector) is structured around `extern type Application` / `extern fn createApplication(...)` declarations whose implementations are intended to be linked at runtime via "Typed WASM imports" (per the file's header comment). Without `extern` parsing, the connector itself cannot be compiled, which means any consumer of `@affinescript/pixijs` is blocked.

For IDApTIK specifically, this gates the entire screen-and-rendering side of the migration (Wave 3 popups, screens, UI primitives) because they all need to call into PixiJS.

## Cross-reference

- This is the connector-layer counterpart to issue #03 (effect handling) — both are about how AffineScript code talks to its host environment. `extern` is the structural mechanism; effect handlers are the runtime mechanism.
- A fix here unblocks `@affinescript/pixijs`, `@affinescript/dom`, and any future `@affinescript/*` package that uses FFI imports.
- If `extern` is intentionally being held back pending a final design, document the interim shape (e.g. `@module` annotations? hand-written wasm imports table?) so connector authors know what to write.
