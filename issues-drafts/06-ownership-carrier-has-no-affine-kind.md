<!--
SPDX-License-Identifier: MPL-2.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
-->

# typed-wasm ownership carrier has no `Affine` kind — AffineScript `own` is mis-mapped to Linear (L10, exactly-once)

**Surfaced by:** typed-wasm round-trip verification (2026-06-16, `tools/typed-wasm-roundtrip-gate.sh`)
**Affected:** the `typedwasm.ownership` v1 carrier (cross-repo: AffineScript + ephapax + typed-wasm verifier)
**Severity:** Semantic / contract — not a crash. AffineScript can emit modules its own (affine) semantics accept but the typed-wasm L10 (linear) verifier rejects.

## Observation

The v1 ownership-section `kind` enum is `{0=Unrestricted, 1=Linear,
2=SharedBorrow, 3=ExclBorrow}` (see `docs/specs/TYPED-WASM-INTERFACE.adoc`).
There is **no `Affine` (at-most-once) kind.** AffineScript maps `own` → `Linear`
(`lib/codegen.ml` `ownership_kind_of_param`), and `Linear` is verified as
**exactly-once on every path** (L10).

But AffineScript is **affine**, not linear — dropping an owned value is
legitimate (this is the load-bearing result of the Solo-core affine-preservation
mechanisation: the honest theorem is reduct-in-a-`Weaker`-sub-context; resources
*may* be dropped). So:

```affinescript
fn drop_owned(x: own String) -> () = ();   // x dropped — FINE in affine AffineScript
```

round-trips to a module that **both** verifiers reject as an L10 violation
(verified bit-exactly: AffineScript `tw_verify.ml` and typed-wasm Rust
`tw-verify` give the identical "Level 10 violation: param 0 — Linear (own) param
dropped on all paths"). The mapping is *too strict*: `own` has no faithful
carrier representation — only `Linear` (rejects legal drops) or `Unrestricted`
(loses the discipline entirely).

This is why `compile`'s Stage-8 ownership check is **advisory** (warns, still
emits) while `verify` is **fatal** — the compiler already knows it is affine and
that L10 over-rejects it.

## Why it matters

The integration *works* (carrier round-trips, both verifiers agree) — but it
agrees on a verdict that is wrong *for an affine source language*. As more
affine programs target typed-wasm, the advisory-warning noise grows and the
contract misrepresents the source semantics.

## Fix direction (multi-producer ABI change — coordinate, don't unilaterally patch)

Add an `Affine` kind (at-most-once) to the ownership carrier — a v2 schema
change requiring coordination across AffineScript, `hyperpolymath/ephapax`, and
the `hyperpolymath/typed-wasm` Rust verifier (the spec's stated ABI-change
protocol; cf. ADR-020 "Schema versioning" in `TYPED-WASM-ROADMAP.adoc`). The
verifier would check `Affine` as `≤1-use-on-every-path` (the same machinery as
`ExclBorrow`'s L7 check, minus the aliasing part). Then `own` → `Affine` and the
advisory warnings on legal drops disappear.

Carries the Solo-core decision (`project_affinescript_solo_core_affine_preservation`)
up to the typed-wasm boundary: the affine theorem shape needs an affine carrier
kind. Until then, the `own`→`Linear` mapping + advisory check is the honest
interim.
