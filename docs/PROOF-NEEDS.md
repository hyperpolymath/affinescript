# PROOF-NEEDS.md — AffineScript proof obligations & status ledger

> Scope: **AffineScript** (`hyperpolymath/affinescript`, the OCaml compiler).
> NOT ephapax — see `.claude/CLAUDE.md` disambiguation. The only shared formal
> surface is the compile target `hyperpolymath/typed-wasm` and its Rust verifier
> crate.

This file is the single ledger for "what needs proving, what is proved, what is
only argued in prose, and what is *refuted*." It is the proof-side companion to
`docs/TEST-NEEDS.md` (test gaps) and `docs/CAPABILITY-MATRIX.adoc` (capability
claims). Keep it truthful: a row here is load-bearing for any "is this sound?"
answer. Last reconciled 2026-06-16.

## Headline status

**AffineScript has zero mechanised proofs today.** Every soundness property is
currently argued in prose only (`lib/borrow.ml` comments + `CAPABILITY-MATRIX`).
The formal-proof programme is filed as issues **#513–#521 and is unstarted**.
The borrow checker's canonical use-after-move hole (#554) is **closed** (PR #595:
a per-function return-borrow summary driven to a call-graph fixpoint; tested,
incl. transitive and three adversarial-hardening variants) — but that is a
*tested implementation*, not a *mechanized proof*. The first proof avenue with a
concrete plan is *performance*, not safety: ADR-0026 (Isabelle complexity,
Proposed).

Truth-before-vision (owner directive `feedback_fixes_first_firm_foundations`):
do not describe any property below as "proved" until a checker (Isabelle/Coq/…)
actually accepts it. Prose arguments are recorded as **argued**, never **proved**.

## Legend

- **Proved** — machine-checked by a prover that actually runs green here.
- **Argued** — soundness sketch in prose/comments; no mechanisation.
- **Refuted** — an execution-verified counterexample exists; the current
  implementation is unsound on that input.
- **Vacuous** — the feature parses but is not enforced, so any guarantee about
  it is currently empty.
- **Unchecked** — the obligation is recognised but nothing verifies it.

## Proof obligations

| # | Obligation | Status | Mechanisation target | Plan / blocker | Tracking |
|---|---|---|---|---|---|
| P1 | **Borrow-checker soundness** (no use-after-move) | **Argued; canonical hole closed (tested, not proved)** | (none yet) | The #554 use-after-move via a callee-returned borrow (`let r = pick(a); consume(a); *r`) is now **caught** — a per-function return-borrow summary + call-graph fixpoint in `lib/borrow.ml` (PR #595), tested incl. transitive + 3 hardening variants; the summary over-approximates origins (sound direction). Remaining: a *mechanized* proof (#515, deferred per #563) and full Polonius region/loan precision for flows beyond syntactic return-borrows. | #554 (closed), #553/#515 (Polonius/proof) |
| P2 | **Effect-handler lowering correctness** (handler arms preserved across backends) | **Refuted** | (none yet) | core-WASM / JS-text / Deno-ESM silently drop handler arms (interp 42 vs wasm 41 on an effects-free return-arm program). Dispatch is shallow single-shot tail-resume only; zero runtime handler tests. Plan: VM M2 multi-shot + back-port. | #555 |
| P3 | **Async CPS lowering correctness** | **Refuted (silent)** | (none yet) | Table-miss fallback silently lowers synchronously. Filed; no implementation. | #556 |
| P4 | **Refinement-type predicate enforcement** | **Vacuous** | (none yet) | Predicates parse but are not enforced — any refinement guarantee is currently empty. Must enforce before any soundness statement is meaningful. | #558 |
| P5 | **Trait coherence** | **Unchecked** | (none yet) | Coherence/overlap not checked → incoherent instances can break type safety. | #559 |
| P6 | **typed-wasm ownership mapping faithfulness** (`own` ≙ affine ≤1) | **Argued, known-wrong** | typed-wasm verifier (Rust) + conformance | The carrier has no `Affine` kind, so `own` is mis-mapped to L10 **Linear** (exactly-once). The emitted `typedwasm.ownership` section is unfaithful for affine values. Needs an `Affine` kind upstream in `typed-wasm`, then a faithfulness argument/conformance. | issue-draft 06 |
| P7 | **Core-wasm value-representation soundness** (heap cells model their type) | **Argued; durable fix landed for ALL heap aggregates** | wasm-validate gate (15 positive checks, incl. wasmtime f64 round-trips) + (future) Isabelle/property | `Float`-through-heap was silently mismodelled (32-of-64-bit truncation / invalid module). Now type-directed via the **Float wall**: `synth` records every `Float` cell node (total coverage), `elaborate` rewrites to `ExprFloatArray/Index`, `ExprCellTuple/Index`, `ExprCellRecord/Field`, codegen emits 8-byte cells (uniform-8 for heterogeneous tuples/records; records placed by sorted field name). **DONE & wasmtime-verified:** `Array[Float]` (read/write/construct/nested), tuples (all-Float, mixed, both orderings), `Array[(Float,Float)]`, **closed records** (incl. literal-order≠name-order). **Still loud-fails CLEANLY (safe):** `Float` in **closures** (an f64-aware calling-convention gap, task #8), float array compound-assign, open/polymorphic record rows. | issue-draft 05 |
| P8 | **Compile-time complexity is linear** (resolve/codegen O(n)) | **Argued + bench-evidenced; proof planned** | **Isabelle/HOL** (AFP `Time_Monad` / Akra-Bazzi) | Scaling bench shows parse/resolve/codegen now flat after the O(n²) fixes (issue-draft 07, resolved). ADR-0026 F1 proposes the Isabelle proof as the durable guard. **Awaiting owner sign-off on ADR-0026.** | issue-draft 07, ADR-0026 |
| P9 | **Polonius loop soundness** (slice-C′ 2-iteration re-check) | **Argued** | (none yet) | Loop soundness via a 2-iteration re-check (#396/#399); part of the borrow story, downstream of P1. | #396, #399, #553 |

## Mechanisation targets (which prover, when chosen)

- **Complexity (P8):** Isabelle/HOL — the estate already uses AFP running-time
  tooling (`Time_Monad`, Akra-Bazzi, Amortized_Complexity) in
  `tropical-resource-typing/*.thy`; that is the intended home for F1. Gated by
  ADR-0026 sign-off.
- **Soundness (P1–P5):** prover not yet chosen. The disambiguation table notes
  "None mechanized (programme filed: #513–#521, unstarted)." Per
  `feedback_proofs_must_check_and_cross_doc_echo_types`, audit `echo-types`
  first for any reusable graded/affine machinery before standing up new proofs.
- **typed-wasm boundary (P6):** the existing Rust verifier crate
  (`crates/typed-wasm-verify/` in the `typed-wasm` repo) is the mechanised
  surface; faithfulness is a cross-repo conformance obligation.

## Architectural findings (2026-06-16, durable Float-heap fix in progress)

Recorded so the next session does not re-derive them:

- **Codegen runs on the untyped AST.** `lib/ast.ml`'s `expr` carries no type
  annotations; `lib/codegen.ml` reconstructs wasm value types with a *partial*
  inferer `expr_val_type` (Float literals, float ops, `local_types`,
  `fn_ret_types`). There is no typed AST threaded from the typechecker.
- **Consequence for P7:** a *fully general* type-directed heap layout needs the
  static element/field type at every access site (`a[i]`, `t.0`, `.field`).
  Those are recoverable for **function params** (annotated) and
  **annotated/inferable `let`s**, but NOT for field-of-field, function-return
  element types, etc., without a type environment. The bounded fix (param- and
  annotated-let-typed aggregates → 8-byte `f64` cells, precise loud-fail for the
  rest) covers all of issue-draft 05's reproducers; the general fix is an
  architectural change (thread a type env / typed AST into codegen) worth its
  own ADR before it lands, because of heap-layout blast radius (string/list ops
  hardcode 4-byte/byte strides that must not desync).

## How to update this ledger

When a proof lands, move its row to **Proved** and cite the prover + the file
that checks green (ground-truth by running it, not by status docs). When a new
soundness hole is execution-verified, add a **Refuted** row with the minimal
reproducer. Never silently upgrade **Argued → Proved**.

---

## Historical note — Template ABI cleanup (2026-03-29)

Template ABI removed — was creating a false impression of formal verification.
The removed files (`Types.idr`, `Layout.idr`, `Foreign.idr`) contained only RSR
template scaffolding with unresolved `{{PROJECT}}`/`{{AUTHOR}}` placeholders and
no domain-specific proofs. When this project needs formal ABI verification,
create domain-specific Idris2 proofs following the pattern in repos like
`typed-wasm`, `proven`, `echidna`, or `boj-server`.
