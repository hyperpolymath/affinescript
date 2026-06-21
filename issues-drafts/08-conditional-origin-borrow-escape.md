# Borrow checker: use-after-move via a borrow bound through an `if`/`match`/`block` expression (caller-side origin escape)

> **Parked draft (2026-06-16).** No SPDX header added — owner-only per the
> no-automated-licence-edits directive; owner adds the standard issue-draft
> header (as on 01–07) and commits signed. Found by the soundness adversarial
> probe (Polonius phase, step 2).

> **✅ RESOLVED 2026-06-16 (same session).** Closed *without* full Polonius — a
> bounded fix in the borrow-checker's *checking pass* (`lib/borrow.ml`). All 8
> reproducers below now reject (`MoveWhileBorrowed`); a second adversarial round
> (if-of-match, match-of-block, `try`-bound, tuple-smuggled, ref-var-forwarding
> block, deep mix) also rejects; anti-over-rejection (NLL use-before-move,
> unrelated move, value-blocks) still passes; full suite green (483 tests, +6).
> **The root cause differed from the hypothesis in "Proposed fix" below — see
> "What the fix actually was".**

## What the fix actually was (supersedes "Proposed fix")

The hypothesis below pointed at `origins_of_ref_source` inside
`compute_ret_borrow_params` (the per-function return-borrow *summary* builder).
That was the wrong site: the repro is in `main`, which does not *return* the
borrow, so the summary is irrelevant to it. The real defect was in the
*checking pass*:

1. **`check_block`** restored `state.borrows` to block-entry and cleared
   `state.result_borrows` at block exit, so a block/branch whose **value** is a
   returned borrow of an *outer* place silently swallowed that borrow. Fixed by
   computing the tail's escaping borrows *before* the lexical restore (filtering
   out borrows rooted at block-local owners — those genuinely die / are caught
   by `BorrowOutlivesOwner`) and **re-publishing** them past the restore onto
   `state.borrows` + `state.result_borrows`.
2. **`ExprIf` / `ExprMatch` joins** intersected branch borrows by `b_id`; since
   each branch mints a *distinct* borrow record for the same origin, the
   intersection dropped both. Fixed by capturing each branch/arm's escaping
   borrows and re-publishing their **union** (the value is one branch tail or
   another, so union is the sound merge; it can only keep more borrows live).
3. **`record_ref_binding`** (and the `StmtAssign` reassign path) only claimed
   `result_borrows` for an `ExprApp` value. Broadened to `ExprApp | ExprIf |
   ExprMatch | ExprBlock`, so `let r = if … { pick(a) }` aliases `a` exactly as
   `let r = pick(a)` does.

A new helper `value_escaping` dispatches on the *shape* of a checked value
(call/if/match/block → the `result_borrows` channel; `&p`/ref-var → structural
`ref_source_borrow`; else none) so a stale channel left by an earlier sibling
statement is never mis-attributed.

**Severity:** Soundness — *false negative* (accepts a use-after-move). Same
loan-propagation class as #554, on the **caller** side.

## Summary

#554 (PR #595) made a callee-returned borrow register against its argument via a
per-function **return-borrow summary** + call-graph fixpoint, so
`let r = pick(a); consume(a); *r` is caught. But when the result binder is bound
through a **conditional or compound expression**, the origin is lost and the
move slips past again:

```affinescript
fn pick(ref x: Int) -> ref Int { return &x; }
fn consume(own v: Int) -> Int { return v; }

fn main() -> Int {
  let a: Int = 7;
  let c: Bool = true;
  let r = if c { pick(a) } else { pick(a) };  // r borrows a — origin LOST here
  let _g = consume(a);                         // moves a while r is live
  return *r;                                   // use-after-move — ACCEPTED (unsound)
}
```

## Confirmed by probe (2026-06-16) — all ACCEPTED (should be rejected)

| Form binding the borrow | Result |
|---|---|
| `let r = if c { pick(a) } else { pick(a) }` | ❌ accepted |
| `let r = if c { pick(a) } else { pick(b) }` (partial) | ❌ accepted |
| `let r = if c { { pick(a) } } else { pick(a) }` (nested) | ❌ accepted |
| `let r = if c { if c { pick(a) } else { pick(a) } } else { pick(a) }` | ❌ accepted |
| `let r = { pick(a) }` (plain block) | ❌ accepted |
| `let r = { { pick(a) } }` (nested block) | ❌ accepted |
| `let r = if c { let t = pick(a); t } else { pick(a) }` | ❌ accepted |
| `let r = match k { 0 => pick(a), _ => pick(a) }` (multi-arm) | ❌ accepted |

**Caught (sound) — for contrast:**

| Form | Result | Why |
|---|---|---|
| `let r = pick(a)` (direct) | ✅ caught | `origins_of_ref_source` handles `ExprApp` |
| `let r = match a { _ => pick(a) }` (single-arm) | ✅ caught | *incidental* — scrutinee `a` is the moved var, not origin tracking |
| `let r = if c { pickm(a) } …` (`&mut`/exclusive) | ✅ caught | exclusive borrow tracked on a separate path |
| callee `fn f(ref x){ if _ {return &x} else {return &x} }` | ✅ caught | the **summary** side (`walk_tail`) already recurses return-tails |

## Root cause

`origins_of_ref_source` (`lib/borrow.ml:233`) — which `record_let` uses to give a
`let`-binder its origins — only matches `ExprUnary(OpRef|OpMutRef)`, `ExprVar`,
and `ExprApp`; everything else (incl. `ExprIf`, `ExprMatch`, `ExprBlock`) falls
to `| _ -> []`, so the binder gets **no origins** and never registers as a borrow
of the argument. The asymmetry is with `walk_tail` (same file, ~line 283), which
*does* descend `if`/`match`/`block` tails when harvesting **return** origins —
that is exactly why the callee-summary side is sound but the caller let-binding
side is not.

## Proposed fix (bounded — not ADR-022)

Make `origins_of_ref_source` recurse into compound expressions, taking the
**union** of the origins of all tail positions, mirroring `walk_tail`:

- `ExprIf (_, then, else?)` → `origins_of_ref_source then @ (else? origins)`
- `ExprMatch (_, arms)` → `List.concat_map (origins_of_ref_source ∘ arm tail) arms`
- `ExprBlock b` → thread the block's own `let`s into `local_origins` (as the
  outer scan does), then `origins_of_ref_source` of the block tail.

This converges the caller-side origin computation with the already-sound
callee-side `walk_tail`. Union-of-branches is conservative (over-approximates
origins → cannot introduce a *new* false negative), matching the documented
"sound direction" invariant.

**Note vs the #554 residual (b):** the #554 close-out recorded residual (b)
("branch-merged / copy-out claim") as *"closed only by Polonius #553."* This
finding suggests at least the **let-bound conditional** manifestation is
closeable by the syntactic extension above, **without** full Polonius
origin/region variables — worth re-checking that characterisation before
attributing it solely to ADR-022.

## Hardening fixtures (✅ ADDED — `test/e2e/fixtures/borrow_cond_origin_*`, wired into `test_e2e.ml` `borrow_tests`)

1. `if`-bound, both branches borrow `a`, move `a`, `*r` after → **reject**.
2. `block`-bound (plain + nested) → **reject**.
3. multi-arm `match`-bound (scrutinee ≠ moved var) → **reject**.
4. partial (one branch borrows `a`, other borrows `b`); move `a` → **reject**;
   move an unrelated `c` → **accept**.
5. anti-over-rejection: each of the above with `*r` **read before** the move →
   **accept** (NLL last-use).
6. nested `if { if … }` and `if { let t = …; t }`.
