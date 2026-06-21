# Soundness probe round 3 — four false-negatives in the borrow + quantity checkers

> **Parked draft (2026-06-16).** No SPDX header added — owner-only per the
> no-automated-licence-edits directive; owner adds the standard issue-draft
> header (as on 01–08) and commits signed. Found by the holes-first adversarial
> soundness probe (round 3, 4 skeptics × ~73 programs) the owner requested after
> #554 / issue-08 were verified closed. Each hole below was **independently
> reproduced** (not just probe-reported): every "hole" program prints
> `Type checking passed`; every paired control is correctly rejected, proving the
> checker is otherwise active. Harness:
> `_build/default/bin/main.exe check <file>` ("Borrow error"/"Quantity error" =
> reject; "Type checking passed" = accept). Each section can become its own issue.

> **STATUS: filed, NOT fixed.** Fixes touch `lib/borrow.ml` + `lib/quantity.ml`
> on the active `feat/solo-core-metatheory-proofs` branch (which carries
> uncommitted owner work + in-progress Polonius M3); per stop-first they are
> surfaced as a plan, not applied. Proposed fixes below are from the probe's
> root-cause analysis and are *unverified hypotheses* until implemented + the full
> suite re-run.

These are the long tail of a **sound-by-testing** checker (PROOF-NEEDS P1: #554
"tested, not proved"): whole *mechanisms* — linearity through loops/branches,
deref-reborrow loans, aggregate-wrapped return borrows — have coverage gaps that
the fixture-driven suite did not exercise. All four are **false-negatives**
(accept genuinely-unsafe source), severity **high** for an affine language whose
thesis is "the checker guarantees no use-after-move / linear discipline".

---

## Hole 1 — callee return-borrow summary ignores aggregate-wrapped / projected borrows

`origins_of_ref_source` (`lib/borrow.ml` ~L251–287) matches only
`ExprUnary(OpRef|OpMutRef)`, `ExprVar`, `ExprApp` — not tuple/array/record
literals or projections. So a borrow returned wrapped in an aggregate is invisible
to the per-function return-borrow summary, and the caller's argument is not held
borrowed.

```affinescript
fn wrap(ref x: Int) -> ref Int { return (&x, 0).0; }   // returns &x, spelled as a projection
fn consume(own v: Int) -> Int { return v; }
fn main() -> Int {
  let a: Int = 7;
  let r = wrap(a);          // r aliases a — NOT recorded
  let _g = consume(a);      // moves a while r is live
  return *r;                // use-after-move — ACCEPTED (unsound)
}
```

* Hole: `Type checking passed` (also compiles to wasm). Variants tuple `(&x,0)`,
  projection `(&x,0).0`, `&mut`-tuple (dangling **exclusive** ref), array `[&x]`
  all accept.
* Control (byte-near-identical, correctly REJECTS):
  `fn wrap(ref x: Int) -> ref Int { return &x; }` →
  `Borrow error: cannot move 'a' while it is shared-borrowed`.
* Class: use-after-move. Same family as #554/issue-08 (the
  `origins_of_ref_source` ↔ `walk_tail` asymmetry) but a NEW spelling; issue-08
  (`let r = if {pick(a)} …`) is verified closed.
* Proposed fix (conservative, sound-direction): make `origins_of_ref_source` +
  `walk_tail`/`walk_expr` descend `ExprTuple`/`ExprArray`/`ExprRecord` (union of
  element origins) and `ExprTupleIndex`/`ExprField`/`ExprIndex` (descend base).
  Over-approximation can only keep MORE arg borrows live — no new false negative.

## Hole 2 — `@linear` binding consumed once-per-iteration in a loop is accepted

`lib/quantity.ml` `StmtWhile`/`StmtFor` (~L628–648) model loop repetition with
`env_join` (per-variable MAX over two passes), not `add_usage`/ω-scaling. Each
pass yields the var at `UOne`; `join_usage UOne UOne = UOne`, so a once-per-iter
use is never promoted to `UMany`. The in-code comment (~L635, "any variable used
in the loop body is used >= 2 times") contradicts the implementation.

```affinescript
fn consume(@linear r: Int) -> Int = r + 1;
fn loop_param(@linear x: Int, n: Int) -> Int {
  let mut i = 0;
  while i < n { consume(x); i = i + 1; }   // consumes use-once x up to n times
  0
}
```

* Hole: `Type checking passed` (+ compiles). For a real linear resource (handle,
  owned buffer, effect token) this is N-fold consume = double-free / use-after-
  consume. `for … in [..]` reproduces.
* Controls (REJECT): straight-line `consume(x); consume(x)` →
  `Quantity error: … used multiple times`; intra-iteration `x + x` inside the
  loop → same.
* Class: linear-violation (quantity checker — independent of borrow.ml).
* Proposed fix: ω-scale (or `add_usage` with itself) the per-iteration usage
  delta in `StmtWhile`/`StmtFor` instead of `env_join`, matching the L635 intent.

## Hole 3 — `@linear` binding dropped on one branch is accepted

`lib/quantity.ml` `join_usage` (~L91–95) returns MAX of branch usages. MAX is the
correct join for **affine** (at-most-once), but AffineScript treats `@linear` as
**exactly-once** (it raises "must be used exactly once, but was never used" for
zero uses). For exactly-once, branch-merge must require consumption on **all**
paths (a meet that flags any branch leaving the var at `UZero`).

```affinescript
fn consume(@linear r: Int) -> Int = r + 1;
fn drop_on_else(@linear x: Int, c: Bool) -> Int {
  if c { consume(x) } else { 0 }    // x never consumed when c = false
}
```

* Hole: `Type checking passed` (+ compiles). When `c` is false the must-use
  linear `x` is silently leaked. Match-arm form (consume in one arm, drop in
  wildcard) reproduces.
* Control (REJECTS): unconditional drop `fn f(@linear x: Int) -> Int { 0 }` →
  `Quantity error: … never used`.
* Class: linear-violation. Closely related to Hole 2 (same file); could be one
  issue "QTT exactly-once is unsound across loops and branches".
* Proposed fix: for `QOne` bindings, branch-merge must flag any branch leaving
  the var at `UZero` (must-use-on-all-paths), not take MAX. Care: must not
  over-reject legitimate affine (`QOmega`) bindings — keep MAX for those.

## Hole 4 — deref-reborrow `&mut *r` / `&*r` records no loan (aliased `&mut` + UAM laundering)

`expr_to_place` (`lib/borrow.ml` ~L832–851) has no `ExprUnary(OpDeref, _)` arm
(falls to `_ -> None`) even though the `place` type HAS `PlaceDeref` (~L21). So at
`check_expr` `ExprUnary OpRef/OpMutRef` (~L1583), `expr_to_place(*r) = None` hits
the None-branch (~L1588) that only checks the inner operand — `record_borrow` is
never called, so the reborrow loan is invisible to `find_conflicting_borrow` /
`find_aliasing_exclusive`.

```affinescript
module P;
fn bad() -> Int {
  let mut x = 5;
  let r = &mut x;
  let r2 = &mut *r;   // SECOND live exclusive alias of x — no loan recorded
  *r2 = 99;
  *r = 100;           // two live mutable paths to one cell
  *r2
}
```

* Hole: `Type checking passed` (+ compiles). A use-after-move variant
  (`let r = &mut x; let r2 = &mut *r; let gone = consume(x); *r2 + gone`) also
  accepts: `&mut *r` records no loan → `r` NLL-expires → `consume(x)` moves `x`
  unblocked → `*r2` reads moved storage. `&*r` (shared) launders identically.
* Control (REJECTS): direct `let a = &mut x; let b = &mut x` →
  `Borrow error: conflicting borrows on 'x'`.
* Class: alias-exclusivity (no callee, no branch — distinct from #554/issue-08).
  `&mut *r` (reborrow) is an everyday pattern, so this is a basic gap.
* Proposed fix (single point): add to `expr_to_place`
  `| ExprUnary (OpDeref, inner) -> Option.map (fun p -> PlaceDeref p) (expr_to_place symbols inner)`
  and route deref-LHS `*p = e` through the place path in `StmtAssign`.

---

## Suggested triage / priority (probe recommendation)

1. **Hole 4** — single-point `expr_to_place` addition; closes aliased-`&mut` +
   UAM laundering. Low-risk, high-value.
2. **Hole 1** — `origins_of_ref_source`/`walk_tail` aggregate descent;
   conservative over-approximation.
3. **Holes 2 + 3** — `lib/quantity.ml` loop ω-scaling + branch all-paths-meet for
   `QOne`. Subtler: must not over-reject `QOmega`/affine bindings — verify the
   full suite (+ add NLL/affine anti-over-rejection fixtures) after.

Each fix should ship with hardening fixtures wired into `test/test_main.ml`
`borrow_tests` / the quantity suite (reject the hole; accept the safe control),
mirroring the #554 / issue-08 closeouts. Re-run the round-3 probe class after.

## VERIFICATION 2026-06-17 — the "proposed fix" recipes above are HYPOTHESES; none is a one-liner

A de-risk pass attempted/analysed the recipes. *Correction: the triage above was
over-optimistic. None of the four holes is a quick patch — they are architectural
gaps. Treat every "Proposed fix" line above as an unverified hypothesis.*

* *Hole 4 — EMPIRICALLY DISPROVEN as a single-point fix (built + ran it, then
  reverted).* Adding the `ExprUnary (OpDeref, inner) -> PlaceDeref` arm to
  `expr_to_place`: (a) OVER-REJECTS legitimate reborrows — `is_mutable` is
  BINDING-based (`let mut`), not reference-based, so `&mut *r` / `*r = e` through a
  non-`let mut` ref binder fails with "cannot borrow `*r` as mutable", rejecting
  the sound `let r = &mut x; let r2 = &mut *r; *r2 = 99; *r2`; (b) does NOT achieve
  soundness even so — `places_overlap` is root-var-based and
  `root_var (PlaceDeref (PlaceVar r)) = r`, while the original loan from
  `let r = &mut x` is rooted at `x`, so the reborrow's loan (root `r`) never
  overlaps the original (root `x`) and the aliasing is undetected; (c) the shared
  `&*r` variant stays accepted. *Sound fix requires resolving `*r` to its referent
  `x` — the reference→referent origin/loan model = Polonius (#553).* H4 belongs
  with #553, not a quick patch.
* *Hole 1 — same class as H4 (borrow-checker origins); Polonius-adjacent.* The
  `origins_of_ref_source` aggregate descent is a pure over-approximation that may
  help, but the aggregate-wrapped return-borrow soundness is part of the same
  origin-tracking story as #554/H4. Treat as #553-adjacent; verify it does not
  over-reject (more origins ⇒ more borrows held live ⇒ can reject legit moves).
* *Holes 2 + 3 — code-analysis shows the one-line recipes are insufficient (not
  yet empirically run).* `join_usage` (quantity.ml:91-95) is MAX and
  QUANTITY-AGNOSTIC; "change the branch join to MEET" would break `QOmega`/affine
  (which legitimately allow 0..n and use of zero on a path). H3 needs a
  quantity-aware branch merge or a usage-lattice extension (e.g. a
  "used-on-some-but-not-all-paths" element that errors for `QOne` only). H2's naive
  ω-scaling of the loop body over-rejects loop-LOCAL linears (a fresh linear born
  and consumed each iteration is sound); the scale must apply only to vars that
  escape the loop scope. Both are contained to `quantity.ml` but are real
  lattice/scope work, not one-liners.

NET (de-risk outcome): the probe correctly FOUND four real false-negatives, but
its FIXES are hypotheses. H1/H4 ⇒ the Polonius origin model (#553); H2/H3 ⇒
quantity-checker enhancements (scope-aware loop scaling; all-paths-meet for
linear). Closing these is design work needing owner review, not a sweep. The
reverted H4 experiment confirms: verify every proposed fix by running it.
