<!-- SPDX-License-Identifier: MPL-2.0 -->
<!-- Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> -->

# DB-Theory #3 — Aggregation-as-Monoid-Homomorphism

**Status**: Carrier wired (`stdlib/Aggregate.affine`, codegen, Deno-ESM smoke); formal proof obligation **pending upstream against [`hyperpolymath/echo-types#175`](https://github.com/hyperpolymath/echo-types/issues/175)**.

## 1. The obligation

For each scalar aggregator `M = (Elem, ε, ⊕)` and any partition `{group_k}` of the row set by key `k`:

**Safety property #DB-3.1 (aggregation-as-fold)**:
```
aggregate(SELECT M(v) FROM t GROUP BY k)
  ≡  { k ↦ foldr ⊕ ε (map agg group_k) }
```

The aggregators are commutative monoids:

| Aggregator | `Elem`              | `ε` | `⊕`   | Commutative? | Idempotent? |
|------------|---------------------|-----|-------|--------------|-------------|
| COUNT      | `ℕ`                 | `0` | `+`   | ✓            | ✗           |
| SUM        | `ℕ` (or `ℤ`, `ℝ`)   | `0` | `+`   | ✓            | ✗           |
| MIN        | `ℕ ∪ {∞}`           | `∞` | `min` | ✓            | ✓           |
| MAX        | `ℕ ∪ {-∞}`          | `-∞`| `max` | ✓            | ✓           |
| AVG        | **not a monoid** (no identity) — derived as `SUM/COUNT` |

## 2. Echo-types audit (2026-06-01)

Per owner directive, every proof must first audit `hyperpolymath/echo-types`.

**Finding**: no existing monoid / semiring / aggregation infrastructure today. Closest scaffolding:

1. `EchoCost.CostAlgebra` — left-identity + monotonicity, but no composition law. Reusable as a `Monoid` *instance* once the carrier exists.
2. `Ordinal/Brouwer/OmegaPow.agda#additive-principal` — exactly the monoid closure property for ω^n exponents.
3. `EchoDecorationStructure.agda` — observer-level lattice; aggregation lives at the data level.
4. `docs/adjacency/provenance-semirings.adoc` — explicitly names the distinctness story (echo adds types; semirings add scalars).

**Steer**: minor extension — one new module. Tracked at echo-types#175.

## 3. Proposed upstream extension

A new module `EchoAggregation.agda`:

```agda
record Monoid (ℓ : Level) : Set (suc ℓ) where
  field
    Elem       : Set ℓ
    ε          : Elem
    _⊕_        : Elem → Elem → Elem
    assoc      : ∀ a b c → (a ⊕ b) ⊕ c ≡ a ⊕ (b ⊕ c)
    identity-l : ∀ a → ε ⊕ a ≡ a
    identity-r : ∀ a → a ⊕ ε ≡ a

record GroupAggregator {ℓ} (K V : Set) (M : Monoid ℓ) : Set ℓ where
  open Monoid M
  field
    agg : V → Elem

-- Headline lemma (signature — proof may follow in stacked PR):
aggregation-as-fold :
  ∀ {ℓ} {K V : Set} {M : Monoid ℓ} (ga : GroupAggregator K V M)
  → (rows : List (K × V))
  → (k : K)
  → group-of k (groupBy proj₁ rows)
    ≡ foldr (_⊕_ ∘ agg) ε (lookup k (partition rows))
```

Plus concrete instances `countMonoid : Monoid ℓ-zero`, `sumMonoid`, `minMonoid`, `maxMonoid`.

## 4. Cross-doc seam

- **AffineScript stdlib**: `stdlib/Aggregate.affine` carries the obligation in its module docstring with the aggregator monoid table.
- **AffineScript codegen + smoke**: `lib/codegen_deno.ml` + `tests/codegen-deno/aggregate_smoke.{affine,harness.mjs}` *witness* the property at the Node runtime level — the mock implements `groupBy` by bucketing rows by key column then folding the aggregator over each bucket. The witness is not a proof; it's an executable check that the runtime mock observes the same invariant the formal proof will eventually establish.
- **Echo-types upstream**: tracked at [`hyperpolymath/echo-types#175`](https://github.com/hyperpolymath/echo-types/issues/175). Once landed, back-link the commit SHA / module path here.

## 5. Why this matters

Aggregation is the most-used non-trivial query shape outside selection/projection. Wiring aggregators as typed commutative monoids (rather than ad-hoc per-shape SQL strings) gives:

- **Distributivity proofs free**: aggregation distributes over filtering (db-theory #4) and indexed scans (db-theory #6) via monoid homomorphism.
- **CRDT bridge for free**: `OR-Set` and `GCounter` (db-theory #9) are precisely *monoids with convergence* — the same carrier extends.
- **Provenance-semiring bridge**: the Green/Karvounarakis/Tannen framing instantiates here once `EchoAggregation` lands.

Sibling: [echo-types#174](https://github.com/hyperpolymath/echo-types/issues/174) (Transaction safety / `no-section-of-collapsing-map`).
