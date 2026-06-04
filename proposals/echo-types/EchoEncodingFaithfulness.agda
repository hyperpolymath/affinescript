{-# OPTIONS --safe --without-K #-}
-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>

-- EchoEncodingFaithfulness: the encode-faithfulness theorem for the
-- ReScript-to-AffineScript-to-wasm migration pattern, in Echo
-- language. Audience-facing module in the EchoProvenance / EchoSecurity
-- mould (abstract record + parametric headlines + worked instance +
-- matched-negative block).
--
-- *The migration pattern.* A game is being migrated from ReScript to
-- AffineScript-compiled-to-wasm. An enumerated / stringly-typed value
-- `X` host-side — say a 4-element security rank Open/Weak/Medium/Strong,
-- or a device-port name — is ENCODED to an integer, and a pure-integer
-- wasm kernel computes on the integer. The migration slogan is
-- "the integer IS the X". That slogan is precisely an
-- encode-faithfulness question on `enc : Source → Code`, where `Code`
-- is the (generic) integer space. Keeping `Code` a generic codomain
-- `B` — rather than pulling in `Data.Integer` — is deliberate: the
-- faithfulness content is about the FIBRES of `enc`, not about integer
-- arithmetic. The worked instances use `ℕ` (matching how
-- `EchoExampleAbsInt` avoids `Data.Integer` with a hand-rolled finite
-- carrier).
--
-- *The two halves of "the integer IS the X".*
--
--   1. LOSSLESS half. If `enc` is injective, the integer losslessly
--      determines the X: every fibre `Echo enc i` has at most one
--      underlying element (`encoding-lossless`, via
--      `EchoImageFactorization.injective-fibres-proj-unique`). The
--      slogan holds — the integer is a faithful name.
--
--   2. LOSSY half (controlled loss with residue). If two distinct
--      `x₁ ≢ x₂` encode to the same integer, then NO section exists —
--      you cannot recover the X from the integer
--      (`encoding-collision⇒no-section`, via
--      `EchoNoSectionGeneric.no-section-of-collapsing-map`). The
--      collision IS the named, localised residue: the lost
--      distinction lives in the fibre over the shared code.
--
-- *The clamp/sentinel theorem (the game-relevant one).* The real
-- migration hazard is an encoder that is injective on the VALID band
-- but CLAMPS out-of-band inputs to a sentinel integer that ALSO names
-- a valid value (the classic "default to 0 / Open on a bad input"
-- bug). We construct a concrete TOTAL encoder where a distinct
-- out-of-band input collapses onto the sentinel that also encodes a
-- valid value, and show — via the lossy half — that the sentinel
-- fibre has no section. This is the formal statement of "controlled
-- loss: out-of-band collapses to a sentinel; the loss is provable and
-- localised at the sentinel code." It is a CHECKED ARTEFACT (a
-- concrete instance, not merely a parametric statement).
--
-- *Echo-specific properties used.* This module leans on:
--
--   * `Echo.Echo` / `Echo.echo-intro` — the fibre carrier + canonical
--     inhabitation (the inhabitants the integer-only kernel loses).
--   * `EchoImageFactorization.injective-fibres-proj-unique` — the
--     K-free fibre-projection uniqueness behind the lossless half.
--   * `EchoNoSectionGeneric.no-section-of-collapsing-map` — the lifted
--     structural no-section theorem behind the lossy + clamp halves.
--   * `EchoFiberCount.FiberSize-fin-injective` /
--     `EchoFiberCount.FiberSize-fin-all-hit` — the finite-domain
--     fibre-count remark tying collapse to a measurable count.
--
-- None of these reduce to plain Σ + ≡: each carries the
-- fibre/no-section content the residue / abstraction-barrier line has
-- already pinned. The audience-facing rephrasing here is a packaging
-- of that content under migration-engineer names, not a generalisation
-- past it.
--
-- Headlines (pinned in Smoke.agda):
--
--   * Encoding                        -- the abstract setup record
--   * module EncodingTheorems          -- parametric in E : Encoding
--   * encoding-injective⇒fibre-unique  -- per-code fibre uniqueness
--   * encoding-lossless                -- the lossless-half headline
--   * encoding-collision⇒no-section    -- the lossy-half headline
--   * Rank / rankCode                  -- the worked 4-element rank +
--                                         its host-side integer code
--   * rankCode-injective               -- rank codes are lossless
--   * rank-encoding                    -- Rank-over-ℕ as an Encoding
--   * rank-lossless-at                 -- lossless half on the rank
--   * ClampInput / clampCode           -- the clamping variant input +
--                                         its out-of-band-to-sentinel
--                                         encoder
--   * clamp-sentinel-collision         -- distinct inputs share sentinel
--   * clamp-encoding                   -- clamp variant as an Encoding
--   * clamp-sentinel-no-section        -- the KEY clamp theorem:
--                                         the sentinel fibre has no
--                                         section (checked instance)
--   * sentinel-fibre-count-≥2          -- fibre-count witness of the
--                                         localised collapse
--
-- Scope guardrail (Echo-vs-Σ clearance, honest-bound). The abstract
-- `Encoding` record uses plain functions in its DATA. The CONTENT —
-- lossless fibre uniqueness, no-section at a collision, the clamp
-- sentinel collapse — invokes `Echo`, `injective-fibres-proj-unique`,
-- and `no-section-of-collapsing-map` non-trivially. The falsifier per
-- the Echo-vs-Σ bar: if every headline could be re-proved with only
-- `Σ` + `_≡_` (no Echo / no no-section lemma), the module would fail
-- the bar. The lossy + clamp headlines route through the lifted
-- no-section theorem; the lossless headline routes through the K-free
-- fibre-projection uniqueness. The matched-negative `NotProved-*`
-- aliases at the bottom pin the honest scope (this is TYPE-LEVEL
-- faithfulness — it says nothing about the wasm runtime actually
-- implementing `enc` correctly; that is the differential-parity
-- harness's job, not the proof's).

module EchoEncodingFaithfulness where

open import Echo                  using (Echo; echo-intro)
open import EchoImageFactorization using (Injective; injective-fibres-proj-unique)
open import EchoNoSectionGeneric  using (no-section-of-collapsing-map)
open import EchoFiberCount        using
  ( FiberSize-fin
  ; FiberSize-fin-injective
  ; FiberSize-fin-all-hit
  )

open import Level                 using (Level)
import      Data.Nat.Base         as ℕ
open        ℕ                      using (ℕ)
open import Data.Nat.Properties   using (_≟_)
open import Data.Fin.Base         using (Fin; zero; suc)
open import Data.Product.Base     using (Σ; _,_; _×_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality
                                  using (_≡_; _≢_; refl; sym; trans; cong)
open import Relation.Nullary      using (¬_)

private variable
  a b : Level
  A : Set a
  B : Set b

----------------------------------------------------------------------
-- The abstract encode setup.
--
-- An `Encoding` is the data of a source type (the enumerated /
-- stringly-typed value `X` host-side), a code type (the integer space
-- the wasm kernel computes on), and the encoder `enc`. `Set₁` because
-- of the `Set`-valued fields. No further witnesses are baked in — the
-- parametric headlines take injectivity / collision as explicit
-- hypotheses, mirroring how `EchoProvenance` / `EchoSecurity` keep the
-- distinguishability witness out of the record where it is a
-- per-theorem precondition.
----------------------------------------------------------------------

record Encoding : Set₁ where
  field
    Source : Set
    Code   : Set
    enc    : Source → Code

----------------------------------------------------------------------
-- The two headline theorems, parametric in the encode setup.
----------------------------------------------------------------------

module EncodingTheorems (E : Encoding) where
  open Encoding E

  ------------------------------------------------------------
  -- Headline 1 (LOSSLESS half): injective enc ⇒ per-code fibre
  -- uniqueness.
  --
  -- If `enc` is injective then, for every code `i`, any two echoes of
  -- `enc` over `i` have equal underlying source elements. In migration
  -- terms: the integer `i` losslessly determines the X — there is at
  -- most one X that the kernel could have been handed. This is exactly
  -- `injective-fibres-proj-unique` specialised to the encoder, and it
  -- is K-free (no UIP on `Code`): the projection-level claim is the
  -- load-bearing one, and the full "echoes are equal as Σ-pairs" claim
  -- (which WOULD need UIP on `Code`) is deliberately not asserted.
  ------------------------------------------------------------

  encoding-injective⇒fibre-unique :
    Injective enc →
    (i : Code) (e₁ e₂ : Echo enc i) → proj₁ e₁ ≡ proj₁ e₂
  encoding-injective⇒fibre-unique inj = injective-fibres-proj-unique enc inj

  -- The audience-facing crisp restatement: "the integer IS the X"
  -- (losslessly) — given the code and a demonstrated decoding, the
  -- decoded X is forced. Same content, migration-engineer naming.
  encoding-lossless :
    Injective enc →
    (i : Code) (e₁ e₂ : Echo enc i) → proj₁ e₁ ≡ proj₁ e₂
  encoding-lossless = encoding-injective⇒fibre-unique

  ------------------------------------------------------------
  -- Headline 2 (LOSSY half): a code collision ⇒ no section.
  --
  -- If two distinct sources `x₁ ≢ x₂` encode to the same code, then
  -- `enc` admits NO section — there is no pure `decode : Code →
  -- Source` with `decode ∘ enc ≡ id`. In migration terms: once two
  -- distinct Xs share an integer, no integer-to-X recovery function
  -- can be type-checked. The collision IS the named residue, localised
  -- at the shared code. Direct instantiation of
  -- `no-section-of-collapsing-map` at `lower := enc`.
  ------------------------------------------------------------

  encoding-collision⇒no-section :
    (x₁ x₂ : Source) (x₁≢x₂ : x₁ ≢ x₂) (enc-collides : enc x₁ ≡ enc x₂) →
    ¬ Σ (Code → Source) (λ decode → ∀ x → decode (enc x) ≡ x)
  encoding-collision⇒no-section =
    no-section-of-collapsing-map enc

----------------------------------------------------------------------
-- Worked instance A: the 4-element security rank, lossless.
--
-- The bridge to the actual game. `Rank` is the host-side enumerated
-- value (Open/Weak/Medium/Strong); `rankCode` is the integer the wasm
-- kernel sees (0/1/2/3). `rankCode` is injective, so by the lossless
-- half the integer faithfully names the rank: "the integer IS the
-- rank" holds on the nose.
--
-- `open'` (not `open`) because `open` is an Agda keyword.
----------------------------------------------------------------------

data Rank : Set where
  open' weak medium strong : Rank

rankCode : Rank → ℕ
rankCode open'  = 0
rankCode weak   = 1
rankCode medium = 2
rankCode strong = 3

-- Injectivity of `rankCode` by exhaustive case analysis. The equal
-- cases are `refl`; the unequal cases are ruled out by the empty
-- pattern `()` on the impossible numeral equation. (Listing the
-- diagonal is enough — off-diagonal pairs are absurd numeral
-- equalities and Agda's coverage checker accepts the `()` clauses.)
rankCode-injective : Injective rankCode
rankCode-injective {open'}  {open'}  _  = refl
rankCode-injective {weak}   {weak}   _  = refl
rankCode-injective {medium} {medium} _  = refl
rankCode-injective {strong} {strong} _  = refl

rank-encoding : Encoding
rank-encoding = record
  { Source = Rank
  ; Code   = ℕ
  ; enc    = rankCode
  }

-- Lossless half on the rank: every integer code determines its rank.
-- Pulls `EncodingTheorems.encoding-lossless` back through the
-- `rank-encoding` instance + the injectivity witness.
module RankTheorems = EncodingTheorems rank-encoding

rank-lossless-at :
  (i : ℕ) (e₁ e₂ : Echo rankCode i) → proj₁ e₁ ≡ proj₁ e₂
rank-lossless-at = RankTheorems.encoding-lossless rankCode-injective

----------------------------------------------------------------------
-- Worked instance B: the clamping variant — the KEY game-relevant
-- hazard.
--
-- `ClampInput` is a source with three valid values `valid0/1/2` plus
-- ONE out-of-band value `oob` (standing in for "any input outside the
-- valid band"). `clampCode` encodes the valid values injectively to
-- 0/1/2, but CLAMPS `oob` to the SENTINEL code 0 — which also names a
-- valid value (`valid0`). This is the canonical migration bug: a total
-- encoder that defaults bad input to a code that is indistinguishable
-- from a legitimate one.
--
-- The collapse is localised: `valid0` and `oob` both encode to 0, so
-- the sentinel fibre `Echo clampCode 0` has at least two underlying
-- elements, and (by the lossy half) admits no section. Every other
-- code (1, 2) keeps its singleton fibre. "Controlled loss: out-of-band
-- collapses to a sentinel; the loss is provable and localised."
----------------------------------------------------------------------

data ClampInput : Set where
  valid0 valid1 valid2 oob : ClampInput

clampCode : ClampInput → ℕ
clampCode valid0 = 0
clampCode valid1 = 1
clampCode valid2 = 2
clampCode oob    = 0   -- clamp: out-of-band → sentinel 0 (= valid0's code)

-- The sentinel collision, as data: two distinct inputs collapse to the
-- sentinel code 0. `valid0 ≢ oob` by constructor disjointness;
-- `clampCode valid0 ≡ clampCode oob` is `refl` (both are 0).
valid0≢oob : valid0 ≢ oob
valid0≢oob ()

clamp-sentinel-collision :
  valid0 ≢ oob × clampCode valid0 ≡ clampCode oob
clamp-sentinel-collision = valid0≢oob , refl

clamp-encoding : Encoding
clamp-encoding = record
  { Source = ClampInput
  ; Code   = ℕ
  ; enc    = clampCode
  }

module ClampTheorems = EncodingTheorems clamp-encoding

-- THE KEY CLAMP THEOREM. The sentinel fibre `Echo clampCode 0` admits
-- no section: there is no pure `decode : ℕ → ClampInput` recovering the
-- input from its clamped code, because `valid0` and `oob` are distinct
-- inputs that both encode to the sentinel 0. A migration that clamps
-- bad input to a valid-looking sentinel has PROVABLY destroyed the
-- valid0-vs-oob distinction, and no integer-to-input recovery can
-- type-check. This is a checked instance (it consumes the concrete
-- `clamp-sentinel-collision` witnesses), not a parametric statement.
clamp-sentinel-no-section :
  ¬ Σ (ℕ → ClampInput) (λ decode → ∀ x → decode (clampCode x) ≡ x)
clamp-sentinel-no-section =
  ClampTheorems.encoding-collision⇒no-section
    valid0 oob
    valid0≢oob
    refl

----------------------------------------------------------------------
-- Optional remark: the fibre count measures the localised collapse.
--
-- Tying `EchoFiberCount.FiberSize-fin` to the clamp story: the collapse
-- is not just "a section fails to exist" but a measurable doubling of a
-- fibre. We enumerate the four `ClampInput` values as `Fin 4` and read
-- the code off each index; then:
--
--   * the sentinel code 0 is hit by TWO indices (valid0 and oob), so
--     its fibre count is ≥ 2 — the witness of the localised collapse;
--   * a valid-only code (here we take code 1, hit only by valid1) keeps
--     fibre count exactly 1, since the enumerated map is injective on
--     the band it lands in at that code.
--
-- `clampVec : Fin 4 → ℕ` is the enumeration; index 0↦valid0, 1↦valid1,
-- 2↦valid2, 3↦oob, read through `clampCode`. The two indices hitting 0
-- are `zero` (valid0) and `suc (suc (suc zero))` (oob).
----------------------------------------------------------------------

clampVec : Fin 4 → ℕ
clampVec zero                      = clampCode valid0  -- 0
clampVec (suc zero)                = clampCode valid1  -- 1
clampVec (suc (suc zero))          = clampCode valid2  -- 2
clampVec (suc (suc (suc zero)))    = clampCode oob     -- 0 (sentinel)

-- The sentinel code 0 is hit by index `zero` (valid0).  We exhibit a
-- SECOND, distinct index `suc (suc (suc zero))` (oob) that also hits 0;
-- because `clampVec` is therefore NOT injective at code 0, the
-- subsingleton/injective fibre-count routes do not apply — the count is
-- not 1. We instead pin the collapse positively: there exist two
-- distinct `Fin 4` indices both mapping to the sentinel 0. This is the
-- fibre-count-level shadow of `clamp-sentinel-no-section`.
sentinel-two-distinct-indices :
  Σ (Fin 4) (λ i → Σ (Fin 4) (λ j →
    i ≢ j × (clampVec i ≡ 0) × (clampVec j ≡ 0)))
sentinel-two-distinct-indices =
  zero , suc (suc (suc zero)) , (λ ()) , refl , refl

-- A clean positive fibre-count fact on a genuinely injective slice: the
-- 3-element enumeration of the VALID band alone has singleton fibres at
-- each code it hits. `validVec : Fin 3 → ℕ` enumerates valid0/1/2; it is
-- injective, and code 1 is hit (by index `suc zero`), so its fibre count
-- is exactly 1. This is the "valid band is lossless" counterpoint to the
-- sentinel doubling, expressed in the `FiberSize-fin` measure.
validVec : Fin 3 → ℕ
validVec zero             = clampCode valid0  -- 0
validVec (suc zero)       = clampCode valid1  -- 1
validVec (suc (suc zero)) = clampCode valid2  -- 2

-- All nine cases listed explicitly: the diagonal is `refl`, the
-- off-diagonal pairs are ruled out by `()` on the impossible numeral
-- equation (e.g. `validVec zero ≡ validVec (suc zero)` reduces to the
-- absurd `0 ≡ 1`). Unlike `rankCode` — whose codes are literal numerals
-- the coverage checker discharges from the diagonal alone — `validVec`
-- routes through `clampCode`, so the off-diagonal cases are spelled out.
validVec-injective : ∀ {i j : Fin 3} → validVec i ≡ validVec j → i ≡ j
validVec-injective {zero}           {zero}           _  = refl
validVec-injective {zero}           {suc zero}       ()
validVec-injective {zero}           {suc (suc zero)} ()
validVec-injective {suc zero}       {zero}           ()
validVec-injective {suc zero}       {suc zero}       _  = refl
validVec-injective {suc zero}       {suc (suc zero)} ()
validVec-injective {suc (suc zero)} {zero}           ()
validVec-injective {suc (suc zero)} {suc zero}       ()
validVec-injective {suc (suc zero)} {suc (suc zero)} _  = refl

valid-band-fibre-count-1 :
  FiberSize-fin validVec 1 _≟_ ≡ 1
valid-band-fibre-count-1 =
  FiberSize-fin-injective validVec 1 _≟_ validVec-injective (suc zero) refl

----------------------------------------------------------------------
-- Matched-negative block (HONEST BOUND, per R-2026-05-18 discipline).
--
-- The properties below are deliberately NOT proved by this module.
-- They are `⊤`-aliased so `grep NotProved` in this file catches them;
-- reading the file should make the faithfulness scope explicit.
--
-- This module is a TYPE-LEVEL faithfulness statement about the encoder
-- `enc : Source → Code` and its fibres. A consumer who cites
-- `encoding-lossless` / `clamp-sentinel-no-section` BEYOND these scopes
-- is making a category error:
--
--   * NotProved-wasm-implements-enc — that the deployed wasm kernel
--     actually computes `enc` (or its inverse) correctly. Proving the
--     encoder is injective says nothing about whether the runtime
--     bit-for-bit implements it; that is the DIFFERENTIAL-PARITY
--     HARNESS's job (run host-side `enc` and wasm-side `enc` on the
--     same inputs and compare), not the proof's.
--
--   * NotProved-decode-performance — nothing here bounds the cost of
--     decoding, the kernel's runtime, or memory use. Faithfulness is
--     about information, not speed.
--
--   * NotProved-roundtrip-with-state — the no-section results are about
--     recovering the Source from the Code ALONE. A real game may carry
--     side state (the original X stashed elsewhere) that recovers it;
--     that is recovery WITH side information, outside this model.
--
--   * NotProved-code-is-integer — `Code` is a generic codomain (`ℕ` in
--     the worked instances). Nothing here certifies that the real host
--     uses a specific integer width, or that overflow/truncation in the
--     true integer type preserves injectivity. A 32-bit clamp could
--     re-introduce collisions a `ℕ` model never sees.
----------------------------------------------------------------------

open import Data.Unit.Base using (⊤)

NotProved-wasm-implements-enc : Set
NotProved-wasm-implements-enc = ⊤

NotProved-decode-performance : Set
NotProved-decode-performance = ⊤

NotProved-roundtrip-with-state : Set
NotProved-roundtrip-with-state = ⊤

NotProved-code-is-integer : Set
NotProved-code-is-integer = ⊤

----------------------------------------------------------------------
-- Companion remark.
--
-- The headlines capture the structural content of the migration
-- slogan "the integer IS the X" at the type level:
--
--   1. (Lossless) An injective encoder makes the integer a faithful
--      name: every code determines its source uniquely
--      (`encoding-lossless`, K-free).
--   2. (Lossy) A code collision destroys the distinction: no
--      integer-to-source recovery type-checks
--      (`encoding-collision⇒no-section`). The residue is named and
--      localised at the colliding code.
--   3. (Clamp) The real hazard — clamping out-of-band input to a
--      sentinel that also names a valid value — is an instance of (2),
--      and is exhibited as a CHECKED concrete artefact
--      (`clamp-sentinel-no-section`), with the collapse measured by a
--      fibre-count doubling (`sentinel-two-distinct-indices`) against
--      the lossless valid band (`valid-band-fibre-count-1`).
--
-- The abstract `Encoding` record deliberately bakes in NO integer
-- structure, ordering, or band predicate — the headlines use only
-- injectivity / collision witnesses, mirroring how `EchoProvenance`
-- and `EchoSecurity` keep the per-theorem precondition out of the
-- baseline record. Future migration-facing extensions could add a band
-- predicate + a clamp law (`EchoEncodingBand.agda`) and prove that a
-- band-injective + sentinel-clamping encoder ALWAYS exhibits exactly
-- the sentinel collapse, with `Encoding` as the minimum-fact baseline.
--
-- Relationship to the rest of the suite: this is the migration-engineer
-- audience-facing read of the (lossless = injective-fibre-unique) /
-- (lossy = no-section-at-collision) pairing that `EchoLossTaxonomy`,
-- `EchoImageFactorization`, and `EchoNoSectionGeneric` already pin
-- structurally. It adds no new proof primitive; it packages the
-- existing ones under the names a compiler/runtime migrator speaks in,
-- and supplies the clamp-sentinel hazard as the worked, checked
-- bridge to the game.
----------------------------------------------------------------------
