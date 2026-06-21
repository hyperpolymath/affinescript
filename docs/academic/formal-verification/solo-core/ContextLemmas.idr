-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- Context algebra for the Solo-core preservation proof.
--
-- The friction in `Context.idr` is that `ctxAdd` is Maybe-valued (a
-- pointwise add only makes sense on equal-shape contexts) and does
-- not track type agreement. For the QTT substitution lemma we need a
-- *type-tracking, total, analysable* notion of "g is the pointwise
-- sum of g1 and g2". We therefore work with the inductive relation
-- `AddCtx g1 g2 g`, which:
--
--   * forces g1, g2, g to share their spine AND their per-position
--     types (the `a` in `ACSnoc` is shared);
--   * records the per-position quantity sum (`qAdd q1 q2`);
--   * is Maybe-free, so all the semiring algebra is equational,
--     reusing the proven `Quantity.idr` laws via `cong`.
--
-- A bridge to `Context.ctxAdd` is provided (`addCtxToAdd`) so the
-- relation can be related back to the original `Just`-valued
-- definition where wanted; the typing rules in `Typing.idr` use
-- `AddCtx` directly.

module ContextLemmas

import Quantity
import Syntax
import Context

%default total

------------------------------------------------------------
-- The type-tracking additive splitting relation
------------------------------------------------------------

||| `AddCtx g1 g2 g` : `g` is the pointwise quantity-sum of `g1` and
||| `g2`, which must share spine and types.
|||
||| `ACSnoc` stores the per-position type and the two summand
||| quantities as RUNTIME arguments. This lets the metatheory rebuild
||| the actual summand contexts from an `AddCtx` value alone — the
||| typed indices `g1`, `g2`, `g` are erased, so without these the
||| decomposition lemmas could not run.
public export
data AddCtx : Ctx -> Ctx -> Ctx -> Type where
  ACEmpty : AddCtx Empty Empty Empty
  ACSnoc  : (a : Ty) -> (q1 : Q) -> (q2 : Q)
         -> AddCtx g1 g2 g
         -> AddCtx (Snoc g1 a q1) (Snoc g2 a q2) (Snoc g a (qAdd q1 q2))

||| Bridge: `AddCtx` implies the partial `ctxAdd` succeeds with the
||| same result. (Kept for cross-checking against `Context.idr`.)
public export
addCtxToAdd : AddCtx g1 g2 g -> ctxAdd g1 g2 = Just g
addCtxToAdd ACEmpty = Refl
addCtxToAdd (ACSnoc _ _ _ ac) = rewrite addCtxToAdd ac in Refl

------------------------------------------------------------
-- Typed-shape relation (same length AND same per-position types)
------------------------------------------------------------

||| `ShapeEq g1 g2` : the two contexts have the same spine and the
||| same type at every position (quantities free). The per-position
||| type/quantities are ERASED indices — `mkAdd` reads the actual
||| quantities from the (runtime) contexts instead, so `shapeScale`
||| does not need the scaling quantity at runtime.
public export
data ShapeEq : Ctx -> Ctx -> Type where
  SEEmpty : ShapeEq Empty Empty
  SESnoc  : ShapeEq g1 g2 -> ShapeEq (Snoc g1 a q1) (Snoc g2 a q2)

||| The two summands of an additive split share a typed shape.
public export
shapeSummands : AddCtx g1 g2 g -> ShapeEq g1 g2
shapeSummands ACEmpty             = SEEmpty
shapeSummands (ACSnoc a q1 q2 ac) = SESnoc (shapeSummands ac)

||| The left summand shares a typed shape with the result.
public export
shapeLeftResult : AddCtx g1 g2 g -> ShapeEq g1 g
shapeLeftResult ACEmpty             = SEEmpty
shapeLeftResult (ACSnoc a q1 q2 ac) = SESnoc (shapeLeftResult ac)

||| Scaling preserves the typed shape (the scaling quantity is erased).
public export
shapeScale : {0 q : Q} -> (g : Ctx) -> ShapeEq g (ctxScale q g)
shapeScale Empty        = SEEmpty
shapeScale (Snoc g a e) = SESnoc (shapeScale g)

||| `ShapeEq` is symmetric.
public export
shapeSym : ShapeEq g1 g2 -> ShapeEq g2 g1
shapeSym SEEmpty       = SEEmpty
shapeSym (SESnoc se)   = SESnoc (shapeSym se)

||| `ShapeEq` is transitive.
public export
shapeTrans : ShapeEq g1 g2 -> ShapeEq g2 g3 -> ShapeEq g1 g3
shapeTrans SEEmpty SEEmpty = SEEmpty
shapeTrans (SESnoc s1) (SESnoc s2) = SESnoc (shapeTrans s1 s2)

||| Build the additive sum of two same-shaped contexts, reading the
||| quantities from the (runtime) contexts. The first context's type
||| is used at each position (matching `AddCtx`'s convention).
public export
mkAdd : (g1, g2 : Ctx) -> ShapeEq g1 g2 -> (g : Ctx ** AddCtx g1 g2 g)
mkAdd Empty Empty SEEmpty = (Empty ** ACEmpty)
mkAdd (Snoc g1 a q1) (Snoc g2 _ q2) (SESnoc se) =
  let (g ** ac) = mkAdd g1 g2 se in
  (Snoc g a (qAdd q1 q2) ** ACSnoc a q1 q2 ac)
mkAdd Empty (Snoc _ _ _) SEEmpty impossible
mkAdd (Snoc _ _ _) Empty SEEmpty impossible

||| `AddCtx` is deterministic in its result.
public export
addDet : AddCtx g1 g2 ga -> AddCtx g1 g2 gb -> ga = gb
addDet ACEmpty ACEmpty = Refl
addDet (ACSnoc a q1 q2 ac1) (ACSnoc _ _ _ ac2) =
  cong (\w => Snoc w a (qAdd q1 q2)) (addDet ac1 ac2)

||| Determinism modulo propositional equality of the summands.
public export
addDetEq : AddCtx a b ga -> AddCtx a' b' gb -> a = a' -> b = b' -> ga = gb
addDetEq ac1 ac2 Refl Refl = addDet ac1 ac2

||| A split whose result is `Empty` has both summands `Empty`.
public export
addEmptyInv : AddCtx g1 g2 Empty -> (g1 = Empty, g2 = Empty)
addEmptyInv ACEmpty = (Refl, Refl)

||| Scaling to `Empty` forces the context to be `Empty`.
public export
scaleEmptyInv : (g : Ctx) -> ctxScale q g = Empty -> g = Empty
scaleEmptyInv Empty        _    = Refl
scaleEmptyInv (Snoc _ _ _) Refl impossible


------------------------------------------------------------
-- IsZero (analysable "everything erased")
------------------------------------------------------------

||| `IsZero g` witnesses that every quantity in `g` is `Zero`.
||| `HasVar`'s `HVHere` carries one so the substitution proof can
||| analyse the surrounding context.
public export
data IsZero : Ctx -> Type where
  IZEmpty : IsZero Empty
  IZSnoc  : IsZero g -> IsZero (Snoc g a Zero)

||| The canonical all-zero context built by `ctxZero` is zero.
public export
ctxZeroIsZero : (g : Ctx) -> IsZero (ctxZero g)
ctxZeroIsZero Empty        = IZEmpty
ctxZeroIsZero (Snoc g a q) = IZSnoc (ctxZeroIsZero g)

||| Invert a zero `Snoc`: the tail is zero and the head quantity is
||| `Zero`.
public export
isZeroSnocInv : IsZero (Snoc g a q) -> (IsZero g, q = Zero)
isZeroSnocInv (IZSnoc iz) = (iz, Refl)

||| Scaling a zero context by anything keeps it zero.
public export
isZeroScale : (q : Q) -> IsZero g -> IsZero (ctxScale q g)
isZeroScale q IZEmpty     = IZEmpty
isZeroScale q (IZSnoc iz) = rewrite qMulZeroR q in IZSnoc (isZeroScale q iz)

------------------------------------------------------------
-- Scaling laws (pure functions, equational)
------------------------------------------------------------

||| Scaling by `One` is the identity.
public export
scaleOne : (g : Ctx) -> ctxScale One g = g
scaleOne Empty        = Refl
scaleOne (Snoc g a q) = rewrite scaleOne g in rewrite qMulOneL q in Refl

||| Scaling by `Zero` produces a manifestly-zero context — namely
||| `ctxZero g`.
public export
scaleZero : (g : Ctx) -> ctxScale Zero g = ctxZero g
scaleZero Empty        = Refl
scaleZero (Snoc g a q) = rewrite scaleZero g in Refl

||| Scaling ANY context by `Zero` yields a zero context.
public export
scaleZeroIsZero : (g : Ctx) -> IsZero (ctxScale Zero g)
scaleZeroIsZero g = rewrite scaleZero g in ctxZeroIsZero g

||| Scaling associates with quantity multiplication.
public export
scaleScale : (q1, q2 : Q) -> (g : Ctx)
          -> ctxScale q1 (ctxScale q2 g) = ctxScale (qMul q1 q2) g
scaleScale q1 q2 Empty        = Refl
scaleScale q1 q2 (Snoc g a q) =
  rewrite scaleScale q1 q2 g in
  rewrite qMulAssoc q1 q2 q in Refl

------------------------------------------------------------
-- AddCtx algebra
------------------------------------------------------------

||| Commutativity: `g` is symmetric in the two summands.
public export
acComm : AddCtx g1 g2 g -> AddCtx g2 g1 g
acComm ACEmpty = ACEmpty
acComm (ACSnoc a q1 q2 ac) =
  rewrite qAddComm q1 q2 in ACSnoc a q2 q1 (acComm ac)

||| Scaling distributes over an additive split.
public export
acScale : (q : Q) -> AddCtx g1 g2 g
       -> AddCtx (ctxScale q g1) (ctxScale q g2) (ctxScale q g)
acScale q ACEmpty = ACEmpty
acScale q (ACSnoc a q1 q2 ac) =
  rewrite qMulDistribL q q1 q2 in ACSnoc a (qMul q q1) (qMul q q2) (acScale q ac)

||| Fusion: scaling the same context by `q1` and `q2` and adding
||| gives the context scaled by `qAdd q1 q2`.
public export
acScaleFuse : (q1, q2 : Q) -> (g : Ctx)
           -> AddCtx (ctxScale q1 g) (ctxScale q2 g) (ctxScale (qAdd q1 q2) g)
acScaleFuse q1 q2 Empty        = ACEmpty
acScaleFuse q1 q2 (Snoc g a q) =
  rewrite qMulDistribR q1 q2 q in
  ACSnoc a (qMul q1 q) (qMul q2 q) (acScaleFuse q1 q2 g)

||| Quantity-level interchange (medial) law, derived from the
||| commutative-monoid laws already proven in `Quantity.idr`.
public export
qInterchange : (qa, qb, qc, qd : Q)
            -> qAdd (qAdd qa qb) (qAdd qc qd) = qAdd (qAdd qa qc) (qAdd qb qd)
qInterchange qa qb qc qd =
  -- (qa+qb)+(qc+qd) = qa+(qb+(qc+qd)) = qa+((qb+qc)+qd)
  --                 = qa+((qc+qb)+qd) = qa+(qc+(qb+qd))
  --                 = (qa+qc)+(qb+qd)
  rewrite qAddAssoc qa qb (qAdd qc qd) in
  rewrite sym (qAddAssoc qb qc qd) in
  rewrite qAddComm qb qc in
  rewrite qAddAssoc qc qb qd in
  rewrite sym (qAddAssoc qa qc (qAdd qb qd)) in
  Refl

||| Context-level interchange: regroup a 2x2 block of additive splits.
||| From `s1 = a+b`, `s2 = c+d`, `s3 = a+c`, `s4 = b+d`, `t = s3+s4`
||| conclude `t = s1+s2`.
public export
acInterchange : AddCtx a b s1 -> AddCtx c d s2 -> AddCtx a c s3
             -> AddCtx b d s4 -> AddCtx s3 s4 t -> AddCtx s1 s2 t
acInterchange ACEmpty ACEmpty ACEmpty ACEmpty ACEmpty = ACEmpty
acInterchange (ACSnoc ty qa qb ab) (ACSnoc _ qc qd cd)
              (ACSnoc _ _ _ ac) (ACSnoc _ _ _ bd) (ACSnoc _ _ _ s34) =
  rewrite sym (qInterchange qa qb qc qd) in
  ACSnoc ty (qAdd qa qb) (qAdd qc qd) (acInterchange ab cd ac bd s34)

||| Interchange in *splitting* form: given the two "row" sums
||| `s1 = a+b`, `s2 = c+d` and the total `t = s1+s2`, construct the
||| two "column" sums `s3 = a+c`, `s4 = b+d` together with the
||| witness `t = s3+s4`. The constructed contexts inherit their
||| types from the input derivations, so no external shape-matching
||| is needed.
public export
acSplit2 : AddCtx a b s1 -> AddCtx c d s2 -> AddCtx s1 s2 t
        -> (s3 : Ctx ** s4 : Ctx **
             (AddCtx a c s3, AddCtx b d s4, AddCtx s3 s4 t))
acSplit2 ACEmpty ACEmpty ACEmpty = (Empty ** Empty ** (ACEmpty, ACEmpty, ACEmpty))
acSplit2 (ACSnoc ty qa qb ab) (ACSnoc _ qc qd cd) (ACSnoc _ _ _ s12) =
  let (s3 ** s4 ** (ac, bd, s34)) = acSplit2 ab cd s12 in
  ( Snoc s3 ty (qAdd qa qc) ** Snoc s4 ty (qAdd qb qd) **
    ( ACSnoc ty qa qc ac
    , ACSnoc ty qb qd bd
    , rewrite qInterchange qa qb qc qd in ACSnoc ty (qAdd qa qc) (qAdd qb qd) s34 ))

||| Associativity (right-leaning): from `(a+b)+c` recover `a+(b+c)`.
||| Returns the middle sum together with both witnesses.
public export
acAssoc : (b, c : Ctx) -> AddCtx ab c abc -> AddCtx a b ab
       -> (bc : Ctx ** (AddCtx b c bc, AddCtx a bc abc))
acAssoc Empty Empty ACEmpty ACEmpty = (Empty ** (ACEmpty, ACEmpty))
acAssoc (Snoc b ty qb) (Snoc c _ qc) (ACSnoc _ _ _ acABC) (ACSnoc _ qa _ acAB) =
  let (bc ** (acBC, acAbc)) = acAssoc b c acABC acAB in
  ((Snoc bc ty (qAdd qb qc)) **
   ( ACSnoc ty qb qc acBC
   , rewrite qAddAssoc qa qb qc in ACSnoc ty qa (qAdd qb qc) acAbc ))
acAssoc Empty (Snoc _ _ _) ACEmpty _ impossible
acAssoc (Snoc _ _ _) Empty (ACSnoc _ _ _ _) _ impossible

------------------------------------------------------------
-- Context append (puts the second context's entries ON TOP)
------------------------------------------------------------

export infixl 6 +++

||| `g +++ d` — append `d` on top of `g`. A variable at position `n`
||| in `g` has index `n + ctxLen d` once `d` binders sit above it.
public export
(+++) : Ctx -> Ctx -> Ctx
g +++ Empty        = g
g +++ (Snoc d a q) = Snoc (g +++ d) a q

||| Length of an append.
public export
ctxLenAppend : (g, d : Ctx) -> ctxLen (g +++ d) = ctxLen d + ctxLen g
ctxLenAppend g Empty        = Refl
ctxLenAppend g (Snoc d a q) = rewrite ctxLenAppend g d in Refl

||| Appending distributes over an additive split: if the base
||| splits as `gB = gB1 + gB2` and the top splits as
||| `cross = cross1 + cross2`, then the append splits accordingly.
public export
acAppend : AddCtx gB1 gB2 gB -> AddCtx c1 c2 c
        -> AddCtx (gB1 +++ c1) (gB2 +++ c2) (gB +++ c)
acAppend acB ACEmpty             = acB
acAppend acB (ACSnoc a q1 q2 acC) = ACSnoc a q1 q2 (acAppend acB acC)

||| Scaling commutes with append.
public export
scaleAppend : (q : Q) -> (g, d : Ctx)
           -> ctxScale q (g +++ d) = (ctxScale q g) +++ (ctxScale q d)
scaleAppend q g Empty        = Refl
scaleAppend q g (Snoc d a e) = rewrite scaleAppend q g d in Refl

||| Local `Nat` injectivity / discreteness helpers (avoid Data.Nat).
public export
sInj : the Nat (S m) = S n -> m = n
sInj Refl = Refl

public export
zNeqS : the Nat Z = S n -> Void
zNeqS Refl impossible

public export
plusLeftCancelZ : (n, a, b : Nat) -> n + a = n + b -> a = b
plusLeftCancelZ Z a b prf = prf
plusLeftCancelZ (S n) a b prf = plusLeftCancelZ n a b (sInj prf)

||| Split a context into its bottom `ctxLen g - ctxLen top` entries
||| and top `ctxLen top` entries (the latter shaped like `top`).
||| Recurses on `top`; the matching `Snoc` of `g` is forced by the
||| length equality.
public export
splitTop : (top : Ctx) -> (g : Ctx)
        -> ctxLen g = ctxLen top + base
        -> (gB : Ctx ** gC : Ctx ** (g = gB +++ gC, ctxLen gC = ctxLen top))
splitTop Empty g lenEq = (g ** Empty ** (Refl, Refl))
splitTop (Snoc top _ _) (Snoc g a e) lenEq =
  let (gB ** gC ** (geq, lenC)) = splitTop top g (sInj lenEq) in
  (gB ** Snoc gC a e ** (cong (\w => Snoc w a e) geq, cong S lenC))
splitTop (Snoc top _ _) Empty lenEq = absurd (zNeqS lenEq)

||| Scaling distributes over a (proven) append decomposition.
public export
scaleSplit : (q : Q) -> (gB, gC : Ctx)
          -> ctxScale q (gB +++ gC) = (ctxScale q gB) +++ (ctxScale q gC)
scaleSplit q gB gC = scaleAppend q gB gC

||| Scaling preserves length.
public export
scaleLen : (q : Q) -> (g : Ctx) -> ctxLen (ctxScale q g) = ctxLen g
scaleLen q Empty        = Refl
scaleLen q (Snoc g a e) = cong S (scaleLen q g)

||| An additive split preserves length (left).
public export
acLenL : AddCtx x y g -> ctxLen x = ctxLen g
acLenL ACEmpty     = Refl
acLenL (ACSnoc _ _ _ ac) = cong S (acLenL ac)

||| An additive split preserves length (right).
public export
acLenR : AddCtx x y g -> ctxLen y = ctxLen g
acLenR ACEmpty     = Refl
acLenR (ACSnoc _ _ _ ac) = cong S (acLenR ac)

||| Snoc injectivity helpers.
public export
snocInjTail : the Ctx (Snoc x1 t1 q1) = Snoc x2 t2 q2 -> x1 = x2
snocInjTail Refl = Refl

public export
snocInjTy : the Ctx (Snoc x1 t1 q1) = Snoc x2 t2 q2 -> t1 = t2
snocInjTy Refl = Refl

public export
snocInjQ : the Ctx (Snoc x1 t1 q1) = Snoc x2 t2 q2 -> q1 = q2
snocInjQ Refl = Refl

public export
cong3Snoc : x1 = x2 -> t1 = t2 -> q1 = q2
         -> the Ctx (Snoc x1 t1 q1) = Snoc x2 t2 q2
cong3Snoc Refl Refl Refl = Refl

||| Append injectivity when the top components have equal length.
public export
appendInjTop : {a, b : Ctx} -> (c1, c2 : Ctx) -> ctxLen c1 = ctxLen c2
            -> a +++ c1 = b +++ c2 -> (a = b, c1 = c2)
appendInjTop Empty Empty lenEq eq = (eq, Refl)
appendInjTop (Snoc c1 t1 q1) (Snoc c2 t2 q2) lenEq eq =
  let recur = appendInjTop {a} {b} c1 c2 (sInj lenEq) (snocInjTail eq) in
  ( fst recur
  , cong3Snoc (snd recur) (snocInjTy eq) (snocInjQ eq) )
appendInjTop Empty (Snoc _ _ _) lenEq eq = absurd (zNeqS lenEq)
appendInjTop (Snoc _ _ _) Empty lenEq eq = absurd (zNeqS (sym lenEq))

||| Recover the (unscaled) `Snoc`-then-append decomposition of `g`
||| from the same decomposition of its scaling `ctxScale q g`. Used
||| in the T-App / T-Let substitution cases, where the argument /
||| RHS context appears only in scaled form in the split.
public export
unscaleSnocAppend : (q : Q) -> (g : Ctx) -> (cross : Ctx) -> (at : Ty)
                 -> {sBB, sC : Ctx} -> {qa : Q}
                 -> ctxLen sC = ctxLen cross
                 -> ctxScale q g = (Snoc sBB at qa) +++ sC
                 -> ( gBB : Ctx ** qa' : Q ** gC : Ctx **
                      ( g = (Snoc gBB at qa') +++ gC
                      , ctxLen gC = ctxLen cross
                      , sBB = ctxScale q gBB
                      , qa = qMul q qa'
                      , sC = ctxScale q gC ))
unscaleSnocAppend q g cross at lenSC eq =
  let lenG : (ctxLen g = ctxLen cross + S (ctxLen sBB))
           := trans (sym (scaleLen q g))
                    (trans (cong ctxLen eq)
                           (trans (ctxLenAppend (Snoc sBB at qa) sC)
                                  (cong (\m => m + S (ctxLen sBB)) lenSC)))
      (gBase ** gC ** (gEq, lenGC)) = splitTop cross g lenG
  in case gBase of
       Empty => absurd (zNeqS (lenGBaseNonZero Empty gC g gEq lenG lenGC))
       Snoc gBB at' qa' =>
         -- ctxScale q g = ctxScale q ((Snoc gBB at' qa') +++ gC)
         --              = (Snoc (ctxScale q gBB) at' (qMul q qa')) +++ ctxScale q gC
         -- match against (Snoc sBB at qa) +++ sC by appendInjTop.
         let scEq : (ctxScale q g
                      = (Snoc (ctxScale q gBB) at' (qMul q qa')) +++ (ctxScale q gC))
                  := trans (cong (ctxScale q) gEq) (scaleAppend q (Snoc gBB at' qa') gC)
             combEq : ((Snoc sBB at qa) +++ sC
                        = (Snoc (ctxScale q gBB) at' (qMul q qa')) +++ (ctxScale q gC))
                    := trans (sym eq) scEq
             lenTopEq : (ctxLen sC = ctxLen (ctxScale q gC))
                      := trans lenSC (trans (sym lenGC) (sym (scaleLen q gC)))
             (headEq, tailEq) = appendInjTop sC (ctxScale q gC) lenTopEq combEq
             -- headEq : Snoc sBB at qa = Snoc (ctxScale q gBB) at' (qMul q qa')
             atEq  : (at = at')   := snocInjTy headEq
             sbbEq : (sBB = ctxScale q gBB) := snocInjTail headEq
             qaEq  : (qa = qMul q qa')      := snocInjQ headEq
         in ( gBB ** qa' ** gC **
              ( rewrite atEq in gEq
              , lenGC
              , sbbEq
              , qaEq
              , tailEq ) )
 where
  lenGBaseNonZero : (gBase : Ctx) -> (gC : Ctx) -> (g : Ctx)
                 -> g = gBase +++ gC -> ctxLen g = ctxLen cross + S (ctxLen sBB)
                 -> ctxLen gC = ctxLen cross -> ctxLen gBase = S (ctxLen sBB)
  lenGBaseNonZero gBase gC g gEq lenG lenGC =
    -- ctxLen g = ctxLen gC + ctxLen gBase = ctxLen cross + ctxLen gBase
    -- and = ctxLen cross + S (ctxLen sBB); so ctxLen gBase = S (ctxLen sBB).
    let l1 : (ctxLen g = ctxLen gC + ctxLen gBase) := rewrite gEq in ctxLenAppend gBase gC
        l2 : (ctxLen cross + ctxLen gBase = ctxLen cross + S (ctxLen sBB))
           := trans (trans (sym (cong (\m => m + ctxLen gBase) lenGC)) (sym l1)) lenG
    in plusLeftCancelZ (ctxLen cross) (ctxLen gBase) (S (ctxLen sBB)) l2

------------------------------------------------------------
-- Inverting an additive split along an append
------------------------------------------------------------

||| Reconstruct the left summand of an additive split from the
||| (runtime) `AddCtx` value, plus a proof it equals the erased index.
public export
acLeft : {0 x : Ctx} -> {0 y : Ctx} -> {0 g : Ctx} -> AddCtx x y g -> Ctx
acLeft ACEmpty            = Empty
acLeft (ACSnoc a q1 _ ac) = Snoc (acLeft ac) a q1

public export
acRight : {0 x : Ctx} -> {0 y : Ctx} -> {0 g : Ctx} -> AddCtx x y g -> Ctx
acRight ACEmpty            = Empty
acRight (ACSnoc a _ q2 ac) = Snoc (acRight ac) a q2

public export
acLeftEq : (ac : AddCtx x y g) -> x = acLeft ac
acLeftEq ACEmpty            = Refl
acLeftEq (ACSnoc a q1 _ ac) = cong (\w => Snoc w a q1) (acLeftEq ac)

public export
acRightEq : (ac : AddCtx x y g) -> y = acRight ac
acRightEq ACEmpty            = Refl
acRightEq (ACSnoc a _ q2 ac) = cong (\w => Snoc w a q2) (acRightEq ac)

||| The reconstructed summands really do add to `g`.
public export
acRebuild : (ac : AddCtx x y g) -> AddCtx (acLeft ac) (acRight ac) g
acRebuild ACEmpty            = ACEmpty
acRebuild (ACSnoc a q1 q2 ac) = ACSnoc a q1 q2 (acRebuild ac)

||| Invert an additive split whose result is a `Snoc`. Recovers the
||| two summand tails, their head quantities (with `q = qAdd q1 q2`),
||| and the tail split — as runtime data plus equations, so the
||| caller can refine other hypotheses with the `q` equation.
public export
acSnocInv : {0 x : Ctx} -> {0 y : Ctx} -> {0 g : Ctx} -> {0 a : Ty} -> {0 q : Q}
         -> AddCtx x y (Snoc g a q)
         -> ( at : Ty ** xt : Ctx ** yt : Ctx ** qa1 : Q ** qa2 : Q **
              ( a = at
              , x = Snoc xt at qa1
              , y = Snoc yt at qa2
              , q = qAdd qa1 qa2
              , AddCtx xt yt g ))
acSnocInv (ACSnoc a q1 q2 ac) =
  ( a ** acLeft ac ** acRight ac ** q1 ** q2 **
    ( Refl
    , cong (\w => Snoc w a q1) (acLeftEq ac)
    , cong (\w => Snoc w a q2) (acRightEq ac)
    , Refl
    , acRebuild ac ))

||| Decompose a split whose result is an append. Splitting
||| `AddCtx x y (gB +++ cross)` yields matching appends of `x` and
||| `y` together with splits of the base and the top.
public export
acAppendInv : {0 x : Ctx} -> {0 y : Ctx} -> {0 gB : Ctx} -> (cross : Ctx)
           -> AddCtx x y (gB +++ cross)
           -> ( xB : Ctx ** yB : Ctx ** xC : Ctx ** yC : Ctx **
                ( x = xB +++ xC
                , y = yB +++ yC
                , AddCtx xB yB gB
                , AddCtx xC yC cross ))
acAppendInv Empty ac =
  (acLeft ac ** acRight ac ** Empty ** Empty **
    (acLeftEq ac, acRightEq ac, acRebuild ac, ACEmpty))
acAppendInv (Snoc cross _ _) (ACSnoc a qx qy ac') =
  let (xB ** yB ** xC ** yC ** (ex, ey, acB, acC)) = acAppendInv cross ac' in
  ( xB ** yB ** Snoc xC a qx ** Snoc yC a qy **
    ( cong (\w => Snoc w a qx) ex
    , cong (\w => Snoc w a qy) ey
    , acB
    , ACSnoc a qx qy acC ) )

------------------------------------------------------------
-- Additive zero identities (type-tracked, so no mismatch)
------------------------------------------------------------

||| Adding a zero context on the LEFT is the identity.
public export
addZeroLEq : IsZero z -> AddCtx z g g' -> g' = g
addZeroLEq IZEmpty ACEmpty = Refl
addZeroLEq (IZSnoc iz) (ACSnoc _ _ q2 ac) =
  rewrite addZeroLEq iz ac in rewrite qAddZeroL q2 in Refl

||| Adding a zero context on the RIGHT is the identity.
public export
addZeroREq : IsZero z -> AddCtx g z g' -> g' = g
addZeroREq IZEmpty ACEmpty = Refl
addZeroREq (IZSnoc iz) (ACSnoc _ q1 _ ac) =
  rewrite addZeroREq iz ac in rewrite qAddZeroR q1 in Refl

||| The sum of two zero contexts is zero.
public export
addZeroResult : IsZero z1 -> IsZero z2 -> AddCtx z1 z2 g -> IsZero g
addZeroResult IZEmpty IZEmpty ACEmpty = IZEmpty
addZeroResult (IZSnoc i1) (IZSnoc i2) (ACSnoc _ _ _ ac) =
  IZSnoc (addZeroResult i1 i2 ac)

------------------------------------------------------------
-- IsZero and append
------------------------------------------------------------

||| Split a zero append into its two zero halves.
public export
isZeroAppendSplit : (d : Ctx) -> IsZero (g +++ d) -> (IsZero g, IsZero d)
isZeroAppendSplit Empty        iz          = (iz, IZEmpty)
isZeroAppendSplit (Snoc d a _) (IZSnoc iz) =
  let (izg, izd) = isZeroAppendSplit d iz in (izg, IZSnoc izd)

||| Rebuild a zero append from zero halves.
public export
isZeroAppendJoin : IsZero g -> IsZero d -> IsZero (g +++ d)
isZeroAppendJoin izg IZEmpty       = izg
isZeroAppendJoin izg (IZSnoc izd)  = IZSnoc (isZeroAppendJoin izg izd)

------------------------------------------------------------
-- Quantity-level ordering facts for the sub-context order
------------------------------------------------------------

||| A summand is below the sum: `q1 <= q1 + q2`. Proved by exhaustive
||| case split on the three-element semiring.
public export
qLeAddL : (q1, q2 : Q) -> qLe q1 (qAdd q1 q2) = True
qLeAddL Zero  Zero  = Refl
qLeAddL Zero  One   = Refl
qLeAddL Zero  Omega = Refl
qLeAddL One   Zero  = Refl
qLeAddL One   One   = Refl
qLeAddL One   Omega = Refl
qLeAddL Omega Zero  = Refl
qLeAddL Omega One   = Refl
qLeAddL Omega Omega = Refl

||| Symmetric form: the right summand is below the sum, `q2 <= q1 + q2`.
public export
qLeAddR : (q1, q2 : Q) -> qLe q2 (qAdd q1 q2) = True
qLeAddR q1 q2 = rewrite qAddComm q1 q2 in qLeAddL q2 q1

||| `qAdd` is monotone in its first argument w.r.t. `qLe`. Proved by
||| exhaustive case split: where the hypothesis `qLe x y` is `False`
||| the case is discharged by `absurd`; the remaining cases hold
||| definitionally.
public export
qAddMono : (x, y, z : Q) -> qLe x y = True -> qLe (qAdd x z) (qAdd y z) = True
qAddMono Zero  Zero  z prf = qLeRefl (qAdd Zero z)
qAddMono Zero  One   z prf = qLeAddRefine z
  where
    qLeAddRefine : (z : Q) -> qLe (qAdd Zero z) (qAdd One z) = True
    qLeAddRefine Zero  = Refl
    qLeAddRefine One   = Refl
    qLeAddRefine Omega = Refl
qAddMono Zero  Omega z prf = qLeAddRefine z
  where
    qLeAddRefine : (z : Q) -> qLe (qAdd Zero z) (qAdd Omega z) = True
    qLeAddRefine Zero  = Refl
    qLeAddRefine One   = Refl
    qLeAddRefine Omega = Refl
qAddMono One   One   z prf = qLeRefl (qAdd One z)
qAddMono One   Omega z prf = qLeAddRefine z
  where
    qLeAddRefine : (z : Q) -> qLe (qAdd One z) (qAdd Omega z) = True
    qLeAddRefine Zero  = Refl
    qLeAddRefine One   = Refl
    qLeAddRefine Omega = Refl
qAddMono Omega Omega z prf = qLeRefl (qAdd Omega z)
qAddMono One   Zero  z prf = absurd prf
qAddMono Omega Zero  z prf = absurd prf
qAddMono Omega One   z prf = absurd prf

------------------------------------------------------------
-- The sub-context order (pointwise `qLe`, same spine and types)
------------------------------------------------------------

||| `Weaker gp g` : `gp` is below `g` in the pointwise quantity order
||| (same spine, same per-position types, and every `gp` quantity is
||| `<=` the corresponding `g` quantity). This is the affine
||| sub-context order: a derivation typed in `gp` can be re-read as
||| living "inside" the larger resource budget `g`.
|||
||| `qp`, `q` and `a` are explicit RUNTIME arguments of `WkSnoc`
||| (following the same convention as `ACSnoc`/`THApp`): the typed
||| indices `gp`, `g`, the quantities and the per-position type are
||| erased, so the monotonicity lemmas (which must rebuild the shrunk
||| `AddCtx` and inspect quantities) need them retained at runtime.
public export
data Weaker : Ctx -> Ctx -> Type where
  WkEmpty : Weaker Empty Empty
  WkSnoc  : (qp : Q) -> (q : Q) -> (a : Ty)
         -> Weaker gp g -> (qLe qp q = True) -> Weaker (Snoc gp a qp) (Snoc g a q)

||| Reflexivity: every context is `Weaker` than itself (via `qLeRefl`).
public export
weakerRefl : (g : Ctx) -> Weaker g g
weakerRefl Empty        = WkEmpty
weakerRefl (Snoc g a q) = WkSnoc q q a (weakerRefl g) (qLeRefl q)

||| Reflexivity recovered from a runtime additive split that produces
||| `g`. Used by the beta cases of preservation, where the redex's
||| context `g` is an erased index but an `AddCtx _ _ g` witness is in
||| hand: the substitution lemma types the reduct in exactly `g`, so
||| the affine conclusion is `Weaker g g`.
public export
weakerReflFromAdd : {0 g1, g2, g : Ctx} -> AddCtx g1 g2 g -> Weaker g g
weakerReflFromAdd ACEmpty             = WkEmpty
weakerReflFromAdd (ACSnoc a q1 q2 ac) =
  WkSnoc (qAdd q1 q2) (qAdd q1 q2) a (weakerReflFromAdd ac) (qLeRefl (qAdd q1 q2))

||| Recover the (erased) result context `g` of an additive split as a
||| RUNTIME value, packaged with the proof that it equals `g`. The beta
||| cases of preservation need this to give the affine existential a
||| runtime witness when the only handle on `g` is the `AddCtx`.
public export
acResult : {0 g1, g2, g : Ctx} -> AddCtx g1 g2 g -> (gr : Ctx ** gr = g)
acResult ACEmpty             = (Empty ** Refl)
acResult (ACSnoc a q1 q2 ac) =
  let (gr ** eq) = acResult ac in
  (Snoc gr a (qAdd q1 q2) ** cong (\w => Snoc w a (qAdd q1 q2)) eq)

||| The left summand of an additive split is below the sum.
public export
addCtxLeftWeaker : AddCtx g1 g2 g -> Weaker g1 g
addCtxLeftWeaker ACEmpty             = WkEmpty
addCtxLeftWeaker (ACSnoc a q1 q2 ac) =
  WkSnoc q1 (qAdd q1 q2) a (addCtxLeftWeaker ac) (qLeAddL q1 q2)

||| The right summand of an additive split is below the sum.
public export
addCtxRightWeaker : AddCtx g1 g2 g -> Weaker g2 g
addCtxRightWeaker ACEmpty             = WkEmpty
addCtxRightWeaker (ACSnoc a q1 q2 ac) =
  WkSnoc q2 (qAdd q1 q2) a (addCtxRightWeaker ac) (qLeAddR q1 q2)

||| `qMul` is monotone in its second argument: from `x <= y` conclude
||| `q*x <= q*y`. Discharged by case split, the `False` hypotheses
||| ruled out by `absurd`.
public export
qMulMono : (q, x, y : Q) -> qLe x y = True -> qLe (qMul q x) (qMul q y) = True
qMulMono Zero  x     y     pf = Refl
qMulMono One   x     y     pf = rewrite qMulOneL x in rewrite qMulOneL y in pf
qMulMono Omega Zero  Zero  pf = Refl
qMulMono Omega Zero  One   pf = Refl
qMulMono Omega Zero  Omega pf = Refl
qMulMono Omega One   One   pf = Refl
qMulMono Omega One   Omega pf = Refl
qMulMono Omega Omega Omega pf = Refl
qMulMono Omega One   Zero  pf = absurd pf
qMulMono Omega Omega Zero  pf = absurd pf
qMulMono Omega Omega One   pf = absurd pf

||| Scaling is monotone in the sub-context order: scaling both sides of
||| a `Weaker` by the same quantity preserves it.
public export
scaleMono : {0 gp, g : Ctx} -> (q : Q) -> Weaker gp g
         -> Weaker (ctxScale q gp) (ctxScale q g)
scaleMono q WkEmpty            = WkEmpty
scaleMono q (WkSnoc qp qq a wk le) =
  WkSnoc (qMul q qp) (qMul q qq) a (scaleMono q wk) (qMulMono q qp qq le)

------------------------------------------------------------
-- Monotonicity of additive splitting under `Weaker`
------------------------------------------------------------

||| `qAdd` monotone in its SECOND argument (mirror of `qAddMono`).
public export
qAddMonoR : (z, x, y : Q) -> qLe x y = True -> qLe (qAdd z x) (qAdd z y) = True
qAddMonoR z x y pf =
  rewrite qAddComm z x in rewrite qAddComm z y in qAddMono x y z pf

||| Congruence for the LEFT summand: if `g1p` is below `g1` and
||| `AddCtx g1 h g`, then re-splitting with the shrunk left summand
||| yields some `gOut` that is below `g`. The output split is built by
||| `ACSnoc` on the shared spine, and `Weaker` follows from
||| `qAddMono` at each position.
public export
addCtxMonoLeft : {0 g1p, g1, h, g : Ctx}
              -> Weaker g1p g1 -> AddCtx g1 h g
              -> (gOut : Ctx ** (AddCtx g1p h gOut, Weaker gOut g))
addCtxMonoLeft WkEmpty ACEmpty = (Empty ** (ACEmpty, WkEmpty))
addCtxMonoLeft (WkSnoc qp q a wk le) (ACSnoc _ _ q2 ac) =
  let (gOut ** (acOut, wkOut)) = addCtxMonoLeft wk ac in
  ( Snoc gOut a (qAdd qp q2)
  ** ( ACSnoc a qp q2 acOut
     , WkSnoc (qAdd qp q2) (qAdd q q2) a wkOut (qAddMono qp q q2 le) ) )

||| Congruence for the RIGHT summand, symmetric to `addCtxMonoLeft`.
public export
addCtxMonoRight : {0 hp, h, g1, g : Ctx}
               -> Weaker hp h -> AddCtx g1 h g
               -> (gOut : Ctx ** (AddCtx g1 hp gOut, Weaker gOut g))
addCtxMonoRight WkEmpty ACEmpty = (Empty ** (ACEmpty, WkEmpty))
addCtxMonoRight (WkSnoc qp q a wk le) (ACSnoc _ q1 _ ac) =
  let (gOut ** (acOut, wkOut)) = addCtxMonoRight wk ac in
  ( Snoc gOut a (qAdd q1 qp)
  ** ( ACSnoc a q1 qp acOut
     , WkSnoc (qAdd q1 qp) (qAdd q1 q) a wkOut (qAddMonoR q1 qp q le) ) )
