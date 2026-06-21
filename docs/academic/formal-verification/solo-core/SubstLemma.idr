-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- The QTT substitution lemma for the Solo core.
--
-- This module supplies the single load-bearing lemma behind
-- preservation: substituting a value for a bound variable preserves
-- typing, with the contexts combining exactly as the QTT semiring
-- prescribes (`Γ + q·Δ`). It is proved by induction on the typing
-- derivation of the body, generalised over a `cross` context of
-- binders crossed since the substituted variable, so the depth
-- bookkeeping is explicit.
--
-- Two supporting weakening lemmas come first:
--   * `weakenVar` / `weakenAt`  — insert an unused (Zero) binder at
--     a given depth, shifting free variables accordingly;
--   * `substVarLemma`           — the variable case of substitution.

module SubstLemma

import Quantity
import Syntax
import Context
import ContextLemmas
import Typing
import Subst

%default total

------------------------------------------------------------
-- Variable weakening: insert one Zero binder at depth ctxLen cross
------------------------------------------------------------

||| Inserting an unused binding `b` (quantity `Zero`) at depth
||| `ctxLen cross` shifts the variable index by one above that depth
||| and preserves the lookup.
public export
weakenVar : (cross : Ctx)
         -> {0 g : Ctx} -> {0 n : Nat} -> {0 bt : Ty}
         -> HasVar (g +++ cross) n bt
         -> (bn : Ty)
         -> HasVar ((Snoc g bn Zero) +++ cross) (shiftVar 1 (ctxLen cross) n) bt
weakenVar Empty hv bn = HVThere hv
weakenVar (Snoc cross _ _) (HVHere iz) bn =
  let (izg, izc) = isZeroAppendSplit cross iz in
  HVHere (isZeroAppendJoin (IZSnoc izg) izc)
weakenVar (Snoc cross _ _) (HVThere hv') bn =
  HVThere (weakenVar cross hv' bn)

||| Algebraic identity used to reshape the scaled argument context in
||| the T-App weakening case.
public export
scaledArgEq : (q : Q) -> (g2B, g2C : Ctx) -> (bn : Ty) -> (sB, sC : Ctx)
           -> sB = ctxScale q g2B -> sC = ctxScale q g2C
           -> ctxScale q ((Snoc g2B bn Zero) +++ g2C) = (Snoc sB bn Zero) +++ sC
scaledArgEq q g2B g2C bn sB sC eSB eSC =
  rewrite eSB in rewrite eSC in
  rewrite scaleAppend q (Snoc g2B bn Zero) g2C in
  rewrite qMulZeroR q in Refl

------------------------------------------------------------
-- Term weakening: insert one Zero binder at depth ctxLen cross
------------------------------------------------------------

||| Insert an unused binding `b` (quantity `Zero`) at depth
||| `ctxLen cross` into the typing context, shifting the term's free
||| variables above that depth. Proved by induction on the typing
||| derivation.
public export
weakenAt : (cross : Ctx)
        -> {0 g : Ctx} -> {0 t : Tm} -> {0 bt : Ty}
        -> Has (g +++ cross) t bt
        -> (bn : Ty)
        -> Has ((Snoc g bn Zero) +++ cross) (shift 1 (ctxLen cross) t) bt
weakenAt cross (THVar hv) bn = THVar (weakenVar cross hv bn)
weakenAt cross (THUnit iz) bn =
  let (izg, izc) = isZeroAppendSplit cross iz in
  THUnit (isZeroAppendJoin (IZSnoc izg) izc)
weakenAt cross (THLam q a bodyD) bn =
  -- body typed in Snoc (g+++cross) a q = g +++ (Snoc cross a q)
  THLam q a (weakenAt (Snoc cross a q) bodyD bn)
weakenAt cross (THFst pD) bn = THFst (weakenAt cross pD bn)
weakenAt cross (THSnd pD) bn = THSnd (weakenAt cross pD bn)
weakenAt cross (THInl pD) bn = THInl (weakenAt cross pD bn)
weakenAt cross (THInr pD) bn = THInr (weakenAt cross pD bn)
weakenAt cross (THApp {g1} {a} {t1} {t2} q g2 fD xD ac) bn =
  let (g1B ** sB ** g1C ** sC ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let lenG1C : (ctxLen g1C = ctxLen cross) := acLenL acC
      lenSC   : (ctxLen sC = ctxLen cross) := acLenR acC
      -- split g2 at the cross depth
      lenG2   : (ctxLen g2 = ctxLen cross + ctxLen sB)
              := trans (sym (scaleLen q g2))
                       (trans (cong ctxLen e2)
                              (trans (ctxLenAppend sB sC)
                                     (cong (\m => m + ctxLen sB) lenSC)))
  in
  let (g2B ** g2C ** (e3, lenG2C)) = splitTop cross g2 lenG2 in
  let -- relate scaled summands
      scaleEq : (sB +++ sC = (ctxScale q g2B) +++ (ctxScale q g2C))
              := trans (sym e2)
                       (trans (cong (ctxScale q) e3) (scaleAppend q g2B g2C))
      lenTop  : (ctxLen sC = ctxLen (ctxScale q g2C))
              := trans lenSC (trans (sym lenG2C) (sym (scaleLen q g2C)))
      injPair : (sB = ctxScale q g2B, sC = ctxScale q g2C)
              := appendInjTop sC (ctxScale q g2C) lenTop scaleEq
      eSB : (sB = ctxScale q g2B) := fst injPair
      eSC : (sC = ctxScale q g2C) := snd injPair
      -- weaken the two sub-derivations
      fD' : (Has (g1B +++ g1C) t1 (TArr q a bt)) := rewrite sym e1 in fD
      xD' : (Has (g2B +++ g2C) t2 a)             := rewrite sym e3 in xD
      fW  : (Has ((Snoc g1B bn Zero) +++ g1C) (shift 1 (ctxLen cross) t1) (TArr q a bt))
          := rewrite sym lenG1C in weakenAt g1C fD' bn
      xW  : (Has ((Snoc g2B bn Zero) +++ g2C) (shift 1 (ctxLen cross) t2) a)
          := rewrite sym lenG2C in weakenAt g2C xD' bn
      -- the scaled second summand equals (Snoc sB bn Zero) +++ sC
      scaledEq : (ctxScale q ((Snoc g2B bn Zero) +++ g2C)
                   = (Snoc sB bn Zero) +++ sC)
               := scaledArgEq q g2B g2C bn sB sC eSB eSC
      base : (AddCtx (Snoc g1B bn Zero) (Snoc sB bn Zero) (Snoc g bn Zero))
           := ACSnoc bn Zero Zero acB
      newSplit : (AddCtx ((Snoc g1B bn Zero) +++ g1C)
                         (ctxScale q ((Snoc g2B bn Zero) +++ g2C))
                         ((Snoc g bn Zero) +++ cross))
               := rewrite scaledEq in acAppend base acC
  in THApp q ((Snoc g2B bn Zero) +++ g2C) fW xW newSplit
weakenAt cross (THPair {t1} {t2} {a} {b} g1 g2 t1D t2D ac) bn =
  -- SPLIT pair: weaken each component in its own summand context,
  -- inserting a Zero binder into both halves of the split (mirroring
  -- the THCase scrutinee weakening, minus the branch binders).
  let (g1B ** g2B ** g1C ** g2C ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let lenG1C : (ctxLen g1C = ctxLen cross) := acLenL acC
      lenG2C : (ctxLen g2C = ctxLen cross) := acLenR acC
      t1D' : (Has (g1B +++ g1C) t1 a) := rewrite sym e1 in t1D
      t2D' : (Has (g2B +++ g2C) t2 b) := rewrite sym e2 in t2D
      t1W  : (Has ((Snoc g1B bn Zero) +++ g1C) (shift 1 (ctxLen cross) t1) a)
           := rewrite sym lenG1C in weakenAt g1C t1D' bn
      t2W  : (Has ((Snoc g2B bn Zero) +++ g2C) (shift 1 (ctxLen cross) t2) b)
           := rewrite sym lenG2C in weakenAt g2C t2D' bn
      newSplit : (AddCtx ((Snoc g1B bn Zero) +++ g1C)
                         ((Snoc g2B bn Zero) +++ g2C)
                         ((Snoc g bn Zero) +++ cross))
               := acAppend (ACSnoc bn Zero Zero acB) acC
  in THPair ((Snoc g1B bn Zero) +++ g1C) ((Snoc g2B bn Zero) +++ g2C) t1W t2W newSplit
weakenAt cross (THCase {g2} {t} {tL} {tR} {c} ca cb g1 sD lD rD ac) bn =
  let (g1B ** g2B ** g1C ** g2C ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let lenG1C : (ctxLen g1C = ctxLen cross) := acLenL acC
      lenG2C : (ctxLen g2C = ctxLen cross) := acLenR acC
      sD' : (Has (g1B +++ g1C) t (TSum ca cb)) := rewrite sym e1 in sD
      sW  : (Has ((Snoc g1B bn Zero) +++ g1C) (shift 1 (ctxLen cross) t) (TSum ca cb))
          := rewrite sym lenG1C in weakenAt g1C sD' bn
      lD' : (Has (g2B +++ (Snoc g2C ca One)) tL c) := rewrite sym e2 in lD
      rD' : (Has (g2B +++ (Snoc g2C cb One)) tR c) := rewrite sym e2 in rD
      lW  : (Has (Snoc ((Snoc g2B bn Zero) +++ g2C) ca One)
                 (shift 1 (S (ctxLen cross)) tL) c)
          := rewrite sym lenG2C in weakenAt (Snoc g2C ca One) lD' bn
      rW  : (Has (Snoc ((Snoc g2B bn Zero) +++ g2C) cb One)
                 (shift 1 (S (ctxLen cross)) tR) c)
          := rewrite sym lenG2C in weakenAt (Snoc g2C cb One) rD' bn
      newSplit : (AddCtx ((Snoc g1B bn Zero) +++ g1C)
                         ((Snoc g2B bn Zero) +++ g2C)
                         ((Snoc g bn Zero) +++ cross))
               := acAppend (ACSnoc bn Zero Zero acB) acC
  in THCase ca cb ((Snoc g1B bn Zero) +++ g1C) sW lW rW newSplit
weakenAt cross (THLet {g2} {t1} {t2} {b=bres} q at g1 e1D e2D ac) bn =
  let (sB ** g2B ** sC ** g2C ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let lenSC   : (ctxLen sC = ctxLen cross) := acLenL acC
      lenG2C  : (ctxLen g2C = ctxLen cross) := acLenR acC
      lenG1   : (ctxLen g1 = ctxLen cross + ctxLen sB)
              := trans (sym (scaleLen q g1))
                       (trans (cong ctxLen e1)
                              (trans (ctxLenAppend sB sC)
                                     (cong (\m => m + ctxLen sB) lenSC)))
  in
  let (g1B ** g1C ** (e3, lenG1C)) = splitTop cross g1 lenG1 in
  let scaleEq : (sB +++ sC = (ctxScale q g1B) +++ (ctxScale q g1C))
              := trans (sym e1)
                       (trans (cong (ctxScale q) e3) (scaleAppend q g1B g1C))
      lenTop  : (ctxLen sC = ctxLen (ctxScale q g1C))
              := trans lenSC (trans (sym lenG1C) (sym (scaleLen q g1C)))
      injPair : (sB = ctxScale q g1B, sC = ctxScale q g1C)
              := appendInjTop sC (ctxScale q g1C) lenTop scaleEq
      eSB : (sB = ctxScale q g1B) := fst injPair
      eSC : (sC = ctxScale q g1C) := snd injPair
      e1D' : (Has (g1B +++ g1C) t1 at) := rewrite sym e3 in e1D
      e2D' : (Has (g2B +++ (Snoc g2C at q)) t2 bres) := rewrite sym e2 in e2D
      e1W  : (Has ((Snoc g1B bn Zero) +++ g1C) (shift 1 (ctxLen cross) t1) at)
           := rewrite sym lenG1C in weakenAt g1C e1D' bn
      e2W  : (Has (Snoc ((Snoc g2B bn Zero) +++ g2C) at q)
                  (shift 1 (S (ctxLen cross)) t2) bres)
           := rewrite sym lenG2C in weakenAt (Snoc g2C at q) e2D' bn
      scaledEq : (ctxScale q ((Snoc g1B bn Zero) +++ g1C)
                   = (Snoc sB bn Zero) +++ sC)
               := scaledArgEq q g1B g1C bn sB sC eSB eSC
      newSplit : (AddCtx (ctxScale q ((Snoc g1B bn Zero) +++ g1C))
                         ((Snoc g2B bn Zero) +++ g2C)
                         ((Snoc g bn Zero) +++ cross))
               := rewrite scaledEq in acAppend (ACSnoc bn Zero Zero acB) acC
  in THLet q at ((Snoc g1B bn Zero) +++ g1C) e1W e2W newSplit

||| Weaken by inserting a single unused (Zero) binder at the TOP of
||| the context, shifting every free variable up by one. (The
||| `cross = Empty` instance of `weakenAt`.)
public export
weaken1 : {0 g : Ctx} -> {0 t : Tm} -> {0 bt : Ty}
       -> Has g t bt -> (bn : Ty) -> Has (Snoc g bn Zero) (shift 1 0 t) bt
weaken1 d bn = weakenAt Empty d bn

------------------------------------------------------------
-- The variable case of substitution
------------------------------------------------------------

||| Substituting the value `v` for the variable at depth
||| `ctxLen cross` inside a single occurrence `Var n`.
|||
||| The substituted binder sits at the bottom of the context with
||| quantity `q` and type `av`; the value `v` is typed in `gV`; the
||| output context is `gOut = gB + q·gV`. Crossing binders shift
||| `v` up by one each (handled by `substVar`).
public export
substVarLemma : (cross : Ctx) -> (gV : Ctx)
             -> {0 gB : Ctx} -> {0 av : Ty} -> {0 q : Q}
             -> {0 n : Nat} -> {0 bt : Ty} -> {0 gOut, v : _}
             -> HasVar ((Snoc gB av q) +++ cross) n bt
             -> Has gV v av
             -> AddCtx gB (ctxScale q gV) gOut
             -> Has (gOut +++ cross) (substVar (ctxLen cross) v n) bt
substVarLemma Empty gV (HVHere izB) vD acOut =
  -- n = Z, bt = av, q = One; substVar Z v Z = v.
  -- gOut = gB + One·gV ; gB zero, One·gV = gV  ==>  gOut = gV.
  let eq1 : (ctxScale One gV = gV) := scaleOne gV
      acOut' : (AddCtx gB gV gOut) := rewrite sym eq1 in acOut
      goutEq : (gOut = gV) := addZeroLEq izB acOut'
  in rewrite goutEq in vD
substVarLemma Empty gV (HVThere {n=n'} hv') vD acOut =
  -- n = S n', q = Zero; substVar Z v (S n') = Var n'.
  -- gOut = gB + Zero·gV = gB.
  let izScale : (IsZero (ctxScale Zero gV)) := scaleZeroIsZero gV
      goutEq  : (gOut = gB) := addZeroREq izScale acOut
  in rewrite goutEq in THVar hv'
substVarLemma (Snoc cross _ _) gV (HVHere izC) vD acOut =
  -- top binder is the crossed one (quantity One here); n = Z.
  -- substVar (S (ctxLen cross)) v Z = Var Z.
  -- izC : IsZero ((Snoc gB av q) +++ cross), so gB zero, q = Zero,
  -- cross zero ==> gOut zero.
  let (izSnocGB, izc) = isZeroAppendSplit cross izC
      (izB, qEq)       = isZeroSnocInv izSnocGB
      -- q = Zero, so ctxScale q gV is zero; gB zero ==> gOut zero.
      acOut0 : (AddCtx gB (ctxScale Zero gV) gOut) := rewrite sym qEq in acOut
      izGout : (IsZero gOut)
             := addZeroResult izB (scaleZeroIsZero gV) acOut0
  in THVar (HVHere (isZeroAppendJoin izGout izc))
substVarLemma (Snoc cross _ _) gV (HVThere {n=n'} hv') vD acOut =
  -- n = S n'; substVar (S (ctxLen cross)) v (S n')
  --   = shift 1 0 (substVar (ctxLen cross) v n').
  -- Recurse below the top binder, then weaken by it.
  weaken1 (substVarLemma cross gV hv' vD acOut) _

------------------------------------------------------------
-- The substitution lemma (generalised over crossed binders)
------------------------------------------------------------

||| `substGen cross gV bodyD vD acOut` — substitute the value `v`
||| (typed in `gV`) for the variable at depth `ctxLen cross` in
||| `body`. The substituted binder sits at the bottom of the body's
||| context with type `av` and quantity `q`; the output context is
||| `gOut = gB + q·gV` (witnessed by `acOut`). Proved by induction on
||| the body's typing derivation.
public export
substGen : (cross : Ctx) -> (gV : Ctx)
        -> {0 gB : Ctx} -> {0 av : Ty} -> {0 q : Q}
        -> {0 body : Tm} -> {0 bt : Ty} -> {0 gOut, v : _}
        -> Has ((Snoc gB av q) +++ cross) body bt
        -> Has gV v av
        -> AddCtx gB (ctxScale q gV) gOut
        -> Has (gOut +++ cross) (subst (ctxLen cross) v body) bt
substGen cross gV (THVar hv) vD acOut =
  substVarLemma cross gV hv vD acOut
substGen cross gV (THUnit iz) vD acOut =
  -- iz : IsZero ((Snoc gB av q) +++ cross) ==> gOut zero.
  let (izSnocGB, izc) = isZeroAppendSplit cross iz
      (izB, qEq)       = isZeroSnocInv izSnocGB
      acOut0 : (AddCtx gB (ctxScale Zero gV) gOut) := rewrite sym qEq in acOut
      izGout : (IsZero gOut)
             := addZeroResult izB (scaleZeroIsZero gV) acOut0
  in THUnit (isZeroAppendJoin izGout izc)
substGen cross gV (THLam q' a' bodyD) vD acOut =
  THLam q' a' (substGen (Snoc cross a' q') gV bodyD vD acOut)
substGen cross gV (THFst pD) vD acOut = THFst (substGen cross gV pD vD acOut)
substGen cross gV (THSnd pD) vD acOut = THSnd (substGen cross gV pD vD acOut)
substGen cross gV (THInl pD) vD acOut = THInl (substGen cross gV pD vD acOut)
substGen cross gV (THInr pD) vD acOut = THInr (substGen cross gV pD vD acOut)
substGen cross gV (THPair {t1} {t2} {a} {b} g1 g2 t1D t2D ac) vD acOut =
  -- SPLIT (multiplicative) pair. Structurally identical to the THCase
  -- scrutinee/branch split, minus the branch binders and with no
  -- scaling: the crossed binder's quantity `q = qAdd qa1 qa2` is
  -- shared out across the two components via `acSplit2` on the
  -- fused scaling `acScaleFuse qa1 qa2 gV`.
  let (g1B ** g2B ** g1C ** g2C ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let (at ** l1 ** r1 ** qa1 ** qa2 ** (eat, eg1B, eg2B, qEq, acBB)) = acSnocInv acB in
  let lenG1C : (ctxLen g1C = ctxLen cross) := acLenL acC
      lenG2C : (ctxLen g2C = ctxLen cross) := acLenR acC
      acOutQ : (AddCtx gB (ctxScale (qAdd qa1 qa2) gV) gOut) := rewrite sym qEq in acOut
      acFuse : (AddCtx (ctxScale qa1 gV) (ctxScale qa2 gV)
                       (ctxScale (qAdd qa1 qa2) gV))
             := acScaleFuse qa1 qa2 gV
  in
  let (gOutF ** gOutX ** (acOutF, acOutX, comb)) = acSplit2 acBB acFuse acOutQ in
  let vDat : (Has gV v at) := rewrite sym eat in vD
      e1'  : (g1 = (Snoc l1 at qa1) +++ g1C)
           := trans e1 (cong (\w => w +++ g1C) eg1B)
      e2'  : (g2 = (Snoc r1 at qa2) +++ g2C)
           := trans e2 (cong (\w => w +++ g2C) eg2B)
      t1D' : (Has ((Snoc l1 at qa1) +++ g1C) t1 a) := rewrite sym e1' in t1D
      t2D' : (Has ((Snoc r1 at qa2) +++ g2C) t2 b) := rewrite sym e2' in t2D
      t1W  : (Has (gOutF +++ g1C) (subst (ctxLen cross) v t1) a)
           := rewrite sym lenG1C in substGen g1C gV t1D' vDat acOutF
      t2W  : (Has (gOutX +++ g2C) (subst (ctxLen cross) v t2) b)
           := rewrite sym lenG2C in substGen g2C gV t2D' vDat acOutX
      newSplit : (AddCtx (gOutF +++ g1C) (gOutX +++ g2C) (gOut +++ cross))
               := acAppend comb acC
  in THPair (gOutF +++ g1C) (gOutX +++ g2C) t1W t2W newSplit
substGen cross gV (THCase {g2} {t} {tL} {tR} {c} ca cb g1 sD lD rD ac) vD acOut =
  let (g1B ** g2B ** g1C ** g2C ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let (at ** l1 ** r1 ** qa1 ** qa2 ** (eat, eg1B, eg2B, qEq, acBB)) = acSnocInv acB in
  let lenG1C : (ctxLen g1C = ctxLen cross) := acLenL acC
      lenG2C : (ctxLen g2C = ctxLen cross) := acLenR acC
      acOutQ : (AddCtx gB (ctxScale (qAdd qa1 qa2) gV) gOut) := rewrite sym qEq in acOut
      acFuse : (AddCtx (ctxScale qa1 gV) (ctxScale qa2 gV)
                       (ctxScale (qAdd qa1 qa2) gV))
             := acScaleFuse qa1 qa2 gV
  in
  let (gOutF ** gOutX ** (acOutF, acOutX, comb)) = acSplit2 acBB acFuse acOutQ in
  let vDat : (Has gV v at) := rewrite sym eat in vD
      e1'  : (g1 = (Snoc l1 at qa1) +++ g1C)
           := trans e1 (cong (\w => w +++ g1C) eg1B)
      e2'  : (g2 = (Snoc r1 at qa2) +++ g2C)
           := trans e2 (cong (\w => w +++ g2C) eg2B)
      sD'  : (Has ((Snoc l1 at qa1) +++ g1C) t (TSum ca cb)) := rewrite sym e1' in sD
      lD'  : (Has ((Snoc r1 at qa2) +++ (Snoc g2C ca One)) tL c)
           := rewrite sym e2' in lD
      rD'  : (Has ((Snoc r1 at qa2) +++ (Snoc g2C cb One)) tR c)
           := rewrite sym e2' in rD
      sW   : (Has (gOutF +++ g1C) (subst (ctxLen cross) v t) (TSum ca cb))
           := rewrite sym lenG1C in substGen g1C gV sD' vDat acOutF
      lW   : (Has (Snoc (gOutX +++ g2C) ca One) (subst (S (ctxLen cross)) v tL) c)
           := rewrite sym lenG2C in substGen (Snoc g2C ca One) gV lD' vDat acOutX
      rW   : (Has (Snoc (gOutX +++ g2C) cb One) (subst (S (ctxLen cross)) v tR) c)
           := rewrite sym lenG2C in substGen (Snoc g2C cb One) gV rD' vDat acOutX
      newSplit : (AddCtx (gOutF +++ g1C) (gOutX +++ g2C) (gOut +++ cross))
               := acAppend comb acC
  in THCase ca cb (gOutF +++ g1C) sW lW rW newSplit
substGen cross gV (THApp {g1} {a} {t1} {t2} q' g2' fD xD ac) vD acOut =
  let (g1B ** s2B ** g1C ** s2C ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let (at ** l1 ** s2BB ** qa1 ** qa2 ** (eat, eg1B, es2B, qEq, acBB)) = acSnocInv acB in
  let lenG1C : (ctxLen g1C = ctxLen cross) := acLenL acC
      lenS2C : (ctxLen s2C = ctxLen cross) := acLenR acC
      e2sc   : (ctxScale q' g2' = (Snoc s2BB at qa2) +++ s2C)
             := trans e2 (cong (\w => w +++ s2C) es2B)
  in
  let (g2BB ** qa2' ** g2C2 ** (g2eq, lenG2C2, sbbEq, qaEq, scEq))
        = unscaleSnocAppend q' g2' cross at lenS2C e2sc in
  let acOutQ : (AddCtx gB (ctxScale (qAdd qa1 qa2) gV) gOut) := rewrite sym qEq in acOut
      acFuse : (AddCtx (ctxScale qa1 gV) (ctxScale qa2 gV)
                       (ctxScale (qAdd qa1 qa2) gV))
             := acScaleFuse qa1 qa2 gV
  in
  let (gOutF ** gOutXS ** (acOutF, acOutXS, comb)) = acSplit2 acBB acFuse acOutQ in
  let -- build the unscaled argument output split via shape
      shGB_gV  : (ShapeEq gB gV)
               := shapeTrans (shapeSummands acOut) (shapeSym (shapeScale gV))
      shS2BB_gB : (ShapeEq s2BB gB)
                := shapeTrans (shapeSym (shapeSummands acBB)) (shapeLeftResult acBB)
      shG2BB_s2BB : (ShapeEq g2BB s2BB)
                  := rewrite sbbEq in shapeScale g2BB
      shG2BB_gV : (ShapeEq g2BB gV)
                := shapeTrans shG2BB_s2BB (shapeTrans shS2BB_gB shGB_gV)
      shWit : (ShapeEq g2BB (ctxScale qa2' gV))
            := shapeTrans shG2BB_gV (shapeScale gV)
  in
  let (gOutX ** acOutX) = mkAdd g2BB (ctxScale qa2' gV) shWit in
  let -- scale the argument output split and identify it with gOutXS
      acScaled0 : (AddCtx (ctxScale q' g2BB) (ctxScale q' (ctxScale qa2' gV))
                          (ctxScale q' gOutX))
                := acScale q' acOutX
      -- gOutXS = ctxScale q' gOutX, by determinism up to the summand
      -- equalities  s2BB = ctxScale q' g2BB  and
      -- ctxScale qa2 gV = ctxScale q' (ctxScale qa2' gV).
      midEq : (ctxScale qa2 gV = ctxScale q' (ctxScale qa2' gV))
            := trans (cong (\qq => ctxScale qq gV) qaEq)
                     (sym (scaleScale q' qa2' gV))
      gOutXSEq : (gOutXS = ctxScale q' gOutX)
               := addDetEq acOutXS acScaled0 sbbEq midEq
      comb' : (AddCtx gOutF (ctxScale q' gOutX) gOut) := rewrite sym gOutXSEq in comb
      vDat : (Has gV v at) := rewrite sym eat in vD
      e1'  : (g1 = (Snoc l1 at qa1) +++ g1C)
           := trans e1 (cong (\w => w +++ g1C) eg1B)
      fD'  : (Has ((Snoc l1 at qa1) +++ g1C) t1 (TArr q' a bt)) := rewrite sym e1' in fD
      xD'  : (Has ((Snoc g2BB at qa2') +++ g2C2) t2 a) := rewrite sym g2eq in xD
      fW   : (Has (gOutF +++ g1C) (subst (ctxLen cross) v t1) (TArr q' a bt))
           := rewrite sym lenG1C in substGen g1C gV fD' vDat acOutF
      xW   : (Has (gOutX +++ g2C2) (subst (ctxLen cross) v t2) a)
           := rewrite sym lenG2C2 in substGen g2C2 gV xD' vDat acOutX
      scaledOutEq : (ctxScale q' (gOutX +++ g2C2) = (ctxScale q' gOutX) +++ s2C)
                  := trans (scaleAppend q' gOutX g2C2)
                           (cong (\w => (ctxScale q' gOutX) +++ w) (sym scEq))
      newSplit : (AddCtx (gOutF +++ g1C)
                         (ctxScale q' (gOutX +++ g2C2))
                         (gOut +++ cross))
               := rewrite scaledOutEq in acAppend comb' acC
  in THApp q' (gOutX +++ g2C2) fW xW newSplit
substGen cross gV (THLet {g2} {t1} {t2} {b=bres} q' at g1' e1D e2D ac) vD acOut =
  let (s1B ** g2B ** s1C ** g2C ** (e1, e2, acB, acC)) = acAppendInv cross ac in
  let (at2 ** s1BB ** g2BB ** qa1 ** qa2 ** (eat, es1B, eg2B, qEq, acBB)) = acSnocInv acB in
  let lenS1C : (ctxLen s1C = ctxLen cross) := acLenL acC
      lenG2C : (ctxLen g2C = ctxLen cross) := acLenR acC
      e1sc   : (ctxScale q' g1' = (Snoc s1BB at2 qa1) +++ s1C)
             := trans e1 (cong (\w => w +++ s1C) es1B)
  in
  let (g1BB ** qa1' ** g1C ** (g1eq, lenG1C, s1bbEq, qa1Eq, s1cEq))
        = unscaleSnocAppend q' g1' cross at2 lenS1C e1sc in
  let acOutQ : (AddCtx gB (ctxScale (qAdd qa1 qa2) gV) gOut) := rewrite sym qEq in acOut
      acFuse : (AddCtx (ctxScale qa1 gV) (ctxScale qa2 gV)
                       (ctxScale (qAdd qa1 qa2) gV))
             := acScaleFuse qa1 qa2 gV
  in
  let (gOut1 ** gOut2 ** (acOut1S, acOut2, comb)) = acSplit2 acBB acFuse acOutQ in
  let -- unscaled RHS output split (acOut1S is over the scaled base s1BB)
      shGB_gV   : (ShapeEq gB gV)
                := shapeTrans (shapeSummands acOut) (shapeSym (shapeScale gV))
      shS1BB_gB : (ShapeEq s1BB gB)
                := shapeLeftResult acBB
      shG1BB_s1BB : (ShapeEq g1BB s1BB)
                  := rewrite s1bbEq in shapeScale g1BB
      shG1BB_gV : (ShapeEq g1BB gV)
                := shapeTrans shG1BB_s1BB (shapeTrans shS1BB_gB shGB_gV)
      shWit : (ShapeEq g1BB (ctxScale qa1' gV))
            := shapeTrans shG1BB_gV (shapeScale gV)
  in
  let (gOut1U ** acOut1U) = mkAdd g1BB (ctxScale qa1' gV) shWit in
  let acScaled0 : (AddCtx (ctxScale q' g1BB) (ctxScale q' (ctxScale qa1' gV))
                          (ctxScale q' gOut1U))
                := acScale q' acOut1U
      midEq : (ctxScale qa1 gV = ctxScale q' (ctxScale qa1' gV))
            := trans (cong (\qq => ctxScale qq gV) qa1Eq)
                     (sym (scaleScale q' qa1' gV))
      gOut1Eq : (gOut1 = ctxScale q' gOut1U)
              := addDetEq acOut1S acScaled0 s1bbEq midEq
      vDat : (Has gV v at2) := rewrite sym eat in vD
      e1D' : (Has ((Snoc g1BB at2 qa1') +++ g1C) t1 at) := rewrite sym g1eq in e1D
      eg2' : (g2 = (Snoc g2BB at2 qa2) +++ g2C)
           := trans e2 (cong (\w => w +++ g2C) eg2B)
      e2D' : (Has ((Snoc g2BB at2 qa2) +++ (Snoc g2C at q')) t2 bres)
           := rewrite sym (cong (\w => Snoc w at q') eg2') in e2D
      e1W  : (Has (gOut1U +++ g1C) (subst (ctxLen cross) v t1) at)
           := rewrite sym lenG1C in substGen g1C gV e1D' vDat acOut1U
      e2W  : (Has (Snoc (gOut2 +++ g2C) at q') (subst (S (ctxLen cross)) v t2) bres)
           := rewrite sym lenG2C in substGen (Snoc g2C at q') gV e2D' vDat acOut2
      scaledOutEq : (ctxScale q' (gOut1U +++ g1C) = gOut1 +++ s1C)
                  := trans (scaleAppend q' gOut1U g1C)
                           (trans (cong (\w => (ctxScale q' gOut1U) +++ w) (sym s1cEq))
                                  (cong (\w => w +++ s1C) (sym gOut1Eq)))
      newSplit : (AddCtx (ctxScale q' (gOut1U +++ g1C))
                         (gOut2 +++ g2C)
                         (gOut +++ cross))
               := rewrite scaledOutEq in acAppend comb acC
  in THLet q' at (gOut1U +++ g1C) e1W e2W newSplit

||| The top-level substitution lemma: `subst0 body v` (the redex
||| substitution `body[v/0]`).
public export
substLemma0 : {0 gB : Ctx} -> {0 av : Ty} -> {0 q : Q}
           -> {0 body : Tm} -> {0 bt : Ty} -> {0 gOut, v : _}
           -> (gV : Ctx)
           -> Has (Snoc gB av q) body bt
           -> Has gV v av
           -> AddCtx gB (ctxScale q gV) gOut
           -> Has gOut (subst0 body v) bt
substLemma0 gV bodyD vD acOut = substGen Empty gV bodyD vD acOut
