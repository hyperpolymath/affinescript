(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) *)

(*
   Rows.v
   ══════
   P-11 — record-row soundness, Wave-W0.  **Mechanized, axiom-free.**

   Verified with Coq 8.18.0 (OCaml 4.14.1): `coqc -Q . ASFormal Rows.v` is
   clean, and the `Print Assumptions` reports at the foot of this file both
   say "Closed under the global context" — no axioms, no `Admitted`.  Wired
   into `formal/_CoqProject` and the `formal/justfile` `check` recipe (which
   fails if any proof gains an axiom/`Admitted`), exactly like its siblings.

   WHAT THIS PROVES (the soundness content of rows):
     Progress + preservation for the simply-typed lambda calculus extended with
     **extensible records** — empty record `{}`, extension `{ l = e | e' }`,
     selection `e.l`, and restriction `e \ l` — over a `lacks`-checked row
     (no duplicate labels).  The payload is canonical-forms for records: a
     well-typed selection of a present label never gets stuck.  This is the
     structural-typing analogue of the function canonical-forms lemma in
     `P2_Stlc.v`, and the seed for AffineScript's row-polymorphic records.

   RUNG LADDER (mirrors P2_Progress → P2_Stlc, K1 → K1Let, R0 → R1):
     * W0 (THIS FILE): monomorphic extensible records.  Rows are concrete
       lists of (label, type); the `lacks` side-condition keeps selection
       deterministic.  No row *variables* yet.
     * W1 (next): row **variables** + the `lacks`-predicate as a real judgment
       — i.e. true *row polymorphism* (`{name : String | ρ} → String`).  Needs
       type-level variables, a row-tail variable, and instantiation; that is
       the genuinely harder development and is deliberately deferred, exactly
       as `P2_Stlc.v` defers the QTT/affine context-splitting layer.

   Encoding choice: record *terms* use explicit `rempty`/`rext` constructors
   (so the standard `tm` induction principle covers record fields), while
   record *types* use `list (id * ty)` (so the row helpers are ordinary list
   fixpoints and no mutual-induction scheme is needed).  This is the
   Software-Foundations `Records.v` idiom.

   Funext-free, in the `P2_Stlc.v` style: contexts are compared only on a
   term's free variables (`context_invariance`), so NO `functional_extensionality`
   is used — hence `Print Assumptions` = "Closed under the global context".

   `.v` is Coq, not V-lang — see formal/README.adoc and .hypatia-ignore.
*)

Require Import PeanoNat.
Require Import List.
Import ListNotations.

Definition id := nat.

(* ── Syntax ─────────────────────────────────────────────────────────────── *)

Inductive ty : Type :=
| TUnit  : ty
| TArrow : ty -> ty -> ty
| TRec   : list (id * ty) -> ty.        (* a record type IS its row *)

Inductive tm : Type :=
| var    (x : id)
| app    (f a : tm)
| abs    (x : id) (A : ty) (b : tm)     (* A is the domain type *)
| tunit
| rempty                                 (* {} *)
| rext   (l : id) (e1 e2 : tm)           (* { l = e1 | e2 } *)
| rsel   (l : id) (e : tm)               (* e.l *)
| rres   (l : id) (e : tm).              (* e \ l   (restriction) *)

(* ── Row helpers (ordinary list fixpoints) ──────────────────────────────── *)

Fixpoint row_lookup (l : id) (r : list (id * ty)) : option ty :=
  match r with
  | []            => None
  | (l', A) :: r' => if Nat.eqb l l' then Some A else row_lookup l r'
  end.

Fixpoint row_remove (l : id) (r : list (id * ty)) : list (id * ty) :=
  match r with
  | []            => []
  | (l', A) :: r' => if Nat.eqb l l' then row_remove l r' else (l', A) :: row_remove l r'
  end.

Lemma row_lookup_head_eq : forall l A r, row_lookup l ((l, A) :: r) = Some A.
Proof. intros. simpl. rewrite Nat.eqb_refl. reflexivity. Qed.

Lemma row_lookup_head_neq : forall l l' A r,
  l <> l' -> row_lookup l ((l', A) :: r) = row_lookup l r.
Proof. intros l l' A r H. simpl. apply Nat.eqb_neq in H. rewrite H. reflexivity. Qed.

Lemma row_remove_head_eq : forall l A r, row_remove l ((l, A) :: r) = row_remove l r.
Proof. intros. simpl. rewrite Nat.eqb_refl. reflexivity. Qed.

Lemma row_remove_head_neq : forall l l' A r,
  l <> l' -> row_remove l ((l', A) :: r) = (l', A) :: row_remove l r.
Proof. intros l l' A r H. simpl. apply Nat.eqb_neq in H. rewrite H. reflexivity. Qed.

(* Removing a label that is absent is the identity. *)
Lemma row_remove_absent : forall l r, row_lookup l r = None -> row_remove l r = r.
Proof.
  induction r as [| [l' A] r' IH]; intros H.
  - reflexivity.
  - simpl in *. destruct (Nat.eqb l l') eqn:E.
    + discriminate H.
    + rewrite IH; [reflexivity | assumption].
Qed.

(* Restriction cannot introduce a label that was absent. *)
Lemma row_lookup_remove_none : forall l l' r,
  row_lookup l' r = None -> row_lookup l' (row_remove l r) = None.
Proof.
  induction r as [| [l0 A] r' IH]; intros H.
  - reflexivity.
  - simpl in H. destruct (Nat.eqb l' l0) eqn:E1.
    + discriminate H.
    + simpl. destruct (Nat.eqb l l0) eqn:E2.
      * apply IH; assumption.
      * simpl. rewrite E1. apply IH; assumption.
Qed.

(* ── Values ─────────────────────────────────────────────────────────────── *)

Inductive value : tm -> Prop :=
| v_abs    : forall x A b, value (abs x A b)
| v_unit   : value tunit
| v_rempty : value rempty
| v_rext   : forall l v1 v2, value v1 -> value v2 -> value (rext l v1 v2).

(* ── Substitution (closed-value convention, as in P2_Stlc.v) ────────────── *)

Fixpoint subst (x : id) (s t : tm) : tm :=
  match t with
  | var y        => if Nat.eqb x y then s else var y
  | abs y A b    => abs y A (if Nat.eqb x y then b else subst x s b)
  | app f a      => app (subst x s f) (subst x s a)
  | tunit        => tunit
  | rempty       => rempty
  | rext l e1 e2 => rext l (subst x s e1) (subst x s e2)
  | rsel l e     => rsel l (subst x s e)
  | rres l e     => rres l (subst x s e)
  end.

(* ── Small-step, call-by-value ──────────────────────────────────────────── *)

Inductive step : tm -> tm -> Prop :=
| ST_AppAbs    : forall x A b v, value v -> step (app (abs x A b) v) (subst x v b)
| ST_App1      : forall f f' a, step f f' -> step (app f a) (app f' a)
| ST_App2      : forall f a a', value f -> step a a' -> step (app f a) (app f a')
(* record construction evaluates left-to-right *)
| ST_Ext1      : forall l e1 e1' e2, step e1 e1' -> step (rext l e1 e2) (rext l e1' e2)
| ST_Ext2      : forall l v1 e2 e2', value v1 -> step e2 e2' -> step (rext l v1 e2) (rext l v1 e2')
(* selection: reduce the record, then project the chain *)
| ST_Sel       : forall l e e', step e e' -> step (rsel l e) (rsel l e')
| ST_SelExt_hit  : forall l v1 v2, value (rext l v1 v2) -> step (rsel l (rext l v1 v2)) v1
| ST_SelExt_miss : forall l l' v1 v2,
    l <> l' -> value (rext l' v1 v2) -> step (rsel l (rext l' v1 v2)) (rsel l v2)
(* restriction: reduce the record, then drop the label from the chain *)
| ST_Res       : forall l e e', step e e' -> step (rres l e) (rres l e')
| ST_ResEmpty  : forall l, step (rres l rempty) rempty
| ST_ResExt_hit  : forall l v1 v2, value (rext l v1 v2) -> step (rres l (rext l v1 v2)) v2
| ST_ResExt_miss : forall l l' v1 v2,
    l <> l' -> value (rext l' v1 v2) ->
    step (rres l (rext l' v1 v2)) (rext l' v1 (rres l v2)).

(* ── Typing ─────────────────────────────────────────────────────────────── *)

Definition context := id -> option ty.
Definition empty : context := fun _ => None.
Definition extend (G : context) (x : id) (T : ty) : context :=
  fun y => if Nat.eqb x y then Some T else G y.

Inductive has_type : context -> tm -> ty -> Prop :=
| T_Var    : forall G x T, G x = Some T -> has_type G (var x) T
| T_Abs    : forall G x A B b, has_type (extend G x A) b B -> has_type G (abs x A b) (TArrow A B)
| T_App    : forall G f a A B, has_type G f (TArrow A B) -> has_type G a A -> has_type G (app f a) B
| T_Unit   : forall G, has_type G tunit TUnit
| T_REmpty : forall G, has_type G rempty (TRec [])
| T_RExt   : forall G l e1 e2 A r,
    has_type G e1 A ->
    has_type G e2 (TRec r) ->
    row_lookup l r = None ->                    (* lacks l: no duplicate labels *)
    has_type G (rext l e1 e2) (TRec ((l, A) :: r))
| T_RSel   : forall G l e r A,
    has_type G e (TRec r) ->
    row_lookup l r = Some A ->
    has_type G (rsel l e) A
| T_RRes   : forall G l e r,
    has_type G e (TRec r) ->
    has_type G (rres l e) (TRec (row_remove l r)).

(* ── Free variables / free-in-context (the funext-free machinery) ───────── *)

Inductive afi : id -> tm -> Prop :=
| afi_var  : forall x, afi x (var x)
| afi_app1 : forall x f a, afi x f -> afi x (app f a)
| afi_app2 : forall x f a, afi x a -> afi x (app f a)
| afi_abs  : forall x y A b, y <> x -> afi x b -> afi x (abs y A b)
| afi_ext1 : forall x l e1 e2, afi x e1 -> afi x (rext l e1 e2)
| afi_ext2 : forall x l e1 e2, afi x e2 -> afi x (rext l e1 e2)
| afi_sel  : forall x l e, afi x e -> afi x (rsel l e)
| afi_res  : forall x l e, afi x e -> afi x (rres l e).

#[local] Hint Constructors afi : core.

Lemma free_in_context : forall x t S G,
  afi x t -> has_type G t S -> exists U, G x = Some U.
Proof.
  intros x t S G Hafi; generalize dependent S; generalize dependent G.
  induction Hafi; intros G S HT; inversion HT; subst.
  - eauto.
  - eapply IHHafi; eauto.
  - eapply IHHafi; eauto.
  - edestruct IHHafi as [U HU]; eauto.
    unfold extend in HU. destruct (Nat.eqb y x) eqn:E.
    + apply Nat.eqb_eq in E; subst y; exfalso; apply H; reflexivity.
    + eauto.
  - eapply IHHafi; eauto.
  - eapply IHHafi; eauto.
  - eapply IHHafi; eauto.
  - eapply IHHafi; eauto.
Qed.

Corollary typable_empty_closed : forall x t S,
  afi x t -> has_type empty t S -> False.
Proof.
  intros x t S Hafi HT.
  destruct (free_in_context x t S empty Hafi HT) as [U HU]; discriminate HU.
Qed.

Lemma context_invariance : forall G G' t S,
  has_type G t S ->
  (forall x, afi x t -> G x = G' x) ->
  has_type G' t S.
Proof.
  intros G G' t S HT; generalize dependent G'.
  induction HT; intros G' Hf.
  - apply T_Var. rewrite <- (Hf x (afi_var x)). assumption.
  - apply T_Abs. apply IHHT. intros z Hz. unfold extend.
    destruct (Nat.eqb x z) eqn:E; auto.
    apply Hf. apply Nat.eqb_neq in E. auto.
  - eapply T_App; [apply IHHT1 | apply IHHT2]; intros z Hz; apply Hf; auto.
  - apply T_Unit.
  - apply T_REmpty.
  - apply T_RExt; [ apply IHHT1; intros z Hz; apply Hf; apply afi_ext1; auto
                  | apply IHHT2; intros z Hz; apply Hf; apply afi_ext2; auto
                  | assumption ].
  - eapply T_RSel; [ apply IHHT; intros z Hz; apply Hf; apply afi_sel; auto
                   | eassumption ].
  - apply T_RRes. apply IHHT; intros z Hz; apply Hf; apply afi_res; auto.
Qed.

(* ── Substitution preserves typing ──────────────────────────────────────── *)

Lemma subst_preserves_typing : forall G x U t v S,
  has_type (extend G x U) t S ->
  has_type empty v U ->
  has_type G (subst x v t) S.
Proof.
  intros G x U t v S Ht Hv; generalize dependent S; generalize dependent G.
  induction t as
    [ y | f IHf a IHa | y A b IHb | | | l e1 IHe1 e2 IHe2 | l e IHe | l e IHe ];
    intros G S Ht; simpl; inversion Ht; subst.
  - (* var y *)
    unfold extend in H1. destruct (Nat.eqb x y) eqn:E.
    + injection H1 as H1. rewrite <- H1.
      apply (context_invariance empty G); [assumption |].
      intros z Hz. exfalso; apply (typable_empty_closed z v U Hz Hv).
    + apply T_Var; assumption.
  - (* app f a *)
    eapply T_App; [apply IHf | apply IHa]; eassumption.
  - (* abs y A b *)
    destruct (Nat.eqb x y) eqn:E.
    + apply Nat.eqb_eq in E; subst y.
      apply T_Abs.
      apply (context_invariance (extend (extend G x U) x A) (extend G x A));
        [assumption |].
      intros z Hz. unfold extend. destruct (Nat.eqb x z); reflexivity.
    + apply T_Abs. apply IHb.
      apply (context_invariance (extend (extend G x U) y A)
                                (extend (extend G y A) x U)); [assumption |].
      intros z Hz. unfold extend.
      destruct (Nat.eqb y z) eqn:E1; destruct (Nat.eqb x z) eqn:E2;
        try reflexivity.
      exfalso. apply Nat.eqb_neq in E. apply E.
      apply Nat.eqb_eq in E1; apply Nat.eqb_eq in E2.
      rewrite E2, <- E1; reflexivity.
  - (* tunit *) apply T_Unit.
  - (* rempty *) apply T_REmpty.
  - (* rext l e1 e2 *)
    apply T_RExt; [ apply IHe1; eassumption
                  | apply IHe2; eassumption
                  | assumption ].
  - (* rsel l e *)
    eapply T_RSel; [ apply IHe; eassumption | eassumption ].
  - (* rres l e *)
    apply T_RRes. apply IHe; eassumption.
Qed.

(* ── Canonical forms ────────────────────────────────────────────────────── *)

Lemma canon_arrow : forall v A B,
  value v -> has_type empty v (TArrow A B) -> exists x b, v = abs x A b.
Proof.
  intros v A B Hv HT; destruct Hv.
  - inversion HT; subst; eauto.
  - inversion HT.
  - inversion HT.
  - inversion HT.
Qed.

(* A record value is the empty record or an extension. *)
Lemma canon_rec : forall v r,
  value v -> has_type empty v (TRec r) ->
  v = rempty \/ (exists l v1 v2, v = rext l v1 v2).
Proof.
  intros v r Hv HT; destruct Hv.
  - inversion HT.
  - inversion HT.
  - left; reflexivity.
  - right; eauto.
Qed.

(* ── Progress ───────────────────────────────────────────────────────────── *)

Theorem progress : forall t S, has_type empty t S -> value t \/ exists t', step t t'.
Proof.
  intros t S HT; remember empty as G eqn:HG.
  induction HT.
  - subst G; discriminate H.
  - left; apply v_abs.
  - right; subst G.
    destruct IHHT1 as [Hvf | [f' Hf]]; [reflexivity | |].
    + destruct (canon_arrow _ _ _ Hvf HT1) as [x [b ->]].
      destruct IHHT2 as [Hva | [a' Ha]]; [reflexivity | |].
      * eexists; apply ST_AppAbs; assumption.
      * eexists; apply ST_App2; [apply v_abs | eassumption].
    + eexists; apply ST_App1; eassumption.
  - left; apply v_unit.
  - left; apply v_rempty.
  - (* T_RExt *)
    subst G.
    destruct IHHT1 as [Hv1 | [e1' H1]]; [reflexivity | |].
    + destruct IHHT2 as [Hv2 | [e2' H2]]; [reflexivity | |].
      * left; apply v_rext; assumption.
      * right; eexists; apply ST_Ext2; [assumption | eassumption].
    + right; eexists; apply ST_Ext1; eassumption.
  - (* T_RSel: has_type G e (TRec r), row_lookup l r = Some A *)
    right; subst G.
    destruct IHHT as [Hve | [e' He]]; [reflexivity | |].
    + destruct (canon_rec e r Hve ltac:(eassumption)) as [Hemp | [l0 [v1 [v2 Hext]]]].
      * subst e.
        match goal with Hr : has_type empty rempty (TRec _) |- _ =>
          inversion Hr; subst end.
        match goal with Hl : row_lookup _ _ = Some _ |- _ =>
          simpl in Hl; discriminate Hl end.
      * subst e. destruct (Nat.eqb l l0) eqn:E.
        -- apply Nat.eqb_eq in E; subst l0.
           eexists; apply ST_SelExt_hit; assumption.
        -- apply Nat.eqb_neq in E.
           eexists; apply ST_SelExt_miss; [exact E | assumption].
    + eexists; apply ST_Sel; eassumption.
  - (* T_RRes: has_type G e (TRec r) *)
    right; subst G.
    destruct IHHT as [Hve | [e' He]]; [reflexivity | |].
    + destruct (canon_rec e r Hve ltac:(eassumption)) as [Hemp | [l0 [v1 [v2 Hext]]]].
      * subst e. eexists; apply ST_ResEmpty.
      * subst e. destruct (Nat.eqb l l0) eqn:E.
        -- apply Nat.eqb_eq in E; subst l0.
           eexists; apply ST_ResExt_hit; assumption.
        -- apply Nat.eqb_neq in E.
           eexists; apply ST_ResExt_miss; [exact E | assumption].
    + eexists; apply ST_Res; eassumption.
Qed.

(* ── Preservation ───────────────────────────────────────────────────────── *)

Theorem preservation : forall t t' S,
  has_type empty t S -> step t t' -> has_type empty t' S.
Proof.
  intros t t' S HT Hstep; generalize dependent S.
  induction Hstep; intros S HT; inversion HT; subst.
  - (* ST_AppAbs *)
    match goal with H : has_type _ (abs _ _ _) _ |- _ => inversion H; subst end.
    eapply subst_preserves_typing; eassumption.
  - (* ST_App1 *) eapply T_App; [ apply IHHstep; eassumption | eassumption ].
  - (* ST_App2 *) eapply T_App; [ eassumption | apply IHHstep; eassumption ].
  - (* ST_Ext1 *)
    apply T_RExt; [ apply IHHstep; eassumption | eassumption | eassumption ].
  - (* ST_Ext2 *)
    apply T_RExt; [ eassumption | apply IHHstep; eassumption | eassumption ].
  - (* ST_Sel (congruence) *)
    eapply T_RSel; [ apply IHHstep; eassumption | eassumption ].
  - (* ST_SelExt_hit *)
    match goal with H : has_type empty (rext _ _ _) (TRec _) |- _ =>
      inversion H; subst end.
    match goal with Hl : row_lookup _ _ = Some _ |- _ =>
      rewrite row_lookup_head_eq in Hl; injection Hl as Hl; subst end.
    assumption.
  - (* ST_SelExt_miss *)
    match goal with Hr : has_type empty (rext _ _ _) (TRec _) |- _ =>
      inversion Hr; subst end.
    match goal with
    | Hl : row_lookup ?l ((?l',_) :: _) = Some _, Hn : ?l <> ?l' |- _ =>
        rewrite (row_lookup_head_neq l l' _ _ Hn) in Hl
    end.
    eapply T_RSel; eassumption.
  - (* ST_Res (congruence) *)
    apply T_RRes. apply IHHstep; eassumption.
  - (* ST_ResEmpty *)
    match goal with H : has_type empty rempty _ |- _ => inversion H; subst end.
    simpl. apply T_REmpty.
  - (* ST_ResExt_hit *)
    match goal with H : has_type empty (rext _ _ _) (TRec _) |- _ =>
      inversion H; subst end.
    rewrite row_remove_head_eq.
    match goal with Hn : row_lookup ?l ?r0 = None |- _ =>
      rewrite (row_remove_absent l r0 Hn) end.
    assumption.
  - (* ST_ResExt_miss *)
    match goal with Hr : has_type empty (rext _ _ _) (TRec _) |- _ =>
      inversion Hr; subst end.
    match goal with Hn : ?a <> ?b |- _ =>
      rewrite (row_remove_head_neq a b _ _ Hn) end.
    apply T_RExt.
    + assumption.
    + apply T_RRes. assumption.
    + match goal with Hlk : row_lookup _ _ = None |- _ =>
        apply row_lookup_remove_none; exact Hlk end.
Qed.

(* ── Stated obligation (local mirror of the Siblings_Stated.v pattern) ───── *)
(* The obligation shape is progress + preservation over the record calculus —
   structurally the same parametric Prop as `Siblings_Stated.P2_progress` /
   `P2_preservation`, restated here with record-specific naming so P-11's
   identity is self-documenting.  Kept local (not lifted into
   Siblings_Stated.v) precisely to avoid a near-duplicate of the P-2 section;
   the discharges below type-check the concrete record model against it. *)

Section Rows_Stated.
  Variable Tm Ty Ctx : Type.
  Variable Empty    : Ctx.
  Variable HasType  : Ctx -> Tm -> Ty -> Prop.
  Variable Value    : Tm -> Prop.
  Variable Step     : Tm -> Tm -> Prop.

  Definition Rows_progress : Prop :=
    forall t a, HasType Empty t a -> Value t \/ (exists t', Step t t').

  Definition Rows_preservation : Prop :=
    forall g t t' a, HasType g t a -> Step t t' -> HasType g t' a.
End Rows_Stated.

(* Discharge for the concrete model (closed-term, ctx := unit), exactly as
   P2_Stlc.v discharges P2_progress / P2_preservation. *)

Definition Rows_progress_discharged
  : Rows_progress tm ty unit tt
      (fun (_ : unit) (t : tm) (T : ty) => has_type empty t T) value step
  := progress.

Definition Rows_preservation_discharged
  : Rows_preservation tm ty unit
      (fun (_ : unit) (t : tm) (T : ty) => has_type empty t T) step
  := fun (_ : unit) t t' T HT Hs => preservation t t' T HT Hs.

Print Assumptions progress.
Print Assumptions preservation.
