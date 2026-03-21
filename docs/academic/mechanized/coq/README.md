# AffineScript Coq Formalization

**Status**: Stub / Planned

This directory will contain the mechanized Coq proof development for AffineScript's metatheory.

## Planned Structure

```
coq/
├── Syntax.v           -- Abstract syntax definitions
├── Typing.v           -- Typing rules
├── Quantities.v       -- QTT semiring and quantity operations
├── Reduction.v        -- Small-step operational semantics
├── TypeSoundness.v    -- Progress and preservation
├── Effects.v          -- Effect typing and handling
├── Ownership.v        -- Ownership and borrowing
├── Rows.v             -- Row polymorphism
├── Dependent.v        -- Dependent types
├── Refinements.v      -- Refinement types (axiomatized)
├── Semantics.v        -- Denotational semantics
└── Adequacy.v         -- Adequacy theorem
```

## Dependencies

- Coq 8.17+
- MetaCoq (for reflection)
- stdpp (for common structures)
- iris (for concurrent separation logic, if needed)

## Building

```bash
# Install dependencies
opam install coq coq-stdpp

# Build all proofs
make -j4

# Check specific file
coqc -Q . AffineScript Typing.v
```

## TODO

### Phase 1: Core Type System

- [ ] Define syntax (terms, types, contexts)
- [ ] Define typing judgments
- [ ] Prove substitution lemmas
- [ ] Prove progress theorem
- [ ] Prove preservation theorem

### Phase 2: Quantitative Types

- [ ] Define quantity semiring
- [ ] Define context scaling and addition
- [ ] Prove quantity soundness (usage matches annotation)

### Phase 3: Effects

- [ ] Define effect signatures
- [ ] Define handler typing
- [ ] Prove effect safety

### Phase 4: Ownership

- [ ] Define ownership modalities
- [ ] Define borrow typing
- [ ] Prove memory safety properties

### Phase 5: Advanced Features

- [ ] Row polymorphism
- [ ] Dependent types (stratified)
- [ ] Refinement types (axiomatized SMT)

## Proof Approach

We use:
1. **Locally nameless** representation for binding
2. **Intrinsically-typed syntax** where practical
3. **Small-step semantics** for operational behavior
4. **Logical relations** for semantic properties

## Example Proof Structure

```coq
(* Syntax.v *)
Inductive ty : Type :=
  | TyUnit : ty
  | TyBool : ty
  | TyInt : ty
  | TyArrow : ty -> ty -> ty
  | TyForall : ty -> ty
  (* ... *)
.

Inductive expr : Type :=
  | EVar : nat -> expr
  | ELam : ty -> expr -> expr
  | EApp : expr -> expr -> expr
  (* ... *)
.

(* Typing.v *)
Inductive has_type : ctx -> expr -> ty -> Prop :=
  | T_Var : forall Γ x τ,
      lookup Γ x = Some τ ->
      has_type Γ (EVar x) τ
  | T_Lam : forall Γ τ₁ τ₂ e,
      has_type (τ₁ :: Γ) e τ₂ ->
      has_type Γ (ELam τ₁ e) (TyArrow τ₁ τ₂)
  | T_App : forall Γ e₁ e₂ τ₁ τ₂,
      has_type Γ e₁ (TyArrow τ₁ τ₂) ->
      has_type Γ e₂ τ₁ ->
      has_type Γ (EApp e₁ e₂) τ₂
  (* ... *)
.

(* TypeSoundness.v *)
Theorem progress : forall e τ,
  has_type nil e τ ->
  value e \/ exists e', step e e'.
Proof.
  intros e τ Hty.
  remember nil as Γ.
  induction Hty; subst.
  - (* Var *) discriminate.
  - (* Lam *) left. constructor.
  - (* App *)
    right.
    destruct IHHty1 as [Hval1 | [e1' Hstep1]]; auto.
    + (* e1 is a value *)
      destruct IHHty2 as [Hval2 | [e2' Hstep2]]; auto.
      * (* e2 is a value - beta reduction *)
        inversion Hval1; subst.
        exists (subst e2 e). constructor; auto.
      * (* e2 steps *)
        exists (EApp e1 e2'). constructor; auto.
    + (* e1 steps *)
      exists (EApp e1' e2). constructor; auto.
Qed.

Theorem preservation : forall Γ e e' τ,
  has_type Γ e τ ->
  step e e' ->
  has_type Γ e' τ.
Proof.
  intros Γ e e' τ Hty Hstep.
  generalize dependent e'.
  induction Hty; intros e' Hstep; inversion Hstep; subst.
  - (* Beta *)
    apply substitution_lemma; auto.
  - (* Cong-App-Left *)
    eapply T_App; eauto.
  - (* Cong-App-Right *)
    eapply T_App; eauto.
  (* ... *)
Qed.
```

## References

1. Software Foundations (Pierce et al.)
2. MetaCoq documentation
3. Iris Proof Mode documentation
4. RustBelt Coq development
