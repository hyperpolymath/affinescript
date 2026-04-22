# Type Inference Algorithm Specification

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Complete specification `[IMPL-DEP: type-checker]`

## Abstract

This document specifies the complete type inference algorithm for AffineScript, covering bidirectional type checking, unification, constraint solving, and inference for quantities, effects, rows, and refinements.

## 1. Introduction

AffineScript's type inference combines:
1. Bidirectional type checking (local type inference)
2. Hindley-Milner style polymorphism
3. Row unification for records and effects
4. Quantity inference
5. Effect inference
6. SMT-based refinement checking

## 2. Algorithm Overview

### 2.1 High-Level Structure

```
infer(Γ, e) : (τ, ε, C)
```

Returns:
- τ: The inferred type
- ε: The inferred effect
- C: Constraint set to be solved

### 2.2 Phases

1. **Elaboration**: Parse to untyped AST
2. **Constraint generation**: Traverse AST, generate constraints
3. **Constraint solving**: Unify types, solve rows
4. **Quantity checking**: Verify usage patterns
5. **Effect inference**: Compute effect signatures
6. **Refinement checking**: Discharge to SMT

## 3. Bidirectional Type Checking

### 3.1 Judgments

```
Γ ⊢ e ⇒ τ    (synthesis: infer type of e)
Γ ⊢ e ⇐ τ    (checking: check e has type τ)
```

### 3.2 Algorithm

```ocaml
type result = (typ * effect * constraints)

(* Synthesis: infer type *)
let rec synth (ctx : context) (e : expr) : result =
  match e with
  | Var x ->
      let τ = lookup ctx x in
      (τ, Pure, [])

  | Lit l ->
      (type_of_literal l, Pure, [])

  | Lam (x, Some τ, body) ->
      let (σ, ε, c) = synth (extend ctx x τ) body in
      (Arrow (τ, σ, ε), Pure, c)

  | Lam (x, None, body) ->
      let α = fresh_tyvar () in
      let (σ, ε, c) = synth (extend ctx x α) body in
      (Arrow (α, σ, ε), Pure, c)

  | App (f, a) ->
      let (τ_f, ε_f, c_f) = synth ctx f in
      let α = fresh_tyvar () in
      let β = fresh_tyvar () in
      let ρ = fresh_effvar () in
      let c_arr = (τ_f, Arrow (α, β, ρ)) in
      let (ε_a, c_a) = check ctx a α in
      (β, union [ε_f; ε_a; ρ], c_f @ c_a @ [c_arr])

  | Let (x, e1, e2) ->
      let (τ1, ε1, c1) = synth ctx e1 in
      let σ = generalize ctx τ1 in
      let (τ2, ε2, c2) = synth (extend ctx x σ) e2 in
      (τ2, union [ε1; ε2], c1 @ c2)

  | Ann (e, τ) ->
      let (ε, c) = check ctx e τ in
      (τ, ε, c)

  | Record fields ->
      let (field_types, effs, cs) =
        List.fold_right (fun (l, e) (fts, es, cs) ->
          let (τ, ε, c) = synth ctx e in
          ((l, τ) :: fts, ε :: es, c @ cs)
        ) fields ([], [], [])
      in
      (TyRecord field_types, union effs, cs)

  | RecordProj (e, l) ->
      let (τ, ε, c) = synth ctx e in
      let α = fresh_tyvar () in
      let ρ = fresh_rowvar () in
      let c_row = (τ, TyRecord ((l, α) :: ρ)) in
      (α, ε, c @ [c_row])

  | Perform (op, arg) ->
      let (τ_arg, τ_ret, E) = lookup_operation op in
      let (ε, c) = check ctx arg τ_arg in
      (τ_ret, union [ε; EffSingleton E], c)

  | Handle (body, handler) ->
      let (τ_body, ε_body, c_body) = synth ctx body in
      let (E, handled_eff) = effect_of_handler handler in
      let (τ_result, c_handler) = check_handler ctx handler τ_body in
      let ε_remaining = subtract ε_body handled_eff in
      (τ_result, ε_remaining, c_body @ c_handler)

  | _ -> failwith "Cannot synthesize"

(* Checking: verify type *)
and check (ctx : context) (e : expr) (τ : typ) : (effect * constraints) =
  match (e, τ) with
  | (Lam (x, None, body), Arrow (τ1, τ2, ε)) ->
      let (ε', c) = check (extend ctx x τ1) body τ2 in
      (Pure, c @ [(ε', ε)])

  | (If (cond, then_, else_), τ) ->
      let (ε1, c1) = check ctx cond TyBool in
      let (ε2, c2) = check ctx then_ τ in
      let (ε3, c3) = check ctx else_ τ in
      (union [ε1; ε2; ε3], c1 @ c2 @ c3)

  | (Case (scrut, branches), τ) ->
      let (τ_scrut, ε_scrut, c_scrut) = synth ctx scrut in
      let (effs, cs) = List.split (
        List.map (check_branch ctx τ_scrut τ) branches
      ) in
      (union (ε_scrut :: effs), c_scrut @ List.concat cs)

  | (e, τ) ->
      (* Subsumption *)
      let (τ', ε, c) = synth ctx e in
      (ε, c @ [(τ', τ)])  (* generate constraint τ' <: τ *)
```

## 4. Constraint Solving

### 4.1 Constraint Types

```ocaml
type constraint =
  | Eq of typ * typ                    (* τ₁ = τ₂ *)
  | Sub of typ * typ                   (* τ₁ <: τ₂ *)
  | RowEq of row * row                 (* ρ₁ = ρ₂ *)
  | EffEq of effect * effect           (* ε₁ = ε₂ *)
  | QuantEq of quantity * quantity     (* π₁ = π₂ *)
  | Lacks of row * label               (* ρ lacks l *)
  | Valid of predicate                 (* ⊨ φ (SMT) *)
```

### 4.2 Unification

```ocaml
let rec unify (subst : substitution) (c : constraint) : substitution =
  match c with
  | Eq (TyVar α, τ) when not (occurs α τ) ->
      compose (singleton α τ) subst

  | Eq (τ, TyVar α) when not (occurs α τ) ->
      compose (singleton α τ) subst

  | Eq (Arrow (τ1, σ1, ε1), Arrow (τ2, σ2, ε2)) ->
      let s1 = unify subst (Eq (τ1, τ2)) in
      let s2 = unify s1 (Eq (apply s1 σ1, apply s1 σ2)) in
      unify s2 (EffEq (apply s2 ε1, apply s2 ε2))

  | Eq (TyRecord r1, TyRecord r2) ->
      unify_rows subst r1 r2

  | Eq (TyApp (c1, args1), TyApp (c2, args2)) when c1 = c2 ->
      List.fold_left2 (fun s a1 a2 ->
        unify s (Eq (apply s a1, apply s a2))
      ) subst args1 args2

  | Eq (τ1, τ2) when τ1 = τ2 ->
      subst

  | Eq (τ1, τ2) ->
      raise (UnificationError (τ1, τ2))

  | Sub (τ1, τ2) ->
      (* For now, subtyping degenerates to equality *)
      (* TODO: proper subtyping with refinements *)
      unify subst (Eq (τ1, τ2))

  | RowEq (r1, r2) ->
      unify_rows subst r1 r2

  | EffEq (ε1, ε2) ->
      unify_effects subst ε1 ε2

  | Lacks (r, l) ->
      check_lacks subst r l

  | Valid φ ->
      (* Defer to SMT *)
      if smt_check φ then subst
      else raise (RefinementError φ)
```

### 4.3 Row Unification

```ocaml
let rec unify_rows (subst : substitution) (r1 : row) (r2 : row) : substitution =
  match (apply_row subst r1, apply_row subst r2) with
  | (RowEmpty, RowEmpty) ->
      subst

  | (RowVar ρ, r) | (r, RowVar ρ) when not (row_occurs ρ r) ->
      compose (singleton_row ρ r) subst

  | (RowExtend (l1, τ1, r1'), RowExtend (l2, τ2, r2')) when l1 = l2 ->
      let s1 = unify subst (Eq (τ1, τ2)) in
      unify_rows s1 (apply_row s1 r1') (apply_row s1 r2')

  | (RowExtend (l1, τ1, r1'), RowExtend (l2, τ2, r2')) when l1 <> l2 ->
      (* Rewrite: find l1 in r2, l2 in r1 *)
      let ρ = fresh_rowvar () in
      let s1 = unify_rows subst r1' (RowExtend (l2, τ2, ρ)) in
      let s2 = unify_rows s1 r2' (RowExtend (l1, τ1, apply_row s1 ρ)) in
      s2

  | (RowEmpty, RowExtend _) | (RowExtend _, RowEmpty) ->
      raise (RowMismatch (r1, r2))
```

### 4.4 Effect Unification

```ocaml
let rec unify_effects (subst : substitution) (ε1 : effect) (ε2 : effect) : substitution =
  match (apply_eff subst ε1, apply_eff subst ε2) with
  | (EffPure, EffPure) -> subst
  | (EffVar ρ, ε) | (ε, EffVar ρ) when not (eff_occurs ρ ε) ->
      compose (singleton_eff ρ ε) subst
  | (EffUnion es1, EffUnion es2) ->
      (* Set-based unification *)
      unify_effect_sets subst es1 es2
  | _ ->
      raise (EffectMismatch (ε1, ε2))
```

## 5. Generalization

### 5.1 Let-Generalization

```ocaml
let generalize (ctx : context) (τ : typ) : scheme =
  let free_in_ctx = free_tyvars_ctx ctx in
  let free_in_type = free_tyvars τ in
  let generalizable = SetDiff free_in_type free_in_ctx in
  Forall (Set.elements generalizable, τ)
```

### 5.2 Instantiation

```ocaml
let instantiate (scheme : scheme) : typ =
  match scheme with
  | Forall (vars, τ) ->
      let fresh_vars = List.map (fun _ -> fresh_tyvar ()) vars in
      let subst = List.combine vars fresh_vars in
      apply_subst subst τ
```

## 6. Quantity Inference

### 6.1 Usage Analysis

```ocaml
type usage = Zero | One | Many

let rec analyze_usage (x : var) (e : expr) : usage =
  match e with
  | Var y -> if x = y then One else Zero
  | Lam (y, _, body) -> if x = y then Zero else analyze_usage x body
  | App (f, a) -> combine (analyze_usage x f) (analyze_usage x a)
  | Let (y, e1, e2) ->
      let u1 = analyze_usage x e1 in
      let u2 = if x = y then Zero else analyze_usage x e2 in
      combine u1 u2
  | _ -> fold_expr (combine) Zero (analyze_usage x) e

let combine u1 u2 =
  match (u1, u2) with
  | (Zero, u) | (u, Zero) -> u
  | _ -> Many
```

### 6.2 Quantity Constraints

```ocaml
let check_quantity (expected : quantity) (actual : usage) : bool =
  match (expected, actual) with
  | (QZero, Zero) -> true
  | (QOne, One) -> true
  | (QOne, Zero) -> true    (* Affine: can drop *)
  | (QOmega, _) -> true
  | _ -> false
```

## 7. Effect Inference

### 7.1 Effect Collection

```ocaml
let rec collect_effects (e : expr) : effect =
  match e with
  | Perform (op, _) -> EffSingleton (effect_of_op op)
  | Handle (body, handler) ->
      let ε_body = collect_effects body in
      let handled = handled_effects handler in
      EffSubtract ε_body handled
  | App (f, a) ->
      let ε_f = collect_effects f in
      let ε_a = collect_effects a in
      let ε_call = effect_of_call f in
      EffUnion [ε_f; ε_a; ε_call]
  | _ -> fold_effects EffUnion EffPure collect_effects e
```

## 8. Refinement Checking

### 8.1 VC Generation

```ocaml
let rec generate_vc (ctx : context) (e : expr) (τ : typ) : predicate list =
  match (e, τ) with
  | (_, TyRefine (base, φ)) ->
      let base_vcs = generate_vc ctx e base in
      let inst_φ = substitute_expr e φ in
      inst_φ :: base_vcs

  | (If (cond, then_, else_), τ) ->
      let then_ctx = assume ctx cond in
      let else_ctx = assume ctx (Not cond) in
      generate_vc then_ctx then_ τ @
      generate_vc else_ctx else_ τ

  | _ -> []
```

### 8.2 SMT Discharge

```ocaml
let check_refinements (vcs : predicate list) : unit =
  List.iter (fun vc ->
    let smt_query = translate_to_smt vc in
    match Smt.check smt_query with
    | Smt.Valid -> ()
    | Smt.Invalid model -> raise (RefinementViolation (vc, model))
    | Smt.Unknown -> raise (RefinementTimeout vc)
  ) vcs
```

## 9. Complete Algorithm

### 9.1 Main Entry Point

```ocaml
let type_check (program : program) : typed_program =
  (* Phase 1: Parse *)
  let ast = parse program in

  (* Phase 2: Constraint generation *)
  let (typed_ast, constraints) = elaborate empty_ctx ast in

  (* Phase 3: Constraint solving *)
  let subst = solve constraints in

  (* Phase 4: Apply substitution *)
  let resolved_ast = apply_subst_program subst typed_ast in

  (* Phase 5: Quantity checking *)
  check_quantities resolved_ast;

  (* Phase 6: Effect checking *)
  check_effects resolved_ast;

  (* Phase 7: Refinement checking *)
  let vcs = collect_vcs resolved_ast in
  check_refinements vcs;

  (* Phase 8: Borrow checking *)
  borrow_check resolved_ast;

  resolved_ast
```

## 10. Correctness

### 10.1 Soundness

**Theorem 10.1 (Inference Soundness)**: If `type_check(e) = τ` then `⊢ e : τ`.

### 10.2 Completeness

**Theorem 10.2 (Inference Completeness)**: If `⊢ e : τ` then `type_check(e)` succeeds with a type τ' such that τ is an instance of τ'.

### 10.3 Principal Types

**Theorem 10.3 (Principal Types)**: The algorithm computes principal types.

## 11. Complexity Analysis

| Phase | Complexity |
|-------|------------|
| Parsing | O(n) |
| Constraint generation | O(n) |
| Unification | O(n²) worst, O(n) typical |
| Quantity checking | O(n) |
| Effect inference | O(n) |
| Refinement checking | Depends on SMT |
| Borrow checking | O(n²) worst |

## 12. Implementation Notes

See `lib/` for OCaml implementation (pending).

```ocaml
(* lib/infer.mli *)
module Infer : sig
  val infer : Context.t -> Ast.expr -> (Ast.typ * Ast.effect) result
  val check : Context.t -> Ast.expr -> Ast.typ -> Ast.effect result
  val elaborate : Ast.program -> TypedAst.program result
end
```

## 13. References

1. Dunfield, J., & Krishnaswami, N. (2021). Bidirectional Typing. *ACM Computing Surveys*.
2. Pottier, F., & Rémy, D. (2005). The Essence of ML Type Inference. *ATTAPL*.
3. Vytiniotis, D., et al. (2011). OutsideIn(X): Modular Type Inference with Local Assumptions. *JFP*.

---

**Document Metadata**:
- Implementation: `lib/infer.ml` (pending)
- Dependencies: Parser, AST, SMT interface
