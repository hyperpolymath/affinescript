# Type Checker

The AffineScript type checker implements bidirectional type inference with support for dependent types, row polymorphism, and effects.

## Overview

**File**: `lib/typecheck.ml` (planned)
**Algorithm**: Bidirectional type checking with constraint solving

## Architecture

```
┌─────────────────────────────────────────────────┐
│                Type Checker                      │
├─────────────────────────────────────────────────┤
│                                                  │
│  ┌─────────────┐  ┌─────────────┐               │
│  │Kind Checker │  │ Unifier     │               │
│  └──────┬──────┘  └──────┬──────┘               │
│         │                │                       │
│         ▼                ▼                       │
│  ┌────────────────────────────┐                 │
│  │  Bidirectional Checker     │                 │
│  │  ├── synth (infer)         │                 │
│  │  └── check (verify)        │                 │
│  └─────────────┬──────────────┘                 │
│                │                                 │
│         ┌──────┴──────┐                         │
│         ▼             ▼                         │
│  ┌───────────┐  ┌───────────┐                   │
│  │ Quantity  │  │  Effect   │                   │
│  │ Checker   │  │ Inferencer│                   │
│  └───────────┘  └───────────┘                   │
│                                                  │
└─────────────────────────────────────────────────┘
```

## Type Representation

```ocaml
type typ =
  (* Base types *)
  | T_Int | T_Float | T_Bool | T_Char | T_String | T_Unit | T_Never

  (* Type constructors *)
  | T_Named of path                    (* Named type *)
  | T_App of typ * typ list            (* Type application *)
  | T_Var of int                       (* Type variable (de Bruijn) *)
  | T_Meta of meta ref                 (* Unification variable *)

  (* Composite types *)
  | T_Tuple of typ list                (* (A, B, C) *)
  | T_Record of row                    (* {x: A, y: B, ..r} *)
  | T_Variant of row                   (* [A | B | ..r] *)

  (* Function types *)
  | T_Arrow of typ * typ * effects     (* A -{E}-> B *)
  | T_Forall of string * kind * typ    (* forall a. T *)
  | T_Pi of string * typ * typ         (* (x: A) -> B *)

  (* Ownership *)
  | T_Own of typ                       (* own T *)
  | T_Ref of typ                       (* &T *)
  | T_MutRef of typ                    (* &mut T *)

  (* Advanced *)
  | T_Refined of typ * expr            (* T where P *)
  | T_Erased of typ                    (* 0 T (compile-time only) *)
  | T_Linear of typ                    (* 1 T (used exactly once) *)

and row =
  | Row_Empty
  | Row_Extend of string * typ * row
  | Row_Var of int

and effects =
  | Eff_Pure
  | Eff_Row of effect_row

and meta =
  | Unsolved of int * kind
  | Solved of typ
```

## Bidirectional Type Checking

### Core Judgments

```
Γ ⊢ e ⇒ A    (synthesis/inference)
Γ ⊢ e ⇐ A    (checking)
```

### Implementation

```ocaml
(* Synthesis: infer type from expression *)
let rec synth (ctx : context) (expr : expr) : typ * typed_expr =
  match expr.kind with
  | E_Lit (Lit_Int n) ->
      (T_Int, { kind = TE_Lit (Lit_Int n); typ = T_Int; span = expr.span })

  | E_Lit (Lit_Float f) ->
      (T_Float, { kind = TE_Lit (Lit_Float f); typ = T_Float; span = expr.span })

  | E_Lit (Lit_String s) ->
      (T_String, { kind = TE_Lit (Lit_String s); typ = T_String; span = expr.span })

  | E_Var name ->
      let typ = Context.lookup ctx name in
      (typ, { kind = TE_Var name; typ; span = expr.span })

  | E_App (fn, args) ->
      let (fn_typ, fn_te) = synth ctx fn in
      synth_app ctx fn_te fn_typ args expr.span

  | E_Field (record, field) ->
      let (rec_typ, rec_te) = synth ctx record in
      synth_field ctx rec_te rec_typ field expr.span

  | E_Binary (e1, op, e2) ->
      let (t1, te1) = synth ctx e1 in
      let (t2, te2) = synth ctx e2 in
      let result_typ = check_binop op t1 t2 in
      (result_typ, { kind = TE_Binary (te1, op, te2); typ = result_typ; span = expr.span })

  | E_Annot (e, typ) ->
      let te = check ctx e typ in
      (typ, te)

  | E_Lambda _ ->
      error expr.span "Cannot infer type of lambda; add annotation"

  | _ ->
      (* More cases... *)

(* Checking: verify expression has expected type *)
and check (ctx : context) (expr : expr) (expected : typ) : typed_expr =
  match (expr.kind, expected) with
  | (E_Lambda (params, body), T_Arrow (arg_ty, ret_ty, eff)) ->
      let ctx' = Context.add_params ctx params arg_ty in
      let body_te = check ctx' body ret_ty in
      { kind = TE_Lambda (params, body_te);
        typ = expected;
        span = expr.span }

  | (E_If (cond, then_, else_), _) ->
      let cond_te = check ctx cond T_Bool in
      let then_te = check ctx then_ expected in
      let else_te = check ctx else_ expected in
      { kind = TE_If (cond_te, then_te, else_te);
        typ = expected;
        span = expr.span }

  | (E_Match (scrutinee, arms), _) ->
      let (scrut_typ, scrut_te) = synth ctx scrutinee in
      let arms_te = List.map (check_arm ctx scrut_typ expected) arms in
      { kind = TE_Match (scrut_te, arms_te);
        typ = expected;
        span = expr.span }

  | _ ->
      (* Fall back to synthesis + subsumption *)
      let (inferred, te) = synth ctx expr in
      subsume ctx inferred expected;
      te
```

## Unification

```ocaml
let rec unify (ctx : context) (t1 : typ) (t2 : typ) : unit =
  match (t1, t2) with
  | (T_Meta { contents = Solved t1' }, _) -> unify ctx t1' t2
  | (_, T_Meta { contents = Solved t2' }) -> unify ctx t1 t2'

  | (T_Meta ({ contents = Unsolved (id1, k1) } as r1),
     T_Meta ({ contents = Unsolved (id2, k2) })) when id1 = id2 ->
      ()  (* Same variable *)

  | (T_Meta ({ contents = Unsolved (id, k) } as r), t)
  | (t, T_Meta ({ contents = Unsolved (id, k) } as r)) ->
      occurs_check id t;
      kind_check ctx t k;
      r := Solved t

  | (T_Int, T_Int) | (T_Float, T_Float) | (T_Bool, T_Bool)
  | (T_String, T_String) | (T_Unit, T_Unit) ->
      ()

  | (T_Arrow (a1, r1, e1), T_Arrow (a2, r2, e2)) ->
      unify ctx a1 a2;
      unify ctx r1 r2;
      unify_effects ctx e1 e2

  | (T_Tuple ts1, T_Tuple ts2) when List.length ts1 = List.length ts2 ->
      List.iter2 (unify ctx) ts1 ts2

  | (T_Record row1, T_Record row2) ->
      unify_rows ctx row1 row2

  | (T_App (f1, args1), T_App (f2, args2)) ->
      unify ctx f1 f2;
      List.iter2 (unify ctx) args1 args2

  | _ ->
      type_error (Type_mismatch (t1, t2))
```

## Row Unification

```ocaml
let rec unify_rows (ctx : context) (r1 : row) (r2 : row) : unit =
  match (r1, r2) with
  | (Row_Empty, Row_Empty) -> ()

  | (Row_Var v1, Row_Var v2) when v1 = v2 -> ()

  | (Row_Extend (l1, t1, r1'), Row_Extend (l2, t2, r2')) when l1 = l2 ->
      unify ctx t1 t2;
      unify_rows ctx r1' r2'

  | (Row_Extend (l1, t1, r1'), r2) ->
      (* Rewrite r2 to match structure of r1 *)
      let (t2, r2') = row_extract l1 r2 in
      unify ctx t1 t2;
      unify_rows ctx r1' r2'

  | (Row_Var v, Row_Empty) ->
      solve_row_var v Row_Empty

  | (Row_Var v, Row_Extend (l, t, r)) ->
      (* v ~ {l: t, ..r'} where r' is fresh *)
      let r' = fresh_row_var () in
      solve_row_var v (Row_Extend (l, t, r'));
      unify_rows ctx r' r

  | _ ->
      type_error Row_mismatch
```

## Kind Checking

```ocaml
type kind =
  | K_Type                  (* * - types of values *)
  | K_Row                   (* Row - row kinds *)
  | K_Effect                (* Effect - effect kinds *)
  | K_Arrow of kind * kind  (* k1 -> k2 *)
  | K_Nat                   (* Natural numbers for dependent types *)

let rec kind_of (ctx : context) (typ : typ) : kind =
  match typ with
  | T_Int | T_Float | T_Bool | T_Char | T_String | T_Unit | T_Never ->
      K_Type

  | T_Named path ->
      Context.lookup_type_kind ctx path

  | T_App (f, args) ->
      let k = kind_of ctx f in
      kind_apply ctx k args

  | T_Arrow (_, _, _) | T_Forall (_, _, _) | T_Pi (_, _, _) ->
      K_Type

  | T_Record row | T_Variant row ->
      check_row_kind ctx row;
      K_Type

  | T_Meta { contents = Unsolved (_, k) } -> k
  | T_Meta { contents = Solved t } -> kind_of ctx t

  | _ -> K_Type
```

## Effect Inference

```ocaml
let rec infer_effects (ctx : context) (expr : expr) : effects =
  match expr.kind with
  | E_Lit _ | E_Var _ ->
      Eff_Pure

  | E_App (fn, args) ->
      let fn_eff = infer_effects ctx fn in
      let args_eff = List.map (infer_effects ctx) args |> merge_effects in
      let call_eff = effect_of_call ctx fn in
      merge_effects [fn_eff; args_eff; call_eff]

  | E_Binary (e1, _, e2) ->
      merge_effects [infer_effects ctx e1; infer_effects ctx e2]

  | E_If (cond, then_, else_) ->
      merge_effects [
        infer_effects ctx cond;
        infer_effects ctx then_;
        infer_effects ctx else_
      ]

  | E_Handle (body, handlers) ->
      let body_eff = infer_effects ctx body in
      let handled = effects_handled_by handlers in
      effect_subtract body_eff handled

  | _ -> Eff_Pure
```

## Quantity Checking

```ocaml
type quantity = Zero | One | Many

let rec check_quantity (ctx : context) (expr : typed_expr) : unit =
  match expr.typ with
  | T_Linear inner ->
      let uses = count_uses ctx expr in
      if uses <> 1 then
        error expr.span (Linear_use_error (uses, 1))

  | T_Own inner ->
      let uses = count_uses ctx expr in
      if uses > 1 then
        error expr.span (Affine_use_error uses)

  | T_Erased _ ->
      let uses = count_runtime_uses ctx expr in
      if uses > 0 then
        error expr.span Erased_at_runtime

  | _ -> ()
```

## Dependent Type Support

```ocaml
(* Type-level evaluation for dependent types *)
let rec eval_type (ctx : context) (typ : typ) : typ =
  match typ with
  | T_App (T_Named ["Vec"], [n; t]) ->
      (* Vec[n, T] - n should be a Nat *)
      let n' = eval_nat ctx n in
      T_App (T_Named ["Vec"], [n'; eval_type ctx t])

  | T_Pi (x, a, b) ->
      T_Pi (x, eval_type ctx a, b)  (* Don't eval b yet *)

  | T_Refined (t, pred) ->
      T_Refined (eval_type ctx t, pred)

  | _ -> typ

(* Check refinement predicates via SMT *)
let check_refinement (ctx : context) (value : expr) (pred : expr) : unit =
  let smt_ctx = translate_context ctx in
  let smt_pred = translate_pred ctx value pred in
  match Smt.check_sat smt_ctx (Smt.Not smt_pred) with
  | Smt.Unsat -> ()  (* Refinement holds *)
  | Smt.Sat model ->
      error value.span (Refinement_violated (pred, model))
  | Smt.Unknown ->
      warn value.span (Refinement_unknown pred)
```

## Error Messages

```ocaml
let format_type_error (err : type_error) : diagnostic =
  match err with
  | Type_mismatch (expected, actual) ->
      {
        code = E0300;
        message = sprintf "type mismatch";
        labels = [
          (actual.span, sprintf "expected `%s`, found `%s`"
            (show_type expected) (show_type actual.typ))
        ];
        notes = [];
        help = Some (suggest_fix expected actual);
      }

  | Linear_use_error (actual, expected) ->
      {
        code = E0500;
        message = sprintf "linear type used %d times, expected %d" actual expected;
        labels = [(* usage sites *)];
        notes = ["linear types must be used exactly once"];
        help = None;
      }

  | Row_field_missing (field, row) ->
      {
        code = E0350;
        message = sprintf "record missing field `%s`" field;
        (* ... *)
      }
```

---

## See Also

- [Architecture](architecture.md) - Compiler overview
- [Parser](parser.md) - Previous phase
- [Borrow Checker](borrow-checker.md) - Next phase
- [Types](../language-reference/types.md) - Type system reference
