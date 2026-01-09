(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Type unification.

    This module implements unification for types, rows, and effects.
    It uses mutable references for efficient union-find style unification.
*)

open Types

(** Unification errors *)
type unify_error =
  | TypeMismatch of ty * ty
  | OccursCheck of tyvar * ty
  | RowMismatch of row * row
  | RowOccursCheck of rowvar * row
  | EffectMismatch of eff * eff
  | EffectOccursCheck of effvar * eff
  | KindMismatch of kind * kind
  | LabelNotFound of string * row
[@@deriving show]

type 'a result = ('a, unify_error) Result.t

(* Result bind operator *)
let ( let* ) = Result.bind

(** Check if two predicates are compatible (structural equality for now) *)
let rec predicates_compatible (p1 : predicate) (p2 : predicate) : bool =
  match (p1, p2) with
  | (PTrue, PTrue) -> true
  | (PFalse, PFalse) -> true
  | (PEq (a1, b1), PEq (a2, b2)) ->
    nat_eq a1 a2 && nat_eq b1 b2
  | (PLt (a1, b1), PLt (a2, b2)) ->
    nat_eq a1 a2 && nat_eq b1 b2
  | (PLe (a1, b1), PLe (a2, b2)) ->
    nat_eq a1 a2 && nat_eq b1 b2
  | (PGt (a1, b1), PGt (a2, b2)) ->
    nat_eq a1 a2 && nat_eq b1 b2
  | (PGe (a1, b1), PGe (a2, b2)) ->
    nat_eq a1 a2 && nat_eq b1 b2
  | (PAnd (p1a, p1b), PAnd (p2a, p2b)) ->
    predicates_compatible p1a p2a && predicates_compatible p1b p2b
  | (POr (p1a, p1b), POr (p2a, p2b)) ->
    predicates_compatible p1a p2a && predicates_compatible p1b p2b
  | (PNot p1', PNot p2') ->
    predicates_compatible p1' p2'
  | (PImpl (p1a, p1b), PImpl (p2a, p2b)) ->
    predicates_compatible p1a p2a && predicates_compatible p1b p2b
  | _ -> false

(** Check if a type variable occurs in a type (occurs check) *)
let rec occurs_in_ty (var : tyvar) (ty : ty) : bool =
  match repr ty with
  | TVar r ->
    begin match !r with
      | Unbound (v, _) -> v = var
      | Link _ -> failwith "occurs_in_ty: unexpected Link after repr"
    end
  | TCon _ -> false
  | TApp (t, args) ->
    occurs_in_ty var t || List.exists (occurs_in_ty var) args
  | TArrow (a, b, eff) ->
    occurs_in_ty var a || occurs_in_ty var b || occurs_in_eff var eff
  | TDepArrow (_, a, b, eff) ->
    occurs_in_ty var a || occurs_in_ty var b || occurs_in_eff var eff
  | TTuple tys ->
    List.exists (occurs_in_ty var) tys
  | TRecord row | TVariant row ->
    occurs_in_row var row
  | TForall (_, _, body) | TExists (_, _, body) ->
    occurs_in_ty var body
  | TRef t | TMut t | TOwn t ->
    occurs_in_ty var t
  | TRefined (t, _) ->
    occurs_in_ty var t
  | TNat _ -> false

and occurs_in_row (var : tyvar) (row : row) : bool =
  match repr_row row with
  | REmpty -> false
  | RExtend (_, ty, rest) ->
    occurs_in_ty var ty || occurs_in_row var rest
  | RVar _ -> false

and occurs_in_eff (var : tyvar) (e : eff) : bool =
  match repr_eff e with
  | EPure -> false
  | EVar _ -> false
  | ESingleton _ -> false
  | EUnion effs -> List.exists (occurs_in_eff var) effs

(** Check if a row variable occurs in a row *)
let rec rowvar_occurs_in_row (var : rowvar) (row : row) : bool =
  match repr_row row with
  | REmpty -> false
  | RExtend (_, _, rest) -> rowvar_occurs_in_row var rest
  | RVar r ->
    begin match !r with
      | RUnbound (v, _) -> v = var
      | RLink _ -> failwith "rowvar_occurs_in_row: unexpected Link"
    end

(** Check if an effect variable occurs in an effect *)
let rec effvar_occurs_in_eff (var : effvar) (e : eff) : bool =
  match repr_eff e with
  | EPure -> false
  | ESingleton _ -> false
  | EVar r ->
    begin match !r with
      | EUnbound (v, _) -> v = var
      | ELink _ -> failwith "effvar_occurs_in_eff: unexpected Link"
    end
  | EUnion effs -> List.exists (effvar_occurs_in_eff var) effs

(** Unify two types *)
let rec unify (t1 : ty) (t2 : ty) : unit result =
  let t1 = repr t1 in
  let t2 = repr t2 in
  match (t1, t2) with
  (* Same variable *)
  | (TVar r1, TVar r2) when r1 == r2 ->
    Ok ()

  (* Variable on left *)
  | (TVar r, t) ->
    begin match !r with
      | Unbound (var, _level) ->
        if occurs_in_ty var t then
          Error (OccursCheck (var, t))
        else begin
          r := Link t;
          Ok ()
        end
      | Link _ -> failwith "unify: unexpected Link after repr"
    end

  (* Variable on right *)
  | (t, TVar r) ->
    begin match !r with
      | Unbound (var, _level) ->
        if occurs_in_ty var t then
          Error (OccursCheck (var, t))
        else begin
          r := Link t;
          Ok ()
        end
      | Link _ -> failwith "unify: unexpected Link after repr"
    end

  (* Same constructor *)
  | (TCon c1, TCon c2) when c1 = c2 ->
    Ok ()

  (* Type application *)
  | (TApp (t1, args1), TApp (t2, args2))
    when List.length args1 = List.length args2 ->
    let* () = unify t1 t2 in
    unify_list args1 args2

  (* Arrow types *)
  | (TArrow (a1, b1, e1), TArrow (a2, b2, e2)) ->
    let* () = unify a1 a2 in
    let* () = unify b1 b2 in
    unify_eff e1 e2

  (* Dependent arrow types *)
  | (TDepArrow (x1, a1, b1, e1), TDepArrow (x2, a2, b2, e2)) ->
    (* Unify parameter types first *)
    let* () = unify a1 a2 in
    (* For return types, x1 and x2 are bound - they should unify if used consistently *)
    (* This is alpha-equivalence: (x: A) -> B[x] ≡ (y: A) -> B[y] *)
    let* () = unify b1 b2 in
    let _ = (x1, x2) in  (* Names are alpha-equivalent if bodies unify *)
    unify_eff e1 e2

  (* Tuple types *)
  | (TTuple ts1, TTuple ts2) when List.length ts1 = List.length ts2 ->
    unify_list ts1 ts2

  (* Record types *)
  | (TRecord r1, TRecord r2) ->
    unify_row r1 r2

  (* Variant types *)
  | (TVariant r1, TVariant r2) ->
    unify_row r1 r2

  (* Forall types - alpha-equivalence: bound variables are equivalent *)
  | (TForall (v1, k1, body1), TForall (v2, k2, body2)) ->
    if k1 <> k2 then
      Error (KindMismatch (k1, k2))
    else if v1 = v2 then
      (* Same variable name, directly unify bodies *)
      unify body1 body2
    else
      (* Different names: substitute v2 with v1 in body2 for alpha-equivalence *)
      let body2' = subst_ty v2 (TVar (ref (Unbound (v1, 0)))) body2 in
      unify body1 body2'

  (* Reference types *)
  | (TRef t1, TRef t2) -> unify t1 t2
  | (TMut t1, TMut t2) -> unify t1 t2
  | (TOwn t1, TOwn t2) -> unify t1 t2

  (* Refinement types *)
  | (TRefined (t1, p1), TRefined (t2, p2)) ->
    (* Unify base types and check predicate implication *)
    let* () = unify t1 t2 in
    (* For now, predicates unify if structurally equal after normalization *)
    (* Full SMT-based predicate checking would be Phase 3 *)
    if predicates_compatible p1 p2 then Ok ()
    else Error (TypeMismatch (TRefined (t1, p1), TRefined (t2, p2)))

  (* Type-level naturals *)
  | (TNat n1, TNat n2) ->
    (* Normalize and compare *)
    if nat_eq (normalize_nat n1) (normalize_nat n2) then Ok ()
    else Error (TypeMismatch (t1, t2))

  (* Mismatch *)
  | _ ->
    Error (TypeMismatch (t1, t2))

and unify_list (ts1 : ty list) (ts2 : ty list) : unit result =
  match (ts1, ts2) with
  | ([], []) -> Ok ()
  | (t1 :: rest1, t2 :: rest2) ->
    let* () = unify t1 t2 in
    unify_list rest1 rest2
  | _ -> failwith "unify_list: length mismatch"

(** Unify two rows *)
and unify_row (r1 : row) (r2 : row) : unit result =
  let r1 = repr_row r1 in
  let r2 = repr_row r2 in
  match (r1, r2) with
  (* Both empty *)
  | (REmpty, REmpty) -> Ok ()

  (* Same variable *)
  | (RVar rv1, RVar rv2) when rv1 == rv2 -> Ok ()

  (* Variable on left *)
  | (RVar r, row) ->
    begin match !r with
      | RUnbound (var, _level) ->
        if rowvar_occurs_in_row var row then
          Error (RowOccursCheck (var, row))
        else begin
          r := RLink row;
          Ok ()
        end
      | RLink _ -> failwith "unify_row: unexpected RLink"
    end

  (* Variable on right *)
  | (row, RVar r) ->
    begin match !r with
      | RUnbound (var, _level) ->
        if rowvar_occurs_in_row var row then
          Error (RowOccursCheck (var, row))
        else begin
          r := RLink row;
          Ok ()
        end
      | RLink _ -> failwith "unify_row: unexpected RLink"
    end

  (* Both extend with same label *)
  | (RExtend (l1, t1, rest1), RExtend (l2, t2, rest2)) when l1 = l2 ->
    let* () = unify t1 t2 in
    unify_row rest1 rest2

  (* Extend with different labels - row rewriting *)
  | (RExtend (l1, t1, rest1), RExtend (l2, t2, rest2)) ->
    (* l1 ≠ l2, so we need to find l1 in r2 and l2 in r1 *)
    (* Level 0 is appropriate here as unification creates monomorphic types *)
    let new_rest = fresh_rowvar 0 in
    let* () = unify_row rest1 (RExtend (l2, t2, new_rest)) in
    unify_row rest2 (RExtend (l1, t1, new_rest))

  (* Empty vs extend - error *)
  | (REmpty, RExtend (l, _, _)) ->
    Error (LabelNotFound (l, r1))
  | (RExtend (l, _, _), REmpty) ->
    Error (LabelNotFound (l, r2))

(** Unify two effects *)
and unify_eff (e1 : eff) (e2 : eff) : unit result =
  let e1 = repr_eff e1 in
  let e2 = repr_eff e2 in
  match (e1, e2) with
  (* Both pure *)
  | (EPure, EPure) -> Ok ()

  (* Same variable *)
  | (EVar r1, EVar r2) when r1 == r2 -> Ok ()

  (* Variable on left *)
  | (EVar r, eff) ->
    begin match !r with
      | EUnbound (var, _level) ->
        if effvar_occurs_in_eff var eff then
          Error (EffectOccursCheck (var, eff))
        else begin
          r := ELink eff;
          Ok ()
        end
      | ELink _ -> failwith "unify_eff: unexpected ELink"
    end

  (* Variable on right *)
  | (eff, EVar r) ->
    begin match !r with
      | EUnbound (var, _level) ->
        if effvar_occurs_in_eff var eff then
          Error (EffectOccursCheck (var, eff))
        else begin
          r := ELink eff;
          Ok ()
        end
      | ELink _ -> failwith "unify_eff: unexpected ELink"
    end

  (* Same singleton *)
  | (ESingleton e1, ESingleton e2) when e1 = e2 ->
    Ok ()

  (* Union vs union - set-based unification *)
  | (EUnion es1, EUnion es2) ->
    (* Effects are sets, so order doesn't matter *)
    (* Each effect in es1 must have a corresponding effect in es2 *)
    let rec find_and_unify (e : eff) (candidates : eff list) : (eff list, unify_error) Result.t =
      match candidates with
      | [] -> Error (EffectMismatch (e, EUnion es2))
      | c :: rest ->
        match unify_eff e c with
        | Ok () -> Ok rest
        | Error _ ->
          match find_and_unify e rest with
          | Ok remaining -> Ok (c :: remaining)
          | Error err -> Error err
    in
    let* remaining = List.fold_left (fun acc e ->
      let* candidates = acc in
      find_and_unify e candidates
    ) (Ok es2) es1 in
    (* All effects in es2 should be matched *)
    if remaining = [] then Ok ()
    else Error (EffectMismatch (e1, e2))

  (* Mismatch *)
  | _ ->
    Error (EffectMismatch (e1, e2))

(* Result bind operator *)
let ( let* ) = Result.bind

(* Phase 1 complete. Future enhancements (Phase 2+):
   - SMT-based predicate unification (Phase 3)
   - Better error messages with source locations (Phase 2)
   - Higher-order unification for type families (Phase 3)
*)
