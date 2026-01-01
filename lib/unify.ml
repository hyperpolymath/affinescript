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
  | EffectMismatch of effect * effect
  | EffectOccursCheck of effvar * effect
  | KindMismatch of kind * kind
  | LabelNotFound of string * row
[@@deriving show]

type 'a result = ('a, unify_error) Result.t

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

and occurs_in_eff (var : tyvar) (eff : effect) : bool =
  match repr_eff eff with
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
let rec effvar_occurs_in_eff (var : effvar) (eff : effect) : bool =
  match repr_eff eff with
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
  | (TDepArrow (_, a1, b1, e1), TDepArrow (_, a2, b2, e2)) ->
    (* TODO: Handle the binding properly *)
    let* () = unify a1 a2 in
    let* () = unify b1 b2 in
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

  (* Forall types *)
  | (TForall (v1, k1, body1), TForall (v2, k2, body2)) ->
    if k1 <> k2 then
      Error (KindMismatch (k1, k2))
    else
      (* TODO: Alpha-equivalence *)
      unify body1 body2

  (* Reference types *)
  | (TRef t1, TRef t2) -> unify t1 t2
  | (TMut t1, TMut t2) -> unify t1 t2
  | (TOwn t1, TOwn t2) -> unify t1 t2

  (* Refinement types *)
  | (TRefined (t1, _p1), TRefined (t2, _p2)) ->
    (* TODO: Unify predicates via SMT *)
    unify t1 t2

  (* Type-level naturals *)
  | (TNat n1, TNat n2) ->
    (* TODO: Normalize and compare *)
    if n1 = n2 then Ok ()
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
    let level = 0 in (* TODO: Get proper level *)
    let new_rest = fresh_rowvar level in
    let* () = unify_row rest1 (RExtend (l2, t2, new_rest)) in
    unify_row rest2 (RExtend (l1, t1, new_rest))

  (* Empty vs extend - error *)
  | (REmpty, RExtend (l, _, _)) ->
    Error (LabelNotFound (l, r1))
  | (RExtend (l, _, _), REmpty) ->
    Error (LabelNotFound (l, r2))

(** Unify two effects *)
and unify_eff (e1 : effect) (e2 : effect) : unit result =
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

  (* Union vs union *)
  | (EUnion es1, EUnion es2) ->
    (* TODO: Proper set-based unification *)
    if List.length es1 = List.length es2 then
      List.fold_left2 (fun acc e1 e2 ->
        match acc with
        | Error e -> Error e
        | Ok () -> unify_eff e1 e2
      ) (Ok ()) es1 es2
    else
      Error (EffectMismatch (e1, e2))

  (* Mismatch *)
  | _ ->
    Error (EffectMismatch (e1, e2))

(* Result bind operator *)
let ( let* ) = Result.bind

(* TODO: Phase 1 implementation
   - [ ] Level-based generalization
   - [ ] Proper handling of dependent types
   - [ ] Effect row set-based unification
   - [ ] Better error messages with source locations
*)
