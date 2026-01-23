(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Constraint solving for dependent types.

    This module implements constraint solving for:
    - Type-level natural number arithmetic
    - Refinement predicates
    - Dependent function type checking
*)

open Types

(** Constraint on natural numbers *)
type nat_constraint =
  | NCEq of nat_expr * nat_expr      (** n = m *)
  | NCLt of nat_expr * nat_expr      (** n < m *)
  | NCLe of nat_expr * nat_expr      (** n <= m *)
[@@deriving show]

(** Constraint store *)
type constraint_store = {
  (** Natural number constraints *)
  nat_constraints : nat_constraint list;

  (** Predicate assumptions *)
  assumptions : predicate list;

  (** Variable substitutions *)
  nat_subst : (string * nat_expr) list;
}

(** Create empty constraint store *)
let empty_store : constraint_store = {
  nat_constraints = [];
  assumptions = [];
  nat_subst = [];
}

(** Add a nat constraint *)
let add_nat_constraint (c : nat_constraint) (store : constraint_store) : constraint_store =
  { store with nat_constraints = c :: store.nat_constraints }

(** Add an assumption *)
let add_assumption (p : predicate) (store : constraint_store) : constraint_store =
  { store with assumptions = p :: store.assumptions }

(** Apply substitution to nat expression *)
let rec apply_nat_subst (subst : (string * nat_expr) list) (n : nat_expr) : nat_expr =
  match n with
  | NLit _ -> n
  | NVar x ->
    begin match List.assoc_opt x subst with
      | Some n' -> n'
      | None -> n
    end
  | NAdd (a, b) -> NAdd (apply_nat_subst subst a, apply_nat_subst subst b)
  | NSub (a, b) -> NSub (apply_nat_subst subst a, apply_nat_subst subst b)
  | NMul (a, b) -> NMul (apply_nat_subst subst a, apply_nat_subst subst b)
  | NLen x ->
    begin match List.assoc_opt x subst with
      | Some (NLen y) -> NLen y
      | Some _ -> n  (* Can't substitute non-length *)
      | None -> n
    end

(** Normalize nat expression with substitution *)
let normalize_with_subst (subst : (string * nat_expr) list) (n : nat_expr) : nat_expr =
  normalize_nat (apply_nat_subst subst n)

(** Check if two nat expressions are equal under substitution *)
let nat_equal (subst : (string * nat_expr) list) (n1 : nat_expr) (n2 : nat_expr) : bool =
  let n1' = normalize_with_subst subst n1 in
  let n2' = normalize_with_subst subst n2 in
  n1' = n2'

(** Check if n1 < n2 under substitution *)
let rec nat_less_than (subst : (string * nat_expr) list) (n1 : nat_expr) (n2 : nat_expr) : bool option =
  let n1' = normalize_with_subst subst n1 in
  let n2' = normalize_with_subst subst n2 in
  match (n1', n2') with
  | (NLit a, NLit b) -> Some (a < b)
  | (NLit 0, NAdd (NLit b, _)) when b > 0 -> Some true
  | (NLit 0, NAdd (_, NLit b)) when b > 0 -> Some true
  | (NAdd (a, NLit n), b) when n > 0 ->
    (* a + n < b implies a < b - n *)
    nat_less_than subst a (NSub (b, NLit n))
  | _ -> None  (* Cannot decide *)

(** Check if n1 <= n2 under substitution *)
let nat_less_equal (subst : (string * nat_expr) list) (n1 : nat_expr) (n2 : nat_expr) : bool option =
  if nat_equal subst n1 n2 then Some true
  else nat_less_than subst n1 n2

(** Evaluate a predicate under assumptions *)
let rec eval_predicate (store : constraint_store) (p : predicate) : bool option =
  match p with
  | PTrue -> Some true
  | PFalse -> Some false

  | PEq (n1, n2) ->
    Some (nat_equal store.nat_subst n1 n2)

  | PLt (n1, n2) ->
    nat_less_than store.nat_subst n1 n2

  | PLe (n1, n2) ->
    nat_less_equal store.nat_subst n1 n2

  | PGt (n1, n2) ->
    nat_less_than store.nat_subst n2 n1

  | PGe (n1, n2) ->
    nat_less_equal store.nat_subst n2 n1

  | PAnd (p1, p2) ->
    begin match (eval_predicate store p1, eval_predicate store p2) with
      | (Some true, Some true) -> Some true
      | (Some false, _) | (_, Some false) -> Some false
      | _ -> None
    end

  | POr (p1, p2) ->
    begin match (eval_predicate store p1, eval_predicate store p2) with
      | (Some true, _) | (_, Some true) -> Some true
      | (Some false, Some false) -> Some false
      | _ -> None
    end

  | PNot p' ->
    begin match eval_predicate store p' with
      | Some true -> Some false
      | Some false -> Some true
      | None -> None
    end

  | PImpl (p1, p2) ->
    begin match (eval_predicate store p1, eval_predicate store p2) with
      | (Some false, _) -> Some true
      | (Some true, Some true) -> Some true
      | (Some true, Some false) -> Some false
      | _ -> None
    end

(** Check if predicate is entailed by assumptions *)
let entails (store : constraint_store) (goal : predicate) : bool =
  (* First try direct evaluation *)
  match eval_predicate store goal with
  | Some true -> true
  | Some false -> false
  | None ->
    (* Check if goal is in assumptions *)
    List.exists (fun p -> p = goal) store.assumptions

(** Solve a nat constraint *)
let solve_nat_constraint (store : constraint_store) (c : nat_constraint) : bool =
  match c with
  | NCEq (n1, n2) -> nat_equal store.nat_subst n1 n2
  | NCLt (n1, n2) ->
    begin match nat_less_than store.nat_subst n1 n2 with
      | Some b -> b
      | None -> false  (* Conservative: can't prove *)
    end
  | NCLe (n1, n2) ->
    begin match nat_less_equal store.nat_subst n1 n2 with
      | Some b -> b
      | None -> false
    end

(** Check all constraints in store *)
let check_constraints (store : constraint_store) : bool =
  List.for_all (solve_nat_constraint store) store.nat_constraints

(** Get free variables in nat expression *)
let rec nat_vars (n : nat_expr) : string list =
  match n with
  | NLit _ -> []
  | NVar x -> [x]
  | NAdd (a, b) | NSub (a, b) | NMul (a, b) ->
    nat_vars a @ nat_vars b
  | NLen x -> [x]

(** Unify two nat expressions, returning substitution *)
let rec unify_nat (n1 : nat_expr) (n2 : nat_expr) : (string * nat_expr) list option =
  let n1' = normalize_nat n1 in
  let n2' = normalize_nat n2 in
  match (n1', n2') with
  (* Both concrete - must be equal *)
  | (NLit a, NLit b) when a = b -> Some []
  | (NLit _, NLit _) -> None

  (* Variable on left *)
  | (NVar x, n) when not (List.mem x (nat_vars n)) ->
    Some [(x, n)]

  (* Variable on right *)
  | (n, NVar x) when not (List.mem x (nat_vars n)) ->
    Some [(x, n)]

  (* n + k = m + k *)
  | (NAdd (a, NLit k1), NAdd (b, NLit k2)) when k1 = k2 ->
    unify_nat a b

  (* Structural equality *)
  | _ when n1' = n2' -> Some []
  | _ -> None

(** Simplify predicate using store *)
let simplify_predicate (store : constraint_store) (p : predicate) : predicate =
  match eval_predicate store p with
  | Some true -> PTrue
  | Some false -> PFalse
  | None -> p

(** Generate constraints from dependent arrow application *)
let instantiate_dep_arrow
    (param_name : string)
    (param_val : nat_expr)
    (ret_ty : ty)
    : ty * (string * nat_expr) list =
  (* Substitute parameter in return type *)
  let subst = [(param_name, param_val)] in
  let rec subst_ty (ty : ty) : ty =
    match repr ty with
    | TVar _ | TCon _ -> ty
    | TApp (t, args) -> TApp (subst_ty t, List.map subst_ty args)
    | TArrow (a, b, e) -> TArrow (subst_ty a, subst_ty b, e)
    | TDepArrow (x, _, _, _) when x = param_name ->
      ty  (* Shadowed *)
    | TDepArrow (x, a, b, e) ->
      TDepArrow (x, subst_ty a, subst_ty b, e)
    | TTuple tys -> TTuple (List.map subst_ty tys)
    | TRecord row -> TRecord (subst_row row)
    | TVariant row -> TVariant (subst_row row)
    | TForall (v, k, body) -> TForall (v, k, subst_ty body)
    | TExists (v, k, body) -> TExists (v, k, subst_ty body)
    | TRef t -> TRef (subst_ty t)
    | TMut t -> TMut (subst_ty t)
    | TOwn t -> TOwn (subst_ty t)
    | TRefined (t, p) -> TRefined (subst_ty t, subst_pred p)
    | TNat n -> TNat (apply_nat_subst subst n)
  and subst_row (row : row) : row =
    match repr_row row with
    | REmpty -> REmpty
    | RExtend (l, ty, rest) -> RExtend (l, subst_ty ty, subst_row rest)
    | RVar _ -> row
  and subst_pred (p : predicate) : predicate =
    match p with
    | PTrue | PFalse -> p
    | PEq (n1, n2) -> PEq (apply_nat_subst subst n1, apply_nat_subst subst n2)
    | PLt (n1, n2) -> PLt (apply_nat_subst subst n1, apply_nat_subst subst n2)
    | PLe (n1, n2) -> PLe (apply_nat_subst subst n1, apply_nat_subst subst n2)
    | PGt (n1, n2) -> PGt (apply_nat_subst subst n1, apply_nat_subst subst n2)
    | PGe (n1, n2) -> PGe (apply_nat_subst subst n1, apply_nat_subst subst n2)
    | PAnd (p1, p2) -> PAnd (subst_pred p1, subst_pred p2)
    | POr (p1, p2) -> POr (subst_pred p1, subst_pred p2)
    | PNot p' -> PNot (subst_pred p')
    | PImpl (p1, p2) -> PImpl (subst_pred p1, subst_pred p2)
  in
  (subst_ty ret_ty, subst)

(** Pretty print constraint store for debugging *)
let pp_constraint_store (fmt : Format.formatter) (store : constraint_store) : unit =
  Format.fprintf fmt "@[<v>";
  Format.fprintf fmt "Nat constraints:@,";
  List.iter (fun c ->
    match c with
    | NCEq (n1, n2) -> Format.fprintf fmt "  %a = %a@," pp_nat n1 pp_nat n2
    | NCLt (n1, n2) -> Format.fprintf fmt "  %a < %a@," pp_nat n1 pp_nat n2
    | NCLe (n1, n2) -> Format.fprintf fmt "  %a <= %a@," pp_nat n1 pp_nat n2
  ) store.nat_constraints;
  Format.fprintf fmt "Assumptions:@,";
  List.iter (fun p ->
    Format.fprintf fmt "  %a@," pp_pred p
  ) store.assumptions;
  Format.fprintf fmt "Substitutions:@,";
  List.iter (fun (x, n) ->
    Format.fprintf fmt "  %s := %a@," x pp_nat n
  ) store.nat_subst;
  Format.fprintf fmt "@]"
