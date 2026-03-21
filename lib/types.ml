(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Internal type representation for type checking.

    This module defines the internal type representation used during
    type checking, separate from the AST types. It includes type variables
    with levels for let-generalization.
*)

(** Type variable identifier *)
type tyvar = int
[@@deriving show, eq, ord]

(** Row variable identifier *)
type rowvar = int
[@@deriving show, eq, ord]

(** Effect variable identifier *)
type effvar = int
[@@deriving show, eq, ord]

(** Quantity (for QTT) *)
type quantity =
  | QZero      (** 0 - erased at runtime *)
  | QOne       (** 1 - used exactly once *)
  | QOmega     (** ω - used arbitrarily *)
  | QVar of int (** Quantity variable *)
[@@deriving show, eq]

(** Kind *)
type kind =
  | KType                     (** Type kind *)
  | KNat                      (** Natural number kind *)
  | KRow                      (** Row kind *)
  | KEffect                   (** Effect kind *)
  | KArrow of kind * kind     (** Higher-order kind *)
[@@deriving show, eq]

(** Type representation *)
type ty =
  | TVar of tyvar_state ref          (** Type variable (mutable for unification) *)
  | TCon of string                   (** Type constructor (Int, Bool, etc.) *)
  | TApp of ty * ty list             (** Type application *)
  | TArrow of ty * ty * eff          (** Function type with effect *)
  | TDepArrow of string * ty * ty * eff     (** Dependent function type *)
  | TTuple of ty list                (** Tuple type *)
  | TRecord of row                   (** Record type *)
  | TVariant of row                  (** Variant type *)
  | TForall of tyvar * kind * ty     (** Universal quantification *)
  | TExists of tyvar * kind * ty     (** Existential quantification *)
  | TRef of ty                       (** Immutable reference *)
  | TMut of ty                       (** Mutable reference *)
  | TOwn of ty                       (** Owned type *)
  | TRefined of ty * predicate       (** Refinement type *)
  | TNat of nat_expr                 (** Type-level natural *)
[@@deriving show]

(** Type variable state (for unification) *)
and tyvar_state =
  | Unbound of tyvar * int           (** Unbound with level *)
  | Link of ty                       (** Linked to another type *)
[@@deriving show]

(** Row type *)
and row =
  | REmpty                           (** Empty row *)
  | RExtend of string * ty * row     (** Row extension *)
  | RVar of rowvar_state ref         (** Row variable *)
[@@deriving show]

and rowvar_state =
  | RUnbound of rowvar * int
  | RLink of row
[@@deriving show]

(** Effect type *)
and eff =
  | EPure                            (** No effects *)
  | EVar of effvar_state ref         (** Effect variable *)
  | ESingleton of string             (** Single effect *)
  | EUnion of eff list               (** Union of effects *)
[@@deriving show]

and effvar_state =
  | EUnbound of effvar * int
  | ELink of eff
[@@deriving show]

(** Type-level natural expression *)
and nat_expr =
  | NLit of int
  | NVar of string
  | NAdd of nat_expr * nat_expr
  | NSub of nat_expr * nat_expr
  | NMul of nat_expr * nat_expr
  | NLen of string
[@@deriving show]

(** Predicate for refinement types *)
and predicate =
  | PTrue
  | PFalse
  | PEq of nat_expr * nat_expr
  | PLt of nat_expr * nat_expr
  | PLe of nat_expr * nat_expr
  | PGt of nat_expr * nat_expr
  | PGe of nat_expr * nat_expr
  | PAnd of predicate * predicate
  | POr of predicate * predicate
  | PNot of predicate
  | PImpl of predicate * predicate
[@@deriving show]

(** Type scheme (polymorphic type) *)
type scheme = {
  sc_tyvars : (tyvar * kind) list;
  sc_effvars : effvar list;
  sc_rowvars : rowvar list;
  sc_body : ty;
}
[@@deriving show]

(** Fresh variable generation *)
let next_tyvar = ref 0
let next_rowvar = ref 0
let next_effvar = ref 0

let fresh_tyvar (level : int) : ty =
  let id = !next_tyvar in
  next_tyvar := id + 1;
  TVar (ref (Unbound (id, level)))

let fresh_rowvar (level : int) : row =
  let id = !next_rowvar in
  next_rowvar := id + 1;
  RVar (ref (RUnbound (id, level)))

let fresh_effvar (level : int) : eff =
  let id = !next_effvar in
  next_effvar := id + 1;
  EVar (ref (EUnbound (id, level)))

(** Reset all counters (for testing) *)
let reset () =
  next_tyvar := 0;
  next_rowvar := 0;
  next_effvar := 0

(** Primitive types *)
let ty_unit = TCon "Unit"
let ty_bool = TCon "Bool"
let ty_int = TCon "Int"
let ty_float = TCon "Float"
let ty_char = TCon "Char"
let ty_string = TCon "String"
let ty_never = TCon "Never"

(** Construct an arrow type *)
let arrow ?(eff = EPure) (a : ty) (b : ty) : ty =
  TArrow (a, b, eff)

(** Construct a tuple type *)
let tuple (tys : ty list) : ty =
  TTuple tys

(** Follow links in a type variable *)
let rec repr (ty : ty) : ty =
  match ty with
  | TVar r ->
    begin match !r with
      | Link ty' ->
        let ty'' = repr ty' in
        r := Link ty'';  (* Path compression *)
        ty''
      | Unbound _ -> ty
    end
  | _ -> ty

(** Follow links in a row *)
let rec repr_row (row : row) : row =
  match row with
  | RVar r ->
    begin match !r with
      | RLink row' ->
        let row'' = repr_row row' in
        r := RLink row'';
        row''
      | RUnbound _ -> row
    end
  | _ -> row

(** Follow links in an effect *)
let rec repr_eff (e : eff) : eff =
  match e with
  | EVar r ->
    begin match !r with
      | ELink e' ->
        let e'' = repr_eff e' in
        r := ELink e'';
        e''
      | EUnbound _ -> e
    end
  | _ -> e

(** Pretty printing for types *)

let rec pp_ty (fmt : Format.formatter) (ty : ty) : unit =
  match repr ty with
  | TVar r ->
    begin match !r with
      | Unbound (v, _) -> Format.fprintf fmt "'t%d" v
      | Link t -> pp_ty fmt t
    end
  | TCon c -> Format.fprintf fmt "%s" c
  | TApp (t, args) ->
    Format.fprintf fmt "%a[%a]" pp_ty t pp_ty_list args
  | TArrow (a, b, EPure) ->
    Format.fprintf fmt "(%a -> %a)" pp_ty a pp_ty b
  | TArrow (a, b, eff) ->
    Format.fprintf fmt "(%a -> %a / %a)" pp_ty a pp_ty b pp_eff eff
  | TDepArrow (x, a, b, EPure) ->
    Format.fprintf fmt "((%s: %a) -> %a)" x pp_ty a pp_ty b
  | TDepArrow (x, a, b, eff) ->
    Format.fprintf fmt "((%s: %a) -> %a / %a)" x pp_ty a pp_ty b pp_eff eff
  | TTuple tys ->
    Format.fprintf fmt "(%a)" pp_ty_tuple tys
  | TRecord row ->
    Format.fprintf fmt "{%a}" pp_row row
  | TVariant row ->
    Format.fprintf fmt "[%a]" pp_row row
  | TForall (v, k, body) ->
    Format.fprintf fmt "(forall 't%d: %a. %a)" v pp_kind k pp_ty body
  | TExists (v, k, body) ->
    Format.fprintf fmt "(exists 't%d: %a. %a)" v pp_kind k pp_ty body
  | TRef t -> Format.fprintf fmt "ref %a" pp_ty t
  | TMut t -> Format.fprintf fmt "mut %a" pp_ty t
  | TOwn t -> Format.fprintf fmt "own %a" pp_ty t
  | TRefined (t, p) ->
    Format.fprintf fmt "(%a where %a)" pp_ty t pp_pred p
  | TNat n -> pp_nat fmt n

and pp_ty_list (fmt : Format.formatter) (tys : ty list) : unit =
  Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ", ")
    pp_ty fmt tys

and pp_ty_tuple (fmt : Format.formatter) (tys : ty list) : unit =
  Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ", ")
    pp_ty fmt tys

and pp_row (fmt : Format.formatter) (row : row) : unit =
  match repr_row row with
  | REmpty -> ()
  | RExtend (l, ty, REmpty) ->
    Format.fprintf fmt "%s: %a" l pp_ty ty
  | RExtend (l, ty, rest) ->
    Format.fprintf fmt "%s: %a, %a" l pp_ty ty pp_row rest
  | RVar r ->
    begin match !r with
      | RUnbound (v, _) -> Format.fprintf fmt "..r%d" v
      | RLink row' -> pp_row fmt row'
    end

and pp_eff (fmt : Format.formatter) (e : eff) : unit =
  match repr_eff e with
  | EPure -> Format.fprintf fmt "Pure"
  | EVar r ->
    begin match !r with
      | EUnbound (v, _) -> Format.fprintf fmt "e%d" v
      | ELink e' -> pp_eff fmt e'
    end
  | ESingleton name -> Format.fprintf fmt "%s" name
  | EUnion effs ->
    Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f " + ")
      pp_eff fmt effs

and pp_kind (fmt : Format.formatter) (k : kind) : unit =
  match k with
  | KType -> Format.fprintf fmt "Type"
  | KNat -> Format.fprintf fmt "Nat"
  | KRow -> Format.fprintf fmt "Row"
  | KEffect -> Format.fprintf fmt "Effect"
  | KArrow (k1, k2) -> Format.fprintf fmt "(%a -> %a)" pp_kind k1 pp_kind k2

and pp_nat (fmt : Format.formatter) (n : nat_expr) : unit =
  match n with
  | NLit i -> Format.fprintf fmt "%d" i
  | NVar x -> Format.fprintf fmt "%s" x
  | NAdd (a, b) -> Format.fprintf fmt "(%a + %a)" pp_nat a pp_nat b
  | NSub (a, b) -> Format.fprintf fmt "(%a - %a)" pp_nat a pp_nat b
  | NMul (a, b) -> Format.fprintf fmt "(%a * %a)" pp_nat a pp_nat b
  | NLen x -> Format.fprintf fmt "len(%s)" x

and pp_pred (fmt : Format.formatter) (p : predicate) : unit =
  match p with
  | PTrue -> Format.fprintf fmt "true"
  | PFalse -> Format.fprintf fmt "false"
  | PEq (a, b) -> Format.fprintf fmt "%a == %a" pp_nat a pp_nat b
  | PLt (a, b) -> Format.fprintf fmt "%a < %a" pp_nat a pp_nat b
  | PLe (a, b) -> Format.fprintf fmt "%a <= %a" pp_nat a pp_nat b
  | PGt (a, b) -> Format.fprintf fmt "%a > %a" pp_nat a pp_nat b
  | PGe (a, b) -> Format.fprintf fmt "%a >= %a" pp_nat a pp_nat b
  | PAnd (p1, p2) -> Format.fprintf fmt "(%a && %a)" pp_pred p1 pp_pred p2
  | POr (p1, p2) -> Format.fprintf fmt "(%a || %a)" pp_pred p1 pp_pred p2
  | PNot p -> Format.fprintf fmt "!%a" pp_pred p
  | PImpl (p1, p2) -> Format.fprintf fmt "(%a => %a)" pp_pred p1 pp_pred p2

let ty_to_string (ty : ty) : string =
  Format.asprintf "%a" pp_ty ty

(** Type substitution: substitute type variable v with replacement in ty *)
let rec subst_ty (v : tyvar) (replacement : ty) (ty : ty) : ty =
  match repr ty with
  | TVar r ->
    begin match !r with
      | Unbound (v', _) when v' = v -> replacement
      | Unbound _ -> ty
      | Link t -> subst_ty v replacement t
    end
  | TCon _ -> ty
  | TApp (t, args) ->
    TApp (subst_ty v replacement t, List.map (subst_ty v replacement) args)
  | TArrow (a, b, eff) ->
    TArrow (subst_ty v replacement a, subst_ty v replacement b, eff)
  | TDepArrow (x, a, b, eff) ->
    TDepArrow (x, subst_ty v replacement a, subst_ty v replacement b, eff)
  | TTuple tys ->
    TTuple (List.map (subst_ty v replacement) tys)
  | TRecord row ->
    TRecord (subst_row v replacement row)
  | TVariant row ->
    TVariant (subst_row v replacement row)
  | TForall (v', _, _) when v' = v ->
    ty  (* Variable is shadowed *)
  | TForall (v', k, body) ->
    TForall (v', k, subst_ty v replacement body)
  | TExists (v', _, _) when v' = v ->
    ty  (* Variable is shadowed *)
  | TExists (v', k, body) ->
    TExists (v', k, subst_ty v replacement body)
  | TRef t -> TRef (subst_ty v replacement t)
  | TMut t -> TMut (subst_ty v replacement t)
  | TOwn t -> TOwn (subst_ty v replacement t)
  | TRefined (t, p) -> TRefined (subst_ty v replacement t, p)
  | TNat _ -> ty

and subst_row (v : tyvar) (replacement : ty) (row : row) : row =
  match repr_row row with
  | REmpty -> REmpty
  | RExtend (l, ty, rest) ->
    RExtend (l, subst_ty v replacement ty, subst_row v replacement rest)
  | RVar _ -> row

(** Free type variable collection *)
module TyVarSet = Set.Make(Int)

let rec free_tyvars (ty : ty) : TyVarSet.t =
  match repr ty with
  | TVar r ->
    begin match !r with
      | Unbound (v, _) -> TyVarSet.singleton v
      | Link t -> free_tyvars t
    end
  | TCon _ -> TyVarSet.empty
  | TApp (t, args) ->
    List.fold_left TyVarSet.union (free_tyvars t)
      (List.map free_tyvars args)
  | TArrow (a, b, _) ->
    TyVarSet.union (free_tyvars a) (free_tyvars b)
  | TDepArrow (_, a, b, _) ->
    TyVarSet.union (free_tyvars a) (free_tyvars b)
  | TTuple tys ->
    List.fold_left TyVarSet.union TyVarSet.empty (List.map free_tyvars tys)
  | TRecord row | TVariant row ->
    free_tyvars_row row
  | TForall (v, _, body) | TExists (v, _, body) ->
    TyVarSet.remove v (free_tyvars body)
  | TRef t | TMut t | TOwn t ->
    free_tyvars t
  | TRefined (t, _) -> free_tyvars t
  | TNat _ -> TyVarSet.empty

and free_tyvars_row (row : row) : TyVarSet.t =
  match repr_row row with
  | REmpty -> TyVarSet.empty
  | RExtend (_, ty, rest) ->
    TyVarSet.union (free_tyvars ty) (free_tyvars_row rest)
  | RVar _ -> TyVarSet.empty

(** Check if a type variable occurs in a type (for occurs check) *)
let occurs (v : tyvar) (ty : ty) : bool =
  TyVarSet.mem v (free_tyvars ty)

(** Normalize type-level natural expressions *)
let rec normalize_nat (n : nat_expr) : nat_expr =
  match n with
  | NLit _ | NVar _ | NLen _ -> n
  | NAdd (a, b) ->
    begin match (normalize_nat a, normalize_nat b) with
      | (NLit x, NLit y) -> NLit (x + y)
      | (NLit 0, b') -> b'
      | (a', NLit 0) -> a'
      | (a', b') -> NAdd (a', b')
    end
  | NSub (a, b) ->
    begin match (normalize_nat a, normalize_nat b) with
      | (NLit x, NLit y) -> NLit (max 0 (x - y))
      | (a', NLit 0) -> a'
      | (a', b') when a' = b' -> NLit 0
      | (a', b') -> NSub (a', b')
    end
  | NMul (a, b) ->
    begin match (normalize_nat a, normalize_nat b) with
      | (NLit x, NLit y) -> NLit (x * y)
      | (NLit 0, _) | (_, NLit 0) -> NLit 0
      | (NLit 1, b') -> b'
      | (a', NLit 1) -> a'
      | (a', b') -> NMul (a', b')
    end

(** Check if two normalized nat expressions are equal *)
let nat_eq (n1 : nat_expr) (n2 : nat_expr) : bool =
  normalize_nat n1 = normalize_nat n2
