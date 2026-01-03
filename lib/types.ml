(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
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

(* TODO: Phase 1 implementation
   - [ ] Pretty printing for types
   - [ ] Type substitution
   - [ ] Free variable collection
   - [ ] Occurs check helpers
   - [ ] Type normalization for dependent types
*)
