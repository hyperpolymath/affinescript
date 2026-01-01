(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Abstract Syntax Tree for AffineScript *)

(** Identifiers *)
type ident = {
  name : string;
  span : Span.t;
}
[@@deriving show, eq]

(** Quantity annotations for QTT *)
type quantity =
  | QZero    (** Erased - compile time only *)
  | QOne     (** Linear - exactly once *)
  | QOmega   (** Unrestricted *)
[@@deriving show, eq]

(** Ownership modifiers *)
type ownership =
  | Own  (** Owned value *)
  | Ref  (** Immutable borrow *)
  | Mut  (** Mutable borrow *)
[@@deriving show, eq]

(** Visibility modifiers *)
type visibility =
  | Private
  | Public
  | PubCrate
  | PubSuper
  | PubIn of ident list  (** pub(Path.To.Module) *)
[@@deriving show, eq]

(** Kinds *)
type kind =
  | KType                          (** Type *)
  | KNat                           (** Nat *)
  | KRow                           (** Row *)
  | KEffect                        (** Effect *)
  | KArrow of kind * kind          (** κ → κ *)
[@@deriving show, eq]

(** Type parameters *)
type type_param = {
  tp_quantity : quantity option;
  tp_name : ident;
  tp_kind : kind option;
}
[@@deriving show, eq]

(** Nat-level expressions (for dependent types) *)
type nat_expr =
  | NatLit of int * Span.t
  | NatVar of ident
  | NatAdd of nat_expr * nat_expr
  | NatSub of nat_expr * nat_expr
  | NatMul of nat_expr * nat_expr
  | NatLen of ident
  | NatSizeof of type_expr

(** Predicates for refinement types *)
and predicate =
  | PredCmp of nat_expr * cmp_op * nat_expr
  | PredNot of predicate
  | PredAnd of predicate * predicate
  | PredOr of predicate * predicate

and cmp_op = Lt | Le | Gt | Ge | Eq | Ne
[@@deriving show, eq]

(** Type expressions *)
and type_expr =
  | TyVar of ident                                   (** Type variable *)
  | TyCon of ident                                   (** Type constructor *)
  | TyApp of ident * type_arg list                   (** Vec[n, T] *)
  | TyArrow of type_expr * type_expr * effect_expr option  (** T -> U / E *)
  | TyDepArrow of {                                  (** (x: T) -> U / E *)
      da_quantity : quantity option;
      da_param : ident;
      da_param_ty : type_expr;
      da_ret_ty : type_expr;
      da_eff : effect_expr option;
    }
  | TyTuple of type_expr list                        (** (T, U, V) *)
  | TyRecord of row_field list * ident option        (** {x: T, ..r} *)
  | TyOwn of type_expr                               (** own T *)
  | TyRef of type_expr                               (** ref T *)
  | TyMut of type_expr                               (** mut T *)
  | TyRefined of type_expr * predicate               (** T where (P) *)
  | TyHole                                           (** _ - infer *)

and type_arg =
  | TyArg of type_expr
  | NatArg of nat_expr

and row_field = {
  rf_name : ident;
  rf_ty : type_expr;
}

(** Effect expressions *)
and effect_expr =
  | EffVar of ident                                  (** Effect variable *)
  | EffCon of ident * type_arg list                  (** IO, Exn[E], etc *)
  | EffUnion of effect_expr * effect_expr            (** E1 + E2 *)
[@@deriving show, eq]

(** Patterns *)
type pattern =
  | PatWildcard of Span.t                            (** _ *)
  | PatVar of ident                                  (** x *)
  | PatLit of literal                                (** 42, "hello" *)
  | PatCon of ident * pattern list                   (** Some(x) *)
  | PatTuple of pattern list                         (** (a, b, c) *)
  | PatRecord of (ident * pattern option) list * bool  (** {x, y: p, ..} *)
  | PatOr of pattern * pattern                       (** p1 | p2 *)
  | PatAs of ident * pattern                         (** x @ p *)

(** Literals *)
and literal =
  | LitInt of int * Span.t
  | LitFloat of float * Span.t
  | LitBool of bool * Span.t
  | LitChar of char * Span.t
  | LitString of string * Span.t
  | LitUnit of Span.t
[@@deriving show, eq]

(** Expressions *)
type expr =
  | ExprLit of literal
  | ExprVar of ident
  | ExprLet of {
      el_mut : bool;
      el_pat : pattern;
      el_ty : type_expr option;
      el_value : expr;
      el_body : expr option;
    }
  | ExprIf of {
      ei_cond : expr;
      ei_then : expr;
      ei_else : expr option;
    }
  | ExprMatch of {
      em_scrutinee : expr;
      em_arms : match_arm list;
    }
  | ExprLambda of {
      elam_params : param list;
      elam_ret_ty : type_expr option;
      elam_body : expr;
    }
  | ExprApp of expr * expr list                      (** f(x, y) *)
  | ExprField of expr * ident                        (** e.field *)
  | ExprTupleIndex of expr * int                     (** e.0 *)
  | ExprIndex of expr * expr                         (** e[i] *)
  | ExprTuple of expr list                           (** (a, b, c) *)
  | ExprArray of expr list                           (** [a, b, c] *)
  | ExprRecord of {
      er_fields : (ident * expr option) list;        (** {x: 1, y} *)
      er_spread : expr option;                       (** ..base *)
    }
  | ExprRowRestrict of expr * ident                  (** e \ field *)
  | ExprBinary of expr * binary_op * expr
  | ExprUnary of unary_op * expr
  | ExprBlock of block
  | ExprReturn of expr option
  | ExprTry of {
      et_body : block;
      et_catch : match_arm list option;
      et_finally : block option;
    }
  | ExprHandle of {
      eh_body : expr;
      eh_handlers : handler_arm list;
    }
  | ExprResume of expr option
  | ExprUnsafe of unsafe_op list
  | ExprVariant of ident * ident                     (** Type::Variant *)
  | ExprSpan of expr * Span.t                        (** Span wrapper *)

and match_arm = {
  ma_pat : pattern;
  ma_guard : expr option;
  ma_body : expr;
}

and handler_arm =
  | HandlerReturn of pattern * expr
  | HandlerOp of ident * pattern list * expr

and block = {
  blk_stmts : stmt list;
  blk_expr : expr option;
}

and stmt =
  | StmtLet of {
      sl_mut : bool;
      sl_pat : pattern;
      sl_ty : type_expr option;
      sl_value : expr;
    }
  | StmtExpr of expr
  | StmtAssign of expr * assign_op * expr
  | StmtWhile of expr * block
  | StmtFor of pattern * expr * block

and binary_op =
  | OpAdd | OpSub | OpMul | OpDiv | OpMod
  | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
  | OpAnd | OpOr
  | OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr

and unary_op =
  | OpNeg | OpNot | OpBitNot | OpRef | OpDeref

and assign_op =
  | AssignEq | AssignAdd | AssignSub | AssignMul | AssignDiv

and unsafe_op =
  | UnsafeRead of expr
  | UnsafeWrite of expr * expr
  | UnsafeOffset of expr * expr
  | UnsafeTransmute of type_expr * type_expr * expr
  | UnsafeForget of expr
  | UnsafeAssume of predicate
[@@deriving show, eq]

(** Parameters *)
and param = {
  p_quantity : quantity option;
  p_ownership : ownership option;
  p_name : ident;
  p_ty : type_expr;
}
[@@deriving show, eq]

(** Trait bounds *)
type trait_bound = {
  tb_name : ident;
  tb_args : type_arg list;
}
[@@deriving show, eq]

(** Where clause constraints *)
type constraint_ =
  | ConstraintPred of predicate
  | ConstraintTrait of ident * trait_bound list
[@@deriving show, eq]

(** Function declaration *)
type fn_decl = {
  fd_vis : visibility;
  fd_total : bool;
  fd_name : ident;
  fd_type_params : type_param list;
  fd_params : param list;
  fd_ret_ty : type_expr option;
  fd_eff : effect_expr option;
  fd_where : constraint_ list;
  fd_body : fn_body;
}

and fn_body =
  | FnBlock of block
  | FnExpr of expr
[@@deriving show, eq]

(** Type declaration *)
type type_decl = {
  td_vis : visibility;
  td_name : ident;
  td_type_params : type_param list;
  td_body : type_body;
}

and type_body =
  | TyAlias of type_expr
  | TyStruct of struct_field list
  | TyEnum of variant_decl list

and struct_field = {
  sf_vis : visibility;
  sf_name : ident;
  sf_ty : type_expr;
}

and variant_decl = {
  vd_name : ident;
  vd_fields : type_expr list;
  vd_ret_ty : type_expr option;  (** GADT return type *)
}
[@@deriving show, eq]

(** Effect declaration *)
type effect_decl = {
  ed_vis : visibility;
  ed_name : ident;
  ed_type_params : type_param list;
  ed_ops : effect_op_decl list;
}

and effect_op_decl = {
  eod_name : ident;
  eod_params : param list;
  eod_ret_ty : type_expr option;
}
[@@deriving show, eq]

(** Trait declaration *)
type trait_decl = {
  trd_vis : visibility;
  trd_name : ident;
  trd_type_params : type_param list;
  trd_super : trait_bound list;
  trd_items : trait_item list;
}

and trait_item =
  | TraitFn of fn_sig
  | TraitFnDefault of fn_decl
  | TraitType of {
      tt_name : ident;
      tt_kind : kind option;
      tt_default : type_expr option;
    }

and fn_sig = {
  fs_vis : visibility;
  fs_name : ident;
  fs_type_params : type_param list;
  fs_params : param list;
  fs_ret_ty : type_expr option;
  fs_eff : effect_expr option;
}
[@@deriving show, eq]

(** Impl block *)
type impl_block = {
  ib_type_params : type_param list;
  ib_trait_ref : trait_ref option;
  ib_self_ty : type_expr;
  ib_where : constraint_ list;
  ib_items : impl_item list;
}

and trait_ref = {
  tr_name : ident;
  tr_args : type_arg list;
}

and impl_item =
  | ImplFn of fn_decl
  | ImplType of ident * type_expr
[@@deriving show, eq]

(** Module path *)
type module_path = ident list
[@@deriving show, eq]

(** Import declaration *)
type import_decl =
  | ImportSimple of module_path * ident option          (** use A.B as C *)
  | ImportList of module_path * import_item list        (** use A.B::{x, y} *)
  | ImportGlob of module_path                           (** use A.B::* *)

and import_item = {
  ii_name : ident;
  ii_alias : ident option;
}
[@@deriving show, eq]

(** Top-level declarations *)
type top_level =
  | TopFn of fn_decl
  | TopType of type_decl
  | TopEffect of effect_decl
  | TopTrait of trait_decl
  | TopImpl of impl_block
  | TopConst of {
      tc_vis : visibility;
      tc_name : ident;
      tc_ty : type_expr;
      tc_value : expr;
    }
[@@deriving show, eq]

(** Complete program *)
type program = {
  prog_module : module_path option;
  prog_imports : import_decl list;
  prog_decls : top_level list;
}
[@@deriving show, eq]
