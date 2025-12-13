(** Abstract Syntax Tree for AffineScript *)

open Sexplib0.Sexp_conv

(** Identifiers *)
type ident = {
  name : string;
  span : Span.t; [@sexp.opaque]
}
[@@deriving show, eq, sexp]

(** Quantity annotations for QTT *)
type quantity =
  | QZero    (** Erased - compile time only *)
  | QOne     (** Linear - exactly once *)
  | QOmega   (** Unrestricted *)
[@@deriving show, eq, sexp]

(** Ownership modifiers *)
type ownership =
  | Own  (** Owned value *)
  | Ref  (** Immutable borrow *)
  | Mut  (** Mutable borrow *)
[@@deriving show, eq, sexp]

(** Visibility modifiers *)
type visibility =
  | Private
  | Public
  | PubCrate
  | PubSuper
  | PubIn of ident list  (** pub(Path.To.Module) *)
[@@deriving show, eq, sexp]

(** Kinds *)
type kind =
  | KType                          (** Type *)
  | KNat                           (** Nat *)
  | KRow                           (** Row *)
  | KEffect                        (** Effect *)
  | KArrow of kind * kind          (** κ → κ *)
[@@deriving show, eq, sexp]

(** Type parameters *)
type type_param = {
  quantity : quantity option;
  name : ident;
  kind : kind option;
}
[@@deriving show, eq, sexp]

(** Nat-level expressions (for dependent types) *)
type nat_expr =
  | NatLit of int * Span.t [@sexp.opaque]
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
[@@deriving show, eq, sexp]

(** Type expressions *)
and type_expr =
  | TyVar of ident                                   (** Type variable *)
  | TyCon of ident                                   (** Type constructor *)
  | TyApp of ident * type_arg list                   (** Vec[n, T] *)
  | TyArrow of type_expr * type_expr * effect_expr option  (** T -> U / E *)
  | TyDepArrow of {                                  (** (x: T) -> U / E *)
      quantity : quantity option;
      param : ident;
      param_ty : type_expr;
      ret_ty : type_expr;
      effect : effect_expr option;
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
  field_name : ident;
  field_ty : type_expr;
}

(** Effect expressions *)
and effect_expr =
  | EffVar of ident                                  (** Effect variable *)
  | EffCon of ident * type_arg list                  (** IO, Exn[E], etc *)
  | EffUnion of effect_expr * effect_expr            (** E1 + E2 *)
[@@deriving show, eq, sexp]

(** Patterns *)
type pattern =
  | PatWildcard of Span.t [@sexp.opaque]             (** _ *)
  | PatVar of ident                                  (** x *)
  | PatLit of literal                                (** 42, "hello" *)
  | PatCon of ident * pattern list                   (** Some(x) *)
  | PatTuple of pattern list                         (** (a, b, c) *)
  | PatRecord of (ident * pattern option) list * bool  (** {x, y: p, ..} *)
  | PatOr of pattern * pattern                       (** p1 | p2 *)
  | PatAs of ident * pattern                         (** x @ p *)

(** Literals *)
and literal =
  | LitInt of int * Span.t [@sexp.opaque]
  | LitFloat of float * Span.t [@sexp.opaque]
  | LitBool of bool * Span.t [@sexp.opaque]
  | LitChar of char * Span.t [@sexp.opaque]
  | LitString of string * Span.t [@sexp.opaque]
  | LitUnit of Span.t [@sexp.opaque]
[@@deriving show, eq, sexp]

(** Expressions *)
type expr =
  | ExprLit of literal
  | ExprVar of ident
  | ExprLet of {
      mutable_ : bool;
      pattern : pattern;
      ty : type_expr option;
      value : expr;
      body : expr option;
    }
  | ExprIf of {
      cond : expr;
      then_ : expr;
      else_ : expr option;
    }
  | ExprMatch of {
      scrutinee : expr;
      arms : match_arm list;
    }
  | ExprLambda of {
      params : param list;
      ret_ty : type_expr option;
      body : expr;
    }
  | ExprApp of expr * expr list                      (** f(x, y) *)
  | ExprField of expr * ident                        (** e.field *)
  | ExprTupleIndex of expr * int                     (** e.0 *)
  | ExprIndex of expr * expr                         (** e[i] *)
  | ExprTuple of expr list                           (** (a, b, c) *)
  | ExprArray of expr list                           (** [a, b, c] *)
  | ExprRecord of {
      fields : (ident * expr option) list;           (** {x: 1, y} *)
      spread : expr option;                          (** ..base *)
    }
  | ExprRowRestrict of expr * ident                  (** e \ field *)
  | ExprBinary of expr * binary_op * expr
  | ExprUnary of unary_op * expr
  | ExprBlock of block
  | ExprReturn of expr option
  | ExprTry of {
      body : block;
      catch : match_arm list option;
      finally : block option;
    }
  | ExprHandle of {
      body : expr;
      handlers : handler_arm list;
    }
  | ExprResume of expr option
  | ExprUnsafe of unsafe_op list
  | ExprVariant of ident * ident                     (** Type::Variant *)
  | ExprSpan of expr * Span.t [@sexp.opaque]         (** Span wrapper *)

and match_arm = {
  pattern : pattern;
  guard : expr option;
  body : expr;
}

and handler_arm =
  | HandlerReturn of pattern * expr
  | HandlerOp of ident * pattern list * expr

and block = {
  stmts : stmt list;
  expr : expr option;
}

and stmt =
  | StmtLet of {
      mutable_ : bool;
      pattern : pattern;
      ty : type_expr option;
      value : expr;
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
[@@deriving show, eq, sexp]

(** Parameters *)
and param = {
  quantity : quantity option;
  ownership : ownership option;
  name : ident;
  ty : type_expr;
}
[@@deriving show, eq, sexp]

(** Trait bounds *)
type trait_bound = {
  trait_name : ident;
  args : type_arg list;
}
[@@deriving show, eq, sexp]

(** Where clause constraints *)
type constraint_ =
  | ConstraintPred of predicate
  | ConstraintTrait of ident * trait_bound list
[@@deriving show, eq, sexp]

(** Function declaration *)
type fn_decl = {
  visibility : visibility;
  total : bool;
  name : ident;
  type_params : type_param list;
  params : param list;
  ret_ty : type_expr option;
  effect : effect_expr option;
  where_clause : constraint_ list;
  body : fn_body;
}

and fn_body =
  | FnBlock of block
  | FnExpr of expr
[@@deriving show, eq, sexp]

(** Type declaration *)
type type_decl = {
  visibility : visibility;
  name : ident;
  type_params : type_param list;
  body : type_body;
}

and type_body =
  | TyAlias of type_expr
  | TyStruct of struct_field list
  | TyEnum of variant_decl list

and struct_field = {
  visibility : visibility;
  name : ident;
  ty : type_expr;
}

and variant_decl = {
  name : ident;
  fields : type_expr list;
  ret_ty : type_expr option;  (** GADT return type *)
}
[@@deriving show, eq, sexp]

(** Effect declaration *)
type effect_decl = {
  visibility : visibility;
  name : ident;
  type_params : type_param list;
  ops : effect_op_decl list;
}

and effect_op_decl = {
  name : ident;
  params : param list;
  ret_ty : type_expr option;
}
[@@deriving show, eq, sexp]

(** Trait declaration *)
type trait_decl = {
  visibility : visibility;
  name : ident;
  type_params : type_param list;
  super_traits : trait_bound list;
  items : trait_item list;
}

and trait_item =
  | TraitFn of fn_sig
  | TraitFnDefault of fn_decl
  | TraitType of {
      name : ident;
      kind : kind option;
      default : type_expr option;
    }

and fn_sig = {
  visibility : visibility;
  name : ident;
  type_params : type_param list;
  params : param list;
  ret_ty : type_expr option;
  effect : effect_expr option;
}
[@@deriving show, eq, sexp]

(** Impl block *)
type impl_block = {
  type_params : type_param list;
  trait_ref : trait_ref option;
  self_ty : type_expr;
  where_clause : constraint_ list;
  items : impl_item list;
}

and trait_ref = {
  trait_name : ident;
  args : type_arg list;
}

and impl_item =
  | ImplFn of fn_decl
  | ImplType of ident * type_expr
[@@deriving show, eq, sexp]

(** Module path *)
type module_path = ident list
[@@deriving show, eq, sexp]

(** Import declaration *)
type import_decl =
  | ImportSimple of module_path * ident option          (** use A.B as C *)
  | ImportList of module_path * import_item list        (** use A.B::{x, y} *)
  | ImportGlob of module_path                           (** use A.B::* *)

and import_item = {
  name : ident;
  alias : ident option;
}
[@@deriving show, eq, sexp]

(** Top-level declarations *)
type top_level =
  | TopFn of fn_decl
  | TopType of type_decl
  | TopEffect of effect_decl
  | TopTrait of trait_decl
  | TopImpl of impl_block
  | TopConst of {
      visibility : visibility;
      name : ident;
      ty : type_expr;
      value : expr;
    }
[@@deriving show, eq, sexp]

(** Complete program *)
type program = {
  module_path : module_path option;
  imports : import_decl list;
  decls : top_level list;
}
[@@deriving show, eq, sexp]
