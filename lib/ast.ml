(* SPDX-License-Identifier: MPL-2.0 *)
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

(** Type expressions *)
type type_expr =
  | TyVar of ident                                   (** Type variable *)
  | TyCon of ident                                   (** Type constructor *)
  | TyApp of ident * type_arg list                   (** Vec[n, T] *)
  | TyArrow of type_expr * quantity option * type_expr * effect_expr option  (** T -{q}-> U / E *)
  | TyTuple of type_expr list                        (** (T, U, V) *)
  | TyRecord of row_field list * ident option        (** {x: T, ..r} *)
  | TyOwn of type_expr                               (** own T *)
  | TyRef of type_expr                               (** ref T *)
  | TyMut of type_expr                               (** mut T *)
  | TyHole                                           (** _ - infer *)

and type_arg =
  | TyArg of type_expr

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
      el_quantity : quantity option;
        (** QTT binder quantity, per ADR-002 / ADR-007.
            None means: defaults to QOmega (unrestricted), the
            unannotated case. The quantity scales the value context
            in the typing rule q·Γ₁ + Γ₂ ⊢ let x :^q = e1 in e2.
            Surface syntaxes that populate this:
            - @linear / @erased / @unrestricted (Option C, primary)
            - :1 / :0 / :ω                       (Option B, sugar) *)
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
  | ExprStringConcat of expr * expr
      (** String concatenation, `a ++ b` where both sides are `String`.
          Not produced by the parser: it is introduced by a post-typecheck
          *elaboration* (see {!Typecheck.elaborate_string_concat}) that
          rewrites the String case of the polymorphic `++` (`ExprBinary (_,
          OpConcat, _)`) into this node, so the wasm backend can lower it as
          byte concatenation rather than the list-element copy used for array
          `++`. The interpreter and non-wasm backends treat it as ordinary
          string concatenation. String-wall slice 8b. *)
  | ExprStringEq of expr * expr * bool
      (** String (dis)equality, `a == b` / `a != b` where both sides are
          `String`. The [bool] is [true] for `!=` (the negated form). Like
          {!ExprStringConcat}, this is not produced by the parser: it is
          introduced by the post-typecheck elaboration
          (see {!Typecheck.elaborate_string_concat}) that rewrites the String
          case of polymorphic `==`/`!=` (`ExprBinary (_, (OpEq | OpNe), _)`)
          into this node, so the wasm backend can lower it as a
          length-prefixed byte comparison rather than the [I32Eq] pointer
          comparison correct only for Int/Bool. The interpreter and non-wasm
          backends treat it as ordinary string (dis)equality. String-wall
          slice 9. *)
  | ExprStringRel of expr * expr * binary_op
      (** String relational comparison, `a < b` / `a <= b` / `a > b` /
          `a >= b` where both sides are `String`. The [binary_op] is always
          one of [OpLt] / [OpLe] / [OpGt] / [OpGe]. Like {!ExprStringEq}, this
          is not produced by the parser: it is introduced by the post-typecheck
          elaboration (see {!Typecheck.elaborate_string_concat}) that rewrites
          the String case of the relational operators, so the wasm backend can
          lower it as a byte-wise *lexicographic* comparison rather than the
          signed-integer compare on the two `[len][utf8]` pointers (which is
          meaningless). The interpreter and non-wasm backends treat it as an
          ordinary string comparison. String-wall slice 10 (#458). *)
  | ExprFloatBinary of expr * binary_op * expr
      (** Float binary operation, `a <op> b` where both sides are `Float`:
          arithmetic ([OpAdd] / [OpSub] / [OpMul] / [OpDiv], yielding `Float`)
          or comparison ([OpLt] / [OpLe] / [OpGt] / [OpGe] / [OpEq] / [OpNe],
          yielding `Bool`). Like {!ExprStringEq}, this is not produced by the
          parser: it is introduced by the post-typecheck elaboration
          (see {!Typecheck.elaborate_string_concat}) that rewrites the Float
          case of these operators, so the wasm backend lowers them to the f64
          instruction family (`F64Add`, `F64Lt`, …) instead of the i32 family
          `gen_binop` returns — which would emit `i32.lt_s` on f64 operands and
          fail wasm validation. The interpreter and non-wasm backends treat it
          as the ordinary `Float` operation. The "float wall". *)
  | ExprFloatArray of expr list
      (** Array literal whose element type is `Float`. Like {!ExprFloatBinary},
          not produced by the parser: the post-typecheck elaboration
          (see {!Typecheck.elaborate_string_concat}) rewrites an {!ExprArray}
          that `synth` typed with a `Float` element into this node, so the wasm
          backend lays the array out with 8-byte `f64` cells and `f64.store`
          (8-byte stride) instead of the uniform 4-byte i32 cells that would
          truncate f64 (issue-draft 05, durable heap fix). The interpreter and
          non-wasm backends treat it as the ordinary array. The "float heap wall". *)
  | ExprFloatIndex of expr * expr
      (** `a[i]` whose result type is `Float` — the dual of {!ExprFloatArray} on
          the read side. Introduced by the same elaboration from an {!ExprIndex}
          that `synth` typed as `Float`; the wasm backend loads it with an
          8-byte stride and `f64.load`. The interpreter re-dispatches to the
          ordinary {!ExprIndex}. *)
  | ExprCellTuple of (expr * bool) list
      (** Tuple literal that contains at least one `Float` field. Laid out with
          *uniform 8-byte cells* (no length header, field `i` at offset `i*8`):
          the bool flags an f64 cell (`f64.store`) vs an i32 cell (`i32.store`,
          which writes the low 4 bytes of the 8-byte slot). Uniform-8 keeps the
          offset `i*8` independent of the mix of field types, so a mixed
          `(Int, Float)` works without per-field offset accumulation. Produced
          only by the elaboration from an {!ExprTuple} that `synth` typed with a
          `Float` somewhere (issue-draft 05). An all-non-float tuple keeps the
          4-byte {!ExprTuple} layout. The interpreter re-dispatches to {!ExprTuple}. *)
  | ExprCellTupleIndex of expr * int * bool
      (** `t.i` on a float-bearing (uniform-8) tuple: load field `i` at offset
          `i*8`, as f64 if the bool is set else i32. Every access to such a tuple
          is rewritten (not just the float fields), since the whole tuple uses
          8-byte cells. Dual of {!ExprCellTuple}; re-dispatched to
          {!ExprTupleIndex} by the interpreter. *)
  | ExprCellRecord of (ident * expr * bool) list
      (** Record literal that contains at least one `Float` field, on a CLOSED
          row. Uniform 8-byte cells; fields are placed by **field name sorted
          ascending** (not literal order), so construction here and {!ExprCellField}
          access derive identical offsets from the names alone — independent of
          literal-vs-type field order. The bool flags an f64 cell. Produced only
          by the elaboration from an {!ExprRecord} that `synth` typed float-bearing
          and closed (issue-draft 05). Re-dispatched to {!ExprRecord} by the interpreter. *)
  | ExprCellField of expr * int * bool
      (** `r.f` on a float-bearing (uniform-8, sorted-by-name) record: load at the
          given byte offset (the field's sorted-name position × 8, baked at
          elaborate from the closed row), as f64 if the bool is set else i32.
          Dual of {!ExprCellRecord}; re-dispatched to {!ExprField} by the interpreter. *)
  | ExprUnary of unary_op * expr
  | ExprBlock of block
  | ExprReturn of expr option
  | ExprBreak of Span.t                              (** break (in loop) — #459 *)
  | ExprContinue of Span.t                           (** continue (in loop) — #459 *)
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
      sl_quantity : quantity option;
        (** QTT binder quantity for statement-position let, per
            ADR-002 / ADR-007. None defaults to QOmega. Same surface
            syntaxes as ExprLet's el_quantity. *)
      sl_pat : pattern;
      sl_ty : type_expr option;
      sl_value : expr;
    }
  | StmtExpr of expr
  | StmtAssign of expr * assign_op * expr
  | StmtWhile of expr * block
  | StmtFor of pattern * expr * block

and binary_op =
  | OpAdd | OpSub | OpMul | OpDiv | OpMod | OpConcat
  | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
  | OpAnd | OpOr
  | OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr

and unary_op =
  | OpNeg | OpNot | OpBitNot | OpRef | OpMutRef | OpDeref
      (** [OpMutRef] is `&mut e` — an *exclusive* borrow expression. `&e`
          is [OpRef] (shared). Only the borrow checker distinguishes them
          (shared-XOR-exclusive); every other backend treats `&mut e`
          exactly like `&e` (a reference is the same runtime pointer —
          exclusivity is a static property). CORE-01 pt2 / #177. *)

and assign_op =
  | AssignEq | AssignAdd | AssignSub | AssignMul | AssignDiv

and unsafe_op =
  | UnsafeRead of expr
  | UnsafeWrite of expr * expr
  | UnsafeOffset of expr * expr
  | UnsafeTransmute of type_expr * type_expr * expr
  | UnsafeForget of expr
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
  | FnExtern  (** No body: implementation supplied by the host environment.
                  Surfaces as `extern fn name(...) -> Ret;` in user source. *)
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
  | TyExtern  (** Opaque host-supplied type: `extern type Name;` *)

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
      tc_mut : bool;
        (** ADR-014-adjacent (#548): `const mut <name>: T = init;`
            declares a mutable module-level binding. Reads compile to a
            plain identifier lookup; writes are assignment statements
            (`name = new_value;`) and lower to a JS `let` rather than a
            JS `const`. The `mut` qualifier is intentionally folded into
            the existing `TopConst` shape rather than spawning a
            separate `TopMut` constructor so that downstream codegen
            backends that already pattern-match on [TopConst { _ }] keep
            building unchanged; only the JS-family (and any backend with
            true mutability) reads the flag. *)
      tc_name : ident;
      tc_ty : type_expr;
      tc_value : expr;
    }
  | TopExternType of {
      et_name : ident;
    }
  | TopExternFn of {
      ef_name : ident;
      ef_params : param list;
      ef_ret_ty : type_expr option;
    }
[@@deriving show, eq]

(** Complete program *)
type program = {
  prog_module : module_path option;
  prog_imports : import_decl list;
  prog_decls : top_level list;
}
[@@deriving show, eq]
