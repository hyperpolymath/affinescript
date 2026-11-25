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

(** Abstract origin (region) variable — ADR-022. Opaque to source syntax;
    inserted by the borrow checker / elaborator. Fresh-int identity, pretty-printed
    'o0, 'o1, … M1 plants [None] at every site (the single global origin), so the
    Polonius solver's verdicts reduce to the lexical checker's. *)
type origin_var = int [@@deriving show, eq]

(** Type expressions *)
type type_expr =
  | TyVar of ident                                   (** Type variable *)
  | TyCon of ident                                   (** Type constructor *)
  | TyApp of ident * type_arg list                   (** Vec[n, T] *)
  | TyArrow of type_expr * quantity option * type_expr * effect_expr option  (** T -{q}-> U / E *)
  | TyTuple of type_expr list                        (** (T, U, V) *)
  | TyRecord of row_field list * ident option        (** {x: T, ..r} *)
  | TyOwn of type_expr                               (** own T *)
  | TyRef of origin_var option * type_expr           (** ref T — ADR-022 M1: origin defaults None *)
  | TyMut of origin_var option * type_expr           (** mut T — ADR-022 M1: origin defaults None *)
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

(** Does a function body contain a `return`? Used by the experimental Lean/Why3
    emitters to FAIL LOUD rather than silently drop control flow (Refs #624):
    their [gen_block] skips non-`let` statements, so an early `return` would
    otherwise vanish. Covers the standard nesting (block / if / let / binary /
    unary / app / span / while / for / assign); unlisted exotic variants default
    to [false] (conservative — they are not generated by the fixtures this
    fences, and the backends are experimental). *)
let rec expr_contains_return (e : expr) : bool =
  match e with
  | ExprReturn _ -> true
  | ExprSpan (inner, _) -> expr_contains_return inner
  | ExprBlock blk -> block_contains_return blk
  | ExprIf { ei_cond; ei_then; ei_else } ->
    expr_contains_return ei_cond
    || expr_contains_return ei_then
    || (match ei_else with Some e -> expr_contains_return e | None -> false)
  | ExprLet { el_value; el_body; _ } ->
    expr_contains_return el_value
    || (match el_body with Some e -> expr_contains_return e | None -> false)
  | ExprBinary (a, _, b) -> expr_contains_return a || expr_contains_return b
  | ExprUnary (_, x) -> expr_contains_return x
  | ExprApp (f, args) ->
    expr_contains_return f || List.exists expr_contains_return args
  (* A `return` in a match arm (`Err(e) => return Err(e)`) is the common
     early-exit shape; without this arm it was invisible, and the Bun-ESM
     backend compiled it into a `return` from the match's IIFE instead of
     from the function. *)
  | ExprMatch { em_scrutinee; em_arms } ->
    expr_contains_return em_scrutinee
    || List.exists (fun arm ->
         (match arm.ma_guard with Some g -> expr_contains_return g | None -> false)
         || expr_contains_return arm.ma_body) em_arms
  (* Aggregates: `[return 1]`, `(x, return y)`, `#{ f: return z }` and
     projections of them return just as directly. *)
  | ExprTuple es | ExprArray es -> List.exists expr_contains_return es
  | ExprRecord { er_fields; er_spread } ->
    List.exists (fun (_, e) ->
        match e with Some e -> expr_contains_return e | None -> false) er_fields
    || (match er_spread with Some e -> expr_contains_return e | None -> false)
  | ExprField (e, _) | ExprTupleIndex (e, _) -> expr_contains_return e
  | ExprIndex (a, b) | ExprStringConcat (a, b) ->
    expr_contains_return a || expr_contains_return b
  | _ -> false

and block_contains_return (blk : block) : bool =
  List.exists stmt_contains_return blk.blk_stmts
  || (match blk.blk_expr with Some e -> expr_contains_return e | None -> false)

and stmt_contains_return (s : stmt) : bool =
  match s with
  | StmtExpr e -> expr_contains_return e
  | StmtLet { sl_value; _ } -> expr_contains_return sl_value
  | StmtAssign (a, _, b) -> expr_contains_return a || expr_contains_return b
  | StmtWhile (c, b) -> expr_contains_return c || block_contains_return b
  | StmtFor (_, e, b) -> expr_contains_return e || block_contains_return b

let fn_body_contains_return : fn_body -> bool = function
  | FnExpr e -> expr_contains_return e
  | FnBlock b -> block_contains_return b

(** Variables bound by a pattern. *)
let rec pattern_binders (pat : pattern) : string list =
  match pat with
  | PatWildcard _ | PatLit _ -> []
  | PatVar id -> [id.name]
  | PatTuple pats -> List.concat_map pattern_binders pats
  | PatRecord (fields, _) ->
    List.concat_map (fun (id, pat_opt) ->
      match pat_opt with
      | Some p -> pattern_binders p
      | None -> [id.name]
    ) fields
  | PatCon (_, pats) -> List.concat_map pattern_binders pats
  | PatOr (p1, p2) -> pattern_binders p1 @ pattern_binders p2
  | PatAs (id, pat) -> id.name :: pattern_binders pat

(** Map the free occurrences of variables through [look]: an occurrence of
    [n] becomes [n'] when [look n = Some n'] and is left alone when it is
    [None].  Capture-avoiding in the usual way: a binder (let, match arm,
    lambda parameter, block `let`, `for` pattern, handler pattern) hides the
    names it binds from [look] for its scope, so a local that shadows a
    top-level name keeps referring to the local.  A record shorthand field
    `#{ x }` whose variable is mapped becomes `#{ x: x' }`.

    [look] is also called, for observation only, with the type name of a
    qualified constructor `T::C`; its result is ignored there.  That lets
    {!free_names} report the enums a body names without a second walker:
    the renamer and the free-name collector see exactly the same
    occurrences, so a dependency the collector misses is one the renamer
    would also leave unrenamed.  Every expression form is visited. *)
let shadow (look : string -> string option) (names : string list) =
  if names = [] then look
  else fun n -> if List.mem n names then None else look n

let rec map_free (look : string -> string option) (e : expr) : expr =
  let r = map_free look in
  let ro = Option.map r in
  match e with
  | ExprLit _ | ExprBreak _ | ExprContinue _ -> e
  | ExprVariant (ty, _) -> ignore (look ty.name); e
  | ExprVar id ->
    (match look id.name with
     | Some n -> ExprVar { id with name = n }
     | None -> e)
  | ExprLet lb ->
    let inner = shadow look (pattern_binders lb.el_pat) in
    ExprLet { lb with el_value = r lb.el_value;
                      el_body = Option.map (map_free inner) lb.el_body }
  | ExprIf ei ->
    ExprIf { ei_cond = r ei.ei_cond; ei_then = r ei.ei_then;
             ei_else = ro ei.ei_else }
  | ExprMatch m ->
    ExprMatch { em_scrutinee = r m.em_scrutinee;
                em_arms = List.map (map_arm look) m.em_arms }
  | ExprLambda lam ->
    let inner =
      shadow look (List.map (fun (p : param) -> p.p_name.name)
                     lam.elam_params) in
    ExprLambda { lam with elam_body = map_free inner lam.elam_body }
  | ExprApp (f, args) -> ExprApp (r f, List.map r args)
  | ExprField (x, f) -> ExprField (r x, f)
  | ExprTupleIndex (x, i) -> ExprTupleIndex (r x, i)
  | ExprIndex (a, b) -> ExprIndex (r a, r b)
  | ExprTuple es -> ExprTuple (List.map r es)
  | ExprArray es -> ExprArray (List.map r es)
  | ExprRecord rc ->
    let field ((n : ident), eo) =
      match eo with
      | Some x -> (n, Some (r x))
      | None ->
        (match look n.name with
         | Some nn -> (n, Some (ExprVar { n with name = nn }))
         | None -> (n, None))
    in
    ExprRecord { er_fields = List.map field rc.er_fields;
                 er_spread = ro rc.er_spread }
  | ExprRowRestrict (x, f) -> ExprRowRestrict (r x, f)
  | ExprBinary (a, op, b) -> ExprBinary (r a, op, r b)
  | ExprStringConcat (a, b) -> ExprStringConcat (r a, r b)
  | ExprStringEq (a, b, neg) -> ExprStringEq (r a, r b, neg)
  | ExprStringRel (a, b, op) -> ExprStringRel (r a, r b, op)
  | ExprFloatBinary (a, op, b) -> ExprFloatBinary (r a, op, r b)
  | ExprFloatArray es -> ExprFloatArray (List.map r es)
  | ExprFloatIndex (a, b) -> ExprFloatIndex (r a, r b)
  | ExprCellTuple cs -> ExprCellTuple (List.map (fun (x, f) -> (r x, f)) cs)
  | ExprCellTupleIndex (x, i, f) -> ExprCellTupleIndex (r x, i, f)
  | ExprCellRecord fs ->
    ExprCellRecord (List.map (fun (n, x, f) -> (n, r x, f)) fs)
  | ExprCellField (x, i, f) -> ExprCellField (r x, i, f)
  | ExprUnary (op, x) -> ExprUnary (op, r x)
  | ExprBlock b -> ExprBlock (map_block look b)
  | ExprReturn eo -> ExprReturn (ro eo)
  | ExprTry t ->
    ExprTry { et_body = map_block look t.et_body;
              et_catch = Option.map (List.map (map_arm look)) t.et_catch;
              et_finally = Option.map (map_block look) t.et_finally }
  | ExprHandle h ->
    let handler = function
      | HandlerReturn (p, x) ->
        HandlerReturn (p, map_free (shadow look (pattern_binders p)) x)
      | HandlerOp (op, ps, x) ->
        HandlerOp (op, ps,
                   map_free
                     (shadow look (List.concat_map pattern_binders ps)) x)
    in
    ExprHandle { eh_body = r h.eh_body;
                 eh_handlers = List.map handler h.eh_handlers }
  | ExprResume eo -> ExprResume (ro eo)
  | ExprUnsafe ops ->
    ExprUnsafe (List.map (function
        | UnsafeRead x -> UnsafeRead (r x)
        | UnsafeWrite (a, b) -> UnsafeWrite (r a, r b)
        | UnsafeOffset (a, b) -> UnsafeOffset (r a, r b)
        | UnsafeTransmute (t1, t2, x) -> UnsafeTransmute (t1, t2, r x)
        | UnsafeForget x -> UnsafeForget (r x)) ops)
  | ExprSpan (x, sp) -> ExprSpan (r x, sp)

and map_arm look (arm : match_arm) : match_arm =
  let inner = shadow look (pattern_binders arm.ma_pat) in
  { arm with ma_guard = Option.map (map_free inner) arm.ma_guard;
             ma_body = map_free inner arm.ma_body }

and map_block look (blk : block) : block =
  let rec go look acc = function
    | [] -> (List.rev acc, look)
    | StmtLet sl :: rest ->
      let s = StmtLet { sl with sl_value = map_free look sl.sl_value } in
      go (shadow look (pattern_binders sl.sl_pat)) (s :: acc) rest
    | StmtExpr x :: rest -> go look (StmtExpr (map_free look x) :: acc) rest
    | StmtAssign (a, op, b) :: rest ->
      go look (StmtAssign (map_free look a, op, map_free look b) :: acc) rest
    | StmtWhile (c, b) :: rest ->
      go look (StmtWhile (map_free look c, map_block look b) :: acc) rest
    | StmtFor (p, it, b) :: rest ->
      let s = StmtFor (p, map_free look it,
                       map_block (shadow look (pattern_binders p)) b) in
      go look (s :: acc) rest
  in
  let stmts, look_after = go look [] blk.blk_stmts in
  { blk_stmts = stmts; blk_expr = Option.map (map_free look_after) blk.blk_expr }

(** Rename free occurrences of variables: [ren] maps old names to new ones,
    with the capture rules of {!map_free}.  Used by import flattening to
    give each module's declarations module-qualified names (#801). *)
let without_names (ren : (string * string) list) (names : string list) =
  List.filter (fun (old, _) -> not (List.mem old names)) ren

let rename_free (ren : (string * string) list) (e : expr) : expr =
  if ren = [] then e else map_free (fun n -> List.assoc_opt n ren) e

let rename_block (ren : (string * string) list) (blk : block) : block =
  if ren = [] then blk else map_block (fun n -> List.assoc_opt n ren) blk

(** The free names of [e] (variables, record shorthands, and the type names
    of qualified constructors), exactly the occurrences {!rename_free} would
    rewrite.  May contain duplicates. *)
let free_names (e : expr) : string list =
  let acc = ref [] in
  ignore (map_free (fun n -> acc := n :: !acc; None) e);
  !acc

let free_names_block (b : block) : string list =
  let acc = ref [] in
  ignore (map_block (fun n -> acc := n :: !acc; None) b);
  !acc

(** Free variables of an expression.

    Returns the names used in [expr] but not bound within it; [bound_vars]
    lists the names already bound by the enclosing scope (parameters, let
    bindings).

    Shared by every pass that needs to know what a body refers to: wasm
    codegen, and [Module_loader]'s `use`-list flattening, which uses it to
    pull in helpers a selective import never names (`use Dom::{div}` needs
    `div`'s own helper `h`; omitting it emitted a module that called an
    undefined `h`). Like the other walkers in this module it is deliberately
    partial: unhandled constructs, record spreads, shorthand record fields
    and qualified constructors contribute no names. The result may contain
    duplicates. *)
let rec find_free_vars (bound_vars : string list) (expr : expr) : string list =
  match expr with
  | ExprLit _ -> []
  | ExprVar id ->
    if List.mem id.name bound_vars then [] else [id.name]
  | ExprBinary (e1, _, e2) ->
    find_free_vars bound_vars e1 @ find_free_vars bound_vars e2
  | ExprStringConcat (e1, e2) ->
    find_free_vars bound_vars e1 @ find_free_vars bound_vars e2
  | ExprUnary (_, e) ->
    find_free_vars bound_vars e
  | ExprIf ei ->
    find_free_vars bound_vars ei.ei_cond @
    find_free_vars bound_vars ei.ei_then @
    (match ei.ei_else with
     | Some e -> find_free_vars bound_vars e
     | None -> [])
  | ExprLet lb ->
    let rhs_free = find_free_vars bound_vars lb.el_value in
    (* Add bound variable to scope for body *)
    let new_bound = pattern_binders lb.el_pat @ bound_vars in
    let body_free = match lb.el_body with
      | Some e -> find_free_vars new_bound e
      | None -> []
    in
    rhs_free @ body_free
  | ExprLambda lam ->
    (* Parameters are bound within lambda *)
    let param_names = List.map (fun p -> p.p_name.name) lam.elam_params in
    find_free_vars (param_names @ bound_vars) lam.elam_body
  | ExprApp (f, args) ->
    find_free_vars bound_vars f @
    List.concat (List.map (find_free_vars bound_vars) args)
  | ExprBlock blk ->
    (* Statements may introduce bindings *)
    let (bound_after, free) = List.fold_left (fun (bound, acc_free) stmt ->
      match stmt with
      | StmtLet sl ->
        let rhs_free = find_free_vars bound sl.sl_value in
        let new_bound = pattern_binders sl.sl_pat @ bound in
        (new_bound, acc_free @ rhs_free)
      | StmtExpr e ->
        (bound, acc_free @ find_free_vars bound e)
      (* Loops and assignments were skipped entirely, so a name used only
         inside a `for`/`while` body was never reported: import flattening
         then dropped the helper it called (`ReferenceError: apply_attr is
         not defined`), and closure conversion could miss a capture. *)
      | StmtAssign (lhs, _, rhs) ->
        (bound, acc_free @ find_free_vars bound lhs @ find_free_vars bound rhs)
      | StmtWhile (cond, body) ->
        (bound, acc_free @ find_free_vars bound cond
                @ find_free_vars bound (ExprBlock body))
      | StmtFor (pat, iter, body) ->
        (bound, acc_free @ find_free_vars bound iter
                @ find_free_vars (pattern_binders pat @ bound) (ExprBlock body))
    ) (bound_vars, []) blk.blk_stmts in
    (* The tail expression is in scope of the block's own `let`
       bindings, so its free vars must exclude them — use the
       threaded [bound_after], not the original [bound_vars]. (Prior
       code used [bound_vars], spuriously reporting block-local
       binders as free; surfaced by #225 PR3c chained continuations.) *)
    let expr_free = match blk.blk_expr with
      | Some e -> find_free_vars bound_after e
      | None -> []
    in
    free @ expr_free
  | ExprMatch m ->
    find_free_vars bound_vars m.em_scrutinee @
    List.concat (List.map (fun arm ->
      let arm_bound = pattern_binders arm.ma_pat @ bound_vars in
      (match arm.ma_guard with
       | Some g -> find_free_vars arm_bound g
       | None -> [])
      @ find_free_vars arm_bound arm.ma_body) m.em_arms)
  | ExprReturn e_opt ->
    (match e_opt with Some e -> find_free_vars bound_vars e | None -> [])
  | ExprTuple exprs | ExprArray exprs ->
    List.concat (List.map (find_free_vars bound_vars) exprs)
  | ExprRecord r ->
    (* A shorthand field `#{ x }` reads the variable `x`, and a spread
       `..base` reads `base`: both are free uses, which import flattening
       needs to carry the helper or constant they name. *)
    List.concat (List.map (fun ((name : ident), e_opt) ->
      match e_opt with
      | Some e -> find_free_vars bound_vars e
      | None ->
        if List.mem name.name bound_vars then [] else [name.name]
    ) r.er_fields)
    @ (match r.er_spread with
       | Some e -> find_free_vars bound_vars e
       | None -> [])
  | ExprField (e, _) -> find_free_vars bound_vars e
  | ExprTupleIndex (e, _) -> find_free_vars bound_vars e
  | ExprIndex (e1, e2) ->
    find_free_vars bound_vars e1 @ find_free_vars bound_vars e2
  | ExprVariant _ -> []
  | ExprSpan (e, _) -> find_free_vars bound_vars e
  (* Float-wall elaboration nodes (codegen runs on the post-elaborate tree, so
     these CAN appear here): traverse them exactly like their pre-elaboration
     forms, else a variable captured only inside a float expression is missed
     and the closure mis-lowers to UnboundVariable. *)
  | ExprFloatBinary (a, _, b) ->
    find_free_vars bound_vars a @ find_free_vars bound_vars b
  | ExprFloatArray exprs -> List.concat (List.map (find_free_vars bound_vars) exprs)
  | ExprFloatIndex (a, b) ->
    find_free_vars bound_vars a @ find_free_vars bound_vars b
  | ExprCellTuple cells ->
    List.concat (List.map (fun (e, _) -> find_free_vars bound_vars e) cells)
  | ExprCellTupleIndex (e, _, _) -> find_free_vars bound_vars e
  | ExprCellRecord fields ->
    List.concat (List.map (fun (_, e, _) -> find_free_vars bound_vars e) fields)
  | ExprCellField (e, _, _) -> find_free_vars bound_vars e
  | _ -> []  (* Other expressions *)
