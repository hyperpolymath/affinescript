/* Menhir parser for AffineScript */

%{
open Ast

let mk_span startpos endpos =
  let file = startpos.Lexing.pos_fname in
  let start_pos = {
    Span.line = startpos.Lexing.pos_lnum;
    col = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol + 1;
    offset = startpos.Lexing.pos_cnum;
  } in
  let end_pos = {
    Span.line = endpos.Lexing.pos_lnum;
    col = endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol + 1;
    offset = endpos.Lexing.pos_cnum;
  } in
  Span.make ~file ~start_pos ~end_pos

let mk_ident name startpos endpos =
  { name; span = mk_span startpos endpos }

%}

/* Tokens with values */
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> LOWER_IDENT
%token <string> UPPER_IDENT
%token <string> ROW_VAR

/* Literal keywords */
%token TRUE FALSE

/* Keywords */
%token SELF_KW
%token FN LET CONST MUT OWN REF TYPE STRUCT ENUM TRAIT IMPL
%token EFFECT HANDLE RESUME MATCH IF ELSE WHILE FOR
%token RETURN BREAK CONTINUE IN WHERE TOTAL MODULE USE
%token PUB AS UNSAFE ASSUME TRANSMUTE FORGET TRY CATCH FINALLY

/* Built-in types */
%token NAT INT_T BOOL FLOAT_T STRING_T CHAR_T TYPE_K ROW NEVER

/* Punctuation */
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA SEMICOLON COLON COLONCOLON DOT DOTDOT
%token ARROW FAT_ARROW PIPE AT UNDERSCORE BACKSLASH QUESTION

/* Quantity */
%token ZERO ONE OMEGA

/* Operators */
%token PLUS PLUSPLUS MINUS STAR SLASH PERCENT
%token EQ EQEQ NE LT LE GT GE
%token AMPAMP PIPEPIPE BANG
%token AMP CARET TILDE LTLT GTGT
%token PLUSEQ MINUSEQ STAREQ SLASHEQ

/* End of file */
%token EOF

/* Precedence (lowest to highest) */
%right EQ PLUSEQ MINUSEQ STAREQ SLASHEQ
%left PIPEPIPE
%left AMPAMP
%left PIPE
%left CARET
%left AMP
%left EQEQ NE
%left LT LE GT GE
%left LTLT GTGT
%left PLUS PLUSPLUS MINUS
%left STAR SLASH PERCENT
%right BANG TILDE UMINUS UREF UDEREF
%left DOT LBRACKET LPAREN

/* Entry point */
%start <Ast.program> program
%start <Ast.expr> expr_only

%%

/* ========== Program ========== */

program:
  | module_decl = module_decl? imports = list(import_decl) decls = list(top_level) EOF
    { { prog_module = module_decl; prog_imports = imports; prog_decls = decls } }

module_decl:
  | MODULE path = module_path SEMICOLON { path }

module_path:
  | id = ident { [id] }
  | path = module_path DOT id = ident { path @ [id] }

/* ========== Imports ========== */

import_decl:
  | USE path = module_path SEMICOLON
    { ImportSimple (path, None) }
  | USE path = module_path AS alias = ident SEMICOLON
    { ImportSimple (path, Some alias) }
  | USE path = module_path COLONCOLON LBRACE items = separated_list(COMMA, import_item) RBRACE SEMICOLON
    { ImportList (path, items) }
  | USE path = module_path COLONCOLON STAR SEMICOLON
    { ImportGlob path }

import_item:
  | name = ident { { ii_name = name; ii_alias = None } }
  | name = ident AS alias = ident { { ii_name = name; ii_alias = Some alias } }

/* ========== Top-level declarations ========== */

top_level:
  | f = fn_decl { TopFn f }
  | t = type_decl { TopType t }
  | e = effect_decl { TopEffect e }
  | tr = trait_decl { TopTrait tr }
  | i = impl_block { TopImpl i }
  | c = const_decl { c }

const_decl:
  | vis = visibility? CONST name = ident COLON ty = type_expr EQ value = expr SEMICOLON
    { TopConst { tc_vis = Option.value vis ~default:Private;
                 tc_name = name; tc_ty = ty; tc_value = value } }

/* ========== Functions ========== */

fn_decl:
  | vis = visibility? total = TOTAL? FN name = ident
    type_params = type_params?
    LPAREN params = separated_list(COMMA, param) RPAREN
    ret = return_type?
    where_clause = where_clause?
    body = fn_body
    { { fd_vis = Option.value vis ~default:Private;
        fd_total = Option.is_some total;
        fd_name = name;
        fd_type_params = Option.value type_params ~default:[];
        fd_params = params;
        fd_ret_ty = fst (Option.value ret ~default:(None, None));
        fd_eff = snd (Option.value ret ~default:(None, None));
        fd_where = Option.value where_clause ~default:[];
        fd_body = body } }

return_type:
  | ARROW ty = type_expr { (Some ty, None) }
  | MINUS LBRACE eff = effect_expr RBRACE ARROW ty = type_expr { (Some ty, Some eff) }

fn_body:
  | blk = block { FnBlock blk }
  | EQ e = expr SEMICOLON { FnExpr e }

visibility:
  | PUB { Public }
  | PUB LPAREN LOWER_IDENT RPAREN
    { match $3 with
      | "crate" -> PubCrate
      | "super" -> PubSuper
      | _ -> failwith "Expected 'crate' or 'super'" }

type_params:
  | LBRACKET params = separated_nonempty_list(COMMA, type_param) RBRACKET { params }

type_param:
  | qty = quantity? name = ident kind = kind_annotation?
    { { tp_quantity = qty; tp_name = name; tp_kind = kind } }
  (* Row variable type parameter, e.g. `[..r]` — lexed as a single ROW_VAR token *)
  | rv = ROW_VAR
    { { tp_quantity = None;
        tp_name = mk_ident rv $startpos $endpos;
        tp_kind = Some KRow } }

kind_annotation:
  | COLON k = kind { k }

kind:
  | TYPE_K { KType }
  | ROW { KRow }
  | k1 = kind ARROW k2 = kind { KArrow (k1, k2) }
  | LPAREN k = kind RPAREN { k }

quantity:
  | ZERO { QZero }
  | ONE { QOne }
  | OMEGA { QOmega }

(* ADR-007 Option C — primary attribute form for quantity annotations.
   `@linear` ≡ QOne, `@erased` ≡ QZero, `@unrestricted` ≡ QOmega.
   Rejects unknown attribute names with a parse error. *)
quantity_attr:
  | AT name = lower_ident
    { match name with
      | "linear" -> QOne
      | "erased" -> QZero
      | "unrestricted" -> QOmega
      | other ->
        let msg = Printf.sprintf
          "unknown quantity attribute '@%s'; expected @linear, @erased, or @unrestricted"
          other in
        raise (Parser_errors.Parse_action_error (msg, $startpos, $endpos)) }

(* ADR-007 Option B — sugar form for quantity annotations on let/stmt_let.
   Reads as `:1`, `:0`, or `:ω` immediately after the pattern. The lexer
   emits INT for `0` and `1`, so we accept INT here and validate the value
   at parse time, rejecting any integer outside {0, 1}. OMEGA is the
   `omega` keyword or `ω` codepoint. *)
quantity_b_sugar:
  | COLON n = INT
    { match n with
      | 0 -> QZero
      | 1 -> QOne
      | other ->
        let msg = Printf.sprintf
          "invalid quantity literal '%d'; expected 0, 1, or ω (omega)"
          other in
        raise (Parser_errors.Parse_action_error (msg, $startpos, $endpos)) }
  | COLON OMEGA { QOmega }

param:
  (* Self receiver: bare `self` — SELF_KW is a distinct keyword token.
     No COLON or type annotation; type defaults to `Self`.
     Four forms below cover all quantity × ownership × self combinations
     that are LR(1) without option() conflicts. *)
  | SELF_KW
    { { p_quantity = None; p_ownership = None;
        p_name = mk_ident "self" $startpos $endpos;
        p_ty = TyCon (mk_ident "Self" $startpos $endpos) } }
  | own = ownership SELF_KW
    { { p_quantity = None; p_ownership = Some own;
        p_name = mk_ident "self" $startpos $endpos;
        p_ty = TyCon (mk_ident "Self" $startpos $endpos) } }
  (* Normal params: explicit combinations to avoid option() LR(1) conflicts.
     Tokens sets are disjoint: SELF_KW / ownership (REF|OWN|MUT) / quantity
     (ZERO|ONE|OMEGA) / AT / ident (LOWER_IDENT|UPPER_IDENT). *)
  | name = ident COLON ty = type_expr
    { { p_quantity = None; p_ownership = None; p_name = name; p_ty = ty } }
  | own = ownership name = ident COLON ty = type_expr
    { { p_quantity = None; p_ownership = Some own; p_name = name; p_ty = ty } }
  | qty = quantity name = ident COLON ty = type_expr
    { { p_quantity = Some qty; p_ownership = None; p_name = name; p_ty = ty } }
  | qty = quantity own = ownership name = ident COLON ty = type_expr
    { { p_quantity = Some qty; p_ownership = Some own; p_name = name; p_ty = ty } }
  (* ADR-007 Option C: @linear / @erased / @unrestricted attribute form *)
  | qty_attr = quantity_attr name = ident COLON ty = type_expr
    { { p_quantity = Some qty_attr; p_ownership = None; p_name = name; p_ty = ty } }
  | qty_attr = quantity_attr own = ownership name = ident COLON ty = type_expr
    { { p_quantity = Some qty_attr; p_ownership = Some own; p_name = name; p_ty = ty } }

ownership:
  | OWN { Own }
  | REF { Ref }
  | MUT { Mut }

where_clause:
  | WHERE constraints = separated_nonempty_list(COMMA, constraint_) { constraints }

constraint_:
  | id = ident COLON bounds = separated_nonempty_list(PLUS, trait_bound)
    { ConstraintTrait (id, bounds) }

trait_bound:
  | name = ident { { tb_name = name; tb_args = [] } }
  | name = ident LBRACKET args = separated_list(COMMA, type_arg) RBRACKET
    { { tb_name = name; tb_args = args } }

/* ========== Types ========== */

type_decl:
  /* Type alias: `type Foo = Bar` — semicolon is optional (conformance spec omits it) */
  | vis = visibility? TYPE name = ident type_params = type_params? EQ ty = type_expr SEMICOLON?
    { { td_vis = Option.value vis ~default:Private;
        td_name = name;
        td_type_params = Option.value type_params ~default:[];
        td_body = TyAlias ty } }
  /* Inline variant syntax with optional leading pipe:
       type X = A | B | C(Int)       (no leading pipe — classic style)
       type X = | A | B | C(Int)     (leading pipe — spec style)
     Semicolon is optional in both forms (conformance spec omits it). */
  | vis = visibility? TYPE name = ident type_params = type_params? EQ
    first = variant_decl PIPE rest = separated_nonempty_list(PIPE, variant_decl) SEMICOLON?
    { { td_vis = Option.value vis ~default:Private;
        td_name = name;
        td_type_params = Option.value type_params ~default:[];
        td_body = TyEnum (first :: rest) } }
  | vis = visibility? TYPE name = ident type_params = type_params? EQ
    PIPE first = variant_decl rest = list(preceded(PIPE, variant_decl)) SEMICOLON?
    { { td_vis = Option.value vis ~default:Private;
        td_name = name;
        td_type_params = Option.value type_params ~default:[];
        td_body = TyEnum (first :: rest) } }
  | vis = visibility? STRUCT name = ident type_params = type_params?
    LBRACE fields = separated_list(COMMA, struct_field) RBRACE
    { { td_vis = Option.value vis ~default:Private;
        td_name = name;
        td_type_params = Option.value type_params ~default:[];
        td_body = TyStruct fields } }
  | vis = visibility? ENUM name = ident type_params = type_params?
    LBRACE variants = separated_list(COMMA, variant_decl) RBRACE
    { { td_vis = Option.value vis ~default:Private;
        td_name = name;
        td_type_params = Option.value type_params ~default:[];
        td_body = TyEnum variants } }

struct_field:
  | vis = visibility? name = field_name COLON ty = type_expr
    { { sf_vis = Option.value vis ~default:Private; sf_name = name; sf_ty = ty } }

variant_decl:
  | name = ident { { vd_name = name; vd_fields = []; vd_ret_ty = None } }
  | name = ident LPAREN fields = separated_list(COMMA, type_expr) RPAREN
    { { vd_name = name; vd_fields = fields; vd_ret_ty = None } }
  | name = ident LPAREN fields = separated_list(COMMA, type_expr) RPAREN COLON ret = type_expr
    { { vd_name = name; vd_fields = fields; vd_ret_ty = Some ret } }

/* ========== Type Expressions ========== */

type_expr:
  | ty = type_expr_arrow { ty }

type_expr_arrow:
  | arg = type_expr_primary ARROW ret = type_expr_arrow
    { TyArrow (arg, None, ret, None) }
  | arg = type_expr_primary MINUS LBRACE eff = effect_expr RBRACE ARROW ret = type_expr_arrow
    { TyArrow (arg, None, ret, Some eff) }
  | ty = type_expr_primary { ty }

type_expr_primary:
  | LPAREN RPAREN { TyTuple [] }
  | LPAREN ty = type_expr RPAREN { ty }
  | LPAREN ty = type_expr COMMA tys = separated_nonempty_list(COMMA, type_expr) RPAREN
    { TyTuple (ty :: tys) }
  | UNDERSCORE { TyHole }
  | OWN ty = type_expr_primary { TyOwn ty }
  | REF ty = type_expr_primary { TyRef ty }
  | MUT ty = type_expr_primary { TyMut ty }
  | name = lower_ident { TyVar (mk_ident name $startpos $endpos) }
  | name = upper_ident { TyCon (mk_ident name $startpos $endpos) }
  | name = upper_ident LBRACKET args = separated_nonempty_list(COMMA, type_arg) RBRACKET
    { TyApp (mk_ident name $startpos(name) $endpos(name), args) }
  /* Row-polymorphic record type.  We use a custom recursive rule rather than
     `separated_list` because Menhir's separated_list greedily consumes the
     COMMA separator and then cannot backtrack when the next token (ROW_VAR)
     is not a valid row_field start.  ty_record_body / ty_record_rest parse
     the interior in one pass without lookahead conflicts. */
  | LBRACE body = ty_record_body RBRACE
    { TyRecord (fst body, snd body) }
  /* Built-in types */
  | NAT { TyCon (mk_ident "Nat" $startpos $endpos) }
  | INT_T { TyCon (mk_ident "Int" $startpos $endpos) }
  | BOOL { TyCon (mk_ident "Bool" $startpos $endpos) }
  | FLOAT_T { TyCon (mk_ident "Float" $startpos $endpos) }
  | STRING_T { TyCon (mk_ident "String" $startpos $endpos) }
  | CHAR_T { TyCon (mk_ident "Char" $startpos $endpos) }
  | NEVER { TyCon (mk_ident "Never" $startpos $endpos) }

/* ty_record_body / ty_record_rest: recursive rules for the interior of a
   row-polymorphic record type `{ f1: T1, f2: T2, ..r }`.

   Using `separated_list` would cause an LALR(1) conflict: after parsing the
   COMMA that separates a row_field from a ROW_VAR tail, the separator has
   already been consumed and the parser cannot determine whether the next
   production should be a row_field continuation or the row tail.  These
   rules shift that decision to the token AFTER the comma. */

ty_record_body:
  (* empty record: {} *)
  |
    { ([], None) }
  (* record with only a row tail: {..r} *)
  | rv = ROW_VAR
    { ([], Some (mk_ident rv $startpos $endpos)) }
  (* record starting with a named field: {name: T, ...} *)
  | field = row_field rest = ty_record_rest
    { (field :: fst rest, snd rest) }

ty_record_rest:
  (* end — no trailing comma, no row tail *)
  |
    { ([], None) }
  (* trailing comma only *)
  | COMMA
    { ([], None) }
  (* row tail after comma: , ..r *)
  | COMMA rv = ROW_VAR
    { ([], Some (mk_ident rv $startpos(rv) $endpos(rv))) }
  (* another named field after comma: , name: T ... *)
  | COMMA field = row_field rest = ty_record_rest
    { (field :: fst rest, snd rest) }

row_field:
  | name = field_name COLON ty = type_expr
    { { rf_name = name; rf_ty = ty } }

type_arg:
  | ty = type_expr { TyArg ty }

/* ========== Effects ========== */

effect_decl:
  | vis = visibility? EFFECT name = ident type_params = type_params?
    LBRACE ops = list(effect_op_decl) RBRACE
    { { ed_vis = Option.value vis ~default:Private;
        ed_name = name;
        ed_type_params = Option.value type_params ~default:[];
        ed_ops = ops } }

effect_op_decl:
  (* Type parameters on effect operations are allowed: `fn await[T](promise: Promise[T]) -> T;` *)
  | FN name = ident _type_params = type_params? LPAREN params = separated_list(COMMA, param) RPAREN ret = return_type? SEMICOLON
    { { eod_name = name;
        eod_params = params;
        eod_ret_ty = fst (Option.value ret ~default:(None, None)) } }

effect_expr:
  | e = effect_term { e }
  | e1 = effect_expr PLUS e2 = effect_term { EffUnion (e1, e2) }

effect_term:
  | name = ident { EffVar name }
  | name = ident LBRACKET args = separated_list(COMMA, type_arg) RBRACKET
    { EffCon (name, args) }

/* ========== Traits ========== */

trait_decl:
  | vis = visibility? TRAIT name = ident type_params = type_params?
    super = supertraits?
    _where_clause = where_clause?
    LBRACE items = list(trait_item) RBRACE
    { { trd_vis = Option.value vis ~default:Private;
        trd_name = name;
        trd_type_params = Option.value type_params ~default:[];
        trd_super = Option.value super ~default:[];
        trd_items = items } }

supertraits:
  | COLON bounds = separated_nonempty_list(PLUS, trait_bound) { bounds }

trait_item:
  | sig_ = fn_sig SEMICOLON { TraitFn sig_ }
  | f = fn_decl { TraitFnDefault f }
  | TYPE name = ident kind = kind_annotation? default = type_default? SEMICOLON
    { TraitType { tt_name = name; tt_kind = kind; tt_default = default } }

type_default:
  | EQ ty = type_expr { ty }

fn_sig:
  | vis = visibility? FN name = ident
    type_params = type_params?
    LPAREN params = separated_list(COMMA, param) RPAREN
    ret = return_type?
    { { fs_vis = Option.value vis ~default:Private;
        fs_name = name;
        fs_type_params = Option.value type_params ~default:[];
        fs_params = params;
        fs_ret_ty = fst (Option.value ret ~default:(None, None));
        fs_eff = snd (Option.value ret ~default:(None, None)) } }

/* ========== Impl ========== */

impl_block:
  | IMPL type_params = type_params?
    trait_ref = impl_trait_ref?
    self_ty = type_expr
    where_clause = where_clause?
    LBRACE items = list(impl_item) RBRACE
    { { ib_type_params = Option.value type_params ~default:[];
        ib_trait_ref = trait_ref;
        ib_self_ty = self_ty;
        ib_where = Option.value where_clause ~default:[];
        ib_items = items } }

impl_trait_ref:
  | name = ident FOR { { tr_name = name; tr_args = [] } }
  | name = ident LBRACKET args = separated_list(COMMA, type_arg) RBRACKET FOR
    { { tr_name = name; tr_args = args } }

impl_item:
  | f = fn_decl { ImplFn f }
  | TYPE name = ident EQ ty = type_expr SEMICOLON { ImplType (name, ty) }

/* ========== Expressions ========== */

expr_only:
  | e = expr EOF { e }

expr:
  | e = expr_assign { e }

expr_assign:
  | lhs = expr_or EQ rhs = expr_assign
    { ExprLet { el_mut = false; el_quantity = None;
                el_pat = PatVar (mk_ident "_" $startpos(lhs) $endpos(lhs));
                el_ty = None; el_value = lhs; el_body = Some rhs } }
  | e = expr_or { e }

expr_or:
  | e1 = expr_or PIPEPIPE e2 = expr_and { ExprBinary (e1, OpOr, e2) }
  | e = expr_and { e }

expr_and:
  | e1 = expr_and AMPAMP e2 = expr_bitor { ExprBinary (e1, OpAnd, e2) }
  | e = expr_bitor { e }

expr_bitor:
  | e1 = expr_bitor PIPE e2 = expr_bitxor { ExprBinary (e1, OpBitOr, e2) }
  | e = expr_bitxor { e }

expr_bitxor:
  | e1 = expr_bitxor CARET e2 = expr_bitand { ExprBinary (e1, OpBitXor, e2) }
  | e = expr_bitand { e }

expr_bitand:
  | e1 = expr_bitand AMP e2 = expr_cmp { ExprBinary (e1, OpBitAnd, e2) }
  | e = expr_cmp { e }

expr_cmp:
  | e1 = expr_cmp EQEQ e2 = expr_shift { ExprBinary (e1, OpEq, e2) }
  | e1 = expr_cmp NE e2 = expr_shift { ExprBinary (e1, OpNe, e2) }
  | e1 = expr_cmp LT e2 = expr_shift { ExprBinary (e1, OpLt, e2) }
  | e1 = expr_cmp LE e2 = expr_shift { ExprBinary (e1, OpLe, e2) }
  | e1 = expr_cmp GT e2 = expr_shift { ExprBinary (e1, OpGt, e2) }
  | e1 = expr_cmp GE e2 = expr_shift { ExprBinary (e1, OpGe, e2) }
  | e = expr_shift { e }

expr_shift:
  | e1 = expr_shift LTLT e2 = expr_add { ExprBinary (e1, OpShl, e2) }
  | e1 = expr_shift GTGT e2 = expr_add { ExprBinary (e1, OpShr, e2) }
  | e = expr_add { e }

expr_add:
  | e1 = expr_add PLUS e2 = expr_mul { ExprBinary (e1, OpAdd, e2) }
  | e1 = expr_add PLUSPLUS e2 = expr_mul { ExprBinary (e1, OpConcat, e2) }
  | e1 = expr_add MINUS e2 = expr_mul { ExprBinary (e1, OpSub, e2) }
  | e = expr_mul { e }


expr_mul:
  | e1 = expr_mul STAR e2 = expr_unary { ExprBinary (e1, OpMul, e2) }
  | e1 = expr_mul SLASH e2 = expr_unary { ExprBinary (e1, OpDiv, e2) }
  | e1 = expr_mul PERCENT e2 = expr_unary { ExprBinary (e1, OpMod, e2) }
  | e = expr_unary { e }

expr_unary:
  | MINUS e = expr_unary %prec UMINUS { ExprUnary (OpNeg, e) }
  | BANG e = expr_unary { ExprUnary (OpNot, e) }
  | TILDE e = expr_unary { ExprUnary (OpBitNot, e) }
  | AMP e = expr_unary %prec UREF { ExprUnary (OpRef, e) }
  | STAR e = expr_unary %prec UDEREF { ExprUnary (OpDeref, e) }
  | e = expr_postfix { e }

expr_postfix:
  /* field_name used here so that `r.handle` parses even though `handle` is a
     keyword; field access is unambiguous after DOT. */
  | e = expr_postfix DOT field = field_name { ExprField (e, field) }
  | e = expr_postfix DOT n = INT { ExprTupleIndex (e, n) }
  | e = expr_postfix LBRACKET idx = expr RBRACKET { ExprIndex (e, idx) }
  | e = expr_postfix LPAREN args = separated_list(COMMA, expr) RPAREN { ExprApp (e, args) }
  | e = expr_postfix BACKSLASH field = field_name { ExprRowRestrict (e, field) }
  | e = expr_postfix QUESTION { ExprTry { et_body = { blk_stmts = []; blk_expr = Some e };
                                          et_catch = None; et_finally = None } }
  | e = expr_primary { e }

expr_primary:
  /* Literals */
  | n = INT { ExprLit (LitInt (n, mk_span $startpos $endpos)) }
  | f = FLOAT { ExprLit (LitFloat (f, mk_span $startpos $endpos)) }
  | c = CHAR { ExprLit (LitChar (c, mk_span $startpos $endpos)) }
  | s = STRING { ExprLit (LitString (s, mk_span $startpos $endpos)) }
  | TRUE { ExprLit (LitBool (true, mk_span $startpos $endpos)) }
  | FALSE { ExprLit (LitBool (false, mk_span $startpos $endpos)) }

  /* Identifiers */
  | name = lower_ident { ExprVar (mk_ident name $startpos $endpos) }
  /* Struct literal: `Point { x: v, y: w }`.  Must come before the plain
     upper_ident production so Menhir shifts LBRACE rather than reducing
     upper_ident to ExprVar when the next token is LBRACE. */
  | _ty = upper_ident LBRACE b = expr_record_body RBRACE
    { ExprRecord { er_fields = fst b; er_spread = snd b } }
  | name = upper_ident { ExprVar (mk_ident name $startpos $endpos) }
  | ty = upper_ident COLONCOLON variant = upper_ident
    { ExprVariant (mk_ident ty $startpos(ty) $endpos(ty),
                   mk_ident variant $startpos(variant) $endpos(variant)) }

  /* Grouping and tuples */
  | LPAREN RPAREN { ExprLit (LitUnit (mk_span $startpos $endpos)) }
  | LPAREN e = expr RPAREN { e }
  | LPAREN e = expr COMMA es = separated_nonempty_list(COMMA, expr) RPAREN
    { ExprTuple (e :: es) }

  /* Arrays */
  | LBRACKET es = separated_list(COMMA, expr) RBRACKET { ExprArray es }

  /* Records — use a recursive rule (expr_record_body / expr_record_rest) to
     avoid the LALR(1) greedy-separator conflict that arises when a ROW_VAR
     spread like `..record` follows a COMMA that `separated_list` has already
     consumed expecting another record_field. */
  | LBRACE b = expr_record_body RBRACE
    { ExprRecord { er_fields = fst b; er_spread = snd b } }

  /* Block */
  | blk = block { ExprBlock blk }

  /* Control flow */
  | IF cond = expr then_blk = block else_part = else_part?
    { ExprIf { ei_cond = cond; ei_then = ExprBlock then_blk; ei_else = else_part } }

  | MATCH scrutinee = expr LBRACE arms = list(match_arm) RBRACE
    { ExprMatch { em_scrutinee = scrutinee; em_arms = arms } }

  /* Let expressions — ADR-007 hybrid surface syntax for quantities.
     Four production paths cover the cross product of {C-attr, B-sugar, neither}
     × {with type, without type}. The C-attribute form (`@linear let x = e`)
     and the B-sugar form (`let x :1 = e`) cannot both appear on the same let
     binder; they are alternative spellings, not stackable annotations. */
  | LET mut_ = MUT? pat = pattern ty = type_annotation? EQ value = expr
    { ExprLet { el_mut = Option.is_some mut_; el_quantity = None; el_pat = pat;
                el_ty = ty; el_value = value; el_body = None } }
  | qty_attr = quantity_attr LET mut_ = MUT? pat = pattern ty = type_annotation? EQ value = expr
    { ExprLet { el_mut = Option.is_some mut_; el_quantity = Some qty_attr; el_pat = pat;
                el_ty = ty; el_value = value; el_body = None } }
  | LET mut_ = MUT? pat = pattern qty = quantity_b_sugar ty = type_annotation? EQ value = expr
    { ExprLet { el_mut = Option.is_some mut_; el_quantity = Some qty; el_pat = pat;
                el_ty = ty; el_value = value; el_body = None } }

  /* Lambda */
  | PIPE params = separated_list(COMMA, lambda_param) PIPE body = expr
    { ExprLambda { elam_params = params; elam_ret_ty = None; elam_body = body } }
  | PIPE params = separated_list(COMMA, lambda_param) PIPE ARROW ret = type_expr body = block
    { ExprLambda { elam_params = params; elam_ret_ty = Some ret; elam_body = ExprBlock body } }

  /* Return */
  | RETURN e = expr? { ExprReturn e }

  /* Handle */
  | HANDLE body = expr LBRACE handlers = list(handler_arm) RBRACE
    { ExprHandle { eh_body = body; eh_handlers = handlers } }

  /* Resume */
  | RESUME e = expr? { ExprResume e }

  /* Try/catch/finally */
  | TRY body = block catch = try_catch? finally = try_finally?
    { ExprTry { et_body = body; et_catch = catch; et_finally = finally } }

  /* Unsafe operations */
  | UNSAFE LBRACE ops = list(unsafe_op) RBRACE
    { ExprUnsafe ops }

/* expr_record_body / expr_record_rest: recursive parse of `{ f:v, ..spread }`
   record expressions.  Spread is lexed as ROW_VAR when it is a bare
   identifier (e.g. `..record`), or starts with DOTDOT when it is an
   arbitrary expression (e.g. `..{ a: 1 }`).  We handle both in
   expr_record_spread and use a recursive structure to avoid the greedy-
   separator conflict. */

expr_record_body:
  (* empty record: {} *)
  |
    { ([], None) }
  (* spread-only: { ..var } or { ..expr } *)
  | sp = expr_record_spread
    { ([], Some sp) }
  (* field possibly followed by more: { f: v, ... } *)
  | field = record_field rest = expr_record_rest
    { (field :: fst rest, snd rest) }

expr_record_rest:
  (* no more fields, no spread *)
  |
    { ([], None) }
  (* trailing comma only *)
  | COMMA
    { ([], None) }
  (* spread after comma *)
  | COMMA sp = expr_record_spread
    { ([], Some sp) }
  (* another field after comma *)
  | COMMA field = record_field rest = expr_record_rest
    { (field :: fst rest, snd rest) }

expr_record_spread:
  (* `..ident` — lexed as a single ROW_VAR token *)
  | rv = ROW_VAR { ExprVar (mk_ident rv $startpos $endpos) }
  (* `..expr` — DOTDOT consumed, then an arbitrary expression *)
  | DOTDOT e = expr { e }

record_field:
  | name = field_name COLON value = expr { (name, Some value) }
  | name = field_name { (name, None) }

type_annotation:
  | COLON ty = type_expr { ty }

else_part:
  | ELSE IF cond = expr then_blk = block else_part = else_part?
    { ExprIf { ei_cond = cond; ei_then = ExprBlock then_blk; ei_else = else_part } }
  | ELSE blk = block { ExprBlock blk }

lambda_param:
  | name = ident { { p_quantity = None; p_ownership = None; p_name = name;
                     p_ty = TyHole } }
  | name = ident COLON ty = type_expr
    { { p_quantity = None; p_ownership = None; p_name = name; p_ty = ty } }
  /* ADR-007 Option C: @linear x or @linear x: Type */
  | qty_attr = quantity_attr name = ident
    { { p_quantity = Some qty_attr; p_ownership = None; p_name = name; p_ty = TyHole } }
  | qty_attr = quantity_attr name = ident COLON ty = type_expr
    { { p_quantity = Some qty_attr; p_ownership = None; p_name = name; p_ty = ty } }

match_arm:
  | pat = pattern guard = match_guard? FAT_ARROW body = expr COMMA?
    { { ma_pat = pat; ma_guard = guard; ma_body = body } }

match_guard:
  | IF cond = expr { cond }

handler_arm:
  | RETURN LPAREN pat = pattern RPAREN FAT_ARROW body = expr COMMA?
    { HandlerReturn (pat, body) }
  | name = ident LPAREN pats = separated_list(COMMA, pattern) RPAREN FAT_ARROW body = expr COMMA?
    { HandlerOp (name, pats, body) }

try_catch:
  | CATCH LBRACE arms = list(match_arm) RBRACE { arms }

try_finally:
  | FINALLY blk = block { blk }

unsafe_op:
  /* UnsafeRead: read(ptr); */
  | name = lower_ident LPAREN ptr = expr RPAREN SEMICOLON
    { match name with
      | "read" -> UnsafeRead ptr
      | "write" -> failwith "write requires two arguments"
      | "offset" -> failwith "offset requires two arguments"
      | _ -> failwith ("unknown unsafe operation: " ^ name) }

  /* UnsafeWrite: write(ptr, value); */
  | name = lower_ident LPAREN ptr = expr COMMA value = expr RPAREN SEMICOLON
    { match name with
      | "write" -> UnsafeWrite (ptr, value)
      | "offset" -> UnsafeOffset (ptr, value)
      | _ -> failwith ("unknown unsafe operation: " ^ name) }

  /* UnsafeForget: forget(expr); */
  | FORGET LPAREN e = expr RPAREN SEMICOLON
    { UnsafeForget e }

  /* UnsafeTransmute: transmute<From, To>(expr); */
  | TRANSMUTE LT from_ty = type_expr COMMA to_ty = type_expr GT LPAREN e = expr RPAREN SEMICOLON
    { UnsafeTransmute (from_ty, to_ty, e) }

/* ========== Statements ========== */

(* Self-delimiting expressions that can serve as the final expression in a
   block without a trailing semicolon.  Because they all end with '}', the
   LR(1) parser can distinguish "this was the last expression" (followed by
   the outer '}') from "this was a statement" (which would need ';'). *)
block_terminator:
  | IF cond = expr then_blk = block else_part = else_part?
    { ExprIf { ei_cond = cond; ei_then = ExprBlock then_blk; ei_else = else_part } }
  | MATCH scrutinee = expr LBRACE arms = list(match_arm) RBRACE
    { ExprMatch { em_scrutinee = scrutinee; em_arms = arms } }
  | inner = block
    { ExprBlock inner }

block:
  | LBRACE stmts = list(stmt) RBRACE
    { { blk_stmts = stmts; blk_expr = None } }
  | LBRACE stmts = list(stmt) final = block_terminator RBRACE
    { { blk_stmts = stmts; blk_expr = Some final } }
  | LBRACE stmts = stmt_list_nonempty_trailing_expr RBRACE
    { { blk_stmts = fst stmts; blk_expr = Some (snd stmts) } }
  | LBRACE e = expr RBRACE
    { { blk_stmts = []; blk_expr = Some e } }

stmt_list_nonempty_trailing_expr:
  | s = stmt rest = stmt_list_nonempty_trailing_expr { (s :: fst rest, snd rest) }
  | s = stmt e = expr { ([s], e) }

stmt:
  | LET mut_ = MUT? pat = pattern ty = type_annotation? EQ value = expr SEMICOLON
    { StmtLet { sl_mut = Option.is_some mut_; sl_quantity = None;
                sl_pat = pat; sl_ty = ty; sl_value = value } }
  | qty_attr = quantity_attr LET mut_ = MUT? pat = pattern ty = type_annotation? EQ value = expr SEMICOLON
    { StmtLet { sl_mut = Option.is_some mut_; sl_quantity = Some qty_attr;
                sl_pat = pat; sl_ty = ty; sl_value = value } }
  | LET mut_ = MUT? pat = pattern qty = quantity_b_sugar ty = type_annotation? EQ value = expr SEMICOLON
    { StmtLet { sl_mut = Option.is_some mut_; sl_quantity = Some qty;
                sl_pat = pat; sl_ty = ty; sl_value = value } }
  | e = expr SEMICOLON { StmtExpr e }
  | IF cond = expr then_blk = block else_part = else_part?
    { StmtExpr (ExprIf { ei_cond = cond; ei_then = ExprBlock then_blk; ei_else = else_part }) }
  | lhs = expr_postfix EQ rhs = expr SEMICOLON { StmtAssign (lhs, AssignEq, rhs) }
  | lhs = expr_postfix PLUSEQ rhs = expr SEMICOLON { StmtAssign (lhs, AssignAdd, rhs) }
  | lhs = expr_postfix MINUSEQ rhs = expr SEMICOLON { StmtAssign (lhs, AssignSub, rhs) }
  | lhs = expr_postfix STAREQ rhs = expr SEMICOLON { StmtAssign (lhs, AssignMul, rhs) }
  | lhs = expr_postfix SLASHEQ rhs = expr SEMICOLON { StmtAssign (lhs, AssignDiv, rhs) }
  | WHILE cond = expr body = block { StmtWhile (cond, body) }
  | FOR pat = pattern IN iter = expr body = block { StmtFor (pat, iter, body) }

/* ========== Patterns ========== */

pattern:
  | p = pattern_or { p }

pattern_or:
  | p1 = pattern_or PIPE p2 = pattern_primary { PatOr (p1, p2) }
  | p = pattern_primary { p }

pattern_primary:
  | UNDERSCORE { PatWildcard (mk_span $startpos $endpos) }
  | name = lower_ident { PatVar (mk_ident name $startpos $endpos) }
  | n = INT { PatLit (LitInt (n, mk_span $startpos $endpos)) }
  | c = CHAR { PatLit (LitChar (c, mk_span $startpos $endpos)) }
  | s = STRING { PatLit (LitString (s, mk_span $startpos $endpos)) }
  | TRUE { PatLit (LitBool (true, mk_span $startpos $endpos)) }
  | FALSE { PatLit (LitBool (false, mk_span $startpos $endpos)) }
  | name = upper_ident { PatCon (mk_ident name $startpos $endpos, []) }
  | name = upper_ident LPAREN pats = separated_list(COMMA, pattern) RPAREN
    { PatCon (mk_ident name $startpos(name) $endpos(name), pats) }
  | LPAREN RPAREN { PatTuple [] }
  | LPAREN p = pattern RPAREN { p }
  | LPAREN p = pattern COMMA ps = separated_nonempty_list(COMMA, pattern) RPAREN
    { PatTuple (p :: ps) }
  | LBRACE fields = separated_list(COMMA, pattern_field) rest = pattern_rest? RBRACE
    { PatRecord (fields, Option.is_some rest) }
  | name = lower_ident AT p = pattern_primary
    { PatAs (mk_ident name $startpos(name) $endpos(name), p) }

pattern_field:
  | name = field_name COLON p = pattern { (name, Some p) }
  | name = field_name { (name, None) }

pattern_rest:
  | COMMA DOTDOT { () }

/* ========== Helpers ========== */

ident:
  | name = lower_ident { mk_ident name $startpos $endpos }
  | name = upper_ident { mk_ident name $startpos $endpos }

(* field_name extends ident with contextual keywords that are legal as struct/
   record field names.  Only keywords that do NOT introduce shift/reduce or
   reduce/reduce conflicts are listed here.  HANDLE is safe because it always
   requires `name COLON ty` in type-record context and `name: expr` in
   expression-record context; the surrounding COLON disambiguates. *)
field_name:
  | id = ident { id }
  | HANDLE { mk_ident "handle" $startpos $endpos }

lower_ident:
  | name = LOWER_IDENT { name }

upper_ident:
  | name = UPPER_IDENT { name }

%%
