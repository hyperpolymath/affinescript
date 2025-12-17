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
%token FN LET MUT OWN REF TYPE STRUCT ENUM TRAIT IMPL
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
%token PLUS MINUS STAR SLASH PERCENT
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
%left PLUS MINUS
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
  | vis = visibility? LET name = ident COLON ty = type_expr EQ value = expr SEMICOLON
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

kind_annotation:
  | COLON k = kind { k }

kind:
  | TYPE_K { KType }
  | NAT { KNat }
  | ROW { KRow }
  | k1 = kind ARROW k2 = kind { KArrow (k1, k2) }
  | LPAREN k = kind RPAREN { k }

quantity:
  | ZERO { QZero }
  | ONE { QOne }
  | OMEGA { QOmega }

param:
  | qty = quantity? own = ownership? name = ident COLON ty = type_expr
    { { p_quantity = qty; p_ownership = own; p_name = name; p_ty = ty } }

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
  | vis = visibility? TYPE name = ident type_params = type_params? EQ ty = type_expr SEMICOLON
    { { td_vis = Option.value vis ~default:Private;
        td_name = name;
        td_type_params = Option.value type_params ~default:[];
        td_body = TyAlias ty } }
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
  | vis = visibility? name = ident COLON ty = type_expr
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
    { TyArrow (arg, ret, None) }
  | arg = type_expr_primary MINUS LBRACE eff = effect_expr RBRACE ARROW ret = type_expr_arrow
    { TyArrow (arg, ret, Some eff) }
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
  | LBRACE fields = separated_list(COMMA, row_field) row = row_tail? RBRACE
    { TyRecord (fields, row) }
  /* Built-in types */
  | NAT { TyCon (mk_ident "Nat" $startpos $endpos) }
  | INT_T { TyCon (mk_ident "Int" $startpos $endpos) }
  | BOOL { TyCon (mk_ident "Bool" $startpos $endpos) }
  | FLOAT_T { TyCon (mk_ident "Float" $startpos $endpos) }
  | STRING_T { TyCon (mk_ident "String" $startpos $endpos) }
  | CHAR_T { TyCon (mk_ident "Char" $startpos $endpos) }
  | NEVER { TyCon (mk_ident "Never" $startpos $endpos) }

row_tail:
  | COMMA rv = ROW_VAR { mk_ident rv $startpos(rv) $endpos(rv) }

row_field:
  | name = ident COLON ty = type_expr
    { { rf_name = name; rf_ty = ty } }

type_arg:
  | ty = type_expr { TyArg ty }
  | n = nat_expr { NatArg n }

nat_expr:
  | n = INT { NatLit (n, mk_span $startpos $endpos) }
  | name = lower_ident { NatVar (mk_ident name $startpos $endpos) }
  | n1 = nat_expr PLUS n2 = nat_expr { NatAdd (n1, n2) }
  | n1 = nat_expr MINUS n2 = nat_expr { NatSub (n1, n2) }
  | n1 = nat_expr STAR n2 = nat_expr { NatMul (n1, n2) }
  | LPAREN n = nat_expr RPAREN { n }

/* ========== Effects ========== */

effect_decl:
  | vis = visibility? EFFECT name = ident type_params = type_params?
    LBRACE ops = list(effect_op_decl) RBRACE
    { { ed_vis = Option.value vis ~default:Private;
        ed_name = name;
        ed_type_params = Option.value type_params ~default:[];
        ed_ops = ops } }

effect_op_decl:
  | FN name = ident LPAREN params = separated_list(COMMA, param) RPAREN ret = return_type? SEMICOLON
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
    where_clause = where_clause?
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
  | name = ident FOR { Some { tr_name = name; tr_args = [] } }
  | name = ident LBRACKET args = separated_list(COMMA, type_arg) RBRACKET FOR
    { Some { tr_name = name; tr_args = args } }

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
    { ExprLet { el_mut = false; el_pat = PatVar (mk_ident "_" $startpos(lhs) $endpos(lhs));
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
  | e = expr_postfix DOT field = ident { ExprField (e, field) }
  | e = expr_postfix DOT n = INT { ExprTupleIndex (e, n) }
  | e = expr_postfix LBRACKET idx = expr RBRACKET { ExprIndex (e, idx) }
  | e = expr_postfix LPAREN args = separated_list(COMMA, expr) RPAREN { ExprApp (e, args) }
  | e = expr_postfix BACKSLASH field = ident { ExprRowRestrict (e, field) }
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

  /* Records */
  | LBRACE fields = separated_list(COMMA, record_field) spread = record_spread? RBRACE
    { ExprRecord { er_fields = fields; er_spread = spread } }

  /* Block */
  | blk = block { ExprBlock blk }

  /* Control flow */
  | IF cond = expr then_blk = block else_part = else_part?
    { ExprIf { ei_cond = cond; ei_then = ExprBlock then_blk; ei_else = else_part } }

  | MATCH scrutinee = expr LBRACE arms = list(match_arm) RBRACE
    { ExprMatch { em_scrutinee = scrutinee; em_arms = arms } }

  /* Let expressions */
  | LET mut_ = MUT? pat = pattern ty = type_annotation? EQ value = expr
    { ExprLet { el_mut = Option.is_some mut_; el_pat = pat;
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

record_field:
  | name = ident COLON value = expr { (name, Some value) }
  | name = ident { (name, None) }

record_spread:
  | COMMA DOTDOT e = expr { e }

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

/* ========== Statements ========== */

block:
  | LBRACE stmts = list(stmt) final = expr? RBRACE
    { { blk_stmts = stmts; blk_expr = final } }

stmt:
  | LET mut_ = MUT? pat = pattern ty = type_annotation? EQ value = expr SEMICOLON
    { StmtLet { sl_mut = Option.is_some mut_; sl_pat = pat; sl_ty = ty; sl_value = value } }
  | e = expr SEMICOLON { StmtExpr e }
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
  | name = ident COLON p = pattern { (name, Some p) }
  | name = ident { (name, None) }

pattern_rest:
  | COMMA DOTDOT { () }

/* ========== Helpers ========== */

ident:
  | name = lower_ident { mk_ident name $startpos $endpos }
  | name = upper_ident { mk_ident name $startpos $endpos }

lower_ident:
  | name = LOWER_IDENT { name }

upper_ident:
  | name = UPPER_IDENT { name }

%%
