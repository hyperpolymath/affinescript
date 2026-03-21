# Parser Implementation

The AffineScript parser converts tokens into a Concrete Syntax Tree (CST).

## Overview

**File**: `lib/parser.ml` (planned)
**Library**: [Menhir](http://gallium.inria.fr/~fpottier/menhir/) - LR(1) parser generator

## Grammar Structure

### Program

```menhir
program:
  | item* EOF { Program $1 }

item:
  | function_def    { Item_Fn $1 }
  | type_def        { Item_Type $1 }
  | struct_def      { Item_Struct $1 }
  | enum_def        { Item_Enum $1 }
  | trait_def       { Item_Trait $1 }
  | impl_block      { Item_Impl $1 }
  | effect_def      { Item_Effect $1 }
  | module_def      { Item_Mod $1 }
  | use_stmt        { Item_Use $1 }
```

### Expressions

```menhir
expr:
  | literal                           { Expr_Lit $1 }
  | IDENT                             { Expr_Var $1 }
  | expr LPAREN args RPAREN           { Expr_App ($1, $3) }
  | expr DOT IDENT                    { Expr_Field ($1, $3) }
  | expr DOT IDENT LPAREN args RPAREN { Expr_Method ($1, $3, $5) }
  | expr binop expr                   { Expr_Binary ($1, $2, $3) }
  | unop expr                         { Expr_Unary ($1, $2) }
  | IF expr block else_branch         { Expr_If ($2, $3, $4) }
  | MATCH expr LBRACE arms RBRACE     { Expr_Match ($2, $4) }
  | PIPE params PIPE expr             { Expr_Lambda ($2, $4) }
  | PIPE params PIPE block            { Expr_Lambda ($2, Block $4) }
  | block                             { Expr_Block $1 }
  | LPAREN expr RPAREN                { $2 }
  | LPAREN exprs RPAREN               { Expr_Tuple $2 }
  | LBRACKET exprs RBRACKET           { Expr_Array $2 }
  | LBRACE fields RBRACE              { Expr_Record $2 }
  | HANDLE expr LBRACE handlers RBRACE { Expr_Handle ($2, $4) }
  | expr QUESTION                     { Expr_Try $1 }
  | expr AS type_expr                 { Expr_Cast ($1, $3) }

binop:
  | PLUS     { Op_Add }
  | MINUS    { Op_Sub }
  | STAR     { Op_Mul }
  | SLASH    { Op_Div }
  | PERCENT  { Op_Mod }
  | EQ       { Op_Eq }
  | NE       { Op_Ne }
  | LT       { Op_Lt }
  | GT       { Op_Gt }
  | LE       { Op_Le }
  | GE       { Op_Ge }
  | AND      { Op_And }
  | OR       { Op_Or }
  | AMPERSAND { Op_BitAnd }
  | PIPE     { Op_BitOr }
  | CARET    { Op_BitXor }
  | SHL      { Op_Shl }
  | SHR      { Op_Shr }
  | PLUS_PLUS { Op_Concat }
  | PIPE_GT  { Op_Pipe }

unop:
  | MINUS    { Op_Neg }
  | NOT      { Op_Not }
  | TILDE    { Op_BitNot }
  | AMPERSAND { Op_Ref }
  | AMPERSAND MUT { Op_RefMut }
  | STAR     { Op_Deref }
```

### Types

```menhir
type_expr:
  | IDENT                                    { Type_Named $1 }
  | UPPER_IDENT                              { Type_Named $1 }
  | type_expr LBRACKET type_args RBRACKET    { Type_App ($1, $3) }
  | LPAREN type_exprs RPAREN                 { Type_Tuple $2 }
  | LPAREN RPAREN                            { Type_Unit }
  | type_expr ARROW type_expr                { Type_Fn ($1, $3) }
  | type_expr effect_arrow type_expr         { Type_FnEff ($1, $2, $3) }
  | OWN type_expr                            { Type_Own $2 }
  | REF type_expr                            { Type_Ref $2 }
  | MUT REF type_expr                        { Type_MutRef $3 }
  | LBRACE row_fields RBRACE                 { Type_Record $2 }
  | type_expr WHERE LPAREN expr RPAREN       { Type_Refined ($1, $4) }

effect_arrow:
  | MINUS LBRACE effects RBRACE ARROW        { $3 }

effects:
  | effect_list                              { Effects $1 }
  | effect_list COMMA ROW_VAR                { Effects_Row ($1, $3) }

row_fields:
  | field_list                               { Row_Closed $1 }
  | field_list COMMA ROW_VAR                 { Row_Open ($1, $3) }
```

### Patterns

```menhir
pattern:
  | UNDERSCORE                               { Pat_Wildcard }
  | IDENT                                    { Pat_Var $1 }
  | literal                                  { Pat_Lit $1 }
  | UPPER_IDENT                              { Pat_Ctor ($1, []) }
  | UPPER_IDENT LPAREN patterns RPAREN       { Pat_Ctor ($1, $3) }
  | LPAREN patterns RPAREN                   { Pat_Tuple $2 }
  | LBRACE pat_fields RBRACE                 { Pat_Record $2 }
  | pattern PIPE pattern                     { Pat_Or ($1, $3) }
  | IDENT AT pattern                         { Pat_Bind ($1, $3) }
  | pattern COLON type_expr                  { Pat_Typed ($1, $3) }
```

### Statements

```menhir
stmt:
  | LET pattern ASSIGN expr SEMICOLON        { Stmt_Let ($2, $4) }
  | LET pattern COLON type_expr ASSIGN expr SEMICOLON
                                             { Stmt_LetTyped ($2, $4, $6) }
  | expr ASSIGN expr SEMICOLON               { Stmt_Assign ($1, $3) }
  | expr SEMICOLON                           { Stmt_Expr $1 }
  | WHILE expr block                         { Stmt_While ($2, $3) }
  | FOR pattern IN expr block                { Stmt_For ($2, $4, $5) }
  | RETURN expr SEMICOLON                    { Stmt_Return (Some $2) }
  | RETURN SEMICOLON                         { Stmt_Return None }
  | BREAK SEMICOLON                          { Stmt_Break }
  | CONTINUE SEMICOLON                       { Stmt_Continue }

block:
  | LBRACE stmts expr? RBRACE                { Block ($2, $3) }
```

### Declarations

```menhir
function_def:
  | visibility? TOTAL? FN IDENT type_params? LPAREN params RPAREN
    return_type? where_clause? block
    { {
        vis = $1;
        total = $2 <> None;
        name = $4;
        type_params = $5;
        params = $7;
        ret = $9;
        where_clause = $10;
        body = $11;
      } }

struct_def:
  | visibility? STRUCT IDENT type_params? where_clause?
    LBRACE struct_fields RBRACE
    { { vis = $1; name = $3; type_params = $4; where_clause = $5; fields = $7 } }

enum_def:
  | visibility? ENUM IDENT type_params? where_clause?
    LBRACE variants RBRACE
    { { vis = $1; name = $3; type_params = $4; where_clause = $5; variants = $7 } }

trait_def:
  | visibility? TRAIT IDENT type_params? supertraits? where_clause?
    LBRACE trait_items RBRACE
    { { vis = $1; name = $3; type_params = $4; super = $5;
        where_clause = $6; items = $8 } }

impl_block:
  | IMPL type_params? type_expr FOR type_expr where_clause?
    LBRACE impl_items RBRACE
    { Impl_Trait { type_params = $2; trait_ = $3; for_ = $5;
                   where_clause = $6; items = $8 } }
  | IMPL type_params? type_expr where_clause?
    LBRACE impl_items RBRACE
    { Impl_Inherent { type_params = $2; type_ = $3;
                      where_clause = $4; items = $6 } }

effect_def:
  | visibility? EFFECT IDENT type_params? LBRACE effect_ops RBRACE
    { { vis = $1; name = $3; type_params = $4; ops = $6 } }
```

## Operator Precedence

Defined via Menhir precedence declarations:

```menhir
%left OR
%left AND
%left PIPE
%left CARET
%left AMPERSAND
%left EQ NE
%left LT GT LE GE
%left SHL SHR
%left PLUS MINUS PLUS_PLUS
%left STAR SLASH PERCENT
%right NOT TILDE UMINUS
%left DOT LBRACKET LPAREN
```

## Error Recovery

Menhir supports error recovery via:

```menhir
%on_error_reduce expr stmt

stmt:
  | error SEMICOLON { Stmt_Error }

expr:
  | error { Expr_Error }
```

## Concrete Syntax Tree

The CST preserves all syntactic information:

```ocaml
type cst_expr = {
  kind: cst_expr_kind;
  span: span;
}

and cst_expr_kind =
  | CST_Lit of literal
  | CST_Var of string
  | CST_Binary of cst_expr * binop * cst_expr
  | CST_Unary of unop * cst_expr
  | CST_App of cst_expr * cst_expr list
  | CST_Field of cst_expr * string
  | CST_Method of cst_expr * string * cst_expr list
  | CST_If of cst_expr * cst_block * cst_else option
  | CST_Match of cst_expr * cst_arm list
  | CST_Lambda of cst_param list * cst_expr
  | CST_Block of cst_block
  | CST_Tuple of cst_expr list
  | CST_Array of cst_expr list
  | CST_Record of cst_field list
  | CST_Handle of cst_expr * cst_handler list
  | CST_Error
```

## AST Transformation

The CST is transformed to a cleaner AST:

```ocaml
let rec desugar_expr (cst : cst_expr) : Ast.expr =
  match cst.kind with
  | CST_Lit lit -> { kind = E_Lit lit; span = cst.span }

  | CST_Binary (e1, Op_Pipe, e2) ->
      (* x |> f  becomes  f(x) *)
      desugar_expr { kind = CST_App (e2, [e1]); span = cst.span }

  | CST_If (cond, then_, None) ->
      (* if without else returns Unit *)
      { kind = E_If (desugar_expr cond,
                     desugar_block then_,
                     { kind = E_Unit; span = cst.span });
        span = cst.span }

  (* ... more cases ... *)
```

## Implementation Plan

### Phase 1: Core Expressions
- Literals, variables, operators
- Parentheses, tuples
- Function application
- Blocks

### Phase 2: Types
- Named types, type application
- Function types, tuple types
- Ownership modifiers

### Phase 3: Patterns
- Wildcards, variables, literals
- Constructors, tuples, records
- Or-patterns, binding

### Phase 4: Declarations
- Functions, structs, enums
- Type aliases
- Imports

### Phase 5: Advanced
- Traits and impls
- Effects and handlers
- Dependent types
- Row types

## Testing

```ocaml
let test_parse_expr () =
  let cst = parse_expr "1 + 2 * 3" in
  match cst.kind with
  | CST_Binary (_, Op_Add, CST_Binary (_, Op_Mul, _)) ->
      ()  (* Correct precedence *)
  | _ -> Alcotest.fail "wrong precedence"

let test_parse_function () =
  let cst = parse "fn add(x: Int, y: Int) -> Int { x + y }" in
  match cst with
  | [Item_Fn { name = "add"; params = [_; _]; _ }] -> ()
  | _ -> Alcotest.fail "wrong parse"
```

---

## See Also

- [Architecture](architecture.md) - Compiler overview
- [Lexer](lexer.md) - Previous phase
- [Type Checker](type-checker.md) - Next phase
- [AST](../../lib/ast.ml) - AST definitions
