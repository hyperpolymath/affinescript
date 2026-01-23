# AffineScript Complete Language Specification v2.0

## Purpose

This document fully specifies AffineScript, a programming language combining:
- **Affine types** (Rust-style ownership)
- **Dependent types** (types that depend on values)
- **Row polymorphism** (extensible records)
- **Extensible effect system** (user-defined effects with polymorphism)
- **WASM-first** compilation (no GC required)

This is a self-contained specification. Everything needed to implement the language is here.

---

# PART 1: LEXICAL GRAMMAR

## 1.1 Character Classes

```ebnf
letter        = 'a'..'z' | 'A'..'Z' ;
digit         = '0'..'9' ;
alpha_num     = letter | digit | '_' ;
hex_digit     = digit | 'a'..'f' | 'A'..'F' ;
bin_digit     = '0' | '1' ;
oct_digit     = '0'..'7' ;
```

## 1.2 Whitespace and Comments

```ebnf
whitespace    = ' ' | '\t' | '\n' | '\r' ;
line_comment  = '//' { any_char - '\n' } '\n' ;
block_comment = '/*' { any_char } '*/' ;
skip          = whitespace | line_comment | block_comment ;
```

## 1.3 Identifiers

```ebnf
lower_ident   = ('a'..'z') { alpha_num } ;
upper_ident   = ('A'..'Z') { alpha_num } ;
ident         = lower_ident | upper_ident ;

(* Row variables start with .. *)
row_var       = '..' lower_ident ;

(* Type variables are lowercase, optionally with prime *)
type_var      = lower_ident [ "'" ] ;
```

## 1.4 Keywords

```
fn        let       mut       own       ref
type      struct    enum      trait     impl
effect    handle    resume    handler
match     if        else      while     for
return    break     continue  in
true      false
where     total
module    use       pub       as
unsafe    assume    transmute forget
Nat       Int       Bool      Float     String
Type      Row
```

## 1.5 Literals

```ebnf
int_lit       = [ '-' ] digit { digit }
              | '0x' hex_digit { hex_digit }
              | '0b' bin_digit { bin_digit }
              | '0o' oct_digit { oct_digit } ;

float_lit     = [ '-' ] digit { digit } '.' digit { digit } [ exponent ] ;
exponent      = ('e' | 'E') [ '+' | '-' ] digit { digit } ;

char_lit      = "'" ( escape_seq | any_char - "'" ) "'" ;
string_lit    = '"' { escape_seq | any_char - '"' } '"' ;

escape_seq    = '\\' ( 'n' | 'r' | 't' | '\\' | '"' | "'" | '0'
              | 'x' hex_digit hex_digit
              | 'u' '{' hex_digit { hex_digit } '}' ) ;

bool_lit      = 'true' | 'false' ;
unit_lit      = '()' ;
```

## 1.6 Operators and Punctuation

```ebnf
(* Arithmetic *)
arith_op      = '+' | '-' | '*' | '/' | '%' ;

(* Comparison *)
cmp_op        = '==' | '!=' | '<' | '>' | '<=' | '>=' ;

(* Logical *)
logic_op      = '&&' | '||' | '!' ;

(* Bitwise *)
bit_op        = '&' | '|' | '^' | '~' | '<<' | '>>' ;

(* Assignment *)
assign_op     = '=' | '+=' | '-=' | '*=' | '/=' ;

(* Type-level *)
type_op       = '->' | '=>' | ':' | '/' | '\\' ;

(* Punctuation *)
punct         = '(' | ')' | '{' | '}' | '[' | ']'
              | ',' | ';' | '.' | '..' | '::' | '|' | '@' ;

(* Quantity annotations *)
quantity      = '0' | '1' | 'ω' | 'omega' ;
```

---

# PART 2: SYNTACTIC GRAMMAR

## 2.1 Program Structure

```ebnf
program       = [ module_decl ] { import_decl } { top_level } ;

top_level     = type_decl
              | fn_decl
              | trait_decl
              | impl_block
              | effect_decl
              | const_decl ;
```

## 2.2 Module System

```ebnf
module_decl   = 'module' module_path ';' ;
module_path   = upper_ident { '.' upper_ident } ;

import_decl   = 'use' import_path [ 'as' ident ] ';'
              | 'use' import_path '::' '{' import_list '}' ';'
              | 'use' import_path '::' '*' ';' ;

import_path   = [ '::' ] module_path [ '::' ident ] ;
import_list   = import_item { ',' import_item } ;
import_item   = ident [ 'as' ident ] ;

visibility    = [ 'pub' [ '(' pub_scope ')' ] ] ;
pub_scope     = 'crate' | 'super' | module_path ;
```

## 2.3 Type Declarations

```ebnf
type_decl     = visibility 'type' upper_ident [ type_params ] '=' type_body ;

type_params   = '[' type_param { ',' type_param } ']' ;
type_param    = [ quantity ] ident [ ':' kind ] ;

kind          = 'Type'
              | 'Nat'
              | 'Row'
              | 'Effect'
              | kind '->' kind ;

type_body     = type_expr                          (* alias *)
              | struct_body                        (* record *)
              | enum_body ;                        (* variant *)

struct_body   = '{' field_decl { ',' field_decl } [ ',' ] '}' ;
field_decl    = visibility ident ':' type_expr ;

enum_body     = [ '|' ] variant { '|' variant } ;
variant       = upper_ident [ '(' type_expr { ',' type_expr } ')' ]
                [ ':' type_expr ] ;                (* GADT return type *)
```

## 2.4 Type Expressions

```ebnf
type_expr     = type_atom
              | fn_type
              | dependent_fn_type
              | type_app
              | refined_type
              | row_type
              | owned_type
              | ref_type ;

type_atom     = 'Nat' | 'Int' | 'Bool' | 'Float' | 'String' | 'Type'
              | upper_ident
              | type_var
              | '(' type_expr ')'
              | '(' type_expr { ',' type_expr } ')' ;   (* tuple *)

(* Function types with effects *)
fn_type       = type_expr '->' type_expr [ '/' effects ] ;

(* Dependent function types: (x: T) -> U where U mentions x *)
dependent_fn_type = '(' [ quantity ] ident ':' type_expr ')' '->' type_expr [ '/' effects ] ;

(* Type application *)
type_app      = upper_ident '[' type_arg { ',' type_arg } ']' ;
type_arg      = type_expr | nat_expr ;

(* Refinement types *)
refined_type  = type_expr 'where' '(' predicate ')' ;
predicate     = pred_expr { ('&&' | '||') pred_expr } ;
pred_expr     = nat_expr cmp_op nat_expr
              | '!' pred_expr
              | '(' predicate ')' ;

(* Row polymorphic record types *)
row_type      = '{' row_fields '}' ;
row_fields    = field_type { ',' field_type } [ ',' row_var ]
              | row_var ;
field_type    = ident ':' type_expr ;

(* Ownership modifiers *)
owned_type    = 'own' type_expr ;
ref_type      = 'ref' type_expr
              | 'mut' type_expr ;

(* Effect sets *)
effects       = effect_term { '+' effect_term } ;
effect_term   = upper_ident [ '[' type_arg { ',' type_arg } ']' ]
              | effect_var ;
effect_var    = lower_ident ;
```

## 2.5 Natural Number Expressions (Type-Level)

```ebnf
nat_expr      = nat_atom
              | nat_expr '+' nat_expr
              | nat_expr '-' nat_expr
              | nat_expr '*' nat_expr
              | 'len' '(' ident ')'
              | 'sizeof' '[' type_expr ']' ;

nat_atom      = int_lit
              | ident                              (* type-level variable *)
              | '(' nat_expr ')' ;
```

## 2.6 Effect Declarations

```ebnf
effect_decl   = visibility 'effect' upper_ident [ type_params ] '{' { effect_op } '}' ;

effect_op     = 'fn' lower_ident '(' [ param_list ] ')' [ '->' type_expr ] ';' ;
```

## 2.7 Trait Declarations

```ebnf
trait_decl    = visibility 'trait' upper_ident [ type_params ] [ ':' trait_bounds ]
                '{' { trait_item } '}' ;

trait_bounds  = upper_ident { '+' upper_ident } ;

trait_item    = fn_sig ';'                         (* required method *)
              | fn_decl                            (* default method *)
              | 'type' upper_ident [ ':' kind ] [ '=' type_expr ] ';' ;  (* associated type *)

fn_sig        = visibility 'fn' lower_ident [ type_params ] '(' [ param_list ] ')'
                [ '->' type_expr ] [ '/' effects ] ;
```

## 2.8 Implementation Blocks

```ebnf
impl_block    = 'impl' [ type_params ] [ trait_ref 'for' ] type_expr
                [ where_clause ] '{' { impl_item } '}' ;

trait_ref     = upper_ident [ '[' type_arg { ',' type_arg } ']' ] ;

impl_item     = fn_decl
              | 'type' upper_ident '=' type_expr ';' ;
```

## 2.9 Function Declarations

```ebnf
fn_decl       = visibility [ 'total' ] 'fn' lower_ident
                [ type_params ]
                '(' [ param_list ] ')'
                [ '->' type_expr ]
                [ '/' effects ]
                [ where_clause ]
                fn_body ;

param_list    = param { ',' param } ;
param         = [ quantity ] [ ownership ] ident ':' type_expr ;
ownership     = 'own' | 'ref' | 'mut' ;

where_clause  = 'where' constraint { ',' constraint } ;
constraint    = predicate
              | type_var ':' trait_bound ;
trait_bound   = upper_ident [ '+' upper_ident ] ;

fn_body       = block | '=' expr ;
```

## 2.10 Expressions

```ebnf
expr          = let_expr
              | if_expr
              | match_expr
              | fn_expr
              | try_expr
              | handle_expr
              | block
              | return_expr
              | unsafe_expr
              | binary_expr ;

let_expr      = 'let' [ 'mut' ] pattern [ ':' type_expr ] '=' expr [ 'in' expr ] ;

if_expr       = 'if' expr block [ 'else' ( if_expr | block ) ] ;

match_expr    = 'match' expr '{' { match_arm } '}' ;
match_arm     = pattern [ 'if' expr ] '=>' expr [ ',' ] ;

fn_expr       = [ '|' param_list '|' | '||' ] [ '->' type_expr ] expr
              | 'fn' '(' [ param_list ] ')' [ '->' type_expr ] fn_body ;

try_expr      = 'try' block [ 'catch' '{' { catch_arm } '}' ] [ 'finally' block ] ;
catch_arm     = pattern '=>' expr [ ',' ] ;

handle_expr   = 'handle' expr 'with' '{' { handler_arm } '}' ;
handler_arm   = 'return' pattern '=>' expr [ ',' ]
              | lower_ident '(' [ pattern { ',' pattern } ] ')' '=>' expr [ ',' ] ;

block         = '{' { statement } [ expr ] '}' ;

return_expr   = 'return' [ expr ] ;

unsafe_expr   = 'unsafe' '{' { unsafe_op } '}' ;
unsafe_op     = expr '.' 'read' '(' ')'
              | expr '.' 'write' '(' expr ')'
              | expr '.' 'offset' '(' expr ')'
              | 'transmute' '[' type_expr ',' type_expr ']' '(' expr ')'
              | 'forget' '(' expr ')'
              | 'assume' '(' predicate ')' ;

binary_expr   = unary_expr { binary_op unary_expr } ;
binary_op     = arith_op | cmp_op | logic_op | bit_op ;

unary_expr    = [ unary_op ] postfix_expr ;
unary_op      = '-' | '!' | '~' | '&' | '*' ;

postfix_expr  = primary_expr { postfix } ;
postfix       = '.' ident                          (* field access *)
              | '.' int_lit                        (* tuple index *)
              | '[' expr ']'                       (* array index *)
              | '(' [ arg_list ] ')'               (* function call *)
              | '::' upper_ident                   (* enum variant *)
              | '\\' ident ;                       (* row restriction *)

arg_list      = expr { ',' expr } ;

primary_expr  = literal
              | ident
              | '(' expr ')'
              | '(' expr { ',' expr } ')'          (* tuple *)
              | '[' [ expr { ',' expr } ] ']'      (* array *)
              | '{' [ field_init { ',' field_init } ] [ '..' expr ] '}'  (* record *)
              | 'resume' '(' [ expr ] ')' ;        (* effect handler resume *)

field_init    = ident [ ':' expr ] ;               (* shorthand: {x} means {x: x} *)

literal       = int_lit | float_lit | char_lit | string_lit | bool_lit | unit_lit ;
```

## 2.11 Patterns

```ebnf
pattern       = '_'                                (* wildcard *)
              | ident                              (* binding *)
              | literal                            (* literal match *)
              | upper_ident [ '(' pattern { ',' pattern } ')' ]   (* variant *)
              | '(' pattern { ',' pattern } ')'    (* tuple *)
              | '{' field_pat { ',' field_pat } [ ',' '..' ] '}'  (* record *)
              | pattern '|' pattern                (* or-pattern *)
              | ident '@' pattern ;                (* binding with sub-pattern *)

field_pat     = ident [ ':' pattern ] ;
```

## 2.12 Statements

```ebnf
statement     = let_stmt
              | expr_stmt
              | assign_stmt
              | while_stmt
              | for_stmt ;

let_stmt      = let_expr ';' ;
expr_stmt     = expr ';' ;
assign_stmt   = postfix_expr assign_op expr ';' ;
while_stmt    = 'while' expr block ;
for_stmt      = 'for' pattern 'in' expr block ;
```

---

# PART 3: TYPE SYSTEM

## 3.1 Judgement Forms

```
Γ ⊢ e : τ / ε        Expression e has type τ with effects ε in context Γ
Γ ⊢ τ : κ            Type τ has kind κ in context Γ
Γ ⊢ e ↝ v            Expression e evaluates to value v (for type-level computation)
Γ ⊢ P true           Predicate P is satisfied
σ : Γ → Δ            Substitution σ maps context Γ to context Δ
```

## 3.2 Kinds

```
κ ::= Type           Proper types
    | Nat            Natural numbers (for dependent types)
    | Row            Row kinds (for row polymorphism)
    | Effect         Effect kinds
    | κ → κ          Kind-level functions
```

## 3.3 Quantities (from QTT)

```
q ::= 0              Erased (compile-time only, not present at runtime)
    | 1              Linear (must be used exactly once)
    | ω              Unrestricted (can be used any number of times)
```

**Quantity algebra:**
```
0 + q = q
1 + 1 = ω
1 + ω = ω
ω + ω = ω

0 * q = 0
1 * q = q
ω * ω = ω
```

**Erased (quantity 0) semantics:**

Erased values exist only at compile time. They are computed during type checking and deleted before code generation.

```affinescript
// n is erased - no runtime cost
fn replicate[0 n: Nat, T](value: T) -> Vec[n, T]

// Valid: n only appears in types
fn ok[0 n: Nat](v: Vec[n, Int]) -> Vec[n, Int] { v }

// Invalid: cannot use erased value at runtime
fn bad[0 n: Nat]() -> Nat { n }  // ERROR
```

## 3.4 Effects

Effects are user-defined and extensible:

```
ε ::= ∅              Pure (no effects)
    | E[τ₁,...,τₙ]   Named effect with type arguments
    | α              Effect variable
    | ε + ε          Effect union
```

**Effect algebra:**
```
ε + ∅ = ε
ε + ε = ε
ε₁ + ε₂ = ε₂ + ε₁
```

**Totality and Divergence:**

- Functions are **partial by default** - they may diverge (not terminate)
- `total` functions must provably terminate and cannot have `Div` effect
- Non-total functions implicitly include `Div` in their effect set

```affinescript
// Default: partial, may diverge
fn serverLoop() -> Never {  // Implicitly / Div
  loop { handleRequest(); }
}

// Explicit: must prove termination
total fn factorial(n: Nat) -> Nat / Pure {
  match n {
    0 => 1,
    _ => n * factorial(n - 1)  // Structural recursion - OK
  }
}
```

## 3.5 Type Syntax

```
τ ::= α                          Type variable
    | C                          Type constructor (Nat, Int, Bool, etc.)
    | τ τ                        Type application
    | (q x : τ₁) → τ₂ / ε        Dependent function type with quantity and effects
    | {ρ}                        Record type (row type)
    | own τ                      Owned type
    | ref τ                      Borrowed reference
    | mut τ                      Mutable reference
    | τ where P                  Refinement type
    | ∀(α : κ). τ                Universal quantification

ρ ::= ∅                          Empty row
    | ℓ : τ                      Single field
    | ρ , ρ                      Row concatenation
    | ρ \ ℓ                      Row restriction (remove field)
    | r                          Row variable
```

## 3.6 Core Typing Rules

### Variables
```
────────────────────── (T-Var)
Γ, x :q τ ⊢ x : τ / ∅

(consumes q uses of x)
```

### Functions
```
Γ, x :q τ₁ ⊢ e : τ₂ / ε
─────────────────────────────────────────── (T-Lam)
Γ ⊢ fn(q x: τ₁) => e : (q x : τ₁) → τ₂ / ε

Γ ⊢ e₁ : (q x : τ₁) → τ₂ / ε₁    Γ ⊢ e₂ : τ₁ / ε₂
───────────────────────────────────────────────────── (T-App)
Γ ⊢ e₁(e₂) : τ₂[x ↦ e₂] / ε₁ + ε₂
```

### Let Bindings
```
Γ ⊢ e₁ : τ₁ / ε₁    Γ, x :q τ₁ ⊢ e₂ : τ₂ / ε₂
───────────────────────────────────────────────── (T-Let)
Γ ⊢ let x = e₁ in e₂ : τ₂ / ε₁ + ε₂
```

### Records (Row Polymorphism)
```
Γ ⊢ e₁ : τ₁ / ε₁  ...  Γ ⊢ eₙ : τₙ / εₙ
────────────────────────────────────────────────────── (T-Record)
Γ ⊢ {ℓ₁: e₁, ..., ℓₙ: eₙ} : {ℓ₁: τ₁, ..., ℓₙ: τₙ} / ε₁ + ... + εₙ

Γ ⊢ e : {ℓ: τ, ρ} / ε
─────────────────────── (T-Field)
Γ ⊢ e.ℓ : τ / ε

Γ ⊢ e : {ρ} / ε
──────────────────────────────────────── (T-Extend)
Γ ⊢ {ℓ: e', ..e} : {ℓ: τ', ρ} / ε + ε'

Γ ⊢ e : {ℓ: τ, ρ} / ε
─────────────────────────── (T-Restrict)
Γ ⊢ e \ ℓ : {ρ} / ε
```

### Row Polymorphism and Ownership

Ownership distributes uniformly over row variables:

```
Γ ⊢ e : ref {ℓ: τ, ..r} / ε
────────────────────────────── (T-BorrowRow)
All fields in ..r are immutably borrowed

Γ ⊢ e : mut {ℓ: τ, ..r} / ε
────────────────────────────── (T-MutBorrowRow)
All fields in ..r are mutably borrowed (exclusive)

Γ ⊢ e : own {ℓ: τ, ..r} / ε
────────────────────────────── (T-OwnRow)
All fields in ..r are owned
```

### Ownership
```
Γ ⊢ e : own τ / ε    (e is consumed)
───────────────────────────────────── (T-Move)
Γ ⊢ move e : own τ / ε

Γ ⊢ e : own τ / ε
─────────────────── (T-Borrow)
Γ ⊢ ref e : ref τ / ε

Γ ⊢ e : own τ / ε
─────────────────── (T-BorrowMut)
Γ ⊢ mut e : mut τ / ε
```

### Refinement Types
```
Γ ⊢ e : τ / ε    Γ ⊢ P[e] true
──────────────────────────────── (T-Refine-Intro)
Γ ⊢ e : τ where P / ε

Γ ⊢ e : τ where P / ε
─────────────────────── (T-Refine-Elim)
Γ ⊢ e : τ / ε
```

**Refinement predicate effects:**

Predicates may have effects but Pure is preferred. Effectful predicates emit a warning and are checked at runtime boundaries only.

```affinescript
// Pure - preferred, checked at compile time where possible
type PosInt = Int where (self > 0)

// Effectful - warning, checked at runtime boundaries
type Validated = String where (validate(self))  // If validate has IO
// warning[W0701]: refinement predicate has effects
```

### Dependent Types
```
Γ ⊢ n : Nat    n evaluates to a value
─────────────────────────────────────── (T-Vec)
Γ ⊢ Vec[n, τ] : Type

Γ ⊢ e : Vec[n + 1, τ] / ε
────────────────────────── (T-Head)
Γ ⊢ head(e) : τ / ε
```

### Effect Typing
```
Γ ⊢ e : τ / ε₁    ε₁ ⊆ ε₂
────────────────────────── (T-SubEffect)
Γ ⊢ e : τ / ε₂

effect E { fn op(x: τ₁) -> τ₂; }    Γ ⊢ e : τ₁ / ε
────────────────────────────────────────────────── (T-EffectOp)
Γ ⊢ E.op(e) : τ₂ / E + ε
```

---

# PART 4: OPERATIONAL SEMANTICS

## 4.1 Values

```
v ::= n                          Natural number
    | i                          Integer
    | f                          Float
    | true | false               Booleans
    | "s"                        String
    | ()                         Unit
    | fn(x: τ) => e              Function closure
    | {ℓ₁: v₁, ..., ℓₙ: vₙ}     Record value
    | C(v₁, ..., vₙ)             Variant value
    | [v₁, ..., vₙ]              Array value
    | ptr(a)                     Pointer (owned)
    | ref(a)                     Reference (borrowed)
```

## 4.2 Evaluation Contexts

```
E ::= []
    | E e                        Function position
    | v E                        Argument position
    | let x = E in e             Let binding
    | {ℓ₁: v₁, ..., ℓᵢ: E, ...}  Record construction
    | E.ℓ                        Field access
    | E[e]                       Array index (array position)
    | v[E]                       Array index (index position)
    | if E then e₁ else e₂       Conditional
    | match E { ... }            Match scrutinee
    | handle E with { ... }      Effect handler
```

## 4.3 Small-Step Reduction

```
(fn(x: τ) => e) v  →  e[x ↦ v]                    (β-reduction)

let x = v in e  →  e[x ↦ v]                       (let-reduction)

{..., ℓ: v, ...}.ℓ  →  v                          (field-access)

{ℓ₁: v₁, ..., ℓᵢ: vᵢ, ...} \ ℓᵢ  →                (row-restrict)
    {ℓ₁: v₁, ..., ℓᵢ₋₁: vᵢ₋₁, ℓᵢ₊₁: vᵢ₊₁, ...}

if true then e₁ else e₂  →  e₁                   (if-true)
if false then e₁ else e₂  →  e₂                  (if-false)

match C(v₁,...,vₙ) { C(x₁,...,xₙ) => e, ... }    (match)
  →  e[x₁ ↦ v₁, ..., xₙ ↦ vₙ]

[v₀, ..., vₙ][i]  →  vᵢ   (if 0 ≤ i ≤ n)         (array-index)
```

## 4.4 Effect Handling Semantics

```
handle (return v) with { return x => eᵣ, ... }   (handle-return)
  →  eᵣ[x ↦ v]

handle E[op(v)] with { ..., op(x) => eₒₚ, ... }  (handle-op)
  →  eₒₚ[x ↦ v, resume ↦ fn(y) => handle E[y] with {...}]
```

## 4.5 Ownership Semantics

```
State σ = Map<Address, (Value, Ownership)>
Ownership = Owned | Borrowed(n) | Moved

move(ptr(a), σ) where σ(a) = (v, Owned):
  → (v, σ[a ↦ (v, Moved)])

ref(ptr(a), σ) where σ(a) = (v, Owned):
  → (ref(a), σ[a ↦ (v, Borrowed(1))])

drop(ptr(a), σ) where σ(a) = (v, Owned):
  → ((), σ \ a)                                   (deallocate)

(* Use-after-move is a compile-time rejection *)
use(ptr(a), σ) where σ(a) = (_, Moved):
  → ERROR
```

---

# PART 5: UNSAFE OPERATIONS

## 5.1 Unsafe Block

The `unsafe` block permits exactly the following operations:

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Raw read | `ptr.read()` | Read value through raw pointer |
| Raw write | `ptr.write(v)` | Write value through raw pointer |
| Pointer arithmetic | `ptr.offset(n)` | Offset pointer by n elements |
| Transmute | `transmute[T, U](v)` | Reinterpret bits as different type |
| Forget | `forget(owned)` | Leak owned value without destructor |
| Assume | `assume(predicate)` | Assert refinement without proof |

## 5.2 Not Permitted in Unsafe

Even within `unsafe`, the following remain prohibited:

- Violating type safety (e.g., casting Int to function pointer)
- Accessing private fields of other modules
- Bypassing effect tracking
- Creating invalid enum variants
- Violating memory safety beyond raw pointer operations

## 5.3 Examples

```affinescript
fn dangerousRead(ptr: RawPtr[Int]) -> Int / Pure {
  unsafe { ptr.read() }
}

fn withAssumption(n: Nat) -> Nat where (n > 0) / Pure {
  unsafe {
    assume(n > 0)  // Programmer asserts this
  }
  n
}

fn reinterpret(x: u32) -> f32 / Pure {
  unsafe { transmute[u32, f32](x) }
}

fn leakResource(file: own File) -> () / Pure {
  unsafe { forget(file) }  // Memory leak, but no UB
}
```

---

# PART 6: MEMORY MODEL

## 6.1 Allocation Strategy

**Rule:** Values are stack-allocated unless they escape their scope.

| Situation | Allocation |
|-----------|------------|
| Local `let` binding, not returned | Stack |
| Returned from function | Heap (owned pointer) |
| Captured by closure | Heap (moved into closure struct) |
| Stored in growable collection | Heap |
| Recursive type (e.g., linked list) | Heap for recursive part |
| Behind `ref`/`mut` borrow | Inherits from referent |

```affinescript
fn stackOnly() -> Int / Pure {
  let x = 42;           // Stack
  let r = {a: 1, b: 2}; // Stack (doesn't escape)
  r.a + x
}

fn needsHeap() -> own {x: Int} / Pure {
  let r = {x: 42};  // Heap - returned (escapes)
  r
}
```

## 6.2 Explicit Boxing

```affinescript
type Box[T] = own { ptr: RawPtr[T] }

fn box[T](value: own T) -> own Box[T] / Pure {
  Box { ptr: heapAlloc(value) }
}

fn unbox[T](b: own Box[T]) -> own T / Pure {
  let value = unsafe { b.ptr.read() };
  unsafe { forget(b) };
  value
}
```

## 6.3 Drop Order

Values are dropped in reverse declaration order:

```affinescript
fn example() -> () / IO {
  let a = open("a.txt")?;  // Dropped third
  let b = open("b.txt")?;  // Dropped second
  let c = open("c.txt")?;  // Dropped first
  // implicit: drop(c); drop(b); drop(a);
}
```

## 6.4 WASM Mapping

| AffineScript | WASM |
|--------------|------|
| Stack values | WASM locals, passed by value |
| Heap values | WASM-GC `ref` types or linear memory |
| Borrows | WASM `ref` (compiler proves no use-after-free) |
| Closures | Struct with funcref + captured values |

---

# PART 7: STANDARD LIBRARY

## 7.1 Primitive Types

```affinescript
type Nat = /* built-in natural numbers, 0, 1, 2, ... */
type Int = /* built-in 64-bit signed integers */
type Float = /* built-in 64-bit floats */
type Bool = true | false
type String = /* built-in UTF-8 string */
type Char = /* built-in Unicode scalar value */
type Never = /* uninhabited type */
```

## 7.2 Standard Effects

```affinescript
effect IO {
  fn print(s: String);
  fn println(s: String);
  fn readLine() -> String;
  fn readFile(path: String) -> String;
  fn writeFile(path: String, content: String);
}

effect Exn[E] {
  fn throw(err: E) -> Never;
}

effect Async {
  fn await[T](future: Future[T]) -> T;
  fn spawn[T](f: () -> T / Async) -> Future[T];
}

effect State[S] {
  fn get() -> S;
  fn put(s: S);
  fn modify(f: S -> S);
}

// Div is special - indicates potential non-termination
// Cannot be handled, only discharged at program boundary
effect Div { }
```

## 7.3 Option and Result

```affinescript
type Option[T] =
  | None
  | Some(T)

type Result[T, E] =
  | Ok(T)
  | Err(E)

impl[T] Option[T] {
  fn map[U](self, f: T -> U / Pure) -> Option[U] / Pure {
    match self {
      None => None,
      Some(x) => Some(f(x))
    }
  }

  fn unwrapOr(self, default: T) -> T / Pure {
    match self {
      None => default,
      Some(x) => x
    }
  }
}

impl[T, E] Result[T, E] {
  fn map[U](self, f: T -> U / Pure) -> Result[U, E] / Pure {
    match self {
      Err(e) => Err(e),
      Ok(x) => Ok(f(x))
    }
  }

  fn mapErr[F](self, f: E -> F / Pure) -> Result[T, F] / Pure {
    match self {
      Ok(x) => Ok(x),
      Err(e) => Err(f(e))
    }
  }
}
```

## 7.4 Length-Indexed Vector

```affinescript
type Vec[n: Nat, T: Type] =
  | Nil : Vec[0, T]
  | Cons(head: T, tail: Vec[n, T]) : Vec[n + 1, T]

total fn head[n: Nat, T](v: Vec[n + 1, T]) -> T / Pure {
  match v {
    Cons(h, _) => h
  }
}

total fn tail[n: Nat, T](v: Vec[n + 1, T]) -> Vec[n, T] / Pure {
  match v {
    Cons(_, t) => t
  }
}

total fn append[n: Nat, m: Nat, T](a: Vec[n, T], b: Vec[m, T]) -> Vec[n + m, T] / Pure {
  match a {
    Nil => b,
    Cons(h, t) => Cons(h, append(t, b))
  }
}

total fn map[n: Nat, A, B](v: Vec[n, A], f: A -> B / Pure) -> Vec[n, B] / Pure {
  match v {
    Nil => Nil,
    Cons(h, t) => Cons(f(h), map(t, f))
  }
}

total fn zip[n: Nat, A, B](a: Vec[n, A], b: Vec[n, B]) -> Vec[n, (A, B)] / Pure {
  match (a, b) {
    (Nil, Nil) => Nil,
    (Cons(x, xs), Cons(y, ys)) => Cons((x, y), zip(xs, ys))
  }
}
```

## 7.5 Core Traits

```affinescript
trait Eq {
  fn eq(ref self, other: ref Self) -> Bool / Pure;

  fn neq(ref self, other: ref Self) -> Bool / Pure {
    !self.eq(other)
  }
}

trait Ord: Eq {
  fn cmp(ref self, other: ref Self) -> Ordering / Pure;

  fn lt(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) == Less
  }

  fn le(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) != Greater
  }

  fn gt(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) == Greater
  }

  fn ge(ref self, other: ref Self) -> Bool / Pure {
    self.cmp(other) != Less
  }
}

type Ordering = Less | Equal | Greater

trait Show {
  fn show(ref self) -> String / Pure;
}

trait Clone {
  fn clone(ref self) -> Self / Pure;
}

trait Default {
  fn default() -> Self / Pure;
}

trait Drop {
  fn drop(own self) -> () / Pure;
}

trait Functor[F: Type -> Type] {
  fn map[A, B](self: F[A], f: A -> B / Pure) -> F[B] / Pure;
}

trait Monad[M: Type -> Type]: Functor[M] {
  fn pure[A](value: A) -> M[A] / Pure;
  fn flatMap[A, B](self: M[A], f: A -> M[B] / Pure) -> M[B] / Pure;
}

trait Iterator {
  type Item;
  fn next(mut self) -> Option[Self::Item] / Pure;

  fn collect[C: FromIterator[Self::Item]](self) -> C / Pure {
    C.fromIter(self)
  }
}

trait FromIterator[T] {
  fn fromIter[I: Iterator where I::Item = T](iter: I) -> Self / Pure;
}
```

## 7.6 Owned Resources

```affinescript
type File = own { fd: Int }

fn open(path: ref String) -> Result[own File, IOError] / IO + Exn[IOError]

fn read(file: ref File, buf: mut [u8]) -> Result[Nat, IOError] / IO + Exn[IOError]

fn write(file: mut File, buf: ref [u8]) -> Result[Nat, IOError] / IO + Exn[IOError]

fn close(file: own File) -> Result[(), IOError] / IO

impl Drop for File {
  fn drop(own self) -> () / Pure {
    // Note: drop is Pure but may internally use IO
    // This is safe because drop is called deterministically
    let _ = close(self);
  }
}

fn withFile[T](
  path: ref String,
  action: (ref File) -> Result[T, IOError] / IO + Exn[IOError]
) -> Result[T, IOError] / IO + Exn[IOError] {
  let file = open(path)?;
  let result = action(ref file);
  close(file)?;
  result
}
```

## 7.7 Row-Polymorphic Functions

```affinescript
// Get a field from any record that has it
fn getX[..r](record: {x: Int, ..r}) -> Int / Pure {
  record.x
}

// Add a field to any record
fn withY[..r](record: {..r}, y: Int) -> {y: Int, ..r} / Pure {
  {y: y, ..record}
}

// Update a field
fn mapX[..r](record: {x: Int, ..r}, f: Int -> Int / Pure) -> {x: Int, ..r} / Pure {
  {x: f(record.x), ..record}
}

// Remove a field
fn removeX[..r](record: own {x: Int, ..r}) -> (own Int, own {..r}) / Pure {
  (record.x, record \ x)
}

// Merge two records (requires disjoint fields)
fn merge[..r, ..s](a: {..r}, b: {..s}) -> {..r, ..s} / Pure
  where (r disjoint s)
{
  {..a, ..b}
}
```

---

# PART 8: EXAMPLE PROGRAMS

## 8.1 Basic Arithmetic

```affinescript
fn add(a: Int, b: Int) -> Int / Pure {
  a + b
}

total fn factorial(n: Nat) -> Nat / Pure {
  match n {
    0 => 1,
    _ => n * factorial(n - 1)
  }
}

total fn fibonacci(n: Nat) -> Nat / Pure {
  match n {
    0 => 0,
    1 => 1,
    _ => fibonacci(n - 1) + fibonacci(n - 2)
  }
}
```

## 8.2 Safe Array Access

```affinescript
// Index must be less than length - proved at compile time
total fn safeGet[n: Nat, T](
  arr: ref Vec[n, T],
  i: Nat where (i < n)
) -> ref T / Pure {
  match (arr, i) {
    (Cons(h, _), 0) => ref h,
    (Cons(_, t), _) => safeGet(ref t, i - 1)
  }
}

// Split vector at compile-time known position
total fn splitAt[n: Nat, m: Nat, T](
  v: Vec[n + m, T]
) -> (Vec[n, T], Vec[m, T]) / Pure {
  match n {
    0 => (Nil, v),
    _ => {
      let Cons(h, t) = v;
      let (left, right) = splitAt[n - 1, m, T](t);
      (Cons(h, left), right)
    }
  }
}
```

## 8.3 Effect Polymorphism

```affinescript
// Map is polymorphic in effects
fn map[A, B, E](xs: List[A], f: A -> B / E) -> List[B] / E {
  match xs {
    Nil => Nil,
    Cons(h, t) => Cons(f(h), map(t, f))
  }
}

// Can use with any effect
fn example() -> () / IO {
  let nums = [1, 2, 3];

  // E = Pure
  let doubled = map(nums, |n| n * 2);

  // E = IO
  let logged = map(nums, |n| { println(n.show()); n });
}

// Pure context requires Pure effects
total fn pureMap[A, B](xs: List[A], f: A -> B / Pure) -> List[B] / Pure {
  map(xs, f)
}
```

## 8.4 Resource Management

```affinescript
fn processFile(path: ref String) -> Result[String, IOError] / IO + Exn[IOError] {
  let file = open(path)?;
  try {
    let mut buf = [0u8; 1024];
    let n = read(ref file, mut buf)?;
    Ok(String.fromUtf8(buf[0..n]))
  } finally {
    close(file)?;
  }
}

// Using the RAII pattern
fn processFileSafe(path: ref String) -> Result[String, IOError] / IO + Exn[IOError] {
  withFile(path, |file| {
    let mut buf = [0u8; 1024];
    let n = read(file, mut buf)?;
    Ok(String.fromUtf8(buf[0..n]))
  })
}
```

## 8.5 State Effect

```affinescript
fn counter() -> Int / State[Int] {
  let current = State.get();
  State.put(current + 1);
  current
}

fn runCounter() -> (Int, Int, Int) / Pure {
  handle {
    let a = counter();
    let b = counter();
    let c = counter();
    (a, b, c)
  } with {
    return x => x,
    get() => resume(0),  // Initial state
    put(s) => resume(()),
  }
  // Note: This simple handler doesn't actually thread state
  // A real implementation would need more sophisticated handling
}
```

## 8.6 Combining Features

```affinescript
// Buffer with compile-time capacity tracking + ownership
type Buffer[capacity: Nat] = own {
  data: own [u8; capacity],
  len: Nat where (len <= capacity),
}

fn Buffer.new[cap: Nat]() -> own Buffer[cap] / Pure {
  Buffer {
    data: [0u8; cap],
    len: 0,
  }
}

fn Buffer.write[cap: Nat](
  own self: Buffer[cap],
  data: ref [u8],
) -> Result[own Buffer[cap], BufferFull] / Pure
  where (self.len + len(data) <= cap)
{
  copy(data, mut self.data[self.len..]);
  Ok(Buffer { data: self.data, len: self.len + len(data) })
}

fn Buffer.free[cap: Nat](own self: Buffer[cap]) -> () / Pure {
  // Ownership consumed - memory freed
}

// Usage
fn example() -> () / Pure {
  let buf = Buffer.new[1024]();
  let buf = buf.write(ref "hello")?;
  let buf = buf.write(ref " world")?;
  buf.free();
}
```

## 8.7 Traits and Generics

```affinescript
impl Eq for Int {
  fn eq(ref self, other: ref Int) -> Bool / Pure {
    *self == *other
  }
}

impl Ord for Int {
  fn cmp(ref self, other: ref Int) -> Ordering / Pure {
    if *self < *other { Less }
    else if *self > *other { Greater }
    else { Equal }
  }
}

impl Show for Int {
  fn show(ref self) -> String / Pure {
    intToString(*self)
  }
}

impl[T: Show] Show for Option[T] {
  fn show(ref self) -> String / Pure {
    match self {
      None => "None",
      Some(x) => "Some(" ++ x.show() ++ ")"
    }
  }
}

impl[T: Show, E: Show] Show for Result[T, E] {
  fn show(ref self) -> String / Pure {
    match self {
      Ok(x) => "Ok(" ++ x.show() ++ ")",
      Err(e) => "Err(" ++ e.show() ++ ")"
    }
  }
}

fn printAny[T: Show](value: ref T) -> () / IO {
  println(value.show())
}

fn sortBy[T, K: Ord](items: mut [T], key: (ref T) -> K / Pure) -> () / Pure {
  // Sorting implementation using Ord trait
}
```

---

# PART 9: COMPILATION TO WASM

## 9.1 Type Mapping

| AffineScript | WASM |
|--------------|------|
| `Nat`, `Int` | `i64` |
| `Float` | `f64` |
| `Bool` | `i32` (0 or 1) |
| `Char` | `i32` (Unicode scalar) |
| `String` | `(ref (array i8))` + length |
| `{x: T, y: U}` | `(ref (struct (field $x T') (field $y U')))` |
| `Vec[n, T]` | `(ref (array T'))` with length `n` |
| `own T` | `(ref T')` (ownership tracked statically) |
| `ref T` | `(ref T')` (borrowing tracked statically) |
| `T -> U / ε` | `(ref (struct (field $func funcref) (field $env ...)))` |
| `Option[T]` | Tagged union or nullable ref |
| `A \| B(T)` | Tagged struct hierarchy |

## 9.2 Ownership and Quantity Erasure

Ownership and quantities are erased at runtime. They're purely compile-time disciplines:

```affinescript
// Source
fn useFile(own file: File) -> () / IO { close(file) }

fn replicate[0 n: Nat, T](value: T) -> Vec[n, T] { ... }
```

```wat
;; WASM - no ownership marker, no n parameter
(func $useFile (param $file (ref $File))
  (call $close (local.get $file)))

(func $replicate (param $value (ref $T)) (result (ref $Vec))
  ;; n is erased, known at compile time
  ...)
```

## 9.3 Effect Compilation

Effects compile to either:
1. **Direct style:** For effects handled in the same function
2. **CPS transform:** For effects that cross function boundaries

```affinescript
// Source
fn greet() -> () / IO {
  println("Hello");
}
```

```wat
;; WASM - IO effect becomes direct call to runtime
(func $greet
  (call $runtime_println (... "Hello" ...)))
```

## 9.4 Row Polymorphism Compilation

Monomorphize at call sites:

```affinescript
fn getX[..r](rec: {x: Int, ..r}) -> Int { rec.x }

let a = getX({x: 1, y: 2})
let b = getX({x: 3, z: "hi"})
```

Compiles to specialized functions:

```wat
(func $getX_xy (param $rec (ref $struct_x_y)) (result i64)
  (struct.get $struct_x_y $x (local.get $rec)))

(func $getX_xz (param $rec (ref $struct_x_z)) (result i64)
  (struct.get $struct_x_z $x (local.get $rec)))
```

## 9.5 Closure Compilation

```affinescript
fn makeAdder(n: Int) -> (Int -> Int / Pure) / Pure {
  |x| x + n
}
```

```wat
(type $closure_adder (struct
  (field $func (ref $func_int_int))
  (field $n i64)))

(func $adder_impl (param $env (ref $closure_adder)) (param $x i64) (result i64)
  (i64.add
    (local.get $x)
    (struct.get $closure_adder $n (local.get $env))))

(func $makeAdder (param $n i64) (result (ref $closure_adder))
  (struct.new $closure_adder
    (ref.func $adder_impl)
    (local.get $n)))
```

---

# PART 10: IMPLEMENTATION GUIDE

## 10.1 Compiler Phases

```
Source Code
    │
    ▼
┌─────────┐
│  Lexer  │  → Token stream
└─────────┘
    │
    ▼
┌─────────┐
│ Parser  │  → Concrete Syntax Tree (CST)
└─────────┘
    │
    ▼
┌─────────┐
│Desugarer│  → Abstract Syntax Tree (AST)
└─────────┘
    │
    ▼
┌─────────────┐
│Name Resolver│ → AST with resolved names + modules
└─────────────┘
    │
    ▼
┌─────────────┐
│Type Checker │ → Typed AST + effect constraints
└─────────────┘
    │
    ▼
┌─────────────┐
│Borrow Check │ → Verified ownership
└─────────────┘
    │
    ▼
┌────────────┐
│Trait Solver│ → Resolved trait impls
└────────────┘
    │
    ▼
┌───────────┐
│Monomorphize│ → Specialized functions
└───────────┘
    │
    ▼
┌──────────┐
│Lower to IR│ → Low-level IR
└──────────┘
    │
    ▼
┌──────────┐
│WASM Emit │ → .wasm binary
└──────────┘
```

## 10.2 Key Implementation Challenges

### Challenge 1: Bidirectional Type Checking with Dependent Types

```
infer(Γ, e) → (τ, ε)     Synthesize type from expression
check(Γ, e, τ) → ε       Check expression against type
```

Dependent types require:
- Type-level evaluation during checking
- Unification with type-level terms
- Constraint solving for refinements

### Challenge 2: Row Polymorphism Inference

Use row unification:
- Row variables unify with partial rows
- Track lacks constraints: `r \ ℓ` means r doesn't have field ℓ
- Propagate constraints through function calls

### Challenge 3: Borrow Checking

Similar to Rust's borrow checker:
- Build control flow graph
- Track liveness of owned values
- Verify borrows don't outlive owners
- Verify no aliasing of mutable borrows
- Handle row variables uniformly

### Challenge 4: Totality Checking

For `total` functions:
- Structural recursion: recursive calls on structurally smaller args
- Well-founded recursion: prove termination metric decreases
- Coverage: all pattern matches are exhaustive

### Challenge 5: Effect Polymorphism

- Track effect variables through unification
- Propagate effect constraints
- Handle effect subtyping (ε₁ ⊆ ε₁ + ε₂)

### Challenge 6: Trait Resolution

- Build trait impl database
- Implement coherence checking (orphan rule, overlap)
- Resolve associated types
- Handle trait bounds in generics

## 10.3 Suggested Implementation Order

1. **Lexer + Parser** (2-3 weeks)
   - Use parser combinator or generator
   - Build CST, then desugar to AST
   - Include module syntax

2. **Basic Type Checker** (3-4 weeks)
   - Simple types first (Int, Bool, functions)
   - Add records (non-polymorphic)
   - Add variants

3. **Module System** (1-2 weeks)
   - Name resolution across modules
   - Visibility checking
   - Import/export

4. **Ownership System** (2-3 weeks)
   - Add own/ref/mut modifiers
   - Implement linear type checking
   - Implement borrow checking

5. **Row Polymorphism** (2-3 weeks)
   - Add row variables
   - Implement row unification
   - Handle lacks constraints

6. **Traits** (2-3 weeks)
   - Trait declarations
   - Impl blocks
   - Trait bounds
   - Associated types

7. **Dependent Types** (4-6 weeks)
   - Add Nat kind
   - Implement type-level evaluation
   - Add Vec[n, T]
   - Add refinement types

8. **Extensible Effects** (2-3 weeks)
   - Effect declarations
   - Effect polymorphism
   - Effect constraint propagation
   - Basic handlers (if desired)

9. **WASM Backend** (4-6 weeks)
   - Implement type mapping
   - Emit WASM-GC
   - Handle closures
   - Monomorphization
   - Generate JS glue

## 10.4 Testing Strategy

```
tests/
├── lexer/              # Token output tests
├── parser/             # AST output tests
├── modules/            # Module resolution tests
├── types/
│   ├── positive/       # Should type-check
│   └── negative/       # Should reject
├── ownership/
│   ├── positive/       # Valid ownership patterns
│   └── negative/       # Ownership violations
├── rows/               # Row polymorphism tests
├── traits/             # Trait resolution tests
├── dependent/          # Dependent type tests
├── effects/            # Effect tracking tests
├── codegen/            # WASM output tests
└── e2e/                # Full program tests
```

---

# PART 11: ERROR MESSAGES

## 11.1 Ownership Errors

```
error[E0501]: cannot use `file` after move
  --> src/main.as:10:5
   |
 8 |     close(file);
   |           ---- value moved here
 9 |
10 |     read(file);
   |          ^^^^ value used after move
   |
   = help: consider using `ref file` if you need to read without consuming
```

```
error[E0502]: cannot borrow `x` as mutable because it is also borrowed as immutable
  --> src/main.as:5:10
   |
 4 |     let r = ref x;
   |             ----- immutable borrow occurs here
 5 |     let m = mut x;
   |             ^^^^^ mutable borrow occurs here
 6 |     println(r);
   |             - immutable borrow later used here
```

## 11.2 Type Errors

```
error[E0308]: mismatched types
  --> src/main.as:5:12
   |
 5 |     head(empty)
   |          ^^^^^ expected `Vec[n + 1, T]`, found `Vec[0, T]`
   |
   = note: `head` requires a non-empty vector
   = help: the type `Vec[0, T]` is empty, so `head` cannot be called
```

```
error[E0309]: refinement predicate not satisfied
  --> src/main.as:8:15
   |
 8 |     safeGet(arr, 10)
   |                  ^^ cannot prove `10 < 5`
   |
   = note: array has length 5, but index is 10
```

## 11.3 Effect Errors

```
error[E0601]: effect not handled
  --> src/main.as:3:5
   |
 3 |     println("hello")
   |     ^^^^^^^^^^^^^^^^ this has effect `IO`
   |
   = note: function `pureFunction` is declared as `/ Pure`
   = help: either add `IO` to the function's effects or handle it
```

```
warning[W0602]: owned resource may leak on exception
  --> src/main.as:8:5
   |
 7 |     let file = open(path)?;
   |         ---- owned resource acquired here
 8 |     mayThrow();
   |     ^^^^^^^^^^ this may throw
 9 |     close(file);
   |     ----------- resource closed here
   |
   = help: use `try`/`finally` to ensure resource is closed
   = help: or use `withFile` pattern for automatic cleanup
```

## 11.4 Trait Errors

```
error[E0401]: no implementation of trait `Show` for type `MyType`
  --> src/main.as:5:5
   |
 5 |     println(x.show())
   |             ^^^^^^^^ `Show` is not implemented for `MyType`
   |
   = help: implement the trait:
   |
   | impl Show for MyType {
   |     fn show(ref self) -> String / Pure {
   |         // ...
   |     }
   | }
```

## 11.5 Module Errors

```
error[E0701]: cannot find `Vec` in this scope
  --> src/main.as:3:10
   |
 3 |     let v: Vec[3, Int] = Nil;
   |            ^^^ not found in this scope
   |
   = help: add `use Data.Vec::Vec;` at the top of the file
```

```
error[E0702]: function `helper` is private
  --> src/main.as:5:5
   |
 5 |     Utils.helper()
   |     ^^^^^^^^^^^^^^ private function
   |
   = note: `helper` is defined in `Utils` but not exported
```

---

# APPENDIX A: GRAMMAR SUMMARY

## A.1 Complete EBNF Grammar (Consolidated)

```ebnf
(* === PROGRAM === *)
program       = [ module_decl ] { import_decl } { top_level } ;
top_level     = type_decl | fn_decl | trait_decl | impl_block | effect_decl | const_decl ;

(* === MODULES === *)
module_decl   = 'module' module_path ';' ;
module_path   = UPPER_IDENT { '.' UPPER_IDENT } ;
import_decl   = 'use' import_path [ 'as' IDENT ] ';'
              | 'use' import_path '::' '{' import_list '}' ';'
              | 'use' import_path '::' '*' ';' ;
import_path   = [ '::' ] module_path [ '::' IDENT ] ;
import_list   = import_item { ',' import_item } ;
import_item   = IDENT [ 'as' IDENT ] ;
visibility    = [ 'pub' [ '(' pub_scope ')' ] ] ;
pub_scope     = 'crate' | 'super' | module_path ;

(* === TYPES === *)
type_decl     = visibility 'type' UPPER_IDENT [ type_params ] '=' type_body ;
type_params   = '[' type_param { ',' type_param } ']' ;
type_param    = [ QUANTITY ] IDENT [ ':' kind ] ;
kind          = 'Type' | 'Nat' | 'Row' | 'Effect' | kind '->' kind ;
type_body     = type_expr | struct_body | enum_body ;
struct_body   = '{' field_decl { ',' field_decl } '}' ;
field_decl    = visibility IDENT ':' type_expr ;
enum_body     = [ '|' ] variant { '|' variant } ;
variant       = UPPER_IDENT [ '(' type_expr { ',' type_expr } ')' ] [ ':' type_expr ] ;

type_expr     = type_atom [ '->' type_expr [ '/' effects ] ]
              | '(' [ QUANTITY ] IDENT ':' type_expr ')' '->' type_expr [ '/' effects ]
              | type_expr 'where' '(' predicate ')' ;
type_atom     = PRIM_TYPE | UPPER_IDENT | TYPE_VAR | row_type
              | 'own' type_atom | 'ref' type_atom | 'mut' type_atom
              | '(' type_expr ')' | '(' type_expr { ',' type_expr } ')'
              | UPPER_IDENT '[' type_arg { ',' type_arg } ']' ;
type_arg      = type_expr | nat_expr ;
row_type      = '{' [ field_type { ',' field_type } [ ',' ROW_VAR ] | ROW_VAR ] '}' ;
field_type    = IDENT ':' type_expr ;

effects       = effect_term { '+' effect_term } ;
effect_term   = UPPER_IDENT [ '[' type_arg { ',' type_arg } ']' ] | EFFECT_VAR ;

nat_expr      = NAT_ATOM { ('+' | '-' | '*') NAT_ATOM } ;
NAT_ATOM      = INT_LIT | IDENT | '(' nat_expr ')' | 'len' '(' IDENT ')' ;

predicate     = pred_atom { ('&&' | '||') pred_atom } ;
pred_atom     = nat_expr CMP_OP nat_expr | '!' pred_atom | '(' predicate ')' ;

(* === EFFECTS === *)
effect_decl   = visibility 'effect' UPPER_IDENT [ type_params ] '{' { effect_op } '}' ;
effect_op     = 'fn' LOWER_IDENT '(' [ param_list ] ')' [ '->' type_expr ] ';' ;

(* === TRAITS === *)
trait_decl    = visibility 'trait' UPPER_IDENT [ type_params ] [ ':' trait_bounds ]
                '{' { trait_item } '}' ;
trait_bounds  = UPPER_IDENT { '+' UPPER_IDENT } ;
trait_item    = fn_sig ';' | fn_decl | 'type' UPPER_IDENT [ ':' kind ] [ '=' type_expr ] ';' ;
fn_sig        = visibility 'fn' LOWER_IDENT [ type_params ] '(' [ param_list ] ')'
                [ '->' type_expr ] [ '/' effects ] ;

impl_block    = 'impl' [ type_params ] [ trait_ref 'for' ] type_expr
                [ where_clause ] '{' { impl_item } '}' ;
trait_ref     = UPPER_IDENT [ '[' type_arg { ',' type_arg } ']' ] ;
impl_item     = fn_decl | 'type' UPPER_IDENT '=' type_expr ';' ;

(* === FUNCTIONS === *)
fn_decl       = visibility [ 'total' ] 'fn' LOWER_IDENT [ type_params ]
                '(' [ param_list ] ')' [ '->' type_expr ] [ '/' effects ]
                [ where_clause ] fn_body ;
param_list    = param { ',' param } ;
param         = [ QUANTITY ] [ OWNERSHIP ] IDENT ':' type_expr ;
OWNERSHIP     = 'own' | 'ref' | 'mut' ;
QUANTITY      = '0' | '1' | 'ω' ;
where_clause  = 'where' constraint { ',' constraint } ;
constraint    = predicate | TYPE_VAR ':' trait_bound ;
trait_bound   = UPPER_IDENT { '+' UPPER_IDENT } ;
fn_body       = block | '=' expr ;

(* === EXPRESSIONS === *)
expr          = let_expr | if_expr | match_expr | fn_expr | try_expr
              | handle_expr | return_expr | unsafe_expr | binary_expr ;
let_expr      = 'let' [ 'mut' ] pattern [ ':' type_expr ] '=' expr [ 'in' expr ] ;
if_expr       = 'if' expr block [ 'else' ( if_expr | block ) ] ;
match_expr    = 'match' expr '{' { match_arm } '}' ;
match_arm     = pattern [ 'if' expr ] '=>' expr [ ',' ] ;
fn_expr       = '|' [ param_list ] '|' expr | 'fn' '(' [ param_list ] ')' fn_body ;
try_expr      = 'try' block [ 'catch' '{' { catch_arm } '}' ] [ 'finally' block ] ;
catch_arm     = pattern '=>' expr [ ',' ] ;
handle_expr   = 'handle' expr 'with' '{' { handler_arm } '}' ;
handler_arm   = 'return' pattern '=>' expr [ ',' ]
              | LOWER_IDENT '(' [ pattern { ',' pattern } ] ')' '=>' expr [ ',' ] ;
return_expr   = 'return' [ expr ] ;
unsafe_expr   = 'unsafe' '{' { unsafe_stmt } '}' ;
block         = '{' { statement } [ expr ] '}' ;

binary_expr   = unary_expr { BINARY_OP unary_expr } ;
unary_expr    = [ UNARY_OP ] postfix_expr ;
postfix_expr  = primary_expr { postfix } ;
postfix       = '.' IDENT | '.' INT_LIT | '[' expr ']' | '(' [ arg_list ] ')'
              | '::' UPPER_IDENT | '\\' IDENT ;
primary_expr  = LITERAL | IDENT | '(' expr ')' | '(' expr { ',' expr } ')'
              | '[' [ expr { ',' expr } ] ']'
              | '{' [ field_init { ',' field_init } ] [ '..' expr ] '}'
              | 'resume' '(' [ expr ] ')' ;
field_init    = IDENT [ ':' expr ] ;

(* === PATTERNS === *)
pattern       = '_' | IDENT | LITERAL | UPPER_IDENT [ '(' pattern { ',' pattern } ')' ]
              | '(' pattern { ',' pattern } ')' | '{' field_pat { ',' field_pat } [ '..' ] '}'
              | pattern '|' pattern | IDENT '@' pattern ;
field_pat     = IDENT [ ':' pattern ] ;

(* === STATEMENTS === *)
statement     = let_expr ';' | expr ';' | postfix_expr ASSIGN_OP expr ';'
              | 'while' expr block | 'for' pattern 'in' expr block ;

(* === TOKENS === *)
LOWER_IDENT   = /[a-z][a-zA-Z0-9_]*/ ;
UPPER_IDENT   = /[A-Z][a-zA-Z0-9_]*/ ;
TYPE_VAR      = /[a-z][a-zA-Z0-9_]*'?/ ;
EFFECT_VAR    = /[a-z][a-zA-Z0-9_]*/ ;
ROW_VAR       = /\.\.[a-z][a-zA-Z0-9_]*/ ;
INT_LIT       = /-?[0-9]+/ | /0x[0-9a-fA-F]+/ | /0b[01]+/ | /0o[0-7]+/ ;
FLOAT_LIT     = /-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/ ;
STRING_LIT    = /"([^"\\]|\\.)*"/ ;
CHAR_LIT      = /'([^'\\]|\\.)'/ ;
BINARY_OP     = /[+\-*\/%]/ | /[<>=!]=?/ | /&&/ | /\|\|/ | /[&|^]/ | /<<|>>/ ;
UNARY_OP      = /[-!~&*]/ ;
CMP_OP        = /[<>=!]=?/ ;
ASSIGN_OP     = /=/ | /[+\-*\/]=/ ;
PRIM_TYPE     = 'Nat' | 'Int' | 'Bool' | 'Float' | 'String' | 'Char' | 'Type' | 'Never' ;
```

---

# APPENDIX B: CHANGE LOG

## B.1 Version 2.0 Changes (from 1.0)

| Change | Description |
|--------|-------------|
| Partial by default | Functions are partial by default; only `total` annotation exists |
| Quantity 0 semantics | Erased values are compile-time only, fully specified |
| Strict `unsafe` | Only 6 specific operations permitted |
| Row + ownership | Ownership distributes uniformly over row variables |
| Refinement effects | Predicates can have effects (warning if not Pure) |
| Traits | Full specification added |
| Modules | Hierarchical module system with visibility |
| Memory model | Stack-by-default, heap-on-escape strategy |
| Extensible effects | User-defined effects replace fixed set |
| Row restriction | Value-level `\` operator for splitting records |
| Effect handlers | `handle`/`with`/`resume` syntax added |

---

*End of AffineScript Language Specification v2.0*
