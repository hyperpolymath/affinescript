# AffineScript Language Specification v0.1

A programming language combining affine types, dependent types, row polymorphism, and extensible effects.

## Overview

AffineScript is designed for safe, efficient systems programming with:

- **Affine Types**: Rust-style ownership ensuring memory safety without GC
- **Dependent Types**: Types that depend on values (e.g., `Vec[n, T]`)
- **Row Polymorphism**: Extensible records with compile-time field tracking
- **Extensible Effects**: User-defined, tracked side effects
- **WASM Target**: Compiles to WebAssembly for portable execution

## 1. Lexical Grammar

### 1.1 Identifiers

```
lower_ident   = [a-z][a-zA-Z0-9_]*
upper_ident   = [A-Z][a-zA-Z0-9_]*
row_var       = ".." lower_ident
```

### 1.2 Keywords

```
fn let mut own ref type struct enum trait impl effect handle
resume handler match if else while for return break continue in
true false where total module use pub as unsafe assume transmute
forget Nat Int Bool Float String Type Row
```

### 1.3 Literals

```
int_lit    = [0-9]+ | 0x[0-9a-fA-F]+ | 0b[01]+ | 0o[0-7]+
float_lit  = [0-9]+ "." [0-9]+ ([eE][+-]?[0-9]+)?
char_lit   = "'" (escape | [^'\\]) "'"
string_lit = '"' (escape | [^"\\])* '"'
bool_lit   = "true" | "false"
unit_lit   = "()"
```

### 1.4 Operators

```
Arithmetic:  + - * / %
Comparison:  == != < > <= >=
Logical:     && || !
Bitwise:     & | ^ ~ << >>
Type-level:  -> => : /
Special:     \ (row restriction)
```

### 1.5 Quantity Annotations

```
0    Erased (compile-time only)
1    Linear (use exactly once)
ω    Unrestricted (use any number of times)
```

## 2. Syntactic Grammar

### 2.1 Program Structure

```ebnf
program     = [module_decl] {import_decl} {top_level}
top_level   = fn_decl | type_decl | trait_decl | impl_block | effect_decl
```

### 2.2 Type Declarations

```ebnf
type_decl   = [visibility] "type" UPPER_IDENT [type_params] "=" type_body

type_params = "[" type_param {"," type_param} "]"
type_param  = [quantity] IDENT [":" kind]

kind        = "Type" | "Nat" | "Row" | "Effect" | kind "->" kind

type_body   = type_expr              (* alias *)
            | struct_body            (* record *)
            | enum_body              (* variant *)

struct_body = "{" field {"," field} "}"
enum_body   = ["|"] variant {"|" variant}
variant     = UPPER_IDENT ["(" type_expr {"," type_expr} ")"] [":" type_expr]
```

### 2.3 Type Expressions

```ebnf
type_expr   = type_atom
            | type_expr "->" type_expr ["/" effects]     (* function *)
            | "(" [quantity] IDENT ":" type_expr ")" "->" type_expr ["/" effects]
            | type_expr "where" "(" predicate ")"        (* refinement *)

type_atom   = PRIM_TYPE | UPPER_IDENT | TYPE_VAR
            | UPPER_IDENT "[" type_arg {"," type_arg} "]"
            | "own" type_atom | "ref" type_atom | "mut" type_atom
            | "{" row_fields "}"                         (* record *)
            | "(" type_expr {"," type_expr} ")"          (* tuple *)

row_fields  = field_type {"," field_type} ["," row_var]
            | row_var

effects     = effect_term {"+" effect_term}
effect_term = UPPER_IDENT ["[" type_arg {"," type_arg} "]"]
```

### 2.4 Function Declarations

```ebnf
fn_decl     = [visibility] ["total"] "fn" LOWER_IDENT
              [type_params] "(" [param_list] ")"
              ["->" type_expr] ["/" effects]
              [where_clause] fn_body

param_list  = param {"," param}
param       = [quantity] [ownership] IDENT ":" type_expr
ownership   = "own" | "ref" | "mut"

where_clause = "where" constraint {"," constraint}
constraint   = predicate | TYPE_VAR ":" trait_bounds

fn_body     = block | "=" expr
```

### 2.5 Expressions

```ebnf
expr        = let_expr | if_expr | match_expr | fn_expr
            | handle_expr | return_expr | binary_expr

let_expr    = "let" ["mut"] pattern [":" type_expr] "=" expr ["in" expr]
if_expr     = "if" expr block ["else" (if_expr | block)]
match_expr  = "match" expr "{" {match_arm} "}"
match_arm   = pattern ["if" expr] "=>" expr [","]

fn_expr     = "|" [param_list] "|" expr
            | "fn" "(" [param_list] ")" fn_body

handle_expr = "handle" expr "with" "{" {handler_arm} "}"
handler_arm = "return" pattern "=>" expr [","]
            | LOWER_IDENT "(" [pattern {"," pattern}] ")" "=>" expr [","]

block       = "{" {statement} [expr] "}"

binary_expr = unary_expr {BINARY_OP unary_expr}
unary_expr  = [UNARY_OP] postfix_expr
postfix_expr = primary_expr {postfix}
postfix     = "." IDENT | "." INT | "[" expr "]" | "(" [args] ")"
            | "::" UPPER_IDENT | "\\" IDENT

primary_expr = literal | IDENT
             | "(" expr ")" | "(" expr {"," expr} ")"
             | "[" [expr {"," expr}] "]"
             | "{" [field_init {"," field_init}] [".." expr] "}"
             | "resume" "(" [expr] ")"
```

### 2.6 Patterns

```ebnf
pattern     = "_"                                    (* wildcard *)
            | IDENT                                  (* binding *)
            | literal                                (* literal match *)
            | UPPER_IDENT ["(" pattern {"," pattern} ")"]
            | "(" pattern {"," pattern} ")"          (* tuple *)
            | "{" field_pat {"," field_pat} [".." ] "}"
            | pattern "|" pattern                    (* or-pattern *)
            | IDENT "@" pattern                      (* binding with pattern *)
```

### 2.7 Effect Declarations

```ebnf
effect_decl = [visibility] "effect" UPPER_IDENT [type_params]
              "{" {effect_op} "}"
effect_op   = "fn" LOWER_IDENT "(" [param_list] ")" ["->" type_expr] ";"
```

### 2.8 Trait Declarations

```ebnf
trait_decl  = [visibility] "trait" UPPER_IDENT [type_params]
              [":" trait_bounds] "{" {trait_item} "}"
trait_bounds = UPPER_IDENT {"+" UPPER_IDENT}
trait_item  = fn_sig ";" | fn_decl | assoc_type

impl_block  = "impl" [type_params] [trait_ref "for"] type_expr
              [where_clause] "{" {impl_item} "}"
```

## 3. Type System

### 3.1 Judgement Forms

```
Γ ⊢ e : τ / ε    Expression e has type τ with effects ε
Γ ⊢ τ : κ        Type τ has kind κ
Γ ⊢ P true       Predicate P is satisfied
```

### 3.2 Quantities (QTT)

| Quantity | Meaning | Usage |
|----------|---------|-------|
| `0` | Erased | Compile-time only, no runtime cost |
| `1` | Linear | Must use exactly once |
| `ω` | Unrestricted | Use any number of times |

**Algebra:**
```
0 + q = q      0 * q = 0
1 + 1 = ω     1 * q = q
ω + ω = ω     ω * ω = ω
```

### 3.3 Ownership

| Modifier | Meaning |
|----------|---------|
| `own T` | Owned value - caller transfers ownership |
| `ref T` | Immutable borrow - cannot modify |
| `mut T` | Mutable borrow - exclusive access |

**Rules:**
- Owned values are consumed on use
- Multiple `ref` borrows allowed simultaneously
- Only one `mut` borrow at a time
- Borrows cannot outlive owner

### 3.4 Effects

Functions declare effects after `/`:

```affinescript
fn pure_fn(x: Int) -> Int / Pure { x + 1 }
fn io_fn() -> () / IO { println("hello") }
fn fallible() -> Int / Exn[Error] { throw(Error::new()) }
fn combined() -> () / IO + Exn[Error] { ... }
```

**Partial by Default:**
- Functions are partial by default (may not terminate)
- `total` functions must provably terminate

### 3.5 Dependent Types

Types can depend on values:

```affinescript
type Vec[n: Nat, T: Type] =
  | Nil : Vec[0, T]
  | Cons(T, Vec[n, T]) : Vec[n + 1, T]

// Can only call on non-empty vectors
fn head[n: Nat, T](v: Vec[n + 1, T]) -> T
```

### 3.6 Refinement Types

Constrain types with predicates:

```affinescript
type PosInt = Int where (self > 0)
fn safeDiv(a: Int, b: Int where (b != 0)) -> Int
```

### 3.7 Row Polymorphism

Extensible records with row variables:

```affinescript
// Works on any record with 'name' field
fn greet[..r](person: {name: String, ..r}) -> String {
  "Hello, " ++ person.name
}

// Add fields
fn addAge[..r](rec: {..r}) -> {age: Int, ..r} {
  {age: 0, ..rec}
}

// Remove fields
fn removeName[..r](rec: {name: String, ..r}) -> {..r} {
  rec \ name
}
```

## 4. Core Typing Rules

### Variables
```
Γ, x :q τ ⊢ x : τ / ∅
```

### Functions
```
Γ, x :q τ₁ ⊢ e : τ₂ / ε
────────────────────────────────
Γ ⊢ fn(x: τ₁) => e : τ₁ -> τ₂ / ε
```

### Application
```
Γ ⊢ f : τ₁ -> τ₂ / ε₁    Γ ⊢ e : τ₁ / ε₂
──────────────────────────────────────────
Γ ⊢ f(e) : τ₂ / ε₁ + ε₂
```

### Records
```
Γ ⊢ e : {ℓ: τ, ..r} / ε
────────────────────────
Γ ⊢ e.ℓ : τ / ε

Γ ⊢ e : {..r} / ε
───────────────────────────────
Γ ⊢ {ℓ: v, ..e} : {ℓ: τ, ..r} / ε
```

### Effect Handling
```
handle (return v) with { return x => eᵣ, ... }
  → eᵣ[x ↦ v]

handle E[op(v)] with { op(x) => eₒₚ, ... }
  → eₒₚ[x ↦ v, resume ↦ fn(y) => handle E[y] with {...}]
```

## 5. Standard Library (Core)

### 5.1 Primitive Types

```affinescript
type Nat     // Natural numbers (0, 1, 2, ...)
type Int     // 64-bit signed integers
type Float   // 64-bit floats
type Bool    // true | false
type String  // UTF-8 string
type Char    // Unicode scalar value
type Never   // Uninhabited type
```

### 5.2 Standard Effects

```affinescript
effect IO {
  fn print(s: String);
  fn println(s: String);
  fn readLine() -> String;
}

effect Exn[E] {
  fn throw(err: E) -> Never;
}

effect State[S] {
  fn get() -> S;
  fn put(s: S);
}
```

### 5.3 Option and Result

```affinescript
type Option[T] = None | Some(T)

type Result[T, E] = Ok(T) | Err(E)
```

### 5.4 Core Traits

```affinescript
trait Eq {
  fn eq(ref self, other: ref Self) -> Bool;
}

trait Ord: Eq {
  fn cmp(ref self, other: ref Self) -> Ordering;
}

trait Show {
  fn show(ref self) -> String;
}

trait Clone {
  fn clone(ref self) -> Self;
}

trait Drop {
  fn drop(own self);
}
```

## 6. Example Programs

### 6.1 Hello World

```affinescript
effect IO {
  fn println(s: String);
}

fn main() -> () / IO {
  println("Hello, AffineScript!")
}
```

### 6.2 Length-Indexed Vector

```affinescript
type Vec[n: Nat, T: Type] =
  | Nil : Vec[0, T]
  | Cons(head: T, tail: Vec[n, T]) : Vec[n + 1, T]

total fn head[n: Nat, T](v: Vec[n + 1, T]) -> T / Pure {
  match v { Cons(h, _) => h }
}

total fn append[n: Nat, m: Nat, T](
  a: Vec[n, T], b: Vec[m, T]
) -> Vec[n + m, T] / Pure {
  match a {
    Nil => b,
    Cons(h, t) => Cons(h, append(t, b))
  }
}
```

### 6.3 Ownership and Resources

```affinescript
type File = own { fd: Int }

fn open(path: ref String) -> Result[own File, IOError] / IO
fn read(file: ref File) -> Result[String, IOError] / IO
fn close(file: own File) -> Result[(), IOError] / IO

fn withFile[T](
  path: ref String,
  action: (ref File) -> Result[T, IOError]
) -> Result[T, IOError] / IO + Exn[IOError] {
  let file = open(path)?;
  let result = action(ref file);
  close(file)?;
  result
}
```

### 6.4 Row Polymorphism

```affinescript
fn greet[..r](person: {name: String, ..r}) -> String / Pure {
  "Hello, " ++ person.name
}

fn addField[..r](rec: {..r}, age: Int) -> {age: Int, ..r} / Pure {
  {age: age, ..rec}
}

fn main() -> () / Pure {
  let alice = {name: "Alice", role: "Engineer"};
  let bob = {name: "Bob", dept: "Sales"};

  // Both work despite different shapes
  greet(alice);  // "Hello, Alice"
  greet(bob);    // "Hello, Bob"
}
```

### 6.5 Effect Handlers

```affinescript
effect State[S] {
  fn get() -> S;
  fn put(s: S);
}

fn counter() -> Int / State[Int] {
  let n = State.get();
  State.put(n + 1);
  n
}

fn runState[S, T](init: S, comp: () -> T / State[S]) -> (T, S) / Pure {
  handle comp() with {
    return x => (x, init),
    get() => resume(init),
    put(s) => resume(())
  }
}
```

## 7. WASM Compilation

### 7.1 Type Mapping

| AffineScript | WASM |
|--------------|------|
| `Int`, `Nat` | `i64` |
| `Float` | `f64` |
| `Bool` | `i32` |
| `String` | `(ref (array i8))` |
| `{fields}` | `(ref (struct ...))` |
| `own T` | `(ref T)` (ownership is erased) |
| `T -> U` | `(ref (struct $func $env))` |

### 7.2 Ownership Erasure

Ownership and quantity annotations exist only at compile time:

```affinescript
fn useFile(own file: File) -> () / IO { close(file) }
```

Compiles to (ownership removed):
```wat
(func $useFile (param $file (ref $File))
  (call $close (local.get $file)))
```

## Appendix: Grammar Reference

See the full specification at `affinescript-spec.md` for:
- Complete EBNF grammar
- Detailed typing rules
- Operational semantics
- Error message catalog
- Implementation guide

---

*AffineScript Specification v0.1 - Reference Implementation*
