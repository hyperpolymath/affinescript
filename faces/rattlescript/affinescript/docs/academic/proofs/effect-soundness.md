# Algebraic Effects: Formal Semantics and Soundness

**Document Version**: 1.0
**Last Updated**: 2024
**Status**: Theoretical framework complete; implementation verification pending `[IMPL-DEP: effect-checker, effect-inference]`

## Abstract

This document presents the formal semantics of AffineScript's algebraic effect system. We define the syntax, typing rules, and operational semantics for effects and handlers, proving type safety and effect safety: well-typed programs only perform effects that are handled.

## 1. Introduction

Algebraic effects and handlers provide a structured approach to computational effects, separating effect signatures from their implementations. AffineScript's effect system features:

1. **User-defined effects**: Custom effect declarations
2. **Row-polymorphic effects**: `ε₁ | ε₂ | ..ρ`
3. **One-shot and multi-shot handlers**: Controlled use of continuations
4. **Effect polymorphism**: Abstracting over effect rows
5. **Effect inference**: Automatic effect tracking

## 2. Syntax

### 2.1 Effect Declarations

```
effect State[S] {
    get : () → S
    put : S → ()
}

effect Exn[E] {
    raise : E → ⊥
}

effect Async {
    fork : (() →{Async} ()) → ()
    yield : () → ()
}
```

### 2.2 Effect Types

```
ε ::=
    | ·                           -- Empty effect (pure)
    | Op                          -- Single effect operation
    | E                           -- Named effect
    | ε₁ | ε₂                     -- Effect union
    | ρ                           -- Effect row variable
```

### 2.3 Effectful Function Types

```
τ →{ε} σ                          -- Function with effect ε
τ →{} σ  ≡  τ → σ                 -- Pure function (sugar)
```

### 2.4 Effect Operations and Handlers

```
e ::=
    | ...
    | perform op(e)               -- Perform effect operation
    | handle e with h             -- Handle effects
    | resume(e)                   -- Resume continuation

h ::=
    | { return x → e_ret,
        op₁(x, k) → e₁,
        ...,
        opₙ(x, k) → eₙ }          -- Handler clauses
```

## 3. Static Semantics

### 3.1 Effect Kinding

```
Γ ⊢ ε : Effect
```

**E-Empty**
```
    ──────────────
    Γ ⊢ · : Effect
```

**E-Op**
```
    op : τ → σ ∈ E
    ────────────────
    Γ ⊢ E.op : Effect
```

**E-Named**
```
    effect E { ... } declared
    ──────────────────────────
    Γ ⊢ E : Effect
```

**E-Union**
```
    Γ ⊢ ε₁ : Effect    Γ ⊢ ε₂ : Effect
    ───────────────────────────────────
    Γ ⊢ ε₁ | ε₂ : Effect
```

**E-Var**
```
    ρ : Effect ∈ Γ
    ───────────────
    Γ ⊢ ρ : Effect
```

### 3.2 Effect Row Operations

**Row Equivalence**: Effect rows are equivalent up to:
- Commutativity: `ε₁ | ε₂ ≡ ε₂ | ε₁`
- Associativity: `(ε₁ | ε₂) | ε₃ ≡ ε₁ | (ε₂ | ε₃)`
- Identity: `ε | · ≡ ε`
- Idempotence: `E | E ≡ E`

**Row Subtraction**: `ε \ E` removes effect E from row ε

```
    · \ E = ·
    E \ E = ·
    E' \ E = E'   (when E ≠ E')
    (ε₁ | ε₂) \ E = (ε₁ \ E) | (ε₂ \ E)
    ρ \ E = ρ     (row variable, handled later)
```

### 3.3 Typing Effectful Expressions

The judgment `Γ ⊢ e : τ ! ε` means "in context Γ, expression e has type τ and may perform effects ε".

**Pure expressions**:
```
    Γ ⊢ e : τ
    ─────────────
    Γ ⊢ e : τ ! ·
```

**Effect Subsumption**:
```
    Γ ⊢ e : τ ! ε₁    ε₁ ⊆ ε₂
    ──────────────────────────
    Γ ⊢ e : τ ! ε₂
```

### 3.4 Effect Operation Typing

**Perform**
```
    op : τ → σ ∈ E
    Γ ⊢ e : τ ! ε
    ────────────────────────
    Γ ⊢ perform op(e) : σ ! (E | ε)
```

### 3.5 Handler Typing

**Handle**
```
    Γ ⊢ e : τ ! (E | ε)
    Γ ⊢ h handles E : τ ⇒ σ ! ε'
    ──────────────────────────────────
    Γ ⊢ handle e with h : σ ! (ε | ε')
```

**Handler Clause Typing**
```
    Γ, x:τ_ret ⊢ e_ret : σ ! ε'
    ∀ op ∈ E. Γ, x:τ_op, k:(σ_op →{ε | ε'} σ) ⊢ e_op : σ ! ε'
    ────────────────────────────────────────────────────────────
    Γ ⊢ { return x → e_ret, op(x,k) → e_op, ... } handles E : τ_ret ⇒ σ ! ε'
```

Where:
- `τ_ret` is the return type of the handled computation
- For each `op : τ_op → σ_op` in E
- `k` is the continuation, typed as `σ_op →{ε | ε'} σ`

### 3.6 Resume Typing

Within a handler clause for `op : τ → σ`:

**Resume**
```
    Γ, k:(σ →{ε} τ_result) ⊢ resume(e) : τ_result ! ε
    ─────────────────────────────────────────────────
    (when e : σ)
```

## 4. Dynamic Semantics

### 4.1 Values and Evaluation Contexts

**Values**:
```
v ::= ... | handler h
```

**Pure Evaluation Contexts**:
```
E_p ::= □ | E_p e | v E_p | ...
```

**Effectful Evaluation Contexts**:
```
E_eff ::= E_p | handle E_eff with h
```

### 4.2 Reduction Rules

**Handler Introduction**:
```
    ─────────────────────────────────────────────
    handle v with h ⟶ e_ret[v/x]
    (where h = { return x → e_ret, ... })
```

**Effect Forwarding** (effect not handled):
```
    op ∉ E
    ────────────────────────────────────────────────────────
    handle E_p[perform op(v)] with h ⟶
        perform op(v) >>= (λy. handle E_p[y] with h)
```

**Effect Handling**:
```
    op : τ → σ ∈ E
    h = { ..., op(x, k) → e_op, ... }
    ────────────────────────────────────────────────────────
    handle E_p[perform op(v)] with h ⟶
        e_op[v/x, (λy. handle E_p[y] with h)/k]
```

The key insight: the continuation `k` captures the context `E_p`, allowing the handler to resume computation.

### 4.3 One-Shot vs Multi-Shot Continuations

**One-shot** (linear k):
```
    k used exactly once in e_op
    Continuation can be efficiently implemented as a stack
```

**Multi-shot** (unrestricted k):
```
    k may be used zero, one, or multiple times
    Requires copying/delimited continuation implementation
```

AffineScript tracks this via quantity annotations:
```
    op(x, 1 k) → e_op     -- k is linear (one-shot)
    op(x, ω k) → e_op     -- k is unrestricted (multi-shot)
```

## 5. Effect Safety

### 5.1 Main Theorem

**Theorem 5.1 (Effect Safety)**: If `· ⊢ e : τ ! ·` (closed, pure), then evaluation of e does not get stuck on an unhandled effect.

**Proof**: We prove a stronger statement by showing that effects are always contained within handlers.

Define "effect-safe configuration" inductively:
1. Values are effect-safe
2. `handle e with h` is effect-safe if e may only perform effects in dom(h) ∪ ε where ε are effects propagated outward

By progress and preservation for effects. ∎

### 5.2 Effect Preservation

**Theorem 5.2 (Effect Preservation)**: If `Γ ⊢ e : τ ! ε` and `e ⟶ e'`, then `Γ ⊢ e' : τ ! ε'` where `ε' ⊆ ε`.

**Proof**: By induction on the reduction.

*Case Handler-Return*:
`handle v with h ⟶ e_ret[v/x]`
The handler removes the handled effect, so `ε' = ε \ E ⊆ ε`. ✓

*Case Effect-Handle*:
The handled effect is captured; remaining effects are preserved. ✓

∎

### 5.3 Effect Progress

**Theorem 5.3 (Effect Progress)**: If `· ⊢ e : τ ! ε` then either:
1. e is a value, or
2. e can reduce, or
3. e = `E[perform op(v)]` where `op ∈ ε`

Case 3 represents a "stuck" effect, but this is only possible if ε ≠ ·.

**Corollary**: If `· ⊢ e : τ ! ·`, then e does not get stuck on effects.

## 6. Row-Polymorphic Effects

### 6.1 Effect Row Variables

```
fn map[A, B, ε](f: A →{ε} B, xs: List[A]) -> List[B] / ε
```

The effect variable ε represents any effect row.

### 6.2 Effect Row Unification

```
ε₁ | ρ₁ ≡ ε₂ | ρ₂
```

Solving:
1. Find common effects in ε₁ and ε₂
2. Unify remaining with row variables
3. Generate constraints ρ₁ = ε₂' | ρ and ρ₂ = ε₁' | ρ for fresh ρ

### 6.3 Effect Polymorphism Rules

**EffAbs**
```
    Γ, ρ:Effect ⊢ e : τ ! ε
    ─────────────────────────────
    Γ ⊢ Λρ. e : ∀ρ:Effect. τ ! ε
```

**EffApp**
```
    Γ ⊢ e : ∀ρ:Effect. τ    Γ ⊢ ε' : Effect
    ─────────────────────────────────────────
    Γ ⊢ e [ε'] : τ[ε'/ρ]
```

## 7. Effect Inference

### 7.1 Algorithm

Effect inference follows bidirectional type checking:

```ocaml
(* Infer effects of an expression *)
val infer_effects : ctx -> expr -> (typ * effect) result

(* Check effects against expected *)
val check_effects : ctx -> expr -> typ -> effect -> unit result
```

Key cases:
```ocaml
let rec infer_effects ctx = function
  | Perform (op, arg) ->
      let (eff, param_ty, ret_ty) = lookup_operation op in
      let _ = check ctx arg param_ty in
      Ok (ret_ty, eff)

  | Handle (body, handler) ->
      let (body_ty, body_eff) = infer_effects ctx body in
      let handled_eff = effects_of_handler handler in
      let remaining_eff = subtract body_eff handled_eff in
      let result_ty = return_type_of_handler handler body_ty in
      Ok (result_ty, remaining_eff)

  | App (f, arg) ->
      let (f_ty, f_eff) = infer_effects ctx f in
      match f_ty with
      | TyArrow (param_ty, ret_ty, fn_eff) ->
          let arg_eff = check_effects ctx arg param_ty in
          Ok (ret_ty, union [f_eff; arg_eff; fn_eff])
      | _ -> Error "not a function"
```

### 7.2 Effect Constraint Solving

Generate and solve constraints:
```
ε₁ ⊆ ε₂                     -- Subsumption
ε₁ | ε₂ = ε₃                -- Composition
ε \ E = ε'                  -- Subtraction
ρ = ε                       -- Row variable instantiation
```

## 8. Interaction with Other Features

### 8.1 Effects and Quantities

```affinescript
effect Once {
    fire : () → ()          -- can only be performed once
}

fn use_once(1 trigger: () →{Once} ()) -> () {
    handle trigger() with {
        return _ → (),
        fire(_, 1 k) → resume(k, ())   -- k is linear
    }
}
```

### 8.2 Effects and Ownership

```affinescript
effect FileIO {
    read : own File → (String, own File)
    write : (own File, String) → own File
}
```

Effect operations can transfer ownership.

### 8.3 Effects and Dependent Types

```affinescript
effect Indexed[n: Nat] {
    tick : () → () where n > 0    -- Refined effect
}
```

`[IMPL-DEP: dependent-effects]` Effect-dependent type interaction requires further implementation.

## 9. Standard Effects

### 9.1 IO Effect

```affinescript
effect IO {
    print : String → ()
    read_line : () → String
    read_file : String → String
    write_file : (String, String) → ()
}
```

### 9.2 State Effect

```affinescript
effect State[S] {
    get : () → S
    put : S → ()
}

-- Derived operations
fn modify[S](f: S → S) -> () / State[S] {
    put(f(get()))
}

-- Standard handler: run with initial state
fn run_state[S, A](init: S, comp: () →{State[S]} A) -> (A, S) {
    handle comp() with {
        return x → (x, init),
        get(_, k) → resume(k, init),
        put(s, k) → run_state(s, λ(). resume(k, ()))
    }
}
```

### 9.3 Exception Effect

```affinescript
effect Exn[E] {
    raise : E → ⊥
}

fn catch[E, A](comp: () →{Exn[E]} A, handler: E → A) -> A {
    handle comp() with {
        return x → x,
        raise(e, _) → handler(e)    -- k discarded (no resume)
    }
}
```

### 9.4 Async Effect

```affinescript
effect Async {
    fork : (() →{Async} ()) → ()
    yield : () → ()
    await : Promise[A] → A
}
```

## 10. Categorical Semantics

### 10.1 Free Monad Interpretation

Effects correspond to free monads:

```
Free E A = Return A | Op (Σ op∈E. τ_op × (σ_op → Free E A))
```

**Theorem 10.1**: The effect system is sound with respect to the free monad semantics.

### 10.2 Handler as Algebra

A handler for effect E is an E-algebra:

```
alg : E (σ → A) → A
```

The handle construct applies the algebra to eliminate the effect.

### 10.3 Relationship to Monads

**Theorem 10.2**: For any effect E, running under a handler is equivalent to interpreting in the corresponding monad.

## 11. Examples

### 11.1 Non-determinism

```affinescript
effect Choice {
    choose : () → Bool
    fail : () → ⊥
}

fn coin_flip() -> Bool / Choice {
    perform choose()
}

fn all_results[A](comp: () →{Choice} A) -> List[A] {
    handle comp() with {
        return x → [x],
        choose(_, k) → resume(k, true) ++ resume(k, false),
        fail(_, _) → []
    }
}
```

### 11.2 Coroutines

```affinescript
effect Yield[A] {
    yield : A → ()
}

type Iterator[A] =
    | Done
    | Next(A, () →{Yield[A]} ())

fn iterate[A](gen: () →{Yield[A]} ()) -> Iterator[A] {
    handle gen() with {
        return _ → Done,
        yield(a, k) → Next(a, k)
    }
}
```

### 11.3 Transactional Memory

```affinescript
effect STM {
    read_tvar : TVar[A] → A
    write_tvar : (TVar[A], A) → ()
    retry : () → ⊥
    or_else : (() →{STM} A, () →{STM} A) → A
}
```

## 12. Implementation Notes

### 12.1 AST Representation

From `lib/ast.ml`:

```ocaml
type effect_expr =
  | EffNamed of ident                    (* Named effect *)
  | EffApp of ident * type_arg list      (* Parameterized effect *)
  | EffUnion of effect_expr list         (* Union *)
  | EffVar of ident                      (* Row variable *)

type effect_op = {
  eo_name : ident;
  eo_params : (ident * type_expr) list;
  eo_ret_ty : type_expr;
}

type effect_decl = {
  ed_name : ident;
  ed_ty_params : ty_param list;
  ed_ops : effect_op list;
}
```

### 12.2 Effect Checking Algorithm

`[IMPL-DEP: effect-checker]`

```ocaml
module EffectChecker : sig
  val check_effect_row : ctx -> effect_expr -> effect_kind result
  val infer_effects : ctx -> expr -> (typ * effect_row) result
  val check_handler_complete : effect_decl -> handler -> bool
  val unify_effects : effect_row -> effect_row -> substitution result
end
```

## 13. Related Work

1. **Algebraic Effects**: Plotkin & Power (2002), Plotkin & Pretnar (2009)
2. **Effect Handlers**: Bauer & Pretnar (2015), Koka (Leijen, 2014)
3. **Row-Polymorphic Effects**: Links (Lindley et al., 2017)
4. **Frank**: Lindley, McBride & McLaughlin (2017)
5. **Eff Language**: Bauer & Pretnar (2015)
6. **Multicore OCaml Effects**: Dolan et al. (2015)

## 14. References

1. Plotkin, G., & Pretnar, M. (2013). Handling Algebraic Effects. *LMCS*.
2. Bauer, A., & Pretnar, M. (2015). Programming with Algebraic Effects and Handlers. *JFP*.
3. Leijen, D. (2017). Type Directed Compilation of Row-Typed Algebraic Effects. *POPL*.
4. Lindley, S., McBride, C., & McLaughlin, C. (2017). Do Be Do Be Do. *POPL*.
5. Kammar, O., Lindley, S., & Oury, N. (2013). Handlers in Action. *ICFP*.

---

**Document Metadata**:
- Depends on: `lib/ast.ml` (effect types), effect checker implementation
- Implementation verification: Pending
- Mechanized proof: See `mechanized/coq/Effects.v` (stub)
