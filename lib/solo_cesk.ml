(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)

(** Solo-core CESK abstract machine — the semantics VM, milestone M1 (ADR-0025).

    Per ADR-0025 (owner sign-off 2026-06-16), four decisions are realised here:

    - *Substrate (Q1):* a CESK abstract machine — explicit (C)ontrol, (E)nvironment,
      (K)ontinuation — with a *defunctionalised* continuation (a {!frame} list, i.e.
      DATA not host closures). This is the stepping-stone toward a flat bytecode VM:
      one more defunctionalisation step turns {!frame} into an opcode stream, without
      changing the semantics (Reynolds / Danvy). Solo is pure, so the (S)tore is
      trivial and elided until effects/mutation arrive at M2.
    - *Values (Q2):* a VM-native tagged {!value}, GC'd by the OCaml runtime, with the
      QTT quantity carried on each {!binding} — so runtime affine enforcement is cheap.
    - *Scope (Q3):* the reduced Solo core only — STLC + Unit + ⊗-products + ⊕-sums +
      let, each binder QTT-annotated. This is exactly the fragment mechanised by
      [docs/academic/formal-verification/solo-core/Soundness.idr]; the CESK transitions
      below ARE that small-step [Step] relation (the proof-oracle coupling).
    - *Enforcement (Q4):* affine discipline + tropical cost-metering are ON by default;
      [{enforce = false}] is the [--unchecked] escape.

    Eager call-by-value, matching the Solo operational semantics. The two projections
    [Fst]/[Snd] DROP the unprojected component — legitimate because AffineScript is
    affine (use ≤ once); this is the runtime face of the affine-preservation theorem. *)

(* ── Quantities — mirrors Quantity.idr / lib/quantity.ml {0, 1, ω}. ───────────── *)
type quantity = Zero | One | Omega

let q_str = function Zero -> "0" | One -> "1" | Omega -> "ω"

(** Affine use bound: the maximum number of times a binding may be dereferenced.
    Zero ⇒ erased (0 uses), One ⇒ at most once (affine), ω ⇒ unrestricted. *)
let q_bound = function Zero -> 0 | One -> 1 | Omega -> max_int

(* ── Solo terms, de Bruijn indices (mirrors Syntax.idr). ──────────────────────── *)
type term =
  | Var of int
  | Lam of quantity * term                  (** λ^q. body — binder carries its quantity *)
  | App of term * term
  | TUnit
  | Pair of term * term                     (** ⊗ intro (split context; eager CBV) *)
  | Fst of term
  | Snd of term
  | Inl of term
  | Inr of term
  | Case of term * quantity * term * term    (** case s of inl x^q ⇒ l | inr y^q ⇒ r *)
  | Let of quantity * term * term            (** let x^q = rhs in body *)

(* ── VM-native tagged values (Q2). ───────────────────────────────────────────── *)
type value =
  | VUnit
  | VClos of qenv * quantity * term          (** closure: captured env, param quantity, body *)
  | VPair of value * value
  | VInl of value
  | VInr of value

and binding = { v : value; q : quantity; mutable used : int }
and qenv = binding list                       (** de Bruijn: index 0 = most recent binding *)

(* ── Continuation frames — defunctionalised (Q1): data, not closures. ─────────── *)
type frame =
  | KAppFn of qenv * term                     (** evaluated f; next eval arg in this env *)
  | KAppArg of value                          (** have the closure; evaluating the arg *)
  | KPair1 of qenv * term                     (** evaluated fst; next eval snd *)
  | KPair2 of value                           (** have v1; evaluating v2 *)
  | KFst
  | KSnd
  | KInl
  | KInr
  | KCase of qenv * quantity * term * term
  | KLet of qenv * quantity * term

type kont = frame list

type control = Eval of term | Ret of value

type state = { ctrl : control; env : qenv; k : kont; cost : int }

(** Run configuration. [enforce] = affine discipline on (Q4 default); [budget] caps
    the tropical cost (None ⇒ ∞). *)
type config = { enforce : bool; budget : int option }

let default_config = { enforce = true; budget = None }

exception Affine_violation of string
exception Stuck of string                     (** ill-typed at runtime — impossible on well-typed terms *)
exception Infeasible of int                   (** tropical cost budget exceeded *)

(** Dereference a de Bruijn variable, enforcing the affine bound when [enforce]. *)
let deref cfg env n =
  match List.nth_opt env n with
  | None -> raise (Stuck (Printf.sprintf "unbound de Bruijn index %d" n))
  | Some b ->
    b.used <- b.used + 1;
    if cfg.enforce && b.used > q_bound b.q then
      raise (Affine_violation
        (Printf.sprintf "var (de Bruijn %d, quantity %s) used %d time(s), bound %d"
           n (q_str b.q) b.used (q_bound b.q)));
    b.v

let bind v q env = { v; q; used = 0 } :: env

(** One CESK transition. Cost rises by 1 per step (the tropical grade; (ℕ∪{∞},min,+)
    accumulates by [+]). Each clause below is one Solo small-step rule. *)
let step cfg (st : state) : state =
  let cost = st.cost + 1 in
  (match cfg.budget with Some b when cost > b -> raise (Infeasible b) | _ -> ());
  match st.ctrl, st.k with
  (* focus: evaluate a term *)
  | Eval (Var n), _            -> { st with ctrl = Ret (deref cfg st.env n); cost }
  | Eval (Lam (q, body)), _    -> { st with ctrl = Ret (VClos (st.env, q, body)); cost }
  | Eval TUnit, _              -> { st with ctrl = Ret VUnit; cost }
  | Eval (App (f, a)), _       -> { ctrl = Eval f; env = st.env; k = KAppFn (st.env, a) :: st.k; cost }
  | Eval (Pair (a, b)), _      -> { ctrl = Eval a; env = st.env; k = KPair1 (st.env, b) :: st.k; cost }
  | Eval (Fst e), _            -> { ctrl = Eval e; env = st.env; k = KFst :: st.k; cost }
  | Eval (Snd e), _            -> { ctrl = Eval e; env = st.env; k = KSnd :: st.k; cost }
  | Eval (Inl e), _            -> { ctrl = Eval e; env = st.env; k = KInl :: st.k; cost }
  | Eval (Inr e), _            -> { ctrl = Eval e; env = st.env; k = KInr :: st.k; cost }
  | Eval (Case (s, q, l, r)), _-> { ctrl = Eval s; env = st.env; k = KCase (st.env, q, l, r) :: st.k; cost }
  | Eval (Let (q, rhs, body)), _ -> { ctrl = Eval rhs; env = st.env; k = KLet (st.env, q, body) :: st.k; cost }
  (* refocus: a value meets the top frame of the continuation *)
  | Ret v, KAppFn (env, a) :: k -> { ctrl = Eval a; env; k = KAppArg v :: k; cost }
  | Ret v, KAppArg (VClos (cenv, q, body)) :: k -> { ctrl = Eval body; env = bind v q cenv; k; cost }
  | Ret _, KAppArg _ :: _      -> raise (Stuck "application of a non-closure")
  | Ret v1, KPair1 (env, b) :: k -> { ctrl = Eval b; env; k = KPair2 v1 :: k; cost }
  | Ret v2, KPair2 v1 :: k     -> { st with ctrl = Ret (VPair (v1, v2)); k; cost }
  | Ret (VPair (a, _)), KFst :: k -> { st with ctrl = Ret a; k; cost }   (* drops snd — affine *)
  | Ret (VPair (_, b)), KSnd :: k -> { st with ctrl = Ret b; k; cost }   (* drops fst — affine *)
  | Ret _, (KFst | KSnd) :: _  -> raise (Stuck "projection of a non-pair")
  | Ret v, KInl :: k           -> { st with ctrl = Ret (VInl v); k; cost }
  | Ret v, KInr :: k           -> { st with ctrl = Ret (VInr v); k; cost }
  | Ret (VInl v), KCase (env, q, l, _) :: k -> { ctrl = Eval l; env = bind v q env; k; cost }
  | Ret (VInr v), KCase (env, q, _, r) :: k -> { ctrl = Eval r; env = bind v q env; k; cost }
  | Ret _, KCase _ :: _        -> raise (Stuck "case on a non-sum")
  | Ret v, KLet (env, q, body) :: k -> { ctrl = Eval body; env = bind v q env; k; cost }
  | Ret _, []                  -> st                                     (* final; run/1 detects *)

(** Run a closed Solo term to a value, returning (value, cost). [fuel] guards runaway
    (Solo is strongly normalising, so this is only a safety net). *)
let run ?(cfg = default_config) ?(fuel = 1_000_000) (t : term) : value * int =
  let rec loop st n =
    match st.ctrl, st.k with
    | Ret v, [] -> (v, st.cost)
    | _ -> if n <= 0 then raise (Stuck "fuel exhausted") else loop (step cfg st) (n - 1)
  in
  loop { ctrl = Eval t; env = []; k = []; cost = 0 } fuel

let rec show_value = function
  | VUnit -> "()"
  | VClos _ -> "<closure>"
  | VPair (a, b) -> "(" ^ show_value a ^ ", " ^ show_value b ^ ")"
  | VInl v -> "inl " ^ show_value v
  | VInr v -> "inr " ^ show_value v
