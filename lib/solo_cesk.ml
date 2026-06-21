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
  (* ── M2 (ADR-0025): deep algebraic effects + multi-shot resume (#555). ──────── *)
  | Perform of int * term                    (** perform op^i arg — raise effect op [i] with [arg] *)
  | Handle of term * handler                 (** handle body with { return … | op_i … } *)
  | Resume of term * term                    (** resume k arg — re-enter a captured continuation *)

(** A deep effect handler. [h_ret] is the [return x^q ⇒ body] clause run when the
    handled body returns normally. Each op clause [(op, arg_q, resume_q, body)]
    handles [perform op v]: its [body] sees de Bruijn 0 = the operation argument
    [v] (quantity [arg_q]) and de Bruijn 1 = the resumption [k] (quantity
    [resume_q]); [resume_q = Omega] permits the *multi-shot* re-entry. Deep: the
    captured resumption re-installs this handler, so a [perform] inside the
    resumed computation is caught here again. *)
and handler = {
  h_ret : quantity * term;
  h_ops : (int * quantity * quantity * term) list;
}

(* ── VM-native tagged values (Q2). ───────────────────────────────────────────── *)
(* [value], [binding]/[qenv], [frame] and [kont] are ONE recursive group: a
   captured continuation is itself a first-class [value] ([VCont of kont]), and a
   frame closes over an [qenv] of [value]s — so the knot must be tied together. *)
type value =
  | VUnit
  | VClos of qenv * quantity * term          (** closure: captured env, param quantity, body *)
  | VPair of value * value
  | VInl of value
  | VInr of value
  | VCont of kont                             (** M2: a reified resumption — DATA, hence multi-shot *)

and binding = { v : value; q : quantity; mutable used : int }
and qenv = binding list                       (** de Bruijn: index 0 = most recent binding *)

(* ── Continuation frames — defunctionalised (Q1): data, not closures. ─────────── *)
and frame =
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
  (* ── M2 effect frames. ─────────────────────────────────────────────────────── *)
  | KHandle of qenv * handler                 (** a handler boundary on the stack (deep) *)
  | KPerform of int                           (** evaluating a perform's argument; op label *)
  | KResumeFn of qenv * term                  (** evaluated the resumption; next eval its arg *)
  | KResumeArg of kont                         (** have the captured cont; evaluating the value to inject *)

and kont = frame list

type control = Eval of term | Ret of value

type state = { ctrl : control; env : qenv; k : kont; cost : int }

(** Run configuration. [enforce] = affine discipline on (Q4 default); [budget] caps
    the tropical cost (None ⇒ ∞). *)
type config = { enforce : bool; budget : int option }

let default_config = { enforce = true; budget = None }

exception Affine_violation of string
exception Stuck of string                     (** ill-typed at runtime — impossible on well-typed terms *)
exception Infeasible of int                   (** tropical cost budget exceeded *)
exception Unhandled_effect of int             (** M2: [perform op] with no enclosing handler for [op] *)

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

(** Search the continuation for the nearest handler of [op]. Returns
    [(captured, henv, arg_q, res_q, body, krest)] where [captured] is the
    delimited continuation up to AND INCLUDING that handler frame — so resuming
    re-installs the handler (DEEP handlers: a [perform] inside the resumed
    computation is caught here again) — [henv] is the handler's captured env,
    [(arg_q, res_q, body)] is the matching op clause, and [krest] is the
    continuation *beyond* the handler (what the whole [handle] returns into). An
    inner handler that does not handle [op] is part of the delimited
    continuation, so it stays installed in the resumption. Raises
    {!Unhandled_effect} if no handler matches. *)
let rec find_handler op (k : kont) (acc : frame list)
  : kont * qenv * quantity * quantity * term * kont =
  match k with
  | [] -> raise (Unhandled_effect op)
  | (KHandle (henv, h) as hf) :: rest ->
    (match List.find_opt (fun (o, _, _, _) -> o = op) h.h_ops with
     | Some (_, arg_q, res_q, body) -> (List.rev (hf :: acc), henv, arg_q, res_q, body, rest)
     | None -> find_handler op rest (hf :: acc))
  | f :: rest -> find_handler op rest (f :: acc)

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
  (* M2: effect constructs push their frames, then evaluate left-to-right *)
  | Eval (Perform (op, arg)), _ -> { ctrl = Eval arg; env = st.env; k = KPerform op :: st.k; cost }
  | Eval (Handle (body, h)), _  -> { ctrl = Eval body; env = st.env; k = KHandle (st.env, h) :: st.k; cost }
  | Eval (Resume (kt, at)), _   -> { ctrl = Eval kt; env = st.env; k = KResumeFn (st.env, at) :: st.k; cost }
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
  (* ── M2: effect refocus rules. ─────────────────────────────────────────────── *)
  (* handled body returned normally → run the handler's [return] clause *)
  | Ret v, KHandle (henv, h) :: k ->
    let (rq, rbody) = h.h_ret in { ctrl = Eval rbody; env = bind v rq henv; k; cost }
  (* perform: find the nearest handler for [op]; bind arg (de Bruijn 0) and the
     reified resumption (de Bruijn 1); run the op clause with the handler's own
     continuation [krest].  The resumption re-installs the handler (deep). *)
  | Ret v, KPerform op :: k ->
    let (captured, henv, arg_q, res_q, body, krest) = find_handler op k [] in
    { ctrl = Eval body; env = bind v arg_q (bind (VCont captured) res_q henv); k = krest; cost }
  (* resume: evaluate the continuation expr, then its argument *)
  | Ret (VCont kc), KResumeFn (env, at) :: k -> { ctrl = Eval at; env; k = KResumeArg kc :: k; cost }
  | Ret _, KResumeFn _ :: _    -> raise (Stuck "resume of a non-continuation")
  (* inject the value into the captured continuation and run it; [kc] is data, so
     this same [VCont] may be resumed again — MULTI-SHOT (#555). *)
  | Ret v, KResumeArg kc :: k  -> { ctrl = Ret v; env = st.env; k = kc @ k; cost }
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
  | VCont _ -> "<cont>"
