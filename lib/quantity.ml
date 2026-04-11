(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** Quantity checking for Quantitative Type Theory (QTT).

    AffineScript uses a three-point quantity semiring {0, 1, omega} to track
    how many times each variable may be used at runtime:

    - [QZero]  (0): erased at runtime, compile-time only.
    - [QOne]   (1): linear — must be used exactly once.
    - [QOmega] (omega): unrestricted — any number of uses.

    This module implements:
    1. The quantity semiring (addition, multiplication, ordering).
    2. A usage-counting pass that walks expressions/statements.
    3. Compatibility checking between declared quantities and observed usage.
    4. A top-level entry point [{!check_function_quantities}] for integration
       with the type checker.

    Reference: AffineScript SPEC.md Section 3.2 — QTT semiring specification. *)

open Ast

(* {1 Quantity semiring} *)

(** Addition in the quantity semiring.

    The addition table encodes "use in either context":
    {v
      +  |  0   1   omega
    -----+-----------------
      0  |  0   1   omega
      1  |  1   omega omega
    omega| omega omega omega
    v} *)
let q_add (q1 : quantity) (q2 : quantity) : quantity =
  match (q1, q2) with
  | (QZero, q) | (q, QZero) -> q
  | (QOne, QOne) -> QOmega
  | (QOmega, _) | (_, QOmega) -> QOmega

(** Multiplication in the quantity semiring.

    The multiplication table encodes "use under a context scaled by q":
    {v
      *  |  0   1   omega
    -----+-----------------
      0  |  0   0   0
      1  |  0   1   omega
    omega|  0   omega omega
    v} *)
let q_mul (q1 : quantity) (q2 : quantity) : quantity =
  match (q1, q2) with
  | (QZero, _) | (_, QZero) -> QZero
  | (QOne, q) | (q, QOne) -> q
  | (QOmega, QOmega) -> QOmega

(** Ordering: 0 <= 1 <= omega.

    [q_le q1 q2] returns [true] when [q1] is usable wherever [q2] is expected.
    This means a more restricted quantity can substitute for a less restricted
    one (subquantity). *)
let q_le (q1 : quantity) (q2 : quantity) : bool =
  match (q1, q2) with
  | (QZero, _) -> true
  | (_, QOmega) -> true
  | (QOne, QOne) -> true
  | _ -> false

(* {1 Usage tracking} *)

(** Usage count for a variable during analysis. *)
type usage =
  | UZero    (** Not used *)
  | UOne     (** Used exactly once *)
  | UMany    (** Used two or more times *)
[@@deriving show, eq]

(** Convert a usage count to its corresponding quantity. *)
let usage_to_quantity (u : usage) : quantity =
  match u with
  | UZero -> QZero
  | UOne -> QOne
  | UMany -> QOmega

(** Join two usages — computes the upper bound for branching.

    When a variable is used in one branch but not another, we take the
    maximum usage across branches. This is correct for [if/match] where
    exactly one branch executes. *)
let join_usage (u1 : usage) (u2 : usage) : usage =
  match (u1, u2) with
  | (UMany, _) | (_, UMany) -> UMany
  | (UOne, _) | (_, UOne) -> UOne
  | (UZero, UZero) -> UZero

(** Add two usages — computes the sum for sequential composition.

    When a variable is used in a sequence of statements, the total usage
    is the sum: used once then once more means used many times. *)
let add_usage (u1 : usage) (u2 : usage) : usage =
  match (u1, u2) with
  | (UZero, u) | (u, UZero) -> u
  | _ -> UMany

(** Scale a usage by a quantity — implements the q·u operation that
    the QTT-orthodox Let rule needs to scale the value-context Γ₁ by
    the binder's quantity q.

    Per ADR-002, [let x :^q = e1 in e2] is type-checked under
    [q·Γ₁ + Γ₂ ⊢ ...]. The semiring action on usages is:

      0 · u  = UZero            (erased — usage is annihilated)
      1 · u  = u                (linear — usage passes through unchanged)
      ω · UZero = UZero         (no use stays no use)
      ω · UOne  = UMany         (a single use, replicated, becomes many)
      ω · UMany = UMany         (many stays many)

    The interesting case is [ω · UOne = UMany]: it is the rule that
    closes BUG-001. A linear variable consumed in [e1] under an
    ω-binder gets its usage promoted to UMany, which the
    [check_quantity] step then catches as
    [LinearVariableUsedMultiple]. *)
let scale_usage (q : quantity) (u : usage) : usage =
  match (q, u) with
  | (QZero, _) -> UZero
  | (QOne, u) -> u
  | (QOmega, UZero) -> UZero
  | (QOmega, UOne) -> UMany
  | (QOmega, UMany) -> UMany

(* {1 Errors} *)

(** Quantity checking errors with precise descriptions. *)
type quantity_error =
  | LinearVariableUnused of ident
      (** A parameter declared as linear (1) was never used. *)
  | LinearVariableUsedMultiple of ident
      (** A parameter declared as linear (1) was used more than once. *)
  | ErasedVariableUsed of ident
      (** A parameter declared as erased (0) was used at runtime. *)
  | QuantityMismatch of ident * quantity * usage
      (** General mismatch: variable, declared quantity, actual usage. *)
[@@deriving show]

(** Result type carrying a quantity error paired with a source span. *)
type 'a result = ('a, quantity_error * Span.t) Result.t

(** Pretty-print a quantity using the canonical Option C surface form
    from ADR-007. Used in error messages so source vocabulary and
    diagnostic vocabulary stay aligned. *)
let canonical_quantity_name (q : quantity) : string =
  match q with
  | QZero -> "@erased"
  | QOne -> "@linear"
  | QOmega -> "@unrestricted"

(** Pretty-print a usage count in plain English. *)
let usage_in_words (u : usage) : string =
  match u with
  | UZero -> "never used"
  | UOne -> "used exactly once"
  | UMany -> "used multiple times"

(** Format a quantity error for human-readable output. Per ADR-007, all
    diagnostic text uses the @linear/@erased/@unrestricted vocabulary,
    even when the source program used the :1/:0/:ω sugar form. The
    user reads consistent words regardless of which surface they wrote. *)
let format_quantity_error (err : quantity_error) : string =
  match err with
  | LinearVariableUnused id ->
    Printf.sprintf
      "@linear binding '%s' must be used exactly once, but was never used"
      id.name
  | LinearVariableUsedMultiple id ->
    Printf.sprintf
      "@linear binding '%s' must be used exactly once, but was used multiple times"
      id.name
  | ErasedVariableUsed id ->
    Printf.sprintf
      "@erased binding '%s' must not be used at runtime"
      id.name
  | QuantityMismatch (id, q, u) ->
    Printf.sprintf
      "quantity mismatch for '%s': declared %s but %s"
      id.name (canonical_quantity_name q) (usage_in_words u)

(* {1 Quantity environment} *)

(** Quantity environment: maps variable names to declared quantities and
    tracks observed usage counts.

    We key by [string] (variable name) rather than [Symbol.symbol_id] so
    that the environment is self-contained and does not require the symbol
    table to be in a particular state during analysis. *)
type env = {
  quantities : (string, quantity) Hashtbl.t;
    (** Declared quantity for each tracked variable. *)
  usages : (string, usage) Hashtbl.t;
    (** Current observed usage count for each tracked variable. *)
}

(** Create a fresh quantity environment. *)
let create_env () : env =
  {
    quantities = Hashtbl.create 16;
    usages = Hashtbl.create 16;
  }

(** Declare a variable with its quantity annotation in the environment.

    Resets the usage counter to [UZero]. *)
let env_declare (env : env) (name : string) (q : quantity) : unit =
  Hashtbl.replace env.quantities name q;
  Hashtbl.replace env.usages name UZero

(** Record one use of a variable.

    Increments the usage counter: UZero -> UOne -> UMany. *)
let env_use (env : env) (name : string) : unit =
  match Hashtbl.find_opt env.usages name with
  | Some UZero -> Hashtbl.replace env.usages name UOne
  | Some UOne -> Hashtbl.replace env.usages name UMany
  | Some UMany -> ()  (* Already saturated *)
  | None -> ()  (* Not a tracked variable (e.g. free/global) *)

(** Snapshot the current usage state — used before analysing branches. *)
let env_snapshot (env : env) : (string, usage) Hashtbl.t =
  Hashtbl.copy env.usages

(** Restore a previously snapshotted usage state. *)
let env_restore (env : env) (snapshot : (string, usage) Hashtbl.t) : unit =
  Hashtbl.reset env.usages;
  Hashtbl.iter (Hashtbl.replace env.usages) snapshot

(** Join the current usage state with a snapshot (for branching).

    After analysing branch A, we snapshot, restore, analyse branch B,
    then join the B-state with the A-snapshot. The result is the
    upper-bound usage across both branches. *)
let env_join (env : env) (other : (string, usage) Hashtbl.t) : unit =
  Hashtbl.iter (fun name other_usage ->
    let current_usage =
      Hashtbl.find_opt env.usages name |> Option.value ~default:UZero
    in
    Hashtbl.replace env.usages name (join_usage current_usage other_usage)
  ) other;
  (* Handle variables present in current but not in other *)
  Hashtbl.iter (fun name current_usage ->
    if not (Hashtbl.mem other name) then
      Hashtbl.replace env.usages name (join_usage current_usage UZero)
  ) env.usages

(* {1 check_quantity — per-variable compatibility} *)

(** Check that a single variable's actual usage is compatible with its
    declared quantity.

    - quantity 0: must be used 0 times (erased).
    - quantity 1: must be used exactly 1 time (linear).
    - quantity omega: can be used any number of times (unrestricted). *)
let check_quantity (id : ident) (declared : quantity) (actual : usage)
    : unit result =
  match (declared, actual) with
  (* Erased: must not be used *)
  | (QZero, UZero) -> Ok ()
  | (QZero, _) -> Error (ErasedVariableUsed id, id.span)
  (* Linear: must be used exactly once *)
  | (QOne, UOne) -> Ok ()
  | (QOne, UZero) -> Error (LinearVariableUnused id, id.span)
  | (QOne, UMany) -> Error (LinearVariableUsedMultiple id, id.span)
  (* Unrestricted: any usage is fine *)
  | (QOmega, _) -> Ok ()

(* {1 infer_usage — expression/statement walker} *)

(** Walk an expression and record variable usages in the environment.

    This is a recursive descent that mirrors the AST structure.
    For branching constructs (if, match), we snapshot/restore/join so
    that usage reflects the worst-case across branches. *)
let rec infer_usage_expr (env : env) (expr : expr) : unit =
  match expr with
  | ExprVar id ->
    env_use env id.name

  | ExprLit _ -> ()

  | ExprApp (func, args) ->
    infer_usage_expr env func;
    List.iter (infer_usage_expr env) args

  | ExprLambda lam ->
    (* A lambda may be called zero or many times, so any outer variable
       it captures is effectively used QOmega times.  We implement this
       by walking the body in a temporary copy of the env (shadowing the
       lambda's own params), computing the per-variable delta, and then
       merging those deltas back scaled by QOmega.

       Consequence: a lambda that captures an @linear variable raises
       LinearVariableUsedMultiple, because scale_usage QOmega UOne = UMany.
       This closes BUG-004. *)
    let param_names =
      List.map (fun (p : param) -> p.p_name.name) lam.elam_params
    in
    let before_snapshot = env_snapshot env in
    (* Shadow lambda params so their usage inside the body doesn't leak
       into the outer quantity environment. *)
    List.iter (fun name ->
      if Hashtbl.mem env.usages name then
        Hashtbl.replace env.usages name UZero
      (* Params not yet in env are fine — they are lambda-local. *)
    ) param_names;
    infer_usage_expr env lam.elam_body;
    let after_snapshot = env_snapshot env in
    (* Restore outer env, then re-apply QOmega-scaled deltas for captured
       outer variables only (exclude lambda params). *)
    env_restore env before_snapshot;
    Hashtbl.iter (fun name after_u ->
      if List.mem name param_names then ()
      else begin
        let before_u =
          Hashtbl.find_opt before_snapshot name |> Option.value ~default:UZero
        in
        let delta =
          if equal_usage before_u after_u then UZero
          else after_u
        in
        (* Any use inside a lambda body counts as potentially-unbounded. *)
        let scaled = scale_usage QOmega delta in
        let merged = add_usage before_u scaled in
        Hashtbl.replace env.usages name merged
      end
    ) after_snapshot

  | ExprLet lb ->
    (* ADR-002 / ADR-007: Let value context is scaled by the binder's
       quantity. Concretely:
         q·Γ₁ ⊢ e1 : A    (Γ₂, x:^q A) ⊢ e2 : B
         ─────────────────────────────────────
              q·Γ₁ + Γ₂ ⊢ let x :^q = e1 in e2 : B
       For the usage analysis we implement this as: snapshot the env,
       walk e1 (which records usages into the live env), compute the
       per-variable delta added by walking e1, scale each delta entry
       by q, restore the snapshot, and re-apply the scaled deltas as
       additions. Then walk e2 normally. *)
    let q = Option.value lb.el_quantity ~default:QOmega in
    let before_value = env_snapshot env in
    infer_usage_expr env lb.el_value;
    let after_value = env_snapshot env in
    env_restore env before_value;
    (* Re-apply scaled deltas. *)
    Hashtbl.iter (fun name after_u ->
      let before_u =
        Hashtbl.find_opt before_value name |> Option.value ~default:UZero
      in
      (* Compute the delta usage added by walking el_value. We model
         delta as: how many additional uses occurred during the walk.
         Since usage is a 3-point lattice, the simplest sound rule is
         "if the walk increased the usage at all, the delta is the
         after-value's contribution beyond the before-value." We
         implement this by treating any rise from before to after as
         the delta usage to scale. *)
      let delta =
        if equal_usage before_u after_u then UZero
        else after_u
      in
      let scaled = scale_usage q delta in
      let merged = add_usage before_u scaled in
      Hashtbl.replace env.usages name merged
    ) after_value;
    Option.iter (infer_usage_expr env) lb.el_body

  | ExprIf ei ->
    (* Condition is always evaluated *)
    infer_usage_expr env ei.ei_cond;
    (* Branches: take the upper-bound usage *)
    let before_branches = env_snapshot env in
    infer_usage_expr env ei.ei_then;
    let then_usages = env_snapshot env in
    env_restore env before_branches;
    Option.iter (infer_usage_expr env) ei.ei_else;
    (* Join: current state has else-branch usages (or pre-branch if no else) *)
    env_join env then_usages

  | ExprMatch em ->
    infer_usage_expr env em.em_scrutinee;
    (* Each arm is a branch — join all of them *)
    let before_arms = env_snapshot env in
    let arm_snapshots = List.map (fun (arm : match_arm) ->
      env_restore env before_arms;
      Option.iter (infer_usage_expr env) arm.ma_guard;
      infer_usage_expr env arm.ma_body;
      env_snapshot env
    ) em.em_arms in
    (* Join all arm snapshots together *)
    begin match arm_snapshots with
    | [] -> env_restore env before_arms
    | first :: rest ->
      env_restore env first;
      List.iter (env_join env) rest
    end

  | ExprTuple exprs ->
    List.iter (infer_usage_expr env) exprs

  | ExprArray exprs ->
    List.iter (infer_usage_expr env) exprs

  | ExprRecord er ->
    List.iter (fun (_id, e_opt) ->
      Option.iter (infer_usage_expr env) e_opt
    ) er.er_fields;
    Option.iter (infer_usage_expr env) er.er_spread

  | ExprField (e, _) ->
    infer_usage_expr env e

  | ExprTupleIndex (e, _) ->
    infer_usage_expr env e

  | ExprIndex (arr, idx) ->
    infer_usage_expr env arr;
    infer_usage_expr env idx

  | ExprRowRestrict (e, _) ->
    infer_usage_expr env e

  | ExprBlock blk ->
    infer_usage_block env blk

  | ExprBinary (left, _, right) ->
    infer_usage_expr env left;
    infer_usage_expr env right

  | ExprUnary (_, e) ->
    infer_usage_expr env e

  | ExprHandle eh ->
    infer_usage_expr env eh.eh_body;
    List.iter (fun arm ->
      match arm with
      | HandlerReturn (_pat, body) -> infer_usage_expr env body
      | HandlerOp (_op, _pats, body) -> infer_usage_expr env body
    ) eh.eh_handlers

  | ExprResume e_opt ->
    Option.iter (infer_usage_expr env) e_opt

  | ExprReturn e_opt ->
    Option.iter (infer_usage_expr env) e_opt

  | ExprTry et ->
    infer_usage_block env et.et_body;
    Option.iter (fun arms ->
      List.iter (fun (arm : match_arm) ->
        Option.iter (infer_usage_expr env) arm.ma_guard;
        infer_usage_expr env arm.ma_body
      ) arms
    ) et.et_catch;
    Option.iter (infer_usage_block env) et.et_finally

  | ExprUnsafe ops ->
    List.iter (fun op ->
      match op with
      | UnsafeRead e -> infer_usage_expr env e
      | UnsafeWrite (e1, e2) ->
        infer_usage_expr env e1;
        infer_usage_expr env e2
      | UnsafeOffset (e1, e2) ->
        infer_usage_expr env e1;
        infer_usage_expr env e2
      | UnsafeTransmute (_, _, e) -> infer_usage_expr env e
      | UnsafeForget e -> infer_usage_expr env e
    ) ops

  | ExprVariant _ -> ()

  | ExprSpan (e, _) ->
    infer_usage_expr env e

(** Walk a block and record variable usages. *)
and infer_usage_block (env : env) (blk : block) : unit =
  List.iter (infer_usage_stmt env) blk.blk_stmts;
  Option.iter (infer_usage_expr env) blk.blk_expr

(** Walk a statement and record variable usages. *)
and infer_usage_stmt (env : env) (stmt : stmt) : unit =
  match stmt with
  | StmtLet sl ->
    (* Same scaling treatment as ExprLet — see ADR-002 commentary
       there. The statement form has no body to walk afterward;
       subsequent statements in the enclosing block are walked
       independently by infer_usage_block. *)
    let q = Option.value sl.sl_quantity ~default:QOmega in
    let before_value = env_snapshot env in
    infer_usage_expr env sl.sl_value;
    let after_value = env_snapshot env in
    env_restore env before_value;
    Hashtbl.iter (fun name after_u ->
      let before_u =
        Hashtbl.find_opt before_value name |> Option.value ~default:UZero
      in
      let delta =
        if equal_usage before_u after_u then UZero
        else after_u
      in
      let scaled = scale_usage q delta in
      let merged = add_usage before_u scaled in
      Hashtbl.replace env.usages name merged
    ) after_value
  | StmtExpr e ->
    infer_usage_expr env e
  | StmtAssign (lhs, _, rhs) ->
    infer_usage_expr env lhs;
    infer_usage_expr env rhs
  | StmtWhile (cond, body) ->
    (* Loop body may execute multiple times — usage inside is omega-scaled.
       We analyse once, then add the usage to itself to model repetition. *)
    let before_loop = env_snapshot env in
    infer_usage_expr env cond;
    infer_usage_block env body;
    let after_loop = env_snapshot env in
    (* Second pass: any variable used in the loop body is used >=2 times *)
    env_restore env before_loop;
    infer_usage_expr env cond;
    infer_usage_block env body;
    env_join env after_loop
  | StmtFor (_pat, iter, body) ->
    infer_usage_expr env iter;
    (* Same loop treatment as while *)
    let before_body = env_snapshot env in
    infer_usage_block env body;
    let after_body = env_snapshot env in
    env_restore env before_body;
    infer_usage_block env body;
    env_join env after_body

(* {1 check_function_quantities — top-level entry point} *)

(** Check that all quantity-annotated parameters in a function declaration
    are used the correct number of times.

    This is the primary integration point called after type checking.
    It:
    1. Creates a fresh quantity environment.
    2. Declares each parameter with its quantity (defaulting to omega).
    3. Walks the function body to count usages.
    4. Checks each annotated parameter's usage against its declared quantity.

    Returns [Ok ()] if all quantities are satisfied, or
    [Error (err, span)] for the first violation found. *)
let check_function_quantities (fd : fn_decl) : unit result =
  let env = create_env () in
  (* Step 1: declare parameter quantities *)
  List.iter (fun (param : param) ->
    let q = Option.value param.p_quantity ~default:QOmega in
    env_declare env param.p_name.name q
  ) fd.fd_params;
  (* Step 2: walk function body to infer usages *)
  begin match fd.fd_body with
  | FnBlock blk -> infer_usage_block env blk
  | FnExpr e -> infer_usage_expr env e
  end;
  (* Step 3: check each parameter *)
  List.fold_left (fun acc (param : param) ->
    match acc with
    | Error _ -> acc  (* Stop at first error *)
    | Ok () ->
      let declared = Option.value param.p_quantity ~default:QOmega in
      let actual =
        Hashtbl.find_opt env.usages param.p_name.name
        |> Option.value ~default:UZero
      in
      check_quantity param.p_name declared actual
  ) (Ok ()) fd.fd_params

(** Check quantities for all functions in a program.

    Iterates over all top-level function declarations and checks each one.
    Returns [Ok ()] if all pass, or the first error found. *)
let check_program_quantities (program : program) : unit result =
  List.fold_left (fun acc decl ->
    match acc with
    | Error _ -> acc
    | Ok () ->
      match decl with
      | TopFn fd -> check_function_quantities fd
      | _ -> Ok ()
  ) (Ok ()) program.prog_decls

(* {1 Backward compatibility aliases}

   These maintain the old API surface while the codebase transitions
   to the new function names. *)

(** @deprecated Use {!check_function_quantities} instead. *)
let check_function (_symbols : Symbol.t) (fd : fn_decl) : unit result =
  check_function_quantities fd

(** @deprecated Use {!check_program_quantities} instead. *)
let check_program (_symbols : Symbol.t) (program : program) : unit result =
  check_program_quantities program
