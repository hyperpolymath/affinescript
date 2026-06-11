(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Integer-operand classifier for JS-family backends (#478).

    JS `/` is floating-point division, so a naive lowering of `Int / Int`
    yields `255 / 16 === 15.9375` rather than `15`. We need to emit
    `Math.trunc(a / b)` for integer-typed operands and keep `/` for
    everything else.

    The classifier is conservative: it reports `true` only when an
    operand is *provably* an integer. Unknown-type operands keep plain
    `/`, so a float is never silently truncated.

    The Deno-ESM backend (lib/codegen_deno.ml) carries its own copy of
    the same logic. This module is the canonical source; the long-term
    plan is to migrate codegen_deno to use it as well. For now both
    must stay in sync — the rules are tiny enough that drift is easy
    to spot if it happens. *)

open Ast

(* ---------------------------------------------------------------------- *
 * Pure type predicates
 * ---------------------------------------------------------------------- *)

(** [Int] head of a (possibly ref/own/mut) type expression. Only the
    nominal [Int] constructor counts; type variables / applications do
    not. *)
let rec type_head_is_int : type_expr -> bool = function
  | TyCon id -> id.name = "Int"
  | TyOwn t | TyRef t | TyMut t -> type_head_is_int t
  | _ -> false

(** Is [t] an [Array<Int>] (surface `[Int]`, which the parser desugars
    to `Array[Int]`)? Used to seed for-loop variables and recognise
    indexed element reads as integers. *)
let rec type_is_int_array : type_expr -> bool = function
  | TyApp (id, [ TyArg elem ]) -> id.name = "Array" && type_head_is_int elem
  | TyOwn t | TyRef t | TyMut t -> type_is_int_array t
  | _ -> false

(** Simple-variable pattern name, if [pat] binds exactly one name. *)
let pat_var_name : pattern -> string option = function
  | PatVar id -> Some id.name
  | _ -> None

(** Builtins whose return type is unambiguously [Int]. Calls to these
    count as integer operands. (Excludes e.g. [parse_int], which is
    Option<Int>.) *)
let int_returning_builtins =
  [ "len"; "string_find"; "string_char_code_at"; "char_to_int";
    "string_length" ]

(* ---------------------------------------------------------------------- *
 * Mutable per-pass state
 * ---------------------------------------------------------------------- *)

(** Three tables shared across a single codegen pass:
    - [int_vars]:        local names currently known to hold an [Int]
    - [int_array_vars]:  local names currently known to hold an [Array<Int>]
    - [int_fns]:         module-level fn names with an [Int] return type

    The first two are mutable: they get reset on entering a new function
    scope. The third is populated once per program before codegen begins. *)
type state = {
  mutable int_vars : (string, unit) Hashtbl.t;
  mutable int_array_vars : (string, unit) Hashtbl.t;
  int_fns : (string, unit) Hashtbl.t;
}

let create () = {
  int_vars = Hashtbl.create 16;
  int_array_vars = Hashtbl.create 16;
  int_fns = Hashtbl.create 64;
}

(* ---------------------------------------------------------------------- *
 * Expression classifier
 * ---------------------------------------------------------------------- *)

(** Conservative "is this expression provably an [Int]?" Used only to
    decide whether a [/] should truncate; a [false] is always safe
    (keeps [/], so no float regression). *)
let rec expr_is_int (st : state) (e : expr) : bool =
  match e with
  | ExprLit (LitInt _) -> true
  | ExprLit _ -> false
  | ExprSpan (e, _) -> expr_is_int st e
  | ExprVar id -> Hashtbl.mem st.int_vars id.name
  (* Integer-closed arithmetic: result is [Int] iff both operands are.
     [OpDiv] is included because the truncating emission below makes
     `Int / Int` produce an [Int]. *)
  | ExprBinary (a, (OpAdd | OpSub | OpMul | OpDiv | OpMod), b) ->
      expr_is_int st a && expr_is_int st b
  (* JS bitwise operators coerce to a 32-bit integer regardless of
     input, so the result is always an integer. *)
  | ExprBinary (_, (OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr), _) -> true
  | ExprUnary (OpNeg, e) -> expr_is_int st e
  | ExprUnary (OpBitNot, _) -> true
  | ExprIf { ei_then; ei_else = Some e; _ } ->
      expr_is_int st ei_then && expr_is_int st e
  | ExprApp (ExprVar id, _) ->
      Hashtbl.mem st.int_fns id.name
      || List.mem id.name int_returning_builtins
  (* An element read from a provably-[Array<Int>] value is an [Int]
     (covers `xs[i] / 2` where xs: [Int]). *)
  | ExprIndex (arr, _) -> expr_is_int_array st arr
  | _ -> false

(** Conservative "is this expression provably an [Array<Int>]?"
    Recognises int-array params/locals and array literals whose every
    element is an integer. As with {!expr_is_int}, a [false] is always
    safe. *)
and expr_is_int_array (st : state) (e : expr) : bool =
  match e with
  | ExprSpan (e, _) -> expr_is_int_array st e
  | ExprVar id -> Hashtbl.mem st.int_array_vars id.name
  | ExprArray elems -> elems <> [] && List.for_all (expr_is_int st) elems
  | _ -> false

(* ---------------------------------------------------------------------- *
 * Binding tracking
 * ---------------------------------------------------------------------- *)

(** Record/forget whether [name] currently holds an [Int], after a
    binding or assignment of [value] to it. Keeps [st.int_vars] in
    sync as a function body is emitted top-to-bottom. *)
let track_int_binding (st : state) (name : string) (value : expr) : unit =
  if expr_is_int st value then Hashtbl.replace st.int_vars name ()
  else Hashtbl.remove st.int_vars name

(** Similar for array-of-int tracking. *)
let track_int_array_binding (st : state) (name : string) (value : expr) : unit =
  if expr_is_int_array st value then Hashtbl.replace st.int_array_vars name ()
  else Hashtbl.remove st.int_array_vars name

(** Reset [int_vars] / [int_array_vars] for a new function body and
    seed them from [Int]- and [Array<Int>]-typed params. *)
let enter_fn_scope (st : state) (params : param list) : unit =
  st.int_vars <- Hashtbl.create 16;
  st.int_array_vars <- Hashtbl.create 16;
  List.iter
    (fun (p : param) ->
      if type_head_is_int p.p_ty then
        Hashtbl.replace st.int_vars p.p_name.name ()
      else if type_is_int_array p.p_ty then
        Hashtbl.replace st.int_array_vars p.p_name.name ())
    params

(** Register an [Int]-returning fn so calls to it count as [Int]
    operands. Call once per module-level decl before emitting bodies. *)
let register_int_fn (st : state) (name : string) : unit =
  Hashtbl.replace st.int_fns name ()
