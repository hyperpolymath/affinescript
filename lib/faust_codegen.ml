(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Faust DSP Sublanguage Emitter (MVP).

    Lowers a strict subset of AffineScript to the Faust audio-DSP language.

    Source shape (the kernel sublanguage):
    - one or more [fn] declarations, all over [Float]
    - one of them is the entry point — by convention named [process] or
      [main]; otherwise the first [fn] in the file
    - parameters are scalar [Float] (audio samples or controls)
    - return type is [Float]
    - body uses arithmetic, comparison, [if]/[else] (lowered to [select2]),
      [let] bindings (folded into Faust [with { ... }] locals), and calls to
      a whitelist of Faust built-ins ([sin], [cos], [tanh], ...)

    Output is a single [.dsp] file consumable by the [faust] compiler, which
    can then be re-targeted to C++ / WebAudio / JUCE / VST / Csound /
    LV2 etc. — i.e. this MVP buys all of those targets at once.
*)

open Ast

exception Faust_unsupported of string
let unsupported msg = raise (Faust_unsupported msg)

(* ============================================================================
   Identifier sanitisation
   ============================================================================ *)

(* Actual Faust reserved keywords. [process] / [main] are NOT keywords —
   they are entry-point conventions and the entry function is always emitted
   under the name [process] regardless of what the source called it. *)
let faust_reserved = [
  "with"; "letrec"; "case"; "import"; "library"; "declare";
  "environment"; "component"; "ffunction"; "fconstant"; "fvariable";
  "where"; "of";
]

let mangle s =
  if List.mem s faust_reserved then "as_" ^ s else s

(* ============================================================================
   Type validation

   Faust is essentially monomorphic Float (with int/float subtype distinction
   that the compiler manages). We accept Float and Int (which Faust auto-
   coerces) and reject everything else.
   ============================================================================ *)

let rec scalar_ok (te : type_expr) : unit =
  match te with
  | TyCon id when id.name = "Float" || id.name = "Int" -> ()
  | TyOwn t | TyRef t | TyMut t -> scalar_ok t
  | _ -> unsupported "Faust kernels accept only Int/Float scalars"

(* ============================================================================
   Expressions

   Faust expressions are evaluated as signal flow but written infix.
   We emit them as parenthesised trees identical in shape to the AST.
   ============================================================================ *)

let faust_builtins = [
  "sin"; "cos"; "tan"; "asin"; "acos"; "atan"; "atan2";
  "exp"; "log"; "log10"; "sqrt"; "pow"; "floor"; "ceil"; "round";
  "abs"; "min"; "max"; "fmod"; "tanh"; "sinh"; "cosh";
  "int"; "float";  (* type coercions in Faust *)
]

(* User-defined function names visible at codegen time. Populated once per
   [generate] call before any [gen_expr] is invoked, so inter-fn calls are
   recognised. Module-scoped because gen_expr is otherwise a pure tree
   transformation; threading a context through every node would inflate the
   diff without buying anything in single-shot codegen. *)
let user_fns : string list ref = ref []

let rec gen_expr (e : expr) : string =
  match e with
  | ExprLit lit -> gen_lit lit
  | ExprVar id  -> mangle id.name
  | ExprBinary (a, op, b) ->
      let s = match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%"
        | OpEq  -> "==" | OpNe -> "!="
        | OpLt  -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
        | OpAnd -> "&" | OpOr -> "|"  (* Faust uses bitwise tokens for both *)
        | OpBitAnd -> "&" | OpBitOr -> "|" | OpBitXor -> "xor"
        | OpShl -> "<<" | OpShr -> ">>"
        | OpConcat -> unsupported "string/array concat not supported in Faust"
      in
      "(" ^ gen_expr a ^ " " ^ s ^ " " ^ gen_expr b ^ ")"
  | ExprUnary (op, x) ->
      (match op with
       | OpNeg    -> "(0 - " ^ gen_expr x ^ ")"  (* Faust has no unary minus *)
       | OpNot    -> "(1 - " ^ gen_expr x ^ ")"  (* boolean as 0/1 *)
       | OpBitNot -> unsupported "bitwise not not supported in Faust"
       | OpRef | OpDeref -> unsupported "ref/deref not supported in Faust")
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let c = gen_expr ei_cond in
      let t = gen_expr ei_then in
      let f = match ei_else with
        | Some e -> gen_expr e
        | None   -> unsupported "if without else cannot lower to Faust select2"
      in
      Printf.sprintf "select2(%s, %s, %s)" c f t
  | ExprApp (callee, args) ->
      let name = match callee with
        | ExprVar id -> id.name
        | _ -> unsupported "indirect calls not supported in Faust"
      in
      let emit_name =
        if List.mem name faust_builtins then name
        else if List.mem name !user_fns then mangle name
        else unsupported ("call to non-builtin in Faust kernel: " ^ name)
      in
      Printf.sprintf "%s(%s)" emit_name
        (String.concat ", " (List.map gen_expr args))
  | ExprLet { el_pat; el_value; el_body; el_mut = _; el_quantity = _; el_ty = _ } ->
      (* Faust's [with { var = expr; }] clause attaches local definitions to
         a parent expression. We emit it inline so:
           let v = e1 in e2   ↦   (e2) with { v = e1; } *)
      let var = match el_pat with
        | PatVar id -> mangle id.name
        | _ -> unsupported "non-variable let binding not supported in Faust"
      in
      let body = match el_body with
        | Some e -> gen_expr e
        | None   -> unsupported "statement-position let cannot stand alone in Faust"
      in
      Printf.sprintf "(%s) with { %s = %s; }" body var (gen_expr el_value)
  | ExprBlock blk -> gen_block blk
  | ExprSpan (inner, _) -> gen_expr inner
  | ExprReturn (Some e) -> gen_expr e
  | ExprMatch _   -> unsupported "match not supported in Faust kernel"
  | ExprLambda _  -> unsupported "lambdas not supported in Faust"
  | ExprTuple _ | ExprArray _ | ExprRecord _
  | ExprField _ | ExprTupleIndex _ | ExprIndex _ | ExprRowRestrict _ ->
      unsupported "compound values not supported in Faust kernel"
  | ExprReturn None | ExprTry _ | ExprHandle _
  | ExprResume _ | ExprUnsafe _ | ExprVariant _ ->
      unsupported "control-flow construct not supported in Faust kernel"

and gen_lit (lit : literal) : string =
  match lit with
  | LitInt (n, _)      -> string_of_int n
  | LitFloat (f, _)    ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true,  _) -> "1"
  | LitBool (false, _) -> "0"
  | LitChar _          -> unsupported "char literals not supported in Faust"
  | LitString _        -> unsupported "string literals not supported in Faust"
  | LitUnit _          -> unsupported "unit literal not supported in Faust"

and gen_block (blk : block) : string =
  (* A block becomes a chain of [with { ... }] locals followed by the trailing
     expression. Empty blocks aren't meaningful — Faust requires every
     definition to produce a value. *)
  let result = match blk.blk_expr with
    | Some e -> e
    | None   -> unsupported "block without trailing expression cannot be lowered"
  in
  (* Statements turn into local definitions, applied in reverse so
     earlier bindings are visible to later ones via Faust's scope rules. *)
  let withs = List.filter_map (fun s ->
    match s with
    | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
        Some (Printf.sprintf "%s = %s;" (mangle id.name) (gen_expr sl_value))
    | StmtLet _ -> unsupported "destructuring let not supported in Faust"
    | StmtExpr _ -> unsupported "statement-form expression not supported in Faust"
    | StmtAssign _ -> unsupported "assignment not supported in Faust (signals are immutable)"
    | StmtWhile _ | StmtFor _ ->
        unsupported "imperative loops not supported in Faust"
  ) blk.blk_stmts in
  if withs = [] then gen_expr result
  else
    Printf.sprintf "(%s) with { %s }" (gen_expr result) (String.concat " " withs)

(* ============================================================================
   Top-level
   ============================================================================ *)

let validate_kernel_fn (fd : fn_decl) : unit =
  (match fd.fd_ret_ty with
   | None -> unsupported "kernel function must return Float"
   | Some t -> scalar_ok t);
  List.iter (fun (p : param) -> scalar_ok p.p_ty) fd.fd_params

(* Emit a Faust function definition. [as_entry] forces the emitted name to
   be [process] (the Faust entry point) regardless of the source name. *)
let gen_function ?(as_entry = false) (fd : fn_decl) : string =
  validate_kernel_fn fd;
  let name = if as_entry then "process" else mangle fd.fd_name.name in
  let params = List.map (fun (p : param) -> mangle p.p_name.name) fd.fd_params in
  let body = match fd.fd_body with
    | FnExpr e   -> gen_expr e
    | FnBlock b  -> gen_block b
  in
  if params = [] then
    Printf.sprintf "%s = %s;\n" name body
  else
    Printf.sprintf "%s(%s) = %s;\n" name (String.concat ", " params) body

(* ============================================================================
   Driver
   ============================================================================ *)

let pick_entry (program : program) : fn_decl =
  let fns = List.filter_map (function TopFn fd -> Some fd | _ -> None)
              program.prog_decls in
  let by_name n = List.find_opt (fun fd -> fd.fd_name.name = n) fns in
  match by_name "process" with
  | Some fd -> fd
  | None ->
      match by_name "main" with
      | Some fd -> fd
      | None ->
          match fns with
          | fd :: _ -> fd
          | [] -> unsupported "no function found to lower as Faust process"

let generate (program : program) (_symbols : Symbol.t) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf
    "// Generated by AffineScript compiler (Faust DSP sublanguage)\n";
  Buffer.add_string buf "// SPDX-License-Identifier: PMPL-1.0-or-later\n\n";
  let entry = pick_entry program in
  let entry_name = entry.fd_name.name in
  user_fns := List.filter_map (function
    | TopFn fd -> Some fd.fd_name.name
    | _ -> None
  ) program.prog_decls;
  let other_fns = List.filter_map (function
    | TopFn fd when fd.fd_name.name <> entry_name -> Some fd
    | _ -> None
  ) program.prog_decls in
  let _ = entry_name in
  (* Emit auxiliary functions first so Faust can resolve forward references. *)
  List.iter (fun fd -> Buffer.add_string buf (gen_function fd)) other_fns;
  if other_fns <> [] then Buffer.add_char buf '\n';
  (* Emit the entry as `process` — Faust's required entry-point name. *)
  Buffer.add_string buf (gen_function ~as_entry:true entry);
  Buffer.contents buf

let codegen_faust (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Faust_unsupported msg -> Error ("Faust backend: " ^ msg)
  | Failure msg           -> Error ("Faust codegen error: " ^ msg)
  | e                     -> Error ("Faust codegen error: " ^ Printexc.to_string e)
