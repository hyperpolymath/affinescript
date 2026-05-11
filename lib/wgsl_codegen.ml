(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** WGSL Kernel Sublanguage Emitter (MVP).

    Lowers a strict subset of AffineScript to a WebGPU compute shader.

    Source shape (the kernel sublanguage):
    - exactly one [fn] declaration is the kernel, named [kernel] or [main]
      (or, if neither, the first [fn] in the file)
    - the first parameter is [Int] and represents the global invocation index
    - remaining parameters are [Array[Int]] or [Array[Float]] and become
      WGSL storage buffers; ownership selects access:
        - [ref T]  -> [var<storage, read>]
        - [mut T]  -> [var<storage, read_write>]
        - bare T   -> [var<storage, read>]
    - return type is [Unit] (kernels produce side effects in buffers)
    - body uses arithmetic, comparison, [if]/[let]/blocks, [arr[i]] index
      reads, and [out[i] = expr] assignments

    Anything outside this subset emits an explicit error rather than
    silently miscompiling. The output is a single WGSL file consumable by
    any WebGPU host (browser, [wgpu], Dawn, naga-cli).
*)

open Ast
open Kernel_sublang

(* Per-target type strings (i32 / f32 / bool) stay below; everything else —
   the [Unsupported] exception, [pick_entry], [strip_ownership],
   [array_element], etc. — comes from [Kernel_sublang]. *)

(* ============================================================================
   Context
   ============================================================================ *)

type ctx = {
  output      : Buffer.t;
  indent      : int;
  index_param : string;       (* mangled name of the i: Int parameter *)
  buffer_tys  : (string * string) list; (* (param name, element type "i32"|"f32") *)
}

let new_ctx () = {
  output = Buffer.create 1024;
  indent = 0;
  index_param = "_gid";
  buffer_tys = [];
}

let emit ctx s = Buffer.add_string ctx.output s
let emit_line ctx s =
  Buffer.add_string ctx.output (String.make (ctx.indent * 2) ' ');
  Buffer.add_string ctx.output s;
  Buffer.add_char ctx.output '\n'
let inc ctx = { ctx with indent = ctx.indent + 1 }
let dec ctx = { ctx with indent = max 0 (ctx.indent - 1) }

(* ============================================================================
   Identifier sanitisation
   ============================================================================ *)

let wgsl_reserved = [
  "array"; "atomic"; "bool"; "break"; "case"; "const"; "continue"; "default";
  "discard"; "else"; "enable"; "false"; "fn"; "for"; "if"; "let"; "loop";
  "private"; "ptr"; "return"; "storage"; "struct"; "switch"; "true"; "type";
  "uniform"; "var"; "vec2"; "vec3"; "vec4"; "while"; "workgroup";
  "i32"; "u32"; "f32"; "f16";
  "main"; "kernel"; (* avoid colliding with our entry point *)
]

let mangle s =
  if List.mem s wgsl_reserved then s ^ "_" else s

(* ============================================================================
   Type lowering
   ============================================================================ *)

let scalar_of_type_name = function
  | "Int"   -> "i32"
  | "Float" -> "f32"
  | "Bool"  -> "bool"
  | other   -> unsupported ("type not allowed in WGSL kernel: " ^ other)

let scalar_of (te : type_expr) : string =
  match strip_ownership te with
  | TyCon id -> scalar_of_type_name id.name
  | _ -> unsupported "complex type not allowed in WGSL kernel"

(* WGSL-specific: map Array[Int] -> "i32", Array[Float] -> "f32". *)
let array_element_str (te : type_expr) : string =
  scalar_of_type_name (require_array_element "Array[Int] or Array[Float]" te)

let access_for_ownership (own : ownership option) : string =
  match own with
  | Some Mut -> "read_write"
  | _        -> "read"

(* ============================================================================
   Expressions
   ============================================================================ *)

let rec gen_expr ctx (e : expr) : string =
  match e with
  | ExprLit lit -> gen_lit lit
  | ExprVar id  -> mangle id.name
  | ExprBinary (a, op, b) ->
      let s = match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%"
        | OpEq  -> "==" | OpNe -> "!="
        | OpLt  -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
        | OpAnd -> "&&" | OpOr -> "||"
        | OpBitAnd -> "&" | OpBitOr -> "|" | OpBitXor -> "^"
        | OpShl -> "<<" | OpShr -> ">>"
        | OpConcat -> unsupported "string/array concat not supported in WGSL"
      in
      "(" ^ gen_expr ctx a ^ " " ^ s ^ " " ^ gen_expr ctx b ^ ")"
  | ExprUnary (op, x) ->
      (match op with
       | OpNeg    -> "(-(" ^ gen_expr ctx x ^ "))"
       | OpNot    -> "(!" ^ gen_expr ctx x ^ ")"
       | OpBitNot -> "(~" ^ gen_expr ctx x ^ ")"
       | OpRef | OpDeref -> unsupported "ref/deref not supported in WGSL kernel")
  | ExprIf { ei_cond; ei_then; ei_else } ->
      (* WGSL has no expression-form if; fold to select() for scalars only.
         Block expressions (`if` whose branches are blocks) must appear in
         statement position — see gen_stmt. *)
      let c = gen_expr ctx ei_cond in
      let t = gen_expr ctx ei_then in
      let f = match ei_else with
        | Some e -> gen_expr ctx e
        | None   -> unsupported "if without else cannot be an expression in WGSL"
      in
      Printf.sprintf "select(%s, %s, %s)" f t c
  | ExprIndex (arr, idx) ->
      Printf.sprintf "%s[u32(%s)]" (gen_expr ctx arr) (gen_expr ctx idx)
  | ExprApp (callee, args) ->
      (* Permit calls to a small set of WGSL built-ins by name. Anything else
         fails — kernels can't call user-defined helper fns in MVP. *)
      let name = match callee with
        | ExprVar id -> id.name
        | _ -> unsupported "indirect calls not supported in WGSL kernel"
      in
      let known = ["abs"; "min"; "max"; "clamp"; "sqrt"; "floor"; "ceil";
                   "round"; "sin"; "cos"; "tan"; "exp"; "log"; "pow";
                   "mix"; "step"; "smoothstep"; "f32"; "i32"; "u32"] in
      if not (List.mem name known) then
        unsupported ("call to non-builtin function in WGSL kernel: " ^ name);
      let args_s = List.map (gen_expr ctx) args in
      Printf.sprintf "%s(%s)" name (String.concat ", " args_s)
  | ExprSpan (inner, _) -> gen_expr ctx inner
  | ExprBlock _   -> unsupported "block expression must be in statement position"
  | ExprLet _     -> unsupported "let must be a statement, not an expression"
  | ExprMatch _   -> unsupported "match not supported in WGSL kernel"
  | ExprLambda _  -> unsupported "lambdas not supported in WGSL"
  | ExprTuple _ | ExprArray _ | ExprRecord _
  | ExprField _ | ExprTupleIndex _ | ExprRowRestrict _ ->
      unsupported "compound values not supported in WGSL kernel (yet)"
  | ExprReturn _ | ExprTry _ | ExprHandle _
  | ExprResume _ | ExprUnsafe _ | ExprVariant _ ->
      unsupported "control-flow construct not supported in WGSL kernel"

and gen_lit (lit : literal) : string =
  match lit with
  | LitInt (n, _)      -> Printf.sprintf "%d" n
  | LitFloat (f, _)    ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true,  _) -> "true"
  | LitBool (false, _) -> "false"
  | LitChar _          -> unsupported "char literals not supported in WGSL"
  | LitString _        -> unsupported "string literals not supported in WGSL"
  | LitUnit _          -> unsupported "unit literal in expression position"

(* Statement-form lowering. Block expressions and let-statements live here. *)
let rec gen_stmt ctx (s : stmt) : unit =
  match s with
  | StmtLet { sl_pat; sl_value; sl_mut; sl_quantity = _; sl_ty } ->
      let var = match sl_pat with
        | PatVar id   -> mangle id.name
        | PatWildcard _ -> "_"
        | _ -> unsupported "destructuring let not supported in WGSL"
      in
      let kw = if sl_mut then "var" else "let" in
      let ty_anno = match sl_ty with
        | Some t -> ": " ^ scalar_of t
        | None   -> ""
      in
      emit_line ctx (Printf.sprintf "%s %s%s = %s;" kw var ty_anno (gen_expr ctx sl_value))
  | StmtExpr e ->
      gen_stmt_expr ctx e
  | StmtAssign (lhs, op, rhs) ->
      let op_str = match op with
        | AssignEq  -> "="  | AssignAdd -> "+="
        | AssignSub -> "-=" | AssignMul -> "*="
        | AssignDiv -> "/="
      in
      emit_line ctx
        (Printf.sprintf "%s %s %s;" (gen_expr ctx lhs) op_str (gen_expr ctx rhs))
  | StmtWhile (cond, body) ->
      emit_line ctx (Printf.sprintf "while (%s) {" (gen_expr ctx cond));
      gen_block (inc ctx) body;
      emit_line ctx "}"
  | StmtFor _ ->
      unsupported "for-in loop not supported in WGSL kernel (use while)"

and gen_stmt_expr ctx e =
  (* Statement-position expression: emit if/blocks as control flow, scalar
     expressions as `_ = expr;` (rare; usually a builtin call). *)
  match e with
  | ExprIf { ei_cond; ei_then; ei_else } ->
      emit_line ctx (Printf.sprintf "if (%s) {" (gen_expr ctx ei_cond));
      gen_branch (inc ctx) ei_then;
      (match ei_else with
       | Some else_br ->
           emit_line ctx "} else {";
           gen_branch (inc ctx) else_br;
           emit_line ctx "}"
       | None ->
           emit_line ctx "}")
  | ExprBlock blk -> gen_block ctx blk
  | _ ->
      emit_line ctx (Printf.sprintf "_ = %s;" (gen_expr ctx e))

and gen_branch ctx (e : expr) =
  match e with
  | ExprBlock blk -> gen_block ctx blk
  | _             -> emit_line ctx (gen_expr ctx e ^ ";")

and gen_block ctx (blk : block) =
  List.iter (gen_stmt ctx) blk.blk_stmts;
  (match blk.blk_expr with
   | Some e -> gen_stmt_expr ctx e
   | None   -> ())

(* ============================================================================
   Top-level: pick the kernel function and emit it
   ============================================================================ *)

(* Picking + validation now share Kernel_sublang's helpers; this is the
   canonical compute-kernel shape (first param Int, rest Array buffers,
   returns Unit). *)
let pick_kernel = pick_entry
let validate_kernel = validate_compute_kernel_shape

let emit_buffer_bindings ctx (params : param list) : ctx =
  (* Skip the first param (the index); the rest become storage buffers. *)
  let rec go i ctx = function
    | [] -> ctx
    | (p : param) :: rest ->
        let elem = array_element_str p.p_ty in
        let access = access_for_ownership p.p_ownership in
        let name = mangle p.p_name.name in
        emit_line ctx
          (Printf.sprintf "@group(0) @binding(%d) var<storage, %s> %s : array<%s>;"
             i access name elem);
        go (i + 1) { ctx with buffer_tys = (name, elem) :: ctx.buffer_tys } rest
  in
  match params with
  | [] | [_] -> ctx
  | _ :: bufs -> go 0 ctx bufs

let generate (program : program) (_symbols : Symbol.t) : string =
  let ctx = new_ctx () in
  emit_line ctx "// Generated by AffineScript compiler (WGSL kernel sublanguage)";
  emit_line ctx "// SPDX-License-Identifier: PMPL-1.0-or-later";
  emit ctx "\n";
  let fd = pick_kernel program in
  validate_kernel fd;
  let ctx = emit_buffer_bindings ctx fd.fd_params in
  let idx_name = match fd.fd_params with
    | first :: _ -> mangle first.p_name.name
    | _          -> "i"
  in
  let ctx = { ctx with index_param = idx_name } in

  emit ctx "\n";
  emit_line ctx "@compute @workgroup_size(64)";
  emit_line ctx
    (Printf.sprintf "fn %s(@builtin(global_invocation_id) gid : vec3<u32>) {"
       (mangle fd.fd_name.name));
  let body_ctx = inc ctx in
  emit_line body_ctx (Printf.sprintf "let %s : i32 = i32(gid.x);" idx_name);
  (match fd.fd_body with
   | FnExpr e ->
       gen_stmt_expr body_ctx e
   | FnBlock blk ->
       gen_block body_ctx blk);
  emit_line ctx "}";
  Buffer.contents ctx.output

let codegen_wgsl (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Unsupported msg -> Error ("WGSL backend: " ^ msg)
  | Failure msg          -> Error ("WGSL codegen error: " ^ msg)
  | e                    -> Error ("WGSL codegen error: " ^ Printexc.to_string e)
