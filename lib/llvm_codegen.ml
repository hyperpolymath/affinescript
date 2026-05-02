(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** LLVM IR text emitter (MVP).

    Produces LLVM IR in the textual [.ll] form that [llc] reads. We emit
    SSA via a fresh-name counter; control flow (if/else) becomes basic
    blocks with branches; let-bindings become named SSA values.

    Scope: Int (i64) and Float (double) only. No tuples, records, strings,
    or heap allocation. Each function body is a single FnExpr or FnBlock
    that returns a scalar. This is enough to take the LLVM toolchain to
    x86-64, ARM64, RISC-V, AVR, PTX, AMDGPU via [llc -march=...]. *)

open Ast

exception Llvm_unsupported of string
let unsupported m = raise (Llvm_unsupported m)

(* ============================================================================
   Per-function emit state — SSA / block counter.
   ============================================================================ *)

type fstate = {
  mutable next_ssa     : int;
  mutable next_block   : int;
  body                 : Buffer.t;
  mutable env          : (string * (string * string)) list;
  mutable current_blk  : string;  (* label of the block currently being filled *)
}

let new_fstate () = {
  next_ssa = 0; next_block = 0; body = Buffer.create 256; env = [];
  current_blk = "entry";
}

let fresh_ssa st =
  let n = st.next_ssa in st.next_ssa <- n + 1;
  Printf.sprintf "%%v%d" n

let fresh_block st label =
  let n = st.next_block in st.next_block <- n + 1;
  Printf.sprintf "%s%d" label n

let emit_line st s =
  Buffer.add_string st.body s;
  Buffer.add_char st.body '\n'

let bind st name ssa ty =
  st.env <- (name, (ssa, ty)) :: st.env

let lookup st name =
  try List.assoc name st.env
  with Not_found -> unsupported ("unbound: " ^ name)

let llvm_type = function
  | TyCon id when id.name = "Int"   -> "i64"
  | TyCon id when id.name = "Float" -> "double"
  | TyCon id when id.name = "Bool"  -> "i1"
  | TyCon id when id.name = "Unit"  -> "void"
  | TyOwn t | TyRef t | TyMut t ->
      (* Recurse rather than calling llvm_type recursively here — pattern
         matches don't reduce; we need a separate function. *)
      (match t with
       | TyCon id when id.name = "Int"   -> "i64"
       | TyCon id when id.name = "Float" -> "double"
       | TyCon id when id.name = "Bool"  -> "i1"
       | _ -> unsupported "complex type not supported in LLVM backend")
  | _ -> unsupported "type not supported in LLVM backend"

let ret_type = function None -> "void" | Some t -> llvm_type t

(* ============================================================================
   Expression compilation: returns (ssa_name, llvm_type).
   ============================================================================ *)

let rec gen_expr (st : fstate) (e : expr) : string * string =
  match e with
  | ExprLit (LitInt (n, _))     -> (string_of_int n, "i64")
  | ExprLit (LitBool (true, _)) -> ("1", "i1")
  | ExprLit (LitBool (false, _))-> ("0", "i1")
  | ExprLit (LitFloat (f, _))   ->
      let s = string_of_float f in
      let s = if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s in
      (s, "double")
  | ExprLit _ -> unsupported "non-numeric literal"
  | ExprVar id ->
      let (ssa, ty) = lookup st id.name in
      (ssa, ty)
  | ExprBinary (a, op, b) ->
      let (av, ty) = gen_expr st a in
      let (bv, _)  = gen_expr st b in
      let dst = fresh_ssa st in
      let opcode = match op, ty with
        | OpAdd, "i64"    -> "add" | OpAdd, "double" -> "fadd"
        | OpSub, "i64"    -> "sub" | OpSub, "double" -> "fsub"
        | OpMul, "i64"    -> "mul" | OpMul, "double" -> "fmul"
        | OpDiv, "i64"    -> "sdiv" | OpDiv, "double" -> "fdiv"
        | OpMod, "i64"    -> "srem" | OpMod, "double" -> "frem"
        | OpBitAnd, _     -> "and"
        | OpBitOr,  _     -> "or"
        | OpBitXor, _     -> "xor"
        | OpShl,    _     -> "shl"
        | OpShr,    _     -> "ashr"
        | _ -> unsupported "comparison / logical op needs different lowering"
      in
      let result_ty = ty in
      emit_line st (Printf.sprintf "  %s = %s %s %s, %s" dst opcode result_ty av bv);
      (dst, result_ty)
  | ExprUnary (OpNeg, x) ->
      let (xv, ty) = gen_expr st x in
      let dst = fresh_ssa st in
      (match ty with
       | "i64" -> emit_line st (Printf.sprintf "  %s = sub i64 0, %s" dst xv)
       | "double" -> emit_line st (Printf.sprintf "  %s = fneg double %s" dst xv)
       | _ -> unsupported "OpNeg on non-numeric");
      (dst, ty)
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let (cv, _) = gen_expr_bool st ei_cond in
      let then_lbl = fresh_block st "then" in
      let else_lbl = fresh_block st "else" in
      let cont_lbl = fresh_block st "cont" in
      emit_line st
        (Printf.sprintf "  br i1 %s, label %%%s, label %%%s" cv then_lbl else_lbl);
      emit_line st (then_lbl ^ ":");
      st.current_blk <- then_lbl;
      let (tv, ty) = gen_expr st ei_then in
      let then_end = st.current_blk in  (* may differ from then_lbl if body emitted blocks *)
      emit_line st (Printf.sprintf "  br label %%%s" cont_lbl);
      emit_line st (else_lbl ^ ":");
      st.current_blk <- else_lbl;
      let (ev, _) = match ei_else with
        | Some e -> gen_expr st e
        | None -> unsupported "if without else has no value in LLVM backend"
      in
      let else_end = st.current_blk in
      emit_line st (Printf.sprintf "  br label %%%s" cont_lbl);
      emit_line st (cont_lbl ^ ":");
      st.current_blk <- cont_lbl;
      let dst = fresh_ssa st in
      emit_line st
        (Printf.sprintf "  %s = phi %s [ %s, %%%s ], [ %s, %%%s ]"
           dst ty tv then_end ev else_end);
      (dst, ty)
  | ExprLet { el_pat = PatVar id; el_value; el_body = Some body; _ } ->
      let (v, ty) = gen_expr st el_value in
      bind st id.name v ty;
      gen_expr st body
  | ExprBlock blk ->
      List.iter (fun s ->
        match s with
        | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
            let (v, ty) = gen_expr st sl_value in
            bind st id.name v ty
        | StmtExpr e -> ignore (gen_expr st e)
        | _ -> unsupported "stmt form not supported in LLVM block"
      ) blk.blk_stmts;
      (match blk.blk_expr with
       | Some e -> gen_expr st e
       | None -> ("0", "i64"))
  | ExprApp (callee, args) ->
      let name = match callee with
        | ExprVar id -> id.name
        | _ -> unsupported "indirect call"
      in
      let arg_pairs = List.map (gen_expr st) args in
      let dst = fresh_ssa st in
      let call_args = String.concat ", "
        (List.map (fun (v, ty) -> Printf.sprintf "%s %s" ty v) arg_pairs) in
      let ret_ty = "i64" in  (* assumption; refined when we support typed lookups *)
      emit_line st
        (Printf.sprintf "  %s = call %s @%s(%s)" dst ret_ty name call_args);
      (dst, ret_ty)
  | ExprSpan (inner, _) -> gen_expr st inner
  | ExprReturn (Some e) ->
      let (v, ty) = gen_expr st e in
      emit_line st (Printf.sprintf "  ret %s %s" ty v);
      (v, ty)
  | _ -> unsupported "expression form not supported in LLVM backend MVP"

and gen_expr_bool (st : fstate) (e : expr) : string * string =
  (* Comparison ops produce i1; numeric ops would not. Emit comparisons with
     icmp/fcmp; treat boolean literals normally. *)
  match e with
  | ExprBinary (a, (OpEq|OpNe|OpLt|OpLe|OpGt|OpGe as op), b) ->
      let (av, ty) = gen_expr st a in
      let (bv, _)  = gen_expr st b in
      let pred = match op, ty with
        | OpEq, "i64" -> "eq" | OpNe, "i64" -> "ne"
        | OpLt, "i64" -> "slt" | OpLe, "i64" -> "sle"
        | OpGt, "i64" -> "sgt" | OpGe, "i64" -> "sge"
        | OpEq, "double" -> "oeq" | OpNe, "double" -> "one"
        | OpLt, "double" -> "olt" | OpLe, "double" -> "ole"
        | OpGt, "double" -> "ogt" | OpGe, "double" -> "oge"
        | _ -> unsupported "comparison on non-numeric"
      in
      let dst = fresh_ssa st in
      let cmp = if ty = "double" then "fcmp" else "icmp" in
      emit_line st (Printf.sprintf "  %s = %s %s %s %s, %s" dst cmp pred ty av bv);
      (dst, "i1")
  | ExprLit (LitBool (b, _)) ->
      ((if b then "1" else "0"), "i1")
  | _ ->
      let (v, ty) = gen_expr st e in
      if ty = "i1" then (v, "i1")
      else
        let dst = fresh_ssa st in
        emit_line st (Printf.sprintf "  %s = icmp ne %s %s, 0" dst ty v);
        (dst, "i1")

(* ============================================================================
   Function emission
   ============================================================================ *)

let gen_function (buf : Buffer.t) (fd : fn_decl) : unit =
  let st = new_fstate () in
  (* Bind parameters by their declared name. LLVM accepts named params if
     we declare them so in the signature; the body then references [%name]
     directly. *)
  List.iter (fun (p : param) ->
    let ty = llvm_type p.p_ty in
    bind st p.p_name.name ("%" ^ p.p_name.name) ty
  ) fd.fd_params;
  emit_line st "entry:";
  let body_expr = match fd.fd_body with
    | FnExpr e -> e
    | FnBlock b -> ExprBlock b
  in
  let ret_ty = ret_type fd.fd_ret_ty in
  let (rv, _) = gen_expr st body_expr in
  if ret_ty = "void" then emit_line st "  ret void"
  else emit_line st (Printf.sprintf "  ret %s %s" ret_ty rv);
  let params_str = String.concat ", "
    (List.map (fun (p : param) ->
        Printf.sprintf "%s %%%s" (llvm_type p.p_ty) p.p_name.name)
       fd.fd_params) in
  Buffer.add_string buf
    (Printf.sprintf "define %s @%s(%s) {\n" ret_ty fd.fd_name.name params_str);
  Buffer.add_buffer buf st.body;
  Buffer.add_string buf "}\n\n"

let generate (program : program) (_symbols : Symbol.t) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "; Generated by AffineScript compiler\n";
  Buffer.add_string buf "; SPDX-License-Identifier: PMPL-1.0-or-later\n";
  Buffer.add_string buf "target triple = \"x86_64-unknown-linux-gnu\"\n\n";
  List.iter (function
    | TopFn fd -> gen_function buf fd
    | _ -> ()
  ) program.prog_decls;
  Buffer.contents buf

let codegen_llvm (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Llvm_unsupported m -> Error ("LLVM backend: " ^ m)
  | Failure m          -> Error ("LLVM codegen error: " ^ m)
  | e                  -> Error ("LLVM codegen error: " ^ Printexc.to_string e)
