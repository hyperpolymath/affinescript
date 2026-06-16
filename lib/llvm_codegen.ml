(* SPDX-License-Identifier: MPL-2.0 *)
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

(* Record-type table — maps a type name to the ordered list of (field_name,
   field_llvm_type). Populated by [gen_type_decl] during the pre-pass and
   consulted by [ExprField] to translate a field name into the integer
   index that LLVM's [extractvalue] / [insertvalue] need. *)
let record_table : (string, (string * string) list) Hashtbl.t = Hashtbl.create 8

let record_field_index (type_name : string) (field : string) : int =
  match Hashtbl.find_opt record_table type_name with
  | None -> unsupported ("unknown record type in LLVM backend: " ^ type_name)
  | Some fields ->
      let rec idx i = function
        | [] -> unsupported (Printf.sprintf "field %s not in record %s" field type_name)
        | (n, _) :: _ when n = field -> i
        | _ :: rest -> idx (i + 1) rest
      in
      idx 0 fields

(* Variant lookup table — given a variant constructor name (e.g. "Circle"),
   recover the parent enum's name, the tag's integer index, and the
   variant's arity. Populated by [gen_type_decl] for [TyEnum]. The MVP
   layout is one i64 payload slot, regardless of variant arity. *)
type variant_info = { v_enum : string; v_tag : int; v_arity : int }
let variant_table : (string, variant_info) Hashtbl.t = Hashtbl.create 8

let variant_info_opt (name : string) : variant_info option =
  Hashtbl.find_opt variant_table name

type fstate = {
  mutable next_ssa     : int;
  mutable next_block   : int;
  body                 : Buffer.t;
  mutable env          : (string * (string * string)) list;
  mutable current_blk  : string;
}

let new_fstate () = {
  next_ssa = 0; next_block = 0; body = Buffer.create 256; env = [];
  current_blk = "entry";
}

(* Module-level string-literal table. Each unique literal gets a private
   global with a stable id; expressions reference it by GEP-extracting the
   pointer to the first byte. Populated as gen_lit walks the AST; emitted
   into the module preamble at the end. *)
let string_table : (string, int) Hashtbl.t = Hashtbl.create 16
let next_string_id = ref 0

let intern_string (s : string) : int =
  match Hashtbl.find_opt string_table s with
  | Some i -> i
  | None ->
      let i = !next_string_id in
      incr next_string_id;
      Hashtbl.add string_table s i;
      i

(* LLVM IR escape: \NN for special bytes, \22 for double-quote, \5C for
   backslash. Newline becomes \0A. The trailing nul (\00) is added by
   the caller. *)
let llvm_escape (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    let code = Char.code c in
    if code >= 32 && code < 127 && c <> '"' && c <> '\\'
    then Buffer.add_char buf c
    else Buffer.add_string buf (Printf.sprintf "\\%02X" code)
  ) s;
  Buffer.contents buf

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

let rec llvm_type (te : type_expr) : string =
  match te with
  | TyCon id when id.name = "Int"    -> "i64"
  | TyCon id when id.name = "Float"  -> "double"
  | TyCon id when id.name = "Bool"   -> "i1"
  | TyCon id when id.name = "Unit"   -> "void"
  | TyCon id when id.name = "String" -> "ptr"  (* C-style nul-terminated *)
  | TyCon id                         -> "%" ^ id.name
  | TyTuple [] -> "void"   (* the empty tuple () is Unit, same as TyCon "Unit" *)
  | TyTuple ts -> "{ " ^ String.concat ", " (List.map llvm_type ts) ^ " }"
  | TyOwn t | TyRef (_, t) | TyMut (_, t) -> llvm_type t
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
  | ExprLit (LitString (s, _)) ->
      let id = intern_string s in
      let dst = fresh_ssa st in
      let len = String.length s + 1 in  (* +1 for trailing nul *)
      emit_line st
        (Printf.sprintf "  %s = getelementptr [%d x i8], ptr @.str.%d, i64 0, i64 0"
           dst len id);
      (dst, "ptr")
  | ExprLit _ -> unsupported "non-numeric literal"
  | ExprVar id ->
      let (ssa, ty) = lookup st id.name in
      (ssa, ty)
  | ExprBinary (a, op, b) ->
      let (av, ty) = gen_expr st a in
      let (bv, _)  = gen_expr st b in
      let dst = fresh_ssa st in
      (* OpConcat is special: route through the @as_concat runtime helper.
         Avoids the opcode table entirely. *)
      (match op with
       | OpConcat ->
           emit_line st
             (Printf.sprintf "  %s = call ptr @as_concat(ptr %s, ptr %s)"
                dst av bv);
           (dst, "ptr")
       | _ ->
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
           (dst, result_ty))
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
        | StmtLet { sl_pat = PatVar id; sl_value; sl_ty; _ } ->
            let (v, ty) = gen_expr_with_hint st sl_ty sl_value in
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
      (match variant_info_opt name with
       | Some info ->
           (* Variant constructor: tag at index 0, payload at indices
              1..arity. All payload slots are i64; cast as needed. *)
           let enum_ty = "%" ^ info.v_enum in
           let acc = ref (fresh_ssa st) in
           emit_line st
             (Printf.sprintf "  %s = insertvalue %s undef, i32 %d, 0"
                !acc enum_ty info.v_tag);
           List.iteri (fun i arg ->
             let (a, ty) = gen_expr st arg in
             let payload_v =
               if ty = "i64" then a
               else
                 let c = fresh_ssa st in
                 (match ty with
                  | "double" ->
                      emit_line st
                        (Printf.sprintf "  %s = bitcast double %s to i64" c a)
                  | "i1" ->
                      emit_line st
                        (Printf.sprintf "  %s = zext i1 %s to i64" c a)
                  | _ ->
                      emit_line st
                        (Printf.sprintf "  %s = sext i32 %s to i64" c a));
                 c
             in
             let next = fresh_ssa st in
             emit_line st
               (Printf.sprintf "  %s = insertvalue %s %s, i64 %s, %d"
                  next enum_ty !acc payload_v (i + 1));
             acc := next
           ) args;
           (!acc, enum_ty)
       | None ->
           let arg_pairs = List.map (gen_expr st) args in
           let dst = fresh_ssa st in
           let call_args = String.concat ", "
             (List.map (fun (v, ty) -> Printf.sprintf "%s %s" ty v) arg_pairs) in
           (* Runtime intrinsics get a typed void / ptr return; user fns
              fall back to i64 until we track signatures. *)
           let ret_ty = match name with
             | "println" | "print" -> "void"
             | "read_line" | "as_concat" -> "ptr"
             | _ -> "i64"
           in
           if ret_ty = "void" then begin
             emit_line st
               (Printf.sprintf "  call void @%s(%s)" name call_args);
             ("0", "i64")  (* dummy value; void calls aren't used as values *)
           end else begin
             emit_line st
               (Printf.sprintf "  %s = call %s @%s(%s)" dst ret_ty name call_args);
             (dst, ret_ty)
           end)
  | ExprTuple es ->
      (* Build an aggregate via a chain of [insertvalue] starting from
         [undef]. Final result has type {ty1, ty2, ...}. *)
      let pairs = List.map (gen_expr st) es in
      let tup_ty = "{ " ^ String.concat ", " (List.map snd pairs) ^ " }" in
      let acc = ref ("undef", tup_ty) in
      List.iteri (fun i (v, ty) ->
        let dst = fresh_ssa st in
        emit_line st (Printf.sprintf "  %s = insertvalue %s %s, %s %s, %d"
                        dst tup_ty (fst !acc) ty v i);
        acc := (dst, tup_ty)
      ) pairs;
      !acc
  | ExprTupleIndex (e, n) ->
      let (v, ty) = gen_expr st e in
      let dst = fresh_ssa st in
      (* Element types live inside [ty]; we extract by index. The result's
         LLVM type isn't easily recoverable from the source AST without a
         type pass — for MVP we use [i64] and rely on AS having only
         primitive tuples. *)
      emit_line st (Printf.sprintf "  %s = extractvalue %s %s, %d" dst ty v n);
      (dst, "i64")
  | ExprRecord _ ->
      (* Bare ExprRecord without a type hint can't select the right
         typedef. Callers that know the destination type should use
         [gen_expr_with_hint]. Emit an explicit error instead of silently
         making up a struct that won't match a typedef's field order. *)
      unsupported "record literal in LLVM backend needs a let-binding type annotation"
  | ExprField (record, field) ->
      let (v, ty) = gen_expr st record in
      let dst = fresh_ssa st in
      (* Resolve [field] to an index — only works when [ty] is a named
         record type ([%Name]) for which we have a table entry. *)
      let idx, field_ty =
        if String.length ty > 0 && ty.[0] = '%' then
          let tname = String.sub ty 1 (String.length ty - 1) in
          let i = record_field_index tname field.name in
          let fields =
            match Hashtbl.find_opt record_table tname with
            | Some fs -> fs
            | None -> []
          in
          (i, snd (List.nth fields i))
        else (0, "i64")
      in
      emit_line st (Printf.sprintf "  %s = extractvalue %s %s, %d" dst ty v idx);
      (dst, field_ty)
  | ExprMatch { em_scrutinee; em_arms } ->
      gen_match st em_scrutinee em_arms
  | ExprSpan (inner, _) -> gen_expr st inner
  | ExprReturn (Some e) ->
      let (v, ty) = gen_expr st e in
      emit_line st (Printf.sprintf "  ret %s %s" ty v);
      (v, ty)
  | _ -> unsupported "expression form not supported in LLVM backend MVP"

and gen_match (st : fstate) (scrutinee : expr) (arms : match_arm list) : string * string =
  let (scrut_v, scrut_ty) = gen_expr st scrutinee in
  (* Pull the tag once; payload slots are extracted per-arm based on the
     arm's pattern arity. *)
  let tag = fresh_ssa st in
  emit_line st (Printf.sprintf "  %s = extractvalue %s %s, 0" tag scrut_ty scrut_v);
  (* Per-arm block + a join block. *)
  let arm_blocks = List.map (fun _ -> fresh_block st "arm") arms in
  let default_lbl = fresh_block st "match_default" in
  let join_lbl = fresh_block st "match_join" in
  (* Build the switch. Wildcard / variable arms become the default; con
     arms become typed cases. *)
  let cases, default_arm =
    let rec scan acc = function
      | [] -> (List.rev acc, None)
      | arm :: rest ->
          (match arm.ma_pat with
           | PatWildcard _ | PatVar _ -> (List.rev acc, Some arm)
           | _ -> scan (arm :: acc) rest)
    in
    scan [] arms
  in
  let case_lines =
    List.map2 (fun arm lbl ->
      match arm.ma_pat with
      | PatCon (id, _) ->
          (match variant_info_opt id.name with
           | Some info -> Printf.sprintf "    i32 %d, label %%%s" info.v_tag lbl
           | None -> Printf.sprintf "    ; unknown variant %s" id.name)
      | _ -> "") cases arm_blocks
  in
  emit_line st (Printf.sprintf "  switch i32 %s, label %%%s [\n%s\n  ]"
                  tag default_lbl (String.concat "\n" case_lines));
  (* Emit each arm. Track end-block + result so we can phi at join. *)
  let arm_results = List.map2 (fun arm lbl ->
    emit_line st (lbl ^ ":");
    st.current_blk <- lbl;
    (* Bind each payload arg from its slot (extractvalue at index i+1). *)
    (match arm.ma_pat with
     | PatCon (_, args) ->
         List.iteri (fun i p ->
           match p with
           | PatVar vid ->
               let v = fresh_ssa st in
               emit_line st
                 (Printf.sprintf "  %s = extractvalue %s %s, %d"
                    v scrut_ty scrut_v (i + 1));
               bind st vid.name v "i64"
           | _ -> ()
         ) args
     | _ -> ());
    let (v, ty) = gen_expr st arm.ma_body in
    let end_blk = st.current_blk in
    emit_line st (Printf.sprintf "  br label %%%s" join_lbl);
    (v, ty, end_blk)
  ) cases arm_blocks in
  (* Default arm: a wildcard/var binding if the user gave one, or a poison
     trap otherwise so the LLVM verifier still accepts the module. *)
  emit_line st (default_lbl ^ ":");
  st.current_blk <- default_lbl;
  let default_result =
    match default_arm with
    | Some arm ->
        (match arm.ma_pat with
         | PatVar vid -> bind st vid.name scrut_v scrut_ty
         | _ -> ());
        let (v, ty) = gen_expr st arm.ma_body in
        let end_blk = st.current_blk in
        emit_line st (Printf.sprintf "  br label %%%s" join_lbl);
        Some (v, ty, end_blk)
    | None ->
        emit_line st "  unreachable";
        None
  in
  (* Join with phi. *)
  emit_line st (join_lbl ^ ":");
  st.current_blk <- join_lbl;
  let dst = fresh_ssa st in
  let result_ty = match arm_results with
    | (_, ty, _) :: _ -> ty
    | [] -> (match default_result with Some (_, ty, _) -> ty | None -> "i64")
  in
  let phi_inputs =
    List.map (fun (v, _, blk) -> Printf.sprintf "[ %s, %%%s ]" v blk) arm_results
    @ (match default_result with
       | Some (v, _, blk) -> [Printf.sprintf "[ %s, %%%s ]" v blk]
       | None -> [])
  in
  emit_line st (Printf.sprintf "  %s = phi %s %s"
                  dst result_ty (String.concat ", " phi_inputs));
  (dst, result_ty)

(* When the surrounding context (a let-binding) supplies the destination
   type, ExprRecord can be lowered with the typedef's field order. *)
and gen_expr_with_hint (st : fstate) (hint : type_expr option) (e : expr)
    : string * string =
  match e, hint with
  | ExprRecord { er_fields; _ }, Some (TyCon id) when Hashtbl.mem record_table id.name ->
      let typedef_fields = Hashtbl.find record_table id.name in
      let typedef_name = "%" ^ id.name in
      (* Materialise a value for each provided source field, then write
         them into the typedef in the typedef's declared order. *)
      let user_values = List.map (fun (n, e_opt) ->
        let (v, ty) = match e_opt with
          | Some e -> gen_expr st e
          | None -> lookup st n.name
        in
        (n.name, v, ty)
      ) er_fields in
      let acc = ref ("undef", typedef_name) in
      List.iteri (fun i (fname, _) ->
        let (_, v, ty) =
          try List.find (fun (n, _, _) -> n = fname) user_values
          with Not_found ->
            unsupported (Printf.sprintf "record literal missing field %s" fname)
        in
        let dst = fresh_ssa st in
        emit_line st (Printf.sprintf "  %s = insertvalue %s %s, %s %s, %d"
                        dst typedef_name (fst !acc) ty v i);
        acc := (dst, typedef_name)
      ) typedef_fields;
      !acc
  | _ ->
      gen_expr st e

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
  (* The entry point `main` becomes a C-runtime-compatible `i32 main()` returning
     0, so the emitted module links into a runnable native executable (crt0
     expects `int main`). All other functions keep their declared type; `()`/Unit
     lowers to `void` (see llvm_type). *)
  let is_entry = fd.fd_name.name = "main" in
  let ret_ty = if is_entry then "i32" else ret_type fd.fd_ret_ty in
  let (rv, _) = gen_expr st body_expr in
  if is_entry then emit_line st "  ret i32 0"
  else if ret_ty = "void" then emit_line st "  ret void"
  else emit_line st (Printf.sprintf "  ret %s %s" ret_ty rv);
  let params_str = String.concat ", "
    (List.map (fun (p : param) ->
        Printf.sprintf "%s %%%s" (llvm_type p.p_ty) p.p_name.name)
       fd.fd_params) in
  Buffer.add_string buf
    (Printf.sprintf "define %s @%s(%s) {\n" ret_ty fd.fd_name.name params_str);
  Buffer.add_buffer buf st.body;
  Buffer.add_string buf "}\n\n"

let gen_type_decl (buf : Buffer.t) (td : type_decl) : unit =
  let name = td.td_name.name in
  let emit_struct fields =
    let pairs = List.map (fun (n, ty) -> (n, llvm_type ty)) fields in
    Hashtbl.replace record_table name pairs;
    Buffer.add_string buf
      (Printf.sprintf "%%%s = type { %s }\n\n" name
         (String.concat ", " (List.map snd pairs)))
  in
  match td.td_body with
  | TyAlias (TyRecord (fields, _)) ->
      emit_struct (List.map (fun (rf : row_field) -> (rf.rf_name.name, rf.rf_ty)) fields)
  | TyStruct fields ->
      emit_struct (List.map (fun (sf : struct_field) -> (sf.sf_name.name, sf.sf_ty)) fields)
  | TyAlias t ->
      Buffer.add_string buf
        (Printf.sprintf "%%%s = type %s\n\n" name (llvm_type t))
  | TyEnum variants ->
      (* Tagged-union layout: { i32 tag, i64 slot_0, ..., i64 slot_{N-1} }
         where N is the maximum arity across all variants. Each variant
         fills the first [arity] slots; remaining slots are undef. All
         payloads are coerced to i64 (with bitcast for double / zext for
         i1 / sext for narrower ints). *)
      List.iteri (fun i (vd : variant_decl) ->
        let arity = List.length vd.vd_fields in
        Hashtbl.replace variant_table vd.vd_name.name
          { v_enum = name; v_tag = i; v_arity = arity }
      ) variants;
      let max_arity =
        List.fold_left (fun acc (vd : variant_decl) ->
          max acc (List.length vd.vd_fields)) 0 variants
      in
      let slots =
        if max_arity = 0 then ""
        else ", " ^ String.concat ", " (List.init max_arity (fun _ -> "i64"))
      in
      Buffer.add_string buf
        (Printf.sprintf "%%%s = type { i32%s }\n\n" name slots)

(* Runtime extern declarations + a small println helper. We declare libc's
   [puts]/[fputs]/[fgets]/[strlen]/[malloc]/[memcpy] and define one
   AffineScript-side function — [as_concat] — in pure IR. *)
let runtime_decls = {|
declare i32 @puts(ptr)
declare i32 @fputs(ptr, ptr)
declare ptr @fgets(ptr, i32, ptr)
declare i64 @strlen(ptr)
declare ptr @malloc(i64)
declare ptr @memcpy(ptr, ptr, i64)
@stdin = external global ptr

define void @println(ptr %s) {
entry:
  %0 = call i32 @puts(ptr %s)
  ret void
}

define void @print(ptr %s) {
entry:
  %0 = load ptr, ptr @stdin   ; stdout would be cleaner; puts already adds \n
  ret void
}

define ptr @as_concat(ptr %a, ptr %b) {
entry:
  %la = call i64 @strlen(ptr %a)
  %lb = call i64 @strlen(ptr %b)
  %sum = add i64 %la, %lb
  %cap = add i64 %sum, 1
  %r = call ptr @malloc(i64 %cap)
  %1 = call ptr @memcpy(ptr %r, ptr %a, i64 %la)
  %tail = getelementptr i8, ptr %r, i64 %la
  %2 = call ptr @memcpy(ptr %tail, ptr %b, i64 %lb)
  %end = getelementptr i8, ptr %r, i64 %sum
  store i8 0, ptr %end
  ret ptr %r
}

define ptr @read_line() {
entry:
  %buf = call ptr @malloc(i64 4096)
  %sin = load ptr, ptr @stdin
  %r = call ptr @fgets(ptr %buf, i32 4096, ptr %sin)
  %is_null = icmp eq ptr %r, null
  br i1 %is_null, label %eof, label %strip
eof:
  store i8 0, ptr %buf
  ret ptr %buf
strip:
  %n = call i64 @strlen(ptr %buf)
  %nz = icmp eq i64 %n, 0
  br i1 %nz, label %done, label %check_nl
check_nl:
  %last_idx = sub i64 %n, 1
  %last_p = getelementptr i8, ptr %buf, i64 %last_idx
  %c = load i8, ptr %last_p
  %is_nl = icmp eq i8 %c, 10
  br i1 %is_nl, label %trim, label %done
trim:
  store i8 0, ptr %last_p
  br label %done
done:
  ret ptr %buf
}

|}

(* Resolve a user-supplied target string into a full LLVM triple. Full triples
   (anything containing a '-') pass through verbatim; bare arch shorthands are
   expanded to canonical triples. *)
let resolve_triple (s : string) : string =
  let s = String.trim s in
  match s with
  | "x86_64"                      -> "x86_64-unknown-linux-gnu"
  | "aarch64" | "arm64"           -> "aarch64-unknown-linux-gnu"
  | "aarch64-android" | "android" -> "aarch64-linux-android"
  | "riscv64"                     -> "riscv64-unknown-linux-gnu"
  | "riscv32"                     -> "riscv32-unknown-linux-gnu"
  | "ios"                         -> "aarch64-apple-ios"
  | _ when String.contains s '-'  -> s  (* full triple, verbatim *)
  | _                             -> s

(* The emitted module's target triple, in precedence order: the explicit
   [?triple] (the [compile --target] CLI flag) > the [AFFINESCRIPT_LLVM_TRIPLE]
   env var > the x86-64 Linux default. The emitted IR is target-independent
   (verified: the same .ll lowers to x86-64, aarch64, and riscv64 objects), so
   only the triple string changes. *)
let llvm_target_triple ?triple () : string =
  match triple with
  | Some t when String.trim t <> "" -> resolve_triple t
  | _ ->
    match Sys.getenv_opt "AFFINESCRIPT_LLVM_TRIPLE" with
    | Some t when String.trim t <> "" -> String.trim t
    | _ -> "x86_64-unknown-linux-gnu"

let generate ?triple (program : program) (_symbols : Symbol.t) : string =
  Hashtbl.clear record_table;
  Hashtbl.clear variant_table;
  Hashtbl.clear string_table;
  next_string_id := 0;

  (* Generate function bodies into a separate buffer first so we can collect
     all string literals before emitting them as globals at the top. *)
  let bodies = Buffer.create 1024 in
  List.iter (function
    | TopType td -> gen_type_decl bodies td
    | _ -> ()
  ) program.prog_decls;
  List.iter (function
    | TopFn fd -> gen_function bodies fd
    | _ -> ()
  ) program.prog_decls;

  let buf = Buffer.create 2048 in
  Buffer.add_string buf "; Generated by AffineScript compiler\n";
  Buffer.add_string buf "; SPDX-License-Identifier: MPL-2.0\n";
  Buffer.add_string buf
    (Printf.sprintf "target triple = \"%s\"\n" (llvm_target_triple ?triple ()));
  (* String-literal globals. Sort by id so output is stable. *)
  let strings = Hashtbl.fold (fun s id acc -> (id, s) :: acc) string_table [] in
  let strings = List.sort (fun (a, _) (b, _) -> compare a b) strings in
  List.iter (fun (id, s) ->
    let len = String.length s + 1 in
    Buffer.add_string buf
      (Printf.sprintf "@.str.%d = private unnamed_addr constant [%d x i8] c\"%s\\00\"\n"
         id len (llvm_escape s))
  ) strings;
  Buffer.add_string buf runtime_decls;
  Buffer.add_buffer buf bodies;
  Buffer.contents buf

let codegen_llvm ?triple (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate ?triple program symbols)
  with
  | Llvm_unsupported m -> Error ("LLVM backend: " ^ m)
  | Failure m          -> Error ("LLVM codegen error: " ^ m)
  | e                  -> Error ("LLVM codegen error: " ^ Printexc.to_string e)
