(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** WebAssembly code generation from AffineScript AST.

    This module translates type-checked and borrow-checked AffineScript
    programs into WebAssembly modules.
*)

open Ast
open Wasm

(** Code generation context *)
type context = {
  types : func_type list;            (** type section *)
  funcs : func list;                 (** function definitions *)
  exports : export list;             (** exports *)
  imports : import list;             (** imports *)
  globals : global list;             (** global variables *)
  locals : (string * int) list;      (** local variable name to index map *)
  next_local : int;                  (** next available local index *)
  loop_depth : int;                  (** current loop nesting depth *)
  func_indices : (string * int) list;  (** function name to index map *)
  lambda_funcs : func list;          (** lifted lambda functions *)
  next_lambda_id : int;              (** next lambda function ID *)
  heap_ptr : int option;             (** global index for heap pointer, if initialized *)
  field_layouts : (string * (string * int) list) list;  (** type name -> [(field, offset)] *)
  variant_tags : (string * int) list;  (** constructor name -> tag (int) *)
}

(** Code generation error *)
type codegen_error =
  | UnsupportedFeature of string
  | UnboundVariable of string
  | TypeMismatch of string
[@@deriving show]

type 'a result = ('a, codegen_error) Result.t

(** Result bind operator *)
let ( let* ) = Result.bind

(** Create initial context *)
let create_context () : context = {
  types = [];
  funcs = [];
  exports = [];
  imports = [];
  globals = [];
  locals = [];
  next_local = 0;
  loop_depth = 0;
  func_indices = [];
  lambda_funcs = [];
  next_lambda_id = 0;
  heap_ptr = None;
  field_layouts = [];
  variant_tags = [];
}

(** Map AffineScript type to WASM value type *)
let type_to_wasm (_ty : type_expr) : value_type result =
  (* For now, use I32 as default *)
  (* TODO: Proper type mapping based on type_expr *)
  Ok I32

(** Allocate a new local variable *)
let alloc_local (ctx : context) (name : string) : (context * int) =
  let idx = ctx.next_local in
  let locals' = (name, idx) :: ctx.locals in
  ({ ctx with locals = locals'; next_local = idx + 1 }, idx)

(** Look up local variable index *)
let lookup_local (ctx : context) (name : string) : int result =
  match List.assoc_opt name ctx.locals with
  | Some idx -> Ok idx
  | None -> Error (UnboundVariable name)

(** Ensure heap pointer global is initialized.
    Returns (context, heap_global_idx). *)
let ensure_heap_ptr (ctx : context) : (context * int) =
  match ctx.heap_ptr with
  | Some idx -> (ctx, idx)
  | None ->
    (* Create heap pointer global initialized to 1024 (1KB) *)
    let idx = List.length ctx.globals in
    let heap_global = {
      g_type = I32;
      g_mutable = true;
      g_init = [I32Const 1024l];  (* Start heap at 1KB *)
    } in
    ({ ctx with
       globals = ctx.globals @ [heap_global];
       heap_ptr = Some idx }, idx)

(** Generate code to allocate memory on the heap.
    Returns instructions that leave the allocated address on the stack.
    size_in_bytes: number of bytes to allocate *)
let gen_heap_alloc (ctx : context) (size_in_bytes : int) : (context * instr list) =
  let (ctx', heap_idx) = ensure_heap_ptr ctx in
  (* Get current heap pointer, then increment it *)
  let alloc_code = [
    GlobalGet heap_idx;           (* Get current heap address *)
    GlobalGet heap_idx;           (* Get it again *)
    I32Const (Int32.of_int size_in_bytes);  (* Size to allocate *)
    I32Add;                       (* Calculate new heap pointer *)
    GlobalSet heap_idx;           (* Update heap pointer *)
    (* Stack now has the allocated address *)
  ] in
  (ctx', alloc_code)

(** Find free variables in an expression.
    Returns list of variable names that are used but not bound within the expression.
    bound_vars: variables already bound in enclosing scope (parameters, let bindings) *)
let rec find_free_vars (bound_vars : string list) (expr : expr) : string list =
  match expr with
  | ExprLit _ -> []
  | ExprVar id ->
    if List.mem id.name bound_vars then [] else [id.name]
  | ExprBinary (e1, _, e2) ->
    find_free_vars bound_vars e1 @ find_free_vars bound_vars e2
  | ExprUnary (_, e) ->
    find_free_vars bound_vars e
  | ExprIf ei ->
    find_free_vars bound_vars ei.ei_cond @
    find_free_vars bound_vars ei.ei_then @
    (match ei.ei_else with
     | Some e -> find_free_vars bound_vars e
     | None -> [])
  | ExprLet lb ->
    let rhs_free = find_free_vars bound_vars lb.el_value in
    (* Add bound variable to scope for body *)
    let new_bound = match lb.el_pat with
      | PatVar id -> id.name :: bound_vars
      | _ -> bound_vars
    in
    let body_free = match lb.el_body with
      | Some e -> find_free_vars new_bound e
      | None -> []
    in
    rhs_free @ body_free
  | ExprLambda lam ->
    (* Parameters are bound within lambda *)
    let param_names = List.map (fun p -> p.p_name.name) lam.elam_params in
    find_free_vars (param_names @ bound_vars) lam.elam_body
  | ExprApp (f, args) ->
    find_free_vars bound_vars f @
    List.concat (List.map (find_free_vars bound_vars) args)
  | ExprBlock blk ->
    (* Statements may introduce bindings *)
    let (_, free) = List.fold_left (fun (bound, acc_free) stmt ->
      match stmt with
      | StmtLet sl ->
        let rhs_free = find_free_vars bound sl.sl_value in
        let new_bound = match sl.sl_pat with
          | PatVar id -> id.name :: bound
          | _ -> bound
        in
        (new_bound, acc_free @ rhs_free)
      | StmtExpr e ->
        (bound, acc_free @ find_free_vars bound e)
      | _ -> (bound, acc_free)
    ) (bound_vars, []) blk.blk_stmts in
    let expr_free = match blk.blk_expr with
      | Some e -> find_free_vars bound_vars e
      | None -> []
    in
    free @ expr_free
  | ExprMatch m ->
    find_free_vars bound_vars m.em_scrutinee @
    List.concat (List.map (fun arm -> find_free_vars bound_vars arm.ma_body) m.em_arms)
  | ExprReturn e_opt ->
    (match e_opt with Some e -> find_free_vars bound_vars e | None -> [])
  | ExprTuple exprs | ExprArray exprs ->
    List.concat (List.map (find_free_vars bound_vars) exprs)
  | ExprRecord r ->
    List.concat (List.map (fun (_, e_opt) ->
      match e_opt with
      | Some e -> find_free_vars bound_vars e
      | None -> []
    ) r.er_fields)
  | ExprField (e, _) -> find_free_vars bound_vars e
  | ExprTupleIndex (e, _) -> find_free_vars bound_vars e
  | ExprIndex (e1, e2) ->
    find_free_vars bound_vars e1 @ find_free_vars bound_vars e2
  | ExprVariant _ -> []
  | ExprSpan (e, _) -> find_free_vars bound_vars e
  | _ -> []  (* Other expressions *)

(** Remove duplicates from list *)
let dedup (lst : string list) : string list =
  List.fold_left (fun acc x ->
    if List.mem x acc then acc else x :: acc
  ) [] lst |> List.rev

(** Generate code for a literal *)
let gen_literal (lit : literal) : instr result =
  match lit with
  | LitUnit _ -> Ok (I32Const 0l)  (* Unit represented as 0 *)
  | LitBool (b, _) -> Ok (I32Const (if b then 1l else 0l))
  | LitInt (n, _) -> Ok (I32Const (Int32.of_int n))
  | LitFloat (f, _) -> Ok (F64Const f)
  | LitChar (c, _) -> Ok (I32Const (Int32.of_int (Char.code c)))
  | LitString _ -> Error (UnsupportedFeature "String literals not yet supported in codegen")

(** Generate code for binary operation *)
let gen_binop (op : binary_op) : instr =
  match op with
  | OpAdd -> I32Add
  | OpSub -> I32Sub
  | OpMul -> I32Mul
  | OpDiv -> I32DivS
  | OpMod -> I32RemS
  | OpEq -> I32Eq
  | OpNe -> I32Ne
  | OpLt -> I32LtS
  | OpLe -> I32LeS
  | OpGt -> I32GtS
  | OpGe -> I32GeS
  | OpAnd -> I32And
  | OpOr -> I32Or
  | OpBitAnd -> I32And
  | OpBitOr -> I32Or
  | OpBitXor -> I32Xor
  | OpShl -> I32Shl
  | OpShr -> I32ShrS

(** Generate code for unary operation *)
let gen_unop (op : unary_op) : instr result =
  match op with
  | OpNeg -> Ok I32Sub  (* 0 - x *)
  | OpNot -> Ok I32Eqz  (* x == 0 *)
  | OpBitNot -> Error (UnsupportedFeature "Bitwise NOT")
  | OpRef -> Error (UnsupportedFeature "References not yet supported in codegen")
  | OpDeref -> Error (UnsupportedFeature "Dereference not yet supported in codegen")

(** Generate code for an expression, returning instructions and updated context *)
let rec gen_expr (ctx : context) (expr : expr) : (context * instr list) result =
  match expr with
  | ExprLit lit ->
    let* instr = gen_literal lit in
    Ok (ctx, [instr])

  | ExprVar id ->
    let* idx = lookup_local ctx id.name in
    Ok (ctx, [LocalGet idx])

  | ExprBinary (left, op, right) ->
    let* (ctx', left_code) = gen_expr ctx left in
    let* (ctx'', right_code) = gen_expr ctx' right in
    let op_instr = gen_binop op in
    Ok (ctx'', left_code @ right_code @ [op_instr])

  | ExprUnary (op, operand) ->
    let* (ctx', operand_code) = gen_expr ctx operand in
    let* op_instr = gen_unop op in
    let prefix = match op with
      | OpNeg -> [I32Const 0l]  (* 0 - operand *)
      | _ -> []
    in
    Ok (ctx', prefix @ operand_code @ [op_instr])

  | ExprIf ei ->
    let* (ctx', cond_code) = gen_expr ctx ei.ei_cond in
    let* (ctx'', then_code) = gen_expr ctx' ei.ei_then in
    let else_result = match ei.ei_else with
      | Some e -> gen_expr ctx'' e
      | None -> Ok (ctx'', [I32Const 0l])  (* Default to 0 if no else *)
    in
    let* (ctx_final, else_code) = else_result in
    Ok (ctx_final, cond_code @ [If (BtType I32, then_code, else_code)])

  | ExprBlock blk ->
    gen_block ctx blk

  | ExprReturn e_opt ->
    begin match e_opt with
      | Some e ->
        let* (ctx', code) = gen_expr ctx e in
        Ok (ctx', code @ [Return])
      | None ->
        Ok (ctx, [Return])
    end

  | ExprLet lb ->
    let* (ctx', rhs_code) = gen_expr ctx lb.el_value in
    (* For now, only handle simple variable patterns *)
    begin match lb.el_pat with
      | PatVar id ->
        let (ctx'', idx) = alloc_local ctx' id.name in
        let set_code = [LocalSet idx] in
        begin match lb.el_body with
          | Some body ->
            let* (ctx_final, body_code) = gen_expr ctx'' body in
            Ok (ctx_final, rhs_code @ set_code @ body_code)
          | None ->
            Ok (ctx'', rhs_code @ set_code @ [I32Const 0l])
        end
      | _ -> Error (UnsupportedFeature "Complex patterns not yet supported in codegen")
    end

  | ExprLambda lam ->
    (* Detect free variables (captured from enclosing scope) *)
    let param_names = List.map (fun p -> p.p_name.name) lam.elam_params in
    let all_free = find_free_vars param_names lam.elam_body in
    (* Filter to only variables currently in scope *)
    let captured_vars = List.filter (fun name ->
      List.mem_assoc name ctx.locals
    ) (dedup all_free) in

    let lambda_id = ctx.next_lambda_id in

    (* If there are captured variables, create closure environment *)
    let (ctx_after_env, env_code) = if List.length captured_vars > 0 then
      (* Create environment tuple with captured values *)
      let num_captured = List.length captured_vars in
      let env_size = num_captured * 4 in
      let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx env_size in
      let (ctx_with_temp, env_idx) = alloc_local ctx_with_heap "__closure_env" in

      let save_code = [LocalTee env_idx] in

      (* Store each captured variable in environment *)
      (* Note: Each store consumes env_ptr and value, but we push env_ptr before each store,
         so after all stores, one env_ptr remains on stack *)
      let store_code = List.mapi (fun i var_name ->
        let var_idx = List.assoc var_name ctx.locals in
        [
          LocalGet env_idx;
          LocalGet var_idx;
          I32Store (2, i * 4);
        ]
      ) captured_vars |> List.concat in

      (* Don't push env_idx again - one is already on stack after stores *)
      (ctx_with_temp, alloc_code @ save_code @ store_code)
    else
      (* No captures - environment is null (0) *)
      (ctx, [I32Const 0l])
    in

    (* Create fresh context for lambda function *)
    let lambda_ctx = { ctx_after_env with locals = []; next_local = 0; loop_depth = 0 } in

    (* Environment is always first parameter (even if unused) for uniform calling convention *)
    let (ctx_with_env, _) = alloc_local lambda_ctx "__env" in
    let env_param_offset = 1 in

    (* Regular parameters come after environment *)
    let (ctx_with_params, _) = List.fold_left (fun (c, _) param ->
      alloc_local c param.p_name.name
    ) (ctx_with_env, 0) lam.elam_params in

    (* Add captured variables to local scope (load from environment) *)
    let (ctx_with_captured, load_captured_code) = if List.length captured_vars > 0 then
      let (c, code) = List.fold_left (fun (c_acc, code_acc) (i, var_name) ->
        let (c', var_idx) = alloc_local c_acc var_name in
        let load_code = [
          LocalGet 0;  (* Environment pointer *)
          I32Load (2, i * 4);
          LocalSet var_idx;
        ] in
        (c', code_acc @ load_code)
      ) (ctx_with_params, []) (List.mapi (fun i v -> (i, v)) captured_vars) in
      (c, code)
    else
      (ctx_with_params, [])
    in

    let param_count = env_param_offset + List.length lam.elam_params in

    (* Generate lambda body *)
    let* (ctx_final, body_code) = gen_expr ctx_with_captured lam.elam_body in

    (* Compute additional locals (beyond parameters and captured vars) *)
    let local_count = ctx_final.next_local - param_count in
    let locals = if local_count > 0 then
      [{ l_count = local_count; l_type = I32 }]
    else
      []
    in

    (* Create function type for lambda (env param always included + regular params) *)
    let param_types = I32 :: List.map (fun _ -> I32) lam.elam_params in
    let result_type = [I32] in
    let func_type = { ft_params = param_types; ft_results = result_type } in

    (* Add type to types list *)
    let type_idx = List.length ctx_after_env.types in
    let ctx_with_type = { ctx_after_env with types = ctx_after_env.types @ [func_type] } in

    (* Create lambda function *)
    let lambda_func = {
      f_type = type_idx;
      f_locals = locals;
      f_body = load_captured_code @ body_code;
    } in

    (* Add lambda function to lifted functions *)
    let ctx_with_lambda = {
      ctx_with_type with
      lambda_funcs = ctx_with_type.lambda_funcs @ [lambda_func];
      next_lambda_id = lambda_id + 1;
    } in

    (* Return a closure: (function_id, env_pointer) as a 2-element tuple *)
    let closure_size = 8 in  (* 2 * 4 bytes *)
    let (ctx_final2, closure_alloc) = gen_heap_alloc ctx_with_lambda closure_size in
    let (ctx_final3, closure_idx) = alloc_local ctx_final2 "__closure" in

    let closure_code = closure_alloc @ [LocalTee closure_idx] @ [
      (* Store function ID at offset 0 *)
      LocalGet closure_idx;
      I32Const (Int32.of_int lambda_id);
      I32Store (2, 0);
    ] @ [
      (* Store environment pointer at offset 4 *)
      LocalGet closure_idx;
    ] @ env_code @ [
      (* env_code left env_ptr on stack, closure_idx is below it *)
      (* Stack is now [closure_idx, env_ptr] with env_ptr on top *)
      I32Store (2, 4);
      (* Return closure pointer *)
      LocalGet closure_idx;
    ] in

    Ok (ctx_final3, closure_code)

  | ExprApp (func_expr, args) ->
    (* Generate code for arguments (left to right) *)
    let* (ctx_after_args, all_arg_code) = List.fold_left (fun acc arg ->
      let* (ctx', accumulated_code) = acc in
      let* (ctx'', arg_code) = gen_expr ctx' arg in
      Ok (ctx'', accumulated_code @ arg_code)
    ) (Ok (ctx, [])) args in

    (* Generate code for function expression *)
    begin match func_expr with
      | ExprVar id ->
        (* Check if it's a named function or a variable holding a lambda *)
        begin match List.assoc_opt id.name ctx_after_args.func_indices with
          | Some func_idx ->
            (* Direct function call *)
            let call_instr = Call func_idx in
            Ok (ctx_after_args, all_arg_code @ [call_instr])
          | None ->
            (* Check if it's a local variable (could be a closure) *)
            begin match lookup_local ctx_after_args id.name with
              | Ok local_idx ->
                (* Closure is a tuple: (func_id, env_ptr) *)
                (* Load function ID and environment, then call indirect *)

                (* Allocate temp locals for closure components *)
                let (ctx_temp1, func_id_idx) = alloc_local ctx_after_args "__func_id" in
                let (ctx_temp2, env_ptr_idx) = alloc_local ctx_temp1 "__env_ptr" in

                (* Extract closure components *)
                let extract_closure = [
                  (* Load function ID from offset 0 *)
                  LocalGet local_idx;
                  I32Load (2, 0);
                  LocalSet func_id_idx;

                  (* Load environment pointer from offset 4 *)
                  LocalGet local_idx;
                  I32Load (2, 4);
                  LocalSet env_ptr_idx;
                ] in

                (* Create type signature: env + user args *)
                let param_types = I32 :: List.map (fun _ -> I32) args in
                let result_type = [I32] in
                let call_type = { ft_params = param_types; ft_results = result_type } in

                (* Find or add this type *)
                let type_idx =
                  match List.find_index (fun t -> t = call_type) ctx_temp2.types with
                  | Some idx -> idx
                  | None -> List.length ctx_temp2.types
                in

                (* Call: push env, push user args, push func_id, call indirect *)
                let call_instrs = [
                  LocalGet env_ptr_idx;    (* Environment as first arg *)
                ] @ all_arg_code @ [       (* User arguments *)
                  LocalGet func_id_idx;    (* Function ID for indirect call *)
                  CallIndirect type_idx
                ] in

                Ok (ctx_temp2, extract_closure @ call_instrs)
              | Error _ ->
                Error (UnboundVariable ("Function or variable not found: " ^ id.name))
            end
        end
      | ExprLambda _ ->
        (* Lambda expression as function - generate lambda and call it *)
        let* (ctx_with_lambda, lambda_code) = gen_expr ctx_after_args func_expr in

        (* Create type signature for the call *)
        let param_types = List.map (fun _ -> I32) args in
        let result_type = [I32] in
        let call_type = { ft_params = param_types; ft_results = result_type } in

        (* Find matching type index *)
        let type_idx =
          match List.find_index (fun t -> t = call_type) ctx_with_lambda.types with
          | Some idx -> idx
          | None -> List.length ctx_with_lambda.types
        in

        Ok (ctx_with_lambda, all_arg_code @ lambda_code @ [CallIndirect type_idx])
      | _ ->
        (* Other expressions that evaluate to functions - treat as indirect call *)
        let* (ctx_final, func_code) = gen_expr ctx_after_args func_expr in

        (* Create type signature for the call *)
        let param_types = List.map (fun _ -> I32) args in
        let result_type = [I32] in
        let call_type = { ft_params = param_types; ft_results = result_type } in

        (* Find matching type index *)
        let type_idx =
          match List.find_index (fun t -> t = call_type) ctx_final.types with
          | Some idx -> idx
          | None -> List.length ctx_final.types
        in

        Ok (ctx_final, all_arg_code @ func_code @ [CallIndirect type_idx])
    end

  | ExprMatch match_expr ->
    (* Evaluate scrutinee and store in a temporary local *)
    let* (ctx_after_scrutinee, scrutinee_code) = gen_expr ctx match_expr.em_scrutinee in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_after_scrutinee "__match_tmp" in

    (* Generate code for each match arm *)
    let rec gen_arms ctx arms =
      match arms with
      | [] ->
        (* No arms matched - this shouldn't happen with exhaustive patterns *)
        (* Return 0 as a fallback *)
        Ok (ctx, [I32Const 0l])
      | arm :: rest ->
        (* Generate pattern matching code *)
        let* (ctx_after_pat, pattern_test, _bindings) = gen_pattern ctx temp_idx arm.ma_pat in

        (* Generate body code with bindings *)
        let* (ctx_after_body, body_code) = gen_expr ctx_after_pat arm.ma_body in

        (* If there are more arms, generate else branch *)
        if List.length rest > 0 then
          let* (ctx_final, else_code) = gen_arms ctx_after_body rest in
          (* pattern_test leaves boolean on stack, then If uses it *)
          Ok (ctx_final,
              pattern_test @
              [If (BtType I32,
                   body_code,    (* Then: pattern matched, execute body *)
                   else_code)])  (* Else: try next arm *)
        else
          (* Last arm - just execute body if pattern matches *)
          (* If pattern doesn't match, return 0 (shouldn't happen) *)
          Ok (ctx_after_body,
              pattern_test @
              [If (BtType I32,
                   body_code,        (* Then: pattern matched, execute body *)
                   [I32Const 0l])])  (* Else: fallback (shouldn't reach) *)
    in

    let* (ctx_final, arms_code) = gen_arms ctx_with_temp match_expr.em_arms in

    (* Complete code: eval scrutinee, store in temp, then try arms *)
    Ok (ctx_final, scrutinee_code @ [LocalSet temp_idx] @ arms_code)

  | ExprTuple elements ->
    (* Tuple layout in memory: [elem0: I32][elem1: I32][elem2: I32]... *)
    (* No length field - tuple size is fixed at creation *)
    let num_elements = List.length elements in
    let size_in_bytes = num_elements * 4 in  (* 4 bytes per element *)

    (* Allocate heap memory and save pointer to temp local *)
    let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx size_in_bytes in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_with_heap "__tup_ptr" in

    (* Save allocated address to temp *)
    let save_code = [LocalTee temp_idx] in

    (* Generate code to store each element *)
    let* (ctx_final, store_code) = List.fold_left (fun acc (idx, elem_expr) ->
      let* (ctx_acc, code_acc) = acc in
      (* Generate code for element value *)
      let* (ctx', elem_code) = gen_expr ctx_acc elem_expr in

      (* Store at offset (idx * 4) *)
      let offset = idx * 4 in
      let store_instrs = [
        LocalGet temp_idx;  (* Get base address *)
      ] @ elem_code @ [
        I32Store (2, offset);  (* Store element at offset *)
      ] in
      Ok (ctx', code_acc @ store_instrs)
    ) (Ok (ctx_with_temp, [])) (List.mapi (fun i e -> (i, e)) elements) in

    (* Complete code: allocate, save to temp, store elements, return pointer *)
    Ok (ctx_final, alloc_code @ save_code @ store_code @ [LocalGet temp_idx])

  | ExprArray elements ->
    (* Array layout in memory: [length: I32][elem0: I32][elem1: I32]... *)
    let num_elements = List.length elements in
    let size_in_bytes = 4 + (num_elements * 4) in  (* 4 for length + 4 per element *)

    (* Allocate heap memory and save pointer to temp local *)
    let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx size_in_bytes in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_with_heap "__arr_ptr" in

    (* Save allocated address to temp *)
    let save_code = [LocalTee temp_idx] in

    (* Store length at offset 0 *)
    let length_code = [
      LocalGet temp_idx;
      I32Const (Int32.of_int num_elements);
      I32Store (2, 0);  (* Store length at offset 0 *)
    ] in

    (* Generate code to store each element *)
    let* (ctx_final, store_code) = List.fold_left (fun acc (idx, elem_expr) ->
      let* (ctx_acc, code_acc) = acc in
      (* Generate code for element value *)
      let* (ctx', elem_code) = gen_expr ctx_acc elem_expr in

      (* Store at offset 4 + (idx * 4) *)
      let offset = 4 + (idx * 4) in
      let store_instrs = [
        LocalGet temp_idx;  (* Get base address *)
      ] @ elem_code @ [
        I32Store (2, offset);  (* Store element at offset *)
      ] in
      Ok (ctx', code_acc @ store_instrs)
    ) (Ok (ctx_with_temp, [])) (List.mapi (fun i e -> (i, e)) elements) in

    (* Complete code: allocate, save to temp, store length, store elements, return pointer *)
    Ok (ctx_final, alloc_code @ save_code @ length_code @ store_code @ [LocalGet temp_idx])

  | ExprRecord rec_expr ->
    (* Allocate memory for record fields *)
    let num_fields = List.length rec_expr.er_fields in
    let size_in_bytes = num_fields * 4 in  (* Each field is 4 bytes (I32) *)

    (* Allocate heap memory and save pointer to temp local *)
    let (ctx_with_heap, alloc_code) = gen_heap_alloc ctx size_in_bytes in
    let (ctx_with_temp, temp_idx) = alloc_local ctx_with_heap "__rec_ptr" in

    (* Save allocated address to temp *)
    let save_code = [LocalTee temp_idx] in  (* Tee: set local and keep value on stack *)

    (* Generate code to store each field *)
    let* (ctx_final, store_code) = List.fold_left (fun acc (idx, (field, expr_opt)) ->
      let* (ctx_acc, code_acc) = acc in
      (* Get field value expression *)
      let field_expr = match expr_opt with
        | Some e -> e
        | None -> ExprVar field  (* Field punning: {x} means {x: x} *)
      in
      (* Generate code for field value *)
      let* (ctx', field_code) = gen_expr ctx_acc field_expr in

      (* Store at offset (field_index * 4) from base address *)
      let offset = idx * 4 in
      let store_instrs = [
        LocalGet temp_idx;  (* Get base address *)
      ] @ field_code @ [
        I32Store (2, offset);  (* Store at offset with alignment 2 (4-byte) *)
      ] in
      Ok (ctx', code_acc @ store_instrs)
    ) (Ok (ctx_with_temp, [])) (List.mapi (fun i x -> (i, x)) rec_expr.er_fields) in

    (* Complete code: allocate, save to temp (leaving on stack), store fields, leave address *)
    (* But we already consumed the address from stack when storing, so push it again *)
    Ok (ctx_final, alloc_code @ save_code @ store_code @ [LocalGet temp_idx])

  | ExprField (record_expr, field) ->
    (* Generate code for record expression (gets pointer) *)
    let* (ctx', record_code) = gen_expr ctx record_expr in

    (* Look up field offset from field_layouts *)
    (* For now, only supports field access on variables assigned record literals *)
    let field_offset = match record_expr with
      | ExprVar var_name ->
        (* Look up variable's field layout *)
        begin match List.assoc_opt var_name.name ctx.field_layouts with
          | Some layout ->
            (* Find field offset in layout *)
            begin match List.assoc_opt field.name layout with
              | Some offset -> offset
              | None -> 0  (* Field not found, default to 0 *)
            end
          | None -> 0  (* Variable layout not tracked, default to 0 *)
        end
      | _ -> 0  (* Complex expression, default to 0 *)
    in

    (* Load from memory at field offset *)
    let load_code = [
      I32Load (2, field_offset)  (* Load with alignment 2 (4-byte) and offset *)
    ] in

    Ok (ctx', record_code @ load_code)

  | ExprTupleIndex (tuple_expr, index) ->
    (* Generate code for tuple expression (gets pointer) *)
    let* (ctx', tuple_code) = gen_expr ctx tuple_expr in

    (* Calculate offset: index * 4 (no length field in tuples) *)
    let offset = index * 4 in

    (* Load from memory at offset *)
    let load_code = [
      I32Load (2, offset)  (* Load with alignment 2 (4-byte) and offset *)
    ] in

    Ok (ctx', tuple_code @ load_code)

  | ExprIndex (array_expr, index_expr) ->
    (* Generate code for array (gets pointer) *)
    let* (ctx_after_arr, array_code) = gen_expr ctx array_expr in

    (* Generate code for index *)
    let* (ctx_after_idx, index_code) = gen_expr ctx_after_arr index_expr in

    (* Calculate offset: 4 + (index * 4) *)
    (* Stack after array_code @ index_code: [array_ptr, index] *)
    let offset_calc = [
      I32Const 4l;        (* Constant 4 for element size *)
      I32Mul;             (* index * 4 *)
      I32Const 4l;        (* Add 4 to skip length field *)
      I32Add;             (* offset = 4 + (index * 4) *)
    ] in

    (* Add base pointer to offset and load *)
    let load_code = [
      I32Add;             (* base_ptr + offset *)
      I32Load (2, 0);     (* Load from calculated address *)
    ] in

    (* Complete code: array_ptr, index, calculate offset, add to base, load *)
    Ok (ctx_after_idx, array_code @ index_code @ offset_calc @ load_code)

  | ExprVariant (_type_name, variant_name) ->
    (* Look up variant tag *)
    (* For now, use variant name directly to find tag *)
    begin match List.assoc_opt variant_name.name ctx.variant_tags with
      | Some tag ->
        (* Zero-argument variant: just return the tag as an integer *)
        Ok (ctx, [I32Const (Int32.of_int tag)])
      | None ->
        (* Tag not found - assign a new sequential tag based on name *)
        (* This is a fallback for when type declarations aren't processed *)
        let tag = List.length ctx.variant_tags in
        let ctx' = { ctx with variant_tags = (variant_name.name, tag) :: ctx.variant_tags } in
        Ok (ctx', [I32Const (Int32.of_int tag)])
    end

  | ExprRowRestrict _ ->
    Error (UnsupportedFeature "Row restriction not supported in codegen")

  | ExprHandle _ ->
    Error (UnsupportedFeature "Effect handlers not yet supported in codegen")

  | ExprResume _ ->
    Error (UnsupportedFeature "Resume not yet supported in codegen")

  | ExprTry _ ->
    Error (UnsupportedFeature "Try/catch not yet supported in codegen")

  | ExprUnsafe _ ->
    Error (UnsupportedFeature "Unsafe operations not yet supported in codegen")

  | ExprSpan (e, _) ->
    gen_expr ctx e

(** Generate code for a block *)
and gen_block (ctx : context) (blk : block) : (context * instr list) result =
  let* (ctx', stmt_codes) = List.fold_left (fun acc stmt ->
    let* (c, codes) = acc in
    let* (c', code) = gen_stmt c stmt in
    Ok (c', codes @ code)
  ) (Ok (ctx, [])) blk.blk_stmts in
  match blk.blk_expr with
  | Some e ->
    let* (ctx_final, expr_code) = gen_expr ctx' e in
    Ok (ctx_final, stmt_codes @ expr_code)
  | None ->
    Ok (ctx', stmt_codes @ [I32Const 0l])

(** Generate pattern matching test code.
    Returns (context, test_code, bindings).
    test_code should leave a boolean (i32) on the stack indicating if pattern matches.
    bindings is a list of (name, temp_idx) for variables bound by the pattern. *)
and gen_pattern (ctx : context) (scrutinee_local : int) (pat : pattern)
  : (context * instr list * (string * int) list) result =
  match pat with
  | PatWildcard _ ->
    (* Wildcard always matches, no bindings *)
    Ok (ctx, [I32Const 1l], [])

  | PatVar id ->
    (* Variable pattern always matches and binds the scrutinee to the variable *)
    let (ctx', var_idx) = alloc_local ctx id.name in
    let bind_code = [
      LocalGet scrutinee_local;  (* Get scrutinee value *)
      LocalSet var_idx;          (* Bind to pattern variable *)
      I32Const 1l                (* Pattern matches *)
    ] in
    Ok (ctx', bind_code, [(id.name, var_idx)])

  | PatLit lit ->
    (* Literal pattern matches if scrutinee equals the literal *)
    let* lit_instr = gen_literal lit in
    let test_code = [
      LocalGet scrutinee_local;  (* Get scrutinee value *)
      lit_instr;                 (* Get literal value *)
      I32Eq                      (* Compare *)
    ] in
    Ok (ctx, test_code, [])

  | PatCon (con, sub_patterns) ->
    (* Constructor pattern - match against tag *)
    if List.length sub_patterns = 0 then
      (* Zero-argument constructor: compare scrutinee to tag *)
      begin match List.assoc_opt con.name ctx.variant_tags with
        | Some tag ->
          let test_code = [
            LocalGet scrutinee_local;  (* Get scrutinee (should be tag) *)
            I32Const (Int32.of_int tag);  (* Expected tag *)
            I32Eq  (* Compare *)
          ] in
          Ok (ctx, test_code, [])
        | None ->
          (* Tag not found - auto-assign and match *)
          let tag = List.length ctx.variant_tags in
          let ctx' = { ctx with variant_tags = (con.name, tag) :: ctx.variant_tags } in
          let test_code = [
            LocalGet scrutinee_local;
            I32Const (Int32.of_int tag);
            I32Eq
          ] in
          Ok (ctx', test_code, [])
      end
    else
      (* Constructor with arguments - not yet supported *)
      Error (UnsupportedFeature "Constructor patterns with arguments not yet supported in codegen")

  | PatTuple _ ->
    Error (UnsupportedFeature "Tuple patterns not yet supported in codegen")

  | PatRecord _ ->
    Error (UnsupportedFeature "Record patterns not yet supported in codegen")

  | PatOr _ ->
    Error (UnsupportedFeature "Or patterns not yet supported in codegen")

  | PatAs _ ->
    Error (UnsupportedFeature "As patterns not yet supported in codegen")

(** Generate code for a statement *)
and gen_stmt (ctx : context) (stmt : stmt) : (context * instr list) result =
  match stmt with
  | StmtLet sl ->
    let* (ctx', rhs_code) = gen_expr ctx sl.sl_value in
    (* For now, only handle simple variable patterns *)
    begin match sl.sl_pat with
      | PatVar id ->
        let (ctx'', idx) = alloc_local ctx' id.name in
        (* If RHS is a record, track its field layout *)
        let ctx_with_layout = match sl.sl_value with
          | ExprRecord rec_expr ->
            (* Extract field names in order and assign offsets *)
            let field_layout = List.mapi (fun i (field_name, _) ->
              (field_name.name, i * 4)
            ) rec_expr.er_fields in
            { ctx'' with field_layouts = (id.name, field_layout) :: ctx''.field_layouts }
          | _ -> ctx''
        in
        Ok (ctx_with_layout, rhs_code @ [LocalSet idx])
      | _ -> Error (UnsupportedFeature "Complex patterns not yet supported in codegen")
    end

  | StmtExpr e ->
    let* (ctx', code) = gen_expr ctx e in
    Ok (ctx', code @ [Drop])  (* Discard expression result *)

  | StmtAssign _ ->
    Error (UnsupportedFeature "Assignment not yet supported in codegen")

  | StmtWhile (cond, body) ->
    let* (ctx', cond_code) = gen_expr ctx cond in
    let* (ctx'', body_code) = gen_block ctx' body in
    (* Loop with conditional exit *)
    Ok (ctx'', [Block (BtEmpty, [
      Loop (BtEmpty,
        cond_code @ [I32Eqz; BrIf 1] @  (* If condition false, exit *)
        body_code @ [Br 0]  (* Continue loop *)
      )
    ])])

  | StmtFor _ ->
    Error (UnsupportedFeature "For loops not yet supported in codegen")

(** Generate code for a function *)
let gen_function (ctx : context) (fd : fn_decl) : (context * func) result =
  (* Create fresh context for function scope, but preserve lambda_funcs and next_lambda_id *)
  let fn_ctx = { ctx with locals = []; next_local = 0; loop_depth = 0 } in

  (* Parameters become locals 0..n-1 *)
  let (ctx_with_params, _) = List.fold_left (fun (c, _) param ->
    alloc_local c param.p_name.name
  ) (fn_ctx, 0) fd.fd_params in

  let param_count = List.length fd.fd_params in

  (* Generate function body *)
  let body_expr = match fd.fd_body with
    | FnBlock blk -> ExprBlock blk
    | FnExpr e -> e
  in
  let* (ctx_final, body_code) = gen_expr ctx_with_params body_expr in

  (* Compute additional locals (beyond parameters) *)
  let local_count = ctx_final.next_local - param_count in
  let locals = if local_count > 0 then
    [{ l_count = local_count; l_type = I32 }]
  else
    []
  in

  (* Create function (type index will be set by gen_decl) *)
  let func = {
    f_type = 0;  (* Will be overridden *)
    f_locals = locals;
    f_body = body_code;
  } in

  (* Return updated context with any lambda functions that were created *)
  Ok (ctx_final, func)

(** Generate code for a top-level declaration *)
let gen_decl (ctx : context) (decl : top_level) : context result =
  match decl with
  | TopFn fd ->
    (* Create function type *)
    let param_types = List.map (fun _ -> I32) fd.fd_params in
    let result_type = [I32] in
    let func_type = { ft_params = param_types; ft_results = result_type } in

    (* Add type to types list *)
    let type_idx = List.length ctx.types in
    let ctx_with_type = { ctx with types = ctx.types @ [func_type] } in

    (* Determine function index before generating *)
    let func_idx = List.length ctx_with_type.funcs in

    (* Register function name to index mapping *)
    let ctx_with_func_idx = { ctx_with_type with
      func_indices = ctx_with_type.func_indices @ [(fd.fd_name.name, func_idx)]
    } in

    (* Generate function with correct type index *)
    let* (ctx_after_gen, func) = gen_function ctx_with_func_idx fd in
    let func_with_type = { func with f_type = type_idx } in

    (* Add export for main function *)
    let export = if fd.fd_name.name = "main" then
      [{ e_name = "main"; e_desc = ExportFunc func_idx }]
    else
      []
    in
    (* Use ctx_after_gen to preserve any lambda_funcs created during generation *)
    Ok { ctx_after_gen with
         funcs = ctx_after_gen.funcs @ [func_with_type];
         exports = ctx_after_gen.exports @ export
       }

  | TopConst _ ->
    Error (UnsupportedFeature "Constants not yet supported in codegen")

  | TopType td ->
    (* Register variant tags for enum types *)
    begin match td.td_body with
      | TyEnum variants ->
        (* Assign sequential tags to each variant constructor *)
        let ctx_with_tags = List.fold_left (fun c_acc (idx, vd) ->
          (* Register: constructor_name -> tag *)
          { c_acc with variant_tags = (vd.vd_name.name, idx) :: c_acc.variant_tags }
        ) ctx (List.mapi (fun i v -> (i, v)) variants) in
        Ok ctx_with_tags
      | _ ->
        (* Other type declarations (alias, struct) don't need codegen *)
        Ok ctx
    end

  | TopEffect _ | TopTrait _ | TopImpl _ ->
    (* These declarations don't generate code *)
    Ok ctx

(** Generate WASM module from AffineScript program *)
let generate_module (prog : program) : wasm_module result =
  let ctx = create_context () in
  let* ctx' = List.fold_left (fun acc decl ->
    let* c = acc in
    gen_decl c decl
  ) (Ok ctx) prog.prog_decls in

  (* Merge regular functions and lambda functions *)
  let num_regular_funcs = List.length ctx'.funcs in
  let all_funcs = ctx'.funcs @ ctx'.lambda_funcs in

  (* Create function table if there are lambdas *)
  let (tables, elems) = if List.length ctx'.lambda_funcs > 0 then
    (* Table size = number of lambda functions *)
    let table_size = List.length ctx'.lambda_funcs in
    let table = [{ tab_type = { lim_min = table_size; lim_max = Some table_size } }] in

    (* Create element segment to initialize table with lambda function indices *)
    (* Lambda functions start at index num_regular_funcs *)
    let lambda_func_indices = List.mapi (fun i _ -> num_regular_funcs + i) ctx'.lambda_funcs in
    let elem_seg = [{
      e_table = 0;
      e_offset = 0;
      e_funcs = lambda_func_indices;
    }] in
    (table, elem_seg)
  else
    ([], [])
  in

  Ok {
    types = ctx'.types;
    funcs = all_funcs;
    tables = tables;
    mems = [{ mem_type = { lim_min = 1; lim_max = None } }];  (* 1 page default *)
    globals = ctx'.globals;
    exports = ctx'.exports;
    imports = ctx'.imports;
    elems = elems;
    start = None;
  }
