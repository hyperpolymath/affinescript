(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** ONNX Backend (MVP — proof of wire format).

    Emits a binary [.onnx] [ModelProto] from a strict subset of AffineScript:

    - one entry function (named [graph], [main], or the first [fn])
    - parameters typed [Array[Float]] are graph inputs (1-D, dimension param)
    - return type [Array[Float]] is the graph output
    - body is a chain of [let] bindings whose RHS is one of:
        * a variable reference (passes the SSA value forward)
        * a call to a recognised ONNX op by name
      and a final expression naming the output

    The recognised op set is small: arithmetic ([Add], [Sub], [Mul], [Div]),
    common activations ([Relu], [Sigmoid], [Tanh]), and identity-shaped ops
    ([Identity]). Each is matched by AffineScript function name (case-
    insensitive lowercased — [add] maps to [Add], etc.). Unrecognised calls
    produce a hard error so the regression is loud.

    What this does NOT do (intentional, MVP scope):
    - tensor shape inference
    - broadcasting rules
    - attribute encoding (axes, alpha, etc.)
    - initializer tensors / weights
    - any verification beyond "the bytes parse as ONNX"

    Validation strategy: round-trip the output through any ONNX reader
    (oxionnx-proto, onnxruntime, tract). All three accept the same wire
    format; if the bytes decode they're ONNX-conformant.
*)

open Ast

exception Onnx_unsupported of string
let unsupported msg = raise (Onnx_unsupported msg)

(* ============================================================================
   Op recognition

   Map an AffineScript function name to (ONNX op type, expected arity).
   ============================================================================ *)

let recognise_op (name : string) : (string * int) option =
  match String.lowercase_ascii name with
  | "add"     -> Some ("Add", 2)
  | "sub"     -> Some ("Sub", 2)
  | "mul"     -> Some ("Mul", 2)
  | "div"     -> Some ("Div", 2)
  | "relu"    -> Some ("Relu", 1)
  | "sigmoid" -> Some ("Sigmoid", 1)
  | "tanh"    -> Some ("Tanh", 1)
  | "neg"     -> Some ("Neg", 1)
  | "abs"     -> Some ("Abs", 1)
  | "exp"     -> Some ("Exp", 1)
  | "log"     -> Some ("Log", 1)
  | "sqrt"    -> Some ("Sqrt", 1)
  | "identity" -> Some ("Identity", 1)
  | _         -> None

(* ============================================================================
   Type validation

   Graph inputs and outputs must be [Array[Float]]. We accept the AffineScript
   surface forms [Array[Float]], [ref Array[Float]], [mut Array[Float]].
   ============================================================================ *)

let rec strip_ownership = function
  | TyOwn t | TyRef t | TyMut t -> strip_ownership t
  | t -> t

let is_array_float (te : type_expr) : bool =
  match strip_ownership te with
  | TyApp (id, [TyArg (TyCon e)]) when id.name = "Array" && e.name = "Float" -> true
  | _ -> false

(* ============================================================================
   Build the ONNX graph from an AffineScript function body

   We lower a chain of [let v = call(...)] bindings into a list of ONNX
   nodes. The graph inputs are the function parameters, the output is the
   final expression (which must be a variable reference).
   ============================================================================ *)

type build_state = {
  mutable nodes      : Onnx_proto.node list;
  mutable next_id    : int;
}

let fresh_node_name (st : build_state) (op : string) : string =
  let id = st.next_id in
  st.next_id <- id + 1;
  Printf.sprintf "%s_%d" op id

(** Lower a value-producing expression into either a variable name (already
    in scope) or a new node whose output we name and return. *)
let rec lower_expr (st : build_state) (e : expr) : string =
  match e with
  | ExprVar id -> id.name
  | ExprSpan (inner, _) -> lower_expr st inner
  | ExprApp (callee, args) ->
      let fn_name = match callee with
        | ExprVar id -> id.name
        | _ -> unsupported "indirect calls not supported in ONNX backend"
      in
      let (op_type, expected_arity) = match recognise_op fn_name with
        | Some pair -> pair
        | None -> unsupported ("unknown ONNX op (no name match): " ^ fn_name)
      in
      let actual = List.length args in
      if actual <> expected_arity then
        unsupported
          (Printf.sprintf "%s expects %d args, got %d" op_type expected_arity actual);
      let arg_names = List.map (lower_expr st) args in
      let out_name = fresh_node_name st op_type in
      let node = {
        Onnx_proto.n_input  = arg_names;
        n_output = [out_name];
        n_name   = out_name;
        n_op_type = op_type;
        n_domain  = "";
      } in
      st.nodes <- node :: st.nodes;
      out_name
  | ExprLet { el_pat; el_value; el_body; el_mut = _; el_quantity = _; el_ty = _ } ->
      (* Ignore the binder for the SSA-style lowering: each call already
         produces a uniquely-named output. We propagate the *output of the
         RHS* to the body, threading variables through scope by their
         user-visible name. *)
      let rhs_name = lower_expr st el_value in
      let var = match el_pat with
        | PatVar id -> id.name
        | _ -> unsupported "destructuring let not supported in ONNX backend"
      in
      (* Rename the latest node's output to the bound name so the user's
         identifier survives into the graph. Only safe when the RHS produced
         a fresh node (not a passthrough variable). *)
      (match st.nodes with
       | latest :: rest when latest.n_output = [rhs_name] ->
           let renamed = { latest with
             Onnx_proto.n_output = [var];
             n_name   = var;
           } in
           st.nodes <- renamed :: rest
       | _ ->
           (* RHS was a variable — emit an Identity node so the alias is
              real in the graph. *)
           let node = {
             Onnx_proto.n_input  = [rhs_name];
             n_output = [var];
             n_name   = var;
             n_op_type = "Identity";
             n_domain  = "";
           } in
           st.nodes <- node :: st.nodes);
      (match el_body with
       | Some body -> lower_expr st body
       | None      -> unsupported "let without body cannot produce graph output")
  | ExprBlock blk ->
      List.fold_left (fun _last s ->
        match s with
        | StmtLet { sl_pat = PatVar id; sl_value; _ } ->
            let rhs_name = lower_expr st sl_value in
            (match st.nodes with
             | latest :: rest when latest.n_output = [rhs_name] ->
                 st.nodes <- { latest with
                   Onnx_proto.n_output = [id.name]; n_name = id.name } :: rest
             | _ ->
                 st.nodes <- {
                   Onnx_proto.n_input = [rhs_name]; n_output = [id.name];
                   n_name = id.name; n_op_type = "Identity"; n_domain = "";
                 } :: st.nodes);
            id.name
        | StmtExpr e -> lower_expr st e
        | _ -> unsupported "only let-bindings and trailing expressions allowed in ONNX block"
      ) "" blk.blk_stmts
      |> ignore;
      (match blk.blk_expr with
       | Some e -> lower_expr st e
       | None   -> unsupported "block must end with an expression naming the output")
  | ExprLit _ ->
      unsupported "literal not supported as ONNX value (need Constant op + initializer)"
  | _ ->
      unsupported "expression form not supported in ONNX kernel"

(* ============================================================================
   Driver
   ============================================================================ *)

let pick_entry (program : program) : fn_decl =
  let fns = List.filter_map (function TopFn fd -> Some fd | _ -> None)
              program.prog_decls in
  let by_name n = List.find_opt (fun fd -> fd.fd_name.name = n) fns in
  match by_name "graph" with
  | Some fd -> fd
  | None ->
      match by_name "main" with
      | Some fd -> fd
      | None ->
          match fns with
          | fd :: _ -> fd
          | [] -> unsupported "no function found to lower as ONNX graph"

let validate_entry (fd : fn_decl) : unit =
  List.iter (fun (p : param) ->
    if not (is_array_float p.p_ty) then
      unsupported
        (Printf.sprintf "parameter %s must be Array[Float]" p.p_name.name)
  ) fd.fd_params;
  match fd.fd_ret_ty with
  | Some t when is_array_float t -> ()
  | None -> unsupported "graph function must declare a return type"
  | _ -> unsupported "graph function must return Array[Float]"

(** Produce a ValueInfoProto for a parameter or result name. We use a single
    dynamic dimension named [N] so consumers can pass any-length tensors. *)
let value_info_for (name : string) : Onnx_proto.value_info = {
  vi_name = name;
  vi_type = Onnx_proto.TensorType {
    elem_type = 1;  (* FLOAT *)
    shape     = [Onnx_proto.DimParam "N"];
  };
}

let generate (program : program) (_symbols : Symbol.t) : string =
  let entry = pick_entry program in
  validate_entry entry;
  let st = { nodes = []; next_id = 0 } in
  let output_name = match entry.fd_body with
    | FnExpr e   -> lower_expr st e
    | FnBlock b  -> lower_expr st (ExprBlock b)
  in
  let inputs = List.map (fun (p : param) -> value_info_for p.p_name.name)
                 entry.fd_params in
  let outputs = [value_info_for output_name] in
  let graph = {
    Onnx_proto.g_node   = List.rev st.nodes;
    g_name   = entry.fd_name.name;
    g_input  = inputs;
    g_output = outputs;
  } in
  let model = {
    Onnx_proto.m_ir_version       = 7;   (* ONNX 1.10+ *)
    m_producer_name    = "affinescript";
    m_producer_version = "0.1.0";
    m_opset_import     = [{ op_domain = ""; op_version = 13 }];
    m_graph            = graph;
  } in
  Onnx_proto.serialize_model model

let codegen_onnx (program : program) (symbols : Symbol.t) : (string, string) result =
  try Ok (generate program symbols)
  with
  | Onnx_unsupported msg -> Error ("ONNX backend: " ^ msg)
  | Failure msg          -> Error ("ONNX codegen error: " ^ msg)
  | e                    -> Error ("ONNX codegen error: " ^ Printexc.to_string e)
