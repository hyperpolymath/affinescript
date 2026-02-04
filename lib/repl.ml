(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Read-Eval-Print Loop for AffineScript.

    This module provides an interactive REPL for experimenting
    with AffineScript expressions and declarations.
*)

open Ast
open Value
open Interp
open Types

(** REPL state *)
type state = {
  env : env;                    (** Runtime environment *)
  symbols : Symbol.t;           (** Symbol table for name resolution *)
  type_ctx : Typecheck.context; (** Type checking context *)
}

(** Extract variable names from a pattern *)
let rec pattern_vars (pat : pattern) : string list =
  match pat with
  | PatWildcard _ -> []
  | PatVar id -> [id.name]
  | PatLit _ -> []
  | PatTuple pats -> List.concat_map pattern_vars pats
  | PatRecord (fields, _) ->
    List.concat_map (fun (id, pat_opt) ->
      match pat_opt with
      | Some p -> pattern_vars p
      | None -> [id.name]
    ) fields
  | PatCon (_, pats) -> List.concat_map pattern_vars pats
  | PatOr (p1, p2) -> pattern_vars p1 @ pattern_vars p2
  | PatAs (id, pat) -> id.name :: pattern_vars pat

(** Register symbols for top-level let bindings *)
let register_let_bindings (symbols : Symbol.t) (pat : pattern) : unit =
  let vars = pattern_vars pat in
  List.iter (fun name ->
    let _ = Symbol.define symbols name Symbol.SKVariable Span.dummy Private in
    ()
  ) vars

(** Bind pattern variables to types in the type context *)
let rec bind_pattern_to_ctx (ctx : Typecheck.context) (pat : pattern) (scheme : Types.scheme) : unit =
  match pat with
  | PatVar id ->
    (* Use Typecheck's bind_var_scheme to add the binding *)
    begin match Symbol.lookup ctx.symbols id.name with
      | Some sym ->
        Hashtbl.replace ctx.var_types sym.sym_id scheme
      | None -> ()
    end
  | PatWildcard _ | PatLit _ -> ()
  | PatTuple pats ->
    begin match scheme.sc_body with
      | Types.TTuple tys when List.length pats = List.length tys ->
        List.iter2 (fun pat ty ->
          let sc = { scheme with sc_body = ty } in
          bind_pattern_to_ctx ctx pat sc
        ) pats tys
      | _ -> ()
    end
  | PatRecord (fields, _) ->
    List.iter (fun (id, pat_opt) ->
      match pat_opt with
      | Some p -> bind_pattern_to_ctx ctx p scheme
      | None ->
        begin match Symbol.lookup ctx.symbols id.name with
          | Some sym ->
            Hashtbl.replace ctx.var_types sym.sym_id scheme
          | None -> ()
        end
    ) fields
  | PatCon (_, pats) ->
    List.iter (fun pat ->
      bind_pattern_to_ctx ctx pat scheme
    ) pats
  | PatOr (p1, p2) ->
    bind_pattern_to_ctx ctx p1 scheme;
    bind_pattern_to_ctx ctx p2 scheme
  | PatAs (id, pat) ->
    begin match Symbol.lookup ctx.symbols id.name with
      | Some sym ->
        Hashtbl.replace ctx.var_types sym.sym_id scheme
      | None -> ()
    end;
    bind_pattern_to_ctx ctx pat scheme

(** Create initial REPL state *)
let create_state () : state =
  let symbols = Symbol.create () in
  let type_ctx = Typecheck.create_context symbols in

  (* Register builtins in symbol table with types *)
  let register_builtin name ty =
    let sym = Symbol.define symbols name Symbol.SKFunction Span.dummy Public in
    let scheme = { sc_tyvars = []; sc_effvars = []; sc_rowvars = []; sc_body = ty } in
    Hashtbl.replace type_ctx.var_types sym.sym_id scheme
  in

  (* print : String -> () *)
  let print_ty = TArrow (ty_string, ty_unit, EPure) in
  register_builtin "print" print_ty;

  (* println : String -> () *)
  let println_ty = TArrow (ty_string, ty_unit, EPure) in
  register_builtin "println" println_ty;

  (* len : 'a -> Int (polymorphic, works for arrays and strings) *)
  let alpha_var = TVar (ref (Unbound (0, 0))) in
  let len_ty = TArrow (alpha_var, ty_int, EPure) in
  register_builtin "len" len_ty;

  {
    env = create_initial_env ();
    symbols;
    type_ctx;
  }

(** Process a single line of input *)
let process_line (state : state) (input : string) : (state * string) =
  let resolve_ctx = {
    Resolve.symbols = state.symbols;
    current_module = [];
    imports = [];
  } in
  try
    (* Try parsing as expression first *)
    let expr = Parse_driver.parse_expr ~file:"<repl>" input in
    (* Resolve names *)
    match Resolve.resolve_expr resolve_ctx expr with
    | Ok () ->
      (* Type check *)
      begin match Typecheck.synth state.type_ctx expr with
        | Ok (ty, _eff) ->
          (* Special handling for top-level let expressions *)
          begin match expr with
            | ExprLet lb when lb.el_body = None ->
              (* Top-level let binding - first type-check the RHS *)
              begin match Typecheck.synth state.type_ctx lb.el_value with
                | Ok (rhs_ty, _rhs_eff) ->
                  (* Evaluate the RHS *)
                  begin match eval state.env lb.el_value with
                    | Ok rhs_val ->
                      begin match match_pattern lb.el_pat rhs_val with
                        | Ok bindings ->
                          (* Register symbols *)
                          register_let_bindings state.symbols lb.el_pat;
                          (* Add type bindings to context *)
                          let scheme = Typecheck.generalize state.type_ctx rhs_ty in
                          let _ = bind_pattern_to_ctx state.type_ctx lb.el_pat scheme in
                          (* Update environment *)
                          let state' = { state with env = extend_env_list bindings state.env } in
                          (state', Printf.sprintf "() : Unit")
                        | Error e ->
                          let msg = Printf.sprintf "Pattern match error: %s"
                            (show_eval_error e) in
                          (state, msg)
                      end
                    | Error e ->
                      let msg = Printf.sprintf "Runtime error: %s"
                        (show_eval_error e) in
                      (state, msg)
                  end
                | Error e ->
                  let msg = Printf.sprintf "Type error: %s"
                    (Typecheck.show_type_error e) in
                  (state, msg)
              end
            | _ ->
              (* Regular expression - evaluate and display result *)
              begin match eval state.env expr with
                | Ok value ->
                  let result = Printf.sprintf "%s : %s"
                    (Value.show_value value)
                    (ty_to_string ty) in
                  (state, result)
                | Error e ->
                  let msg = Printf.sprintf "Runtime error: %s"
                    (show_eval_error e) in
                  (state, msg)
              end
          end
        | Error e ->
          let msg = Printf.sprintf "Type error: %s"
            (Typecheck.show_type_error e) in
          (state, msg)
      end
    | Error (e, _span) ->
      let msg = Printf.sprintf "Resolution error: %s"
        (Resolve.show_resolve_error e) in
      (state, msg)
  with
  | Parse_driver.Parse_error _ | Lexer.Lexer_error _ ->
      (* Try parsing as declaration *)
      try
        let prog = Parse_driver.parse_string ~file:"<repl>" input in
        begin match prog.prog_decls with
        | [decl] ->
          (* Resolve names *)
          begin match Resolve.resolve_decl resolve_ctx decl with
          | Ok () ->
            (* Type check *)
            begin match Typecheck.check_decl state.type_ctx decl with
              | Ok () ->
                (* Evaluate *)
                begin match eval_decl state.env decl with
                  | Ok env' ->
                    let state' = { state with env = env' } in
                    let name = match decl with
                      | TopFn fd -> fd.fd_name.name
                      | TopConst tc -> tc.tc_name.name
                      | TopType td -> td.td_name.name
                      | TopEffect ed -> ed.ed_name.name
                      | TopTrait td -> td.trd_name.name
                      | TopImpl _ -> "<impl>"
                    in
                    (state', Printf.sprintf "Defined %s" name)
                  | Error e ->
                    let msg = Printf.sprintf "Runtime error: %s"
                      (show_eval_error e) in
                    (state, msg)
                end
              | Error e ->
                let msg = Printf.sprintf "Type error: %s"
                  (Typecheck.show_type_error e) in
                (state, msg)
            end
          | Error (e, _span) ->
            let msg = Printf.sprintf "Resolution error: %s"
              (Resolve.show_resolve_error e) in
            (state, msg)
          end
        | _ ->
          (state, "Error: Expected single declaration")
        end
  with
  | Parse_driver.Parse_error _ | Lexer.Lexer_error _ ->
      (* Try parsing as declaration *)
      try
        let prog = Parse_driver.parse_string ~file:"<repl>" input in
        begin match prog.prog_decls with
        | [decl] ->
          (* Resolve names *)
          begin match Resolve.resolve_decl resolve_ctx decl with
          | Ok () ->
            (* Type check *)
            begin match Typecheck.check_decl state.type_ctx decl with
              | Ok () ->
                (* Evaluate *)
                begin match eval_decl state.env decl with
                  | Ok env' ->
                    let state' = { state with env = env' } in
                    let name = match decl with
                      | TopFn fd -> fd.fd_name.name
                      | TopConst tc -> tc.tc_name.name
                      | TopType td -> td.td_name.name
                      | TopEffect ed -> ed.ed_name.name
                      | TopTrait td -> td.trd_name.name
                      | TopImpl _ -> "<impl>"
                    in
                    (state', Printf.sprintf "Defined %s" name)
                  | Error e ->
                    let msg = Printf.sprintf "Runtime error: %s"
                      (show_eval_error e) in
                    (state, msg)
                end
              | Error e ->
                let msg = Printf.sprintf "Type error: %s"
                  (Typecheck.show_type_error e) in
                (state, msg)
            end
          | Error (e, _span) ->
            let msg = Printf.sprintf "Resolution error: %s"
              (Resolve.show_resolve_error e) in
            (state, msg)
          end
        | _ ->
          (state, "Error: Expected single declaration")
        end
      with
      | Parse_driver.Parse_error (msg, _) ->
        (state, Printf.sprintf "Parse error: %s" msg)
      | Lexer.Lexer_error (msg, _) ->
        (state, Printf.sprintf "Lexer error: %s" msg)
      | exn ->
        (state, Printf.sprintf "Unexpected error: %s" (Printexc.to_string exn))

(** Print the REPL prompt *)
let print_prompt () =
  print_string ">>> ";
  flush stdout

(** Print the REPL banner *)
let print_banner () =
  print_endline "AffineScript REPL v0.1.0";
  print_endline "Type :help for help, :quit to exit";
  print_endline ""

(** Handle REPL command *)
let handle_command (state : state) (cmd : string) : (state * bool) =
  match cmd with
  | ":quit" | ":q" | ":exit" ->
    print_endline "Goodbye!";
    (state, true)  (* exit = true *)

  | ":help" | ":h" ->
    print_endline "Available commands:";
    print_endline "  :help, :h         - Show this help message";
    print_endline "  :quit, :q, :exit  - Exit the REPL";
    print_endline "  :env              - Show current environment";
    print_endline "  :clear            - Clear environment";
    print_endline "";
    print_endline "Enter expressions or declarations to evaluate them.";
    (state, false)

  | ":env" ->
    print_endline "Current environment:";
    List.iter (fun (name, value) ->
      Printf.printf "  %s = %s\n" name (Value.show_value value)
    ) state.env;
    (state, false)

  | ":clear" ->
    print_endline "Environment cleared.";
    (create_state (), false)

  | _ ->
    Printf.printf "Unknown command: %s\n" cmd;
    print_endline "Type :help for available commands.";
    (state, false)

(** Run the REPL *)
let rec run_loop (state : state) : unit =
  print_prompt ();
  match read_line () with
  | exception End_of_file ->
    print_endline "\nGoodbye!";
    ()
  | "" ->
    run_loop state
  | line when String.length line > 0 && line.[0] = ':' ->
    let (state', should_exit) = handle_command state line in
    if should_exit then ()
    else run_loop state'
  | line ->
    let (state', result) = process_line state line in
    print_endline result;
    run_loop state'

(** Start the REPL *)
let start () : unit =
  print_banner ();
  let state = create_state () in
  run_loop state
