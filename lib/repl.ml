(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** Read-Eval-Print Loop for AffineScript.

    This module provides an interactive REPL for experimenting
    with AffineScript expressions and declarations.
*)

open Value
open Interp

(** REPL state *)
type state = {
  env : env;                    (** Runtime environment *)
  symbols : Symbol.t;           (** Symbol table for name resolution *)
  type_ctx : Typecheck.context; (** Type checking context *)
}

(** Create initial REPL state *)
let create_state () : state =
  let symbols = Symbol.create () in
  {
    env = create_initial_env ();
    symbols;
    type_ctx = Typecheck.create_context symbols;
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
          (* Evaluate *)
          begin match eval state.env expr with
            | Ok value ->
              let result = Printf.sprintf "%s : %s"
                (Value.show_value value)
                (Types.ty_to_string ty) in
              (state, result)
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
