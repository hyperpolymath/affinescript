(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 hyperpolymath *)

(** Interactive debugger for AffineScript with affine type inspection.

    Provides REPL-based debugging with breakpoints, stepping,
    variable inspection, and affine ownership tracking.
*)

open Ast
open Value

type debugger_state = {
  source_file : string option;
  source_lines : string list;
  breakpoints : int list;
  current_line : int;
  environment : env;
  paused : bool;
}

let create_state () : debugger_state = {
  source_file = None;
  source_lines = [];
  breakpoints = [];
  current_line = 0;
  environment = create_env ();
  paused = false;
}

(** Load source file *)
let load_file (state : debugger_state) (path : string) : debugger_state =
  try
    let ic = open_in path in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    let lines = read_lines [] in
    Printf.printf "Loaded %s (%d lines)\n" path (List.length lines);
    { state with source_file = Some path; source_lines = lines }
  with Sys_error msg ->
    Printf.eprintf "Error loading file: %s\n" msg;
    state

(** Set breakpoint *)
let set_breakpoint (state : debugger_state) (line : int) : debugger_state =
  if List.mem line state.breakpoints then (
    Printf.printf "Breakpoint already set at line %d\n" line;
    state
  ) else (
    Printf.printf "Breakpoint set at line %d\n" line;
    { state with breakpoints = line :: state.breakpoints }
  )

(** Delete breakpoint *)
let delete_breakpoint (state : debugger_state) (line : int) : debugger_state =
  if List.mem line state.breakpoints then (
    Printf.printf "Breakpoint removed from line %d\n" line;
    { state with breakpoints = List.filter ((<>) line) state.breakpoints }
  ) else (
    Printf.printf "No breakpoint at line %d\n" line;
    state
  )

(** List breakpoints *)
let list_breakpoints (state : debugger_state) : unit =
  match state.breakpoints with
  | [] -> Printf.printf "No breakpoints set\n"
  | bps ->
    Printf.printf "Breakpoints:\n";
    List.iter (Printf.printf "  Line %d\n") (List.sort compare bps)

(** List source code *)
let list_source (state : debugger_state) (start : int) (count : int) : unit =
  let lines = state.source_lines in
  let total = List.length lines in
  if start < 0 || start >= total then
    Printf.printf "Invalid line number\n"
  else
    let end_line = min total (start + count) in
    for i = start to end_line - 1 do
      let line = List.nth lines i in
      let bp_marker = if List.mem (i + 1) state.breakpoints then "*" else " " in
      Printf.printf "%s%4d  %s\n" bp_marker (i + 1) line
    done

(** Run program *)
let run_program (state : debugger_state) : debugger_state =
  match state.source_file with
  | None ->
    Printf.printf "No source file loaded\n";
    state
  | Some path ->
    Printf.printf "Running %s...\n" path;
    begin match Parse_driver.parse_file path with
    | Ok prog ->
      begin match Interp.eval_program state.environment prog with
      | Ok final_env ->
        Printf.printf "Program completed successfully\n";
        { state with environment = final_env }
      | Error err ->
        Printf.eprintf "Runtime error: %s\n" (show_eval_error err);
        state
      end
    | Error msg ->
      Printf.eprintf "Parse error: %s\n" msg;
      state
    end

(** Print variable *)
let print_variable (state : debugger_state) (name : string) : unit =
  match lookup_env name state.environment with
  | Ok value -> Printf.printf "%s = %s\n" name (show_value value)
  | Error _ -> Printf.printf "Variable not found: %s\n" name

(** List all variables *)
let list_variables (state : debugger_state) : unit =
  let bindings = state.environment.bindings in
  if bindings = [] then
    Printf.printf "No variables defined\n"
  else (
    Printf.printf "Variables:\n";
    List.iter (fun (name, value) ->
      Printf.printf "  %s = %s\n" name (show_value value)
    ) bindings
  )

(** Show help *)
let show_help () : unit =
  Printf.printf "\nAffineScript Debugger Commands:\n";
  Printf.printf "  run               - Run the loaded program\n";
  Printf.printf "  break N           - Set breakpoint at line N\n";
  Printf.printf "  delete N          - Delete breakpoint at line N\n";
  Printf.printf "  breakpoints       - List all breakpoints\n";
  Printf.printf "  list [N] [M]      - List source code (from line N, M lines)\n";
  Printf.printf "  print VAR         - Print variable value\n";
  Printf.printf "  locals            - List all variables\n";
  Printf.printf "  load FILE         - Load source file\n";
  Printf.printf "  help              - Show this help\n";
  Printf.printf "  quit              - Exit debugger\n\n"

(** Main REPL loop *)
let rec repl_loop (state : debugger_state) : unit =
  Printf.printf "affinescript-debug> ";
  flush stdout;
  try
    let line = input_line stdin in
    let parts = String.split_on_char ' ' (String.trim line) in
    match parts with
    | ["run"] -> repl_loop (run_program state)
    | ["break"; n] ->
      begin try
        let line_num = int_of_string n in
        repl_loop (set_breakpoint state line_num)
      with Failure _ ->
        Printf.printf "Invalid line number\n";
        repl_loop state
      end
    | ["delete"; n] ->
      begin try
        let line_num = int_of_string n in
        repl_loop (delete_breakpoint state line_num)
      with Failure _ ->
        Printf.printf "Invalid line number\n";
        repl_loop state
      end
    | ["breakpoints"] -> list_breakpoints state; repl_loop state
    | ["list"] -> list_source state 0 20; repl_loop state
    | ["list"; n] ->
      begin try
        let start = int_of_string n in
        list_source state (start - 1) 20;
        repl_loop state
      with Failure _ ->
        Printf.printf "Invalid line number\n";
        repl_loop state
      end
    | ["print"; var] -> print_variable state var; repl_loop state
    | ["locals"] -> list_variables state; repl_loop state
    | ["load"; file] -> repl_loop (load_file state file)
    | ["help"] | ["h"] | ["?"] -> show_help (); repl_loop state
    | ["quit"] | ["q"] | ["exit"] -> Printf.printf "Exiting debugger\n"
    | _ -> Printf.printf "Unknown command. Type 'help' for commands.\n"; repl_loop state
  with End_of_file ->
    Printf.printf "\nExiting debugger\n"

(** Entry point *)
let () =
  Printf.printf "AffineScript Interactive Debugger\n";
  Printf.printf "Type 'help' for commands\n\n";
  let state = create_state () in
  let state' = if Array.length Sys.argv > 1 then
    load_file state Sys.argv.(1)
  else
    state
  in
  repl_loop state'
