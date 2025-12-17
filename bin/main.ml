(** AffineScript compiler CLI *)

let () = Fmt_tty.setup_std_outputs ()

(** Version string *)
let version = "0.1.0"

(** Read file contents *)
let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(** Lex a file and print tokens *)
let lex_file path =
  let source = read_file path in
  let lexer = Affinescript.Lexer.from_string ~file:path source in
  let rec loop () =
    let (tok, span) = lexer () in
    Format.printf "%a: %s@."
      Affinescript.Span.pp_short span
      (Affinescript.Token.to_string tok);
    if tok <> Affinescript.Token.EOF then loop ()
  in
  try
    loop ();
    `Ok ()
  with Affinescript.Lexer.Lexer_error (msg, pos) ->
    Format.eprintf "@[<v>%s:%d:%d: error: %s@]@." path pos.line pos.col msg;
    `Error (false, "Lexer error")

(** Parse a file and print AST *)
let parse_file path =
  try
    let prog = Affinescript.Parse_driver.parse_file path in
    Format.printf "%s@." (Affinescript.Ast.show_program prog);
    `Ok ()
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
      Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
      `Error (false, "Lexer error")
  | Affinescript.Parse_driver.Parse_error (msg, span) ->
      Format.eprintf "@[<v>%a: parse error: %s@]@."
        Affinescript.Span.pp_short span msg;
      `Error (false, "Parse error")

(** Run a file through the interpreter *)
let run_file path =
  try
    let prog = Affinescript.Parse_driver.parse_file path in
    let env = Affinescript.Value.empty_env () in
    (* Load stdlib prelude *)
    Affinescript.Stdlib.load_prelude env;
    Affinescript.Eval.eval_program env prog;
    `Ok ()
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
      Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.Affinescript.Span.line pos.Affinescript.Span.col msg;
      `Error (false, "Lexer error")
  | Affinescript.Parse_driver.Parse_error (msg, span) ->
      Format.eprintf "@[<v>%a: parse error: %s@]@."
        Affinescript.Span.pp_short span msg;
      `Error (false, "Parse error")
  | Affinescript.Eval.Runtime_error (msg, span_opt) ->
      (match span_opt with
       | Some span -> Format.eprintf "@[<v>%a: runtime error: %s@]@." Affinescript.Span.pp_short span msg
       | None -> Format.eprintf "@[<v>runtime error: %s@]@." msg);
      `Error (false, "Runtime error")
  | Failure msg ->
      Format.eprintf "@[<v>error: %s@]@." msg;
      `Error (false, "Error")

(** Evaluate an expression from command line *)
let eval_expr expr_str =
  let env = Affinescript.Value.empty_env () in
  Affinescript.Stdlib.load_prelude env;
  match Affinescript.Repl.eval_string ~env expr_str with
  | Ok v -> Format.printf "%s@." (Affinescript.Value.show v); `Ok ()
  | Error msg -> Format.eprintf "Error: %s@." msg; `Error (false, msg)

(** Start the REPL *)
let repl_run file_opt =
  match file_opt with
  | Some file -> Affinescript.Repl.run_with_file file; `Ok ()
  | None -> Affinescript.Repl.run (); `Ok ()

(** Check a file (type check - placeholder) *)
let check_file path =
  let source = read_file path in
  let _ = source in
  (* TODO: Implement type checker *)
  Format.printf "Type checking not yet implemented@.";
  `Ok ()

(** Compile a file *)
let compile_file path output =
  let source = read_file path in
  let _ = (source, output) in
  (* TODO: Implement compiler *)
  Format.printf "Compilation not yet implemented@.";
  `Ok ()

(** CLI commands *)
open Cmdliner

let path_arg =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:"Input file")

let optional_path_arg =
  Arg.(value & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:"Input file to load")

let expr_arg =
  Arg.(required & pos 0 (some string) None & info [] ~docv:"EXPR" ~doc:"Expression to evaluate")

let output_arg =
  Arg.(value & opt string "out.wasm" & info ["o"; "output"] ~docv:"FILE" ~doc:"Output file")

let lex_cmd =
  let doc = "Lex a file and print tokens" in
  let info = Cmd.info "lex" ~doc in
  Cmd.v info Term.(ret (const lex_file $ path_arg))

let parse_cmd =
  let doc = "Parse a file and print AST" in
  let info = Cmd.info "parse" ~doc in
  Cmd.v info Term.(ret (const parse_file $ path_arg))

let run_cmd =
  let doc = "Run a file through the interpreter" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(ret (const run_file $ path_arg))

let eval_cmd =
  let doc = "Evaluate an expression" in
  let info = Cmd.info "eval" ~doc in
  Cmd.v info Term.(ret (const eval_expr $ expr_arg))

let repl_cmd =
  let doc = "Start the interactive REPL" in
  let info = Cmd.info "repl" ~doc in
  Cmd.v info Term.(ret (const repl_run $ optional_path_arg))

let check_cmd =
  let doc = "Type check a file" in
  let info = Cmd.info "check" ~doc in
  Cmd.v info Term.(ret (const check_file $ path_arg))

let compile_cmd =
  let doc = "Compile a file to WebAssembly" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(ret (const compile_file $ path_arg $ output_arg))

let default_cmd =
  let doc = "The AffineScript compiler" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "AffineScript is a systems programming language with affine types, \
        dependent types, row polymorphism, and extensible effects.";
    `S Manpage.s_commands;
    `P "Use $(b,affinescript COMMAND --help) for help on a specific command.";
    `S Manpage.s_examples;
    `P "Start the REPL:";
    `Pre "  $(b,affinescript repl)";
    `P "Run a file:";
    `Pre "  $(b,affinescript run hello.afs)";
    `P "Evaluate an expression:";
    `Pre "  $(b,affinescript eval \"1 + 2 * 3\")";
  ] in
  let info = Cmd.info "affinescript" ~version ~doc ~sdocs ~man in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [
    repl_cmd; run_cmd; eval_cmd; lex_cmd; parse_cmd; check_cmd; compile_cmd
  ]

let () = exit (Cmd.eval default_cmd)
