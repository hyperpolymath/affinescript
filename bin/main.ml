(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2025 hyperpolymath *)

(** AffineScript compiler CLI *)

let () = Fmt_tty.setup_std_outputs ()

(** Version string *)
let version = "0.1.0"

(** Read file contents *)
let read_file path =
  let ic = open_in_bin path in
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

(** Check a file *)
let check_file path =
  let source = read_file path in
  let _ = source in
  (* TODO: Implement type checker *)
  Format.printf "Type checking not yet implemented@.";
  `Ok ()

(** Evaluate a file with the interpreter *)
let eval_file path =
  try
    (* Parse the file *)
    let prog = Affinescript.Parse_driver.parse_file path in

    (* Create module loader *)
    let loader_config = Affinescript.Module_loader.default_config () in
    let loader = Affinescript.Module_loader.create loader_config in

    (* Resolve names with module loading *)
    (match Affinescript.Resolve.resolve_program_with_loader prog loader with
    | Error (e, _span) ->
      Format.eprintf "@[<v>Resolution error: %s@]@."
        (Affinescript.Resolve.show_resolve_error e);
      `Error (false, "Resolution error")
    | Ok (resolve_ctx, type_ctx) ->
      (* Type check remaining declarations *)
      let type_ctx = { type_ctx with symbols = resolve_ctx.symbols } in
      (match List.fold_left (fun acc decl ->
        match acc with
        | Error _ as e -> e
        | Ok () -> Affinescript.Typecheck.check_decl type_ctx decl
      ) (Ok ()) prog.prog_decls with
      | Error e ->
        Format.eprintf "@[<v>Type error: %s@]@."
          (Affinescript.Typecheck.show_type_error e);
        `Error (false, "Type error")
      | Ok () ->
        (* Borrow check *)
        (match Affinescript.Borrow.check_program resolve_ctx.symbols prog with
        | Error e ->
          Format.eprintf "@[<v>Borrow check error: %s@]@."
            (Affinescript.Borrow.show_borrow_error e);
          `Error (false, "Borrow check error")
        | Ok () ->
          (* Evaluate *)
          (match Affinescript.Interp.eval_program prog with
          | Ok _env ->
            Format.printf "Program executed successfully@.";
            `Ok ()
          | Error e ->
            Format.eprintf "@[<v>Runtime error: %s@]@."
              (Affinescript.Value.show_eval_error e);
            `Error (false, "Runtime error")))))
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
      Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
      `Error (false, "Lexer error")
  | Affinescript.Parse_driver.Parse_error (msg, span) ->
      Format.eprintf "@[<v>%a: parse error: %s@]@."
        Affinescript.Span.pp_short span msg;
      `Error (false, "Parse error")

(** Start the REPL *)
let repl_cmd_fn () =
  Affinescript.Repl.start ();
  `Ok ()

(** Compile a file *)
let compile_file path output =
  try
    (* Parse the file *)
    let prog = Affinescript.Parse_driver.parse_file path in

    (* Create module loader *)
    let loader_config = Affinescript.Module_loader.default_config () in
    let loader = Affinescript.Module_loader.create loader_config in

    (* Resolve names with module loading *)
    (match Affinescript.Resolve.resolve_program_with_loader prog loader with
    | Error (e, _span) ->
      Format.eprintf "@[<v>Resolution error: %s@]@."
        (Affinescript.Resolve.show_resolve_error e);
      `Error (false, "Resolution error")
    | Ok (_resolve_ctx, _type_ctx) ->
      (* Generate WASM *)
      (match Affinescript.Codegen.generate_module prog with
      | Error e ->
        Format.eprintf "@[<v>Code generation error: %s@]@."
          (Affinescript.Codegen.show_codegen_error e);
        `Error (false, "Code generation error")
      | Ok wasm_module ->
        (* Write WASM to file *)
        Affinescript.Wasm_encode.write_module_to_file output wasm_module;
        Format.printf "Compiled %s -> %s@." path output;
        `Ok ()))
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
      Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
      `Error (false, "Lexer error")
  | Affinescript.Parse_driver.Parse_error (msg, span) ->
      Format.eprintf "@[<v>%a: parse error: %s@]@."
        Affinescript.Span.pp_short span msg;
      `Error (false, "Parse error")

(** CLI commands *)
open Cmdliner

let path_arg =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:"Input file")

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

let check_cmd =
  let doc = "Type check a file" in
  let info = Cmd.info "check" ~doc in
  Cmd.v info Term.(ret (const check_file $ path_arg))

let eval_cmd =
  let doc = "Evaluate a file with the interpreter" in
  let info = Cmd.info "eval" ~doc in
  Cmd.v info Term.(ret (const eval_file $ path_arg))

let repl_cmd =
  let doc = "Start the interactive REPL" in
  let info = Cmd.info "repl" ~doc in
  Cmd.v info Term.(ret (const repl_cmd_fn $ const ()))

let compile_cmd =
  let doc = "Compile a file to WebAssembly" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(ret (const compile_file $ path_arg $ output_arg))

let default_cmd =
  let doc = "The AffineScript compiler" in
  let info = Cmd.info "affinescript" ~version ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [lex_cmd; parse_cmd; check_cmd; eval_cmd; repl_cmd; compile_cmd]

let () = exit (Cmd.eval default_cmd)
