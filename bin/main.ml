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
  let source = read_file path in
  let _ = source in
  (* TODO: Implement parser *)
  Format.printf "Parsing not yet implemented@.";
  `Ok ()

(** Check a file *)
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

let compile_cmd =
  let doc = "Compile a file to WebAssembly" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(ret (const compile_file $ path_arg $ output_arg))

let default_cmd =
  let doc = "The AffineScript compiler" in
  let info = Cmd.info "affinescript" ~version ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [lex_cmd; parse_cmd; check_cmd; compile_cmd]

let () = exit (Cmd.eval default_cmd)
