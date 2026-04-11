(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** AffineScript compiler CLI

    Phase A of the LSP integration adds [--json] to [check], [lint],
    [compile], and [eval] commands.  When enabled, all diagnostics are
    emitted as a single JSON object on stderr (see {!Json_output} for
    the schema), and human-readable output is suppressed.  This is the
    contract the LSP (and any other tool) relies on for phases B-D. *)

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

(** {1 JSON-aware error helpers}

    Each helper collects diagnostics into a list ref when [json] is true,
    or prints the human-readable message and returns a Cmdliner error
    when [json] is false. *)

(** Emit a JSON report and return the appropriate Cmdliner result. *)
let json_finish diags =
  let success = List.for_all (fun (d : Affinescript.Json_output.diagnostic) ->
    match d.severity with
    | Affinescript.Json_output.Error -> false
    | _ -> true
  ) diags in
  Affinescript.Json_output.emit_report ~success diags;
  if success then `Ok () else `Error (false, "")

(** {1 Command implementations} *)

(** Lex a file and print tokens (no --json support — token streams are
    not diagnostics). *)
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

(** Parse a file using the requested face. *)
let parse_with_face face path =
  match face with
  | `Canonical -> Affinescript.Parse_driver.parse_file path
  | `Python    -> Affinescript.Python_face.parse_file_python path

(** Preview the Python-face text transform (debug tool). *)
let preview_python_transform path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  print_string (Affinescript.Python_face.preview_transform s);
  `Ok ()

(** Parse a file and print AST (no --json support). *)
let parse_file face path =
  try
    let prog = match face with
      | `Canonical -> Affinescript.Parse_driver.parse_file path
      | `Python    -> Affinescript.Python_face.parse_file_python path
    in
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

(** Type-check a file.  With [--json], emits a structured diagnostic
    report on stderr. *)
let check_file face json path =
  if json then begin
    let diags = ref [] in
    let add d = diags := d :: !diags in
    let symbols_table = ref None in
    let resolve_refs = ref [] in
    begin try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, span) ->
        add (Affinescript.Json_output.of_resolve_error e span)
      | Ok (resolve_ctx, _type_ctx) ->
        (* Phase B+C: capture symbol table and use-site references. *)
        symbols_table := Some resolve_ctx.symbols;
        resolve_refs := List.rev resolve_ctx.references;
        (match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
        | Error e ->
          add (Affinescript.Json_output.of_type_error e)
        | Ok _ctx -> ()))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
      add (Affinescript.Json_output.of_lexer_error msg pos path)
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
      add (Affinescript.Json_output.of_parse_error msg span)
    end;
    (* Emit v2 report with symbol table when available, v1 otherwise. *)
    let final_diags = List.rev !diags in
    (match !symbols_table with
    | Some symbols ->
      let success = List.for_all (fun (d : Affinescript.Json_output.diagnostic) ->
        match d.severity with Affinescript.Json_output.Error -> false | _ -> true
      ) final_diags in
      (* Convert resolve references to json_output references. *)
      let json_refs = List.map (fun (r : Affinescript.Resolve.reference) ->
        Affinescript.Json_output.{ ref_symbol_id = r.ref_symbol_id; ref_span = r.ref_span }
      ) !resolve_refs in
      Affinescript.Json_output.emit_report_v2 ~success final_diags symbols json_refs;
      if success then `Ok () else `Error (false, "")
    | None ->
      json_finish final_diags)
  end else begin
    try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, _span) ->
        Format.eprintf "@[<v>Resolution error: %s@]@."
          (Affinescript.Resolve.show_resolve_error e);
        `Error (false, "Resolution error")
      | Ok (resolve_ctx, _type_ctx) ->
        (match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
        | Error e ->
          Format.eprintf "@[<v>%s@]@."
            (Affinescript.Typecheck.format_type_error e);
          `Error (false, "Type error")
        | Ok _ctx ->
          Format.printf "Type checking passed@.";
          `Ok ()))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
        Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
        `Error (false, "Lexer error")
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
        Format.eprintf "@[<v>%a: parse error: %s@]@."
          Affinescript.Span.pp_short span msg;
        `Error (false, "Parse error")
  end

(** Evaluate a file with the interpreter.  With [--json], emits
    diagnostics on stderr instead of human-readable error text. *)
let eval_file face json path =
  if json then begin
    let diags = ref [] in
    let add d = diags := d :: !diags in
    begin try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, span) ->
        add (Affinescript.Json_output.of_resolve_error e span)
      | Ok (resolve_ctx, type_ctx) ->
        let type_ctx = { type_ctx with symbols = resolve_ctx.symbols } in
        (match List.fold_left (fun acc decl ->
          match acc with
          | Error _ as e -> e
          | Ok () -> Affinescript.Typecheck.check_decl type_ctx decl
        ) (Ok ()) prog.prog_decls with
        | Error e ->
          add (Affinescript.Json_output.of_type_error e)
        | Ok () ->
          (match Affinescript.Interp.eval_program prog with
          | Ok _env -> ()
          | Error e ->
            add (Affinescript.Json_output.of_eval_error e))))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
      add (Affinescript.Json_output.of_lexer_error msg pos path)
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
      add (Affinescript.Json_output.of_parse_error msg span)
    end;
    json_finish (List.rev !diags)
  end else begin
    try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, _span) ->
        Format.eprintf "@[<v>Resolution error: %s@]@."
          (Affinescript.Resolve.show_resolve_error e);
        `Error (false, "Resolution error")
      | Ok (resolve_ctx, type_ctx) ->
        let type_ctx = { type_ctx with symbols = resolve_ctx.symbols } in
        (match List.fold_left (fun acc decl ->
          match acc with
          | Error _ as e -> e
          | Ok () -> Affinescript.Typecheck.check_decl type_ctx decl
        ) (Ok ()) prog.prog_decls with
        | Error e ->
          Format.eprintf "@[<v>%s@]@."
            (Affinescript.Typecheck.format_type_error e);
          `Error (false, "Type error")
        | Ok () ->
          (match Affinescript.Interp.eval_program prog with
          | Ok _env ->
            Format.printf "Program executed successfully@.";
            `Ok ()
          | Error e ->
            Format.eprintf "@[<v>Runtime error: %s@]@."
              (Affinescript.Value.show_eval_error e);
            `Error (false, "Runtime error"))))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
        Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
        `Error (false, "Lexer error")
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
        Format.eprintf "@[<v>%a: parse error: %s@]@."
          Affinescript.Span.pp_short span msg;
        `Error (false, "Parse error")
  end

(** Start the REPL *)
let repl_cmd_fn () =
  (* TODO: Re-enable when REPL module is restored *)
  (* Affinescript.Repl.start (); *)
  Format.eprintf "REPL not yet implemented@.";
  `Error (false, "REPL not yet implemented")

(** Compile a file.  With [--json], emits diagnostics for any
    compilation errors.  With [--wasm-gc], targets the WebAssembly GC
    proposal instead of WASM 1.0 linear memory. *)
let compile_file face json wasm_gc path output =
  if json then begin
    let diags = ref [] in
    let add d = diags := d :: !diags in
    begin try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, span) ->
        add (Affinescript.Json_output.of_resolve_error e span)
      | Ok (resolve_ctx, _type_ctx) ->
        (match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
        | Error e ->
          add (Affinescript.Json_output.of_type_error e)
        | Ok _type_ctx ->
          let is_julia = Filename.check_suffix output ".jl" in
          if is_julia then begin
            match Affinescript.Julia_codegen.codegen_julia prog resolve_ctx.symbols with
            | Error msg ->
              add { severity = Error; code = "E0800";
                    message = Printf.sprintf "Julia codegen error: %s" msg;
                    span = Affinescript.Span.dummy; help = None; labels = [] }
            | Ok julia_code ->
              let oc = open_out output in
              output_string oc julia_code;
              close_out oc
          end else if wasm_gc then begin
            match Affinescript.Codegen_gc.generate_gc_module prog with
            | Error e ->
              add { severity = Error; code = "E0802";
                    message = Printf.sprintf "WASM GC codegen error: %s"
                      (Affinescript.Codegen_gc.format_codegen_error e);
                    span = Affinescript.Span.dummy; help = None; labels = [] }
            | Ok gc_module ->
              Affinescript.Wasm_gc_encode.write_gc_module_to_file output gc_module
          end else begin
            let optimized_prog = Affinescript.Opt.fold_constants_program prog in
            match Affinescript.Codegen.generate_module optimized_prog with
            | Error e ->
              add { severity = Error; code = "E0801";
                    message = Printf.sprintf "WASM codegen error: %s"
                      (Affinescript.Codegen.show_codegen_error e);
                    span = Affinescript.Span.dummy; help = None; labels = [] }
            | Ok wasm_module ->
              Affinescript.Wasm_encode.write_module_to_file output wasm_module
          end))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
      add (Affinescript.Json_output.of_lexer_error msg pos path)
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
      add (Affinescript.Json_output.of_parse_error msg span)
    end;
    json_finish (List.rev !diags)
  end else begin
    try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, _span) ->
        Format.eprintf "@[<v>Resolution error: %s@]@."
          (Affinescript.Resolve.show_resolve_error e);
        `Error (false, "Resolution error")
      | Ok (resolve_ctx, _type_ctx) ->
        (match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
        | Error e ->
          Format.eprintf "@[<v>%s@]@."
            (Affinescript.Typecheck.format_type_error e);
          `Error (false, "Type error")
        | Ok _type_ctx ->
          let is_julia = Filename.check_suffix output ".jl" in
          if is_julia then
            (match Affinescript.Julia_codegen.codegen_julia prog resolve_ctx.symbols with
            | Error e ->
              Format.eprintf "@[<v>Julia codegen error: %s@]@." e;
              `Error (false, "Julia codegen error")
            | Ok julia_code ->
              let oc = open_out output in
              output_string oc julia_code;
              close_out oc;
              Format.printf "Compiled %s -> %s (Julia)@." path output;
              `Ok ())
          else if wasm_gc then
            (match Affinescript.Codegen_gc.generate_gc_module prog with
            | Error e ->
              Format.eprintf "@[<v>%s@]@."
                (Affinescript.Codegen_gc.format_codegen_error e);
              `Error (false, "WASM GC codegen error")
            | Ok gc_module ->
              Affinescript.Wasm_gc_encode.write_gc_module_to_file output gc_module;
              Format.printf "Compiled %s -> %s (WASM GC)@." path output;
              `Ok ())
          else
            let optimized_prog = Affinescript.Opt.fold_constants_program prog in
            (match Affinescript.Codegen.generate_module optimized_prog with
            | Error e ->
              Format.eprintf "@[<v>Code generation error: %s@]@."
                (Affinescript.Codegen.show_codegen_error e);
              `Error (false, "Code generation error")
            | Ok wasm_module ->
              Affinescript.Wasm_encode.write_module_to_file output wasm_module;
              Format.printf "Compiled %s -> %s (WASM)@." path output;
              `Ok ())))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
        Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
        `Error (false, "Lexer error")
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
        Format.eprintf "@[<v>%a: parse error: %s@]@."
          Affinescript.Span.pp_short span msg;
        `Error (false, "Parse error")
  end

(** Format a file.  Only canonical face is supported — Python-face formatting
    requires a reverse transform that is not yet implemented. *)
let fmt_file face path =
  (match face with
  | `Python ->
    Format.eprintf "fmt --face python is not yet supported \
                    (reverse Python transform is pending).@.";
    (* fall through; format the canonical parse anyway so the file still works *)
    ()
  | `Canonical -> ());
  try
    Affinescript.Formatter.format_file path;
    Format.printf "Formatted %s@." path;
    `Ok ()
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
      Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
      `Error (false, "Lexer error")
  | Affinescript.Parse_driver.Parse_error (msg, span) ->
      Format.eprintf "@[<v>%a: parse error: %s@]@."
        Affinescript.Span.pp_short span msg;
      `Error (false, "Parse error")

(** Lint a file.  With [--json], emits lint diagnostics as structured
    JSON on stderr. *)
let lint_file face json path =
  if json then begin
    let diags = ref [] in
    let add d = diags := d :: !diags in
    begin try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, span) ->
        add (Affinescript.Json_output.of_resolve_error e span)
      | Ok (resolve_ctx, _type_ctx) ->
        let lint_diags = Affinescript.Linter.lint_program resolve_ctx.symbols prog in
        List.iter (fun d ->
          add (Affinescript.Json_output.of_lint_diagnostic d)
        ) lint_diags)
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
      add (Affinescript.Json_output.of_lexer_error msg pos path)
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
      add (Affinescript.Json_output.of_parse_error msg span)
    end;
    json_finish (List.rev !diags)
  end else begin
    try
      let prog = parse_with_face face path in
      let loader_config = Affinescript.Module_loader.default_config () in
      let loader = Affinescript.Module_loader.create loader_config in
      (match Affinescript.Resolve.resolve_program_with_loader prog loader with
      | Error (e, _span) ->
        Format.eprintf "@[<v>Resolution error: %s@]@."
          (Affinescript.Resolve.show_resolve_error e);
        `Error (false, "Resolution error")
      | Ok (resolve_ctx, _type_ctx) ->
        let diagnostics = Affinescript.Linter.lint_program resolve_ctx.symbols prog in
        if List.length diagnostics = 0 then
          (Format.printf "No issues found@."; `Ok ())
        else
          (Affinescript.Linter.print_diagnostics diagnostics;
           `Error (false, "Lint issues found")))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
        Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
        `Error (false, "Lexer error")
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
        Format.eprintf "@[<v>%a: parse error: %s@]@."
          Affinescript.Span.pp_short span msg;
        `Error (false, "Parse error")
  end

(** {1 CLI command definitions} *)
open Cmdliner

(** Shared --json flag *)
let json_arg =
  Arg.(value & flag & info ["json"]
    ~doc:"Emit diagnostics as a single JSON object on stderr. \
          Designed for tool consumption (LSP, CI, editors).")

let path_arg =
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc:"Input file")

let output_arg =
  Arg.(value & opt string "out.wasm" & info ["o"; "output"] ~docv:"FILE" ~doc:"Output file")

let wasm_gc_arg =
  Arg.(value & flag & info ["wasm-gc"]
    ~doc:"Target the WebAssembly GC proposal (struct/array types, no linear memory). \
          Requires a runtime that supports the GC proposal: V8/Chrome ≥ 119, \
          SpiderMonkey/Firefox ≥ 120, or Wasmtime with --wasm-features gc.")

(** Shared --face flag: select the parser surface-syntax face. *)
let face_arg =
  let faces = Arg.enum [("canonical", `Canonical); ("python", `Python)] in
  Arg.(value & opt faces `Canonical & info ["face"]
    ~docv:"FACE"
    ~doc:"Parser face (surface-syntax variant). $(docv) must be $(b,canonical) \
          (default, standard AffineScript) or $(b,python) (Python-style syntax: \
          indentation-based blocks, $(b,def)/$(b,True)/$(b,False)/$(b,None)/\
          $(b,and)/$(b,or)/$(b,not) etc. — compiled to the same canonical AST).")

let lex_cmd =
  let doc = "Lex a file and print tokens" in
  let info = Cmd.info "lex" ~doc in
  Cmd.v info Term.(ret (const lex_file $ path_arg))

let parse_cmd =
  let doc = "Parse a file and print AST" in
  let info = Cmd.info "parse" ~doc in
  Cmd.v info Term.(ret (const parse_file $ face_arg $ path_arg))

let check_cmd =
  let doc = "Type check a file" in
  let info = Cmd.info "check" ~doc in
  Cmd.v info Term.(ret (const check_file $ face_arg $ json_arg $ path_arg))

let eval_cmd =
  let doc = "Evaluate a file with the interpreter" in
  let info = Cmd.info "eval" ~doc in
  Cmd.v info Term.(ret (const eval_file $ face_arg $ json_arg $ path_arg))

let repl_cmd =
  let doc = "Start the interactive REPL" in
  let info = Cmd.info "repl" ~doc in
  Cmd.v info Term.(ret (const repl_cmd_fn $ const ()))

let compile_cmd =
  let doc = "Compile a file to WebAssembly (1.0 or GC proposal) or Julia" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info Term.(ret (const compile_file $ face_arg $ json_arg $ wasm_gc_arg $ path_arg $ output_arg))

let fmt_cmd =
  let doc = "Format a file" in
  let info = Cmd.info "fmt" ~doc in
  Cmd.v info Term.(ret (const fmt_file $ face_arg $ path_arg))

let lint_cmd =
  let doc = "Lint a file for code quality issues" in
  let info = Cmd.info "lint" ~doc in
  Cmd.v info Term.(ret (const lint_file $ face_arg $ json_arg $ path_arg))

let preview_python_cmd =
  let doc = "Preview the Python-face text transform (debug)" in
  let info = Cmd.info "preview-python" ~doc in
  Cmd.v info Term.(ret (const preview_python_transform $ path_arg))

let default_cmd =
  let doc = "The AffineScript compiler" in
  let info = Cmd.info "affinescript" ~version ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [lex_cmd; parse_cmd; check_cmd; eval_cmd; repl_cmd; compile_cmd; fmt_cmd; lint_cmd; preview_python_cmd]

let () = exit (Cmd.eval default_cmd)
