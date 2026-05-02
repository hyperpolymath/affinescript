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

(** Resolve the effective face. Priority order, highest first:

    1. An explicit non-Canonical [--face] flag (always wins).
    2. A [face:] pragma in the leading comment lines of the source file
       (see {!Affinescript.Face_pragma}). This is the recommended mechanism
       once every source file shares the canonical [.affine] extension.
    3. A deprecated face-implying file extension ([.rattle], [.pyaff],
       [.jsaff], [.pseudoaff]). Triggers a one-line stderr warning.
    4. Canonical default.

    [quiet] suppresses the deprecation notice for JSON and LSP commands
    whose stderr is consumed by tools (LSP clients, CI, editors). *)
let resolve_face ?(quiet = false) face path =
  match face with
  | Affinescript.Face.Canonical ->
    (match Affinescript.Face_pragma.detect_in_file path with
    | Some f -> f
    | None ->
      let ext =
        try
          let dot = String.rindex path '.' in
          String.sub path (dot + 1) (String.length path - dot - 1)
        with Not_found -> ""
      in
      let warn_deprecated alias =
        if not quiet then
          Format.eprintf
            "@[<v>warning: file extension .%s is deprecated; \
             rename to .affine and add a pragma '# face: %s' on the first \
             line (use --face %s to silence this warning)@]@."
            ext alias alias
      in
      (match ext with
      | "rattle"    -> warn_deprecated "rattle";     Affinescript.Face.Python
      | "pyaff"     -> warn_deprecated "python";     Affinescript.Face.Python
      | "jsaff"     -> warn_deprecated "js";         Affinescript.Face.Js
      | "pseudoaff" -> warn_deprecated "pseudocode"; Affinescript.Face.Pseudocode
      | _           -> Affinescript.Face.Canonical))
  | other -> other   (* explicit --face flag always wins *)

(** Parse a file using the requested face. *)
let parse_with_face (face : Affinescript.Face.face) path =
  match face with
  | Affinescript.Face.Canonical   -> Affinescript.Parse_driver.parse_file path
  | Affinescript.Face.Python      -> Affinescript.Python_face.parse_file_python path
  | Affinescript.Face.Js          -> Affinescript.Js_face.parse_file_js path
  | Affinescript.Face.Pseudocode  -> Affinescript.Pseudocode_face.parse_file_pseudocode path
  | Affinescript.Face.Lucid       -> Affinescript.Lucid_face.parse_file_lucid path
  | Affinescript.Face.Cafe        -> Affinescript.Cafe_face.parse_file_cafe path

(** Preview the Python-face text transform (debug tool). *)
let preview_python_transform path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  print_string (Affinescript.Python_face.preview_transform s);
  `Ok ()

(** Preview the JS-face text transform (debug tool). *)
let preview_js_transform path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  print_string (Affinescript.Js_face.preview_transform s);
  `Ok ()

(** Preview the pseudocode-face text transform (debug tool). *)
let preview_pseudocode_transform path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  print_string (Affinescript.Pseudocode_face.preview_transform s);
  `Ok ()

(** Preview the Lucid-face (PureScript-style) text transform (debug tool). *)
let preview_lucid_transform path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  print_string (Affinescript.Lucid_face.preview_transform s);
  `Ok ()

(** Preview the Cafe-face (CoffeeScript-style) text transform (debug tool). *)
let preview_cafe_transform path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  print_string (Affinescript.Cafe_face.preview_transform s);
  `Ok ()

(** Parse a file and print AST (no --json support). *)
let parse_file (face : Affinescript.Face.face) path =
  let face = resolve_face face path in
  try
    let prog = parse_with_face face path in
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
  let face = resolve_face ~quiet:json face path in
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
        | Ok _ctx ->
          (match Affinescript.Borrow.check_program resolve_ctx.symbols prog with
          | Error e ->
            add (Affinescript.Json_output.of_borrow_error e)
          | Ok () ->
            (* Stage 1: QTT quantity enforcement *)
            (match Affinescript.Quantity.check_program_quantities prog with
            | Error (err, span) ->
              add (Affinescript.Json_output.of_quantity_error (err, span))
            | Ok () -> ()))))
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
          (Affinescript.Face.format_resolve_error face e);
        `Error (false, "Resolution error")
      | Ok (resolve_ctx, _type_ctx) ->
        (match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
        | Error e ->
          Format.eprintf "@[<v>%s@]@."
            (Affinescript.Face.format_type_error face e);
          `Error (false, "Type error")
        | Ok _ctx ->
          (match Affinescript.Borrow.check_program resolve_ctx.symbols prog with
          | Error e ->
            Format.eprintf "@[<v>Borrow error: %s@]@."
              (Affinescript.Face.format_borrow_error face e);
            `Error (false, "Borrow error")
          | Ok () ->
            (* Stage 1: QTT quantity enforcement *)
            (match Affinescript.Quantity.check_program_quantities prog with
            | Error (err, _span) ->
              Format.eprintf "@[<v>Quantity error: %s@]@."
                (Affinescript.Face.format_quantity_error face err);
              `Error (false, "Quantity error")
            | Ok () ->
              Format.printf "Type checking passed@.";
              `Ok ()))))
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
  let face = resolve_face ~quiet:json face path in
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
        (* Register builtins (print, int_to_string, tea_run, etc.) before type-checking *)
        Affinescript.Typecheck.register_builtins type_ctx;
        (match List.fold_left (fun acc decl ->
          match acc with
          | Error _ as e -> e
          | Ok () -> Affinescript.Typecheck.check_decl type_ctx decl
        ) (Ok ()) prog.prog_decls with
        | Error e ->
          add (Affinescript.Json_output.of_type_error e)
        | Ok () ->
          (* Stage 1: QTT quantity enforcement *)
          (match Affinescript.Quantity.check_program_quantities prog with
          | Error (err, span) ->
            add (Affinescript.Json_output.of_quantity_error (err, span))
          | Ok () ->
            (match Affinescript.Interp.eval_program prog with
            | Ok env ->
              (* Auto-call main() if defined — entry point for TEA apps and scripts *)
              (match Affinescript.Value.lookup_env "main" env with
              | Ok main_fn ->
                (match Affinescript.Interp.apply_function main_fn [] with
                | Ok _ -> ()
                | Error e -> add (Affinescript.Json_output.of_eval_error e))
              | Error _ -> ())  (* no main — that's fine, just eval declarations *)
            | Error e ->
              add (Affinescript.Json_output.of_eval_error e)))))
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
          (Affinescript.Face.format_resolve_error face e);
        `Error (false, "Resolution error")
      | Ok (resolve_ctx, type_ctx) ->
        let type_ctx = { type_ctx with symbols = resolve_ctx.symbols } in
        (* Register builtins (print, int_to_string, tea_run, etc.) before type-checking *)
        Affinescript.Typecheck.register_builtins type_ctx;
        (match List.fold_left (fun acc decl ->
          match acc with
          | Error _ as e -> e
          | Ok () -> Affinescript.Typecheck.check_decl type_ctx decl
        ) (Ok ()) prog.prog_decls with
        | Error e ->
          Format.eprintf "@[<v>%s@]@."
            (Affinescript.Face.format_type_error face e);
          `Error (false, "Type error")
        | Ok () ->
          (* Stage 1: QTT quantity enforcement *)
          (match Affinescript.Quantity.check_program_quantities prog with
          | Error (err, _span) ->
            Format.eprintf "@[<v>Quantity error: %s@]@."
              (Affinescript.Face.format_quantity_error face err);
            `Error (false, "Quantity error")
          | Ok () ->
            (match Affinescript.Interp.eval_program prog with
            | Ok env ->
              (* Auto-call main() if defined — entry point for TEA apps and scripts *)
              (match Affinescript.Value.lookup_env "main" env with
              | Ok main_fn ->
                (match Affinescript.Interp.apply_function main_fn [] with
                | Ok _ -> `Ok ()
                | Error e ->
                  Format.eprintf "@[<v>Runtime error: %s@]@."
                    (Affinescript.Value.show_eval_error e);
                  `Error (false, "Runtime error"))
              | Error _ ->
                (* No main function — declarations evaluated, nothing to run *)
                Format.printf "Program evaluated successfully@.";
                `Ok ())
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
  end

(** Generate the AffineScript TEA bridge Wasm module.

    Emits a validated Wasm 1.0 binary that implements the TitleScreen
    TEA state machine with clean i32 exports for AffineTEA.js.

    No source file is needed — the module is generated directly from the
    TEA ABI specification.  Write to a .wasm file, copy to IDApTIK's
    public/assets/wasm/ directory, and load via AffineTEA.load(). *)
let tea_bridge_cmd_fn output =
  let m = Affinescript.Tea_bridge.generate () in
  (* Stage 8: auto-verify ownership constraints before writing. *)
  (match Affinescript.Tw_verify.verify_from_module m with
  | Ok () ->
    Format.printf "typed-wasm ownership verification: OK@."
  | Error errs ->
    Affinescript.Tw_verify.pp_report Format.std_formatter errs);
  Affinescript.Wasm_encode.write_module_to_file output m;
  Format.printf "TEA bridge written to %s@." output;
  Format.printf "  affinescript_init()                     — initialise TitleModel@.";
  Format.printf "  affinescript_update(msg: i32)           — 0=NewGame 1=LoadGame 2=Settings 3=Credits@.";
  Format.printf "  affinescript_get_selected() -> i32      — 0=none 1=new_game 2=load_game 3=settings 4=credits@.";
  Format.printf "  affinescript_get_screen_w/h() -> i32    — current screen dimensions@.";
  Format.printf "  affinescript_set_screen(w: i32, h: i32) — handle resize events@.";
  Format.printf "  memory                                   — exported linear memory (model at offset 64)@.";
  Format.printf "Custom sections: affinescript.ownership, affinescript.tea_layout@.";
  `Ok ()

(** Generate the Cadre Router Wasm module.

    Produces a WebAssembly 1.0 module encoding IDApTIK's screen back-stack as
    an affine resource.  Push/pop consume the old stack linearly and produce a
    new one — navigation history as a linear type.

    No source file is needed — the module is generated directly from the router
    ABI specification.  Write to a .wasm file, copy to IDApTIK's
    public/assets/wasm/ directory, and load via AffineTEARouter.load(). *)
let router_bridge_cmd_fn output =
  let m = Affinescript.Tea_router.generate () in
  (* Stage 8: auto-verify ownership constraints before writing. *)
  (match Affinescript.Tw_verify.verify_from_module m with
  | Ok () ->
    Format.printf "typed-wasm ownership verification: OK@."
  | Error errs ->
    Affinescript.Tw_verify.pp_report Format.std_formatter errs);
  Affinescript.Wasm_encode.write_module_to_file output m;
  Format.printf "Router bridge written to %s@." output;
  Format.printf "  affinescript_router_init()                          — initialise RouterModel@.";
  Format.printf "  affinescript_router_push(screen_tag: i32)           — push screen (Linear param)@.";
  Format.printf "  affinescript_router_pop()                           — pop current screen@.";
  Format.printf "  affinescript_router_present_popup(popup_tag: i32)   — show popup (Linear param)@.";
  Format.printf "  affinescript_router_dismiss_popup()                 — dismiss popup@.";
  Format.printf "  affinescript_router_resize(w: i32, h: i32)          — update screen dims (Linear)@.";
  Format.printf "  affinescript_router_get_stack_top() -> i32          — current screen (−1 if empty)@.";
  Format.printf "  affinescript_router_get_popup_tag() -> i32          — active popup (−1 if none)@.";
  Format.printf "Screen tags: 0=Title 1=CharacterSelect 2=WorldMap 3=Load 4=Game@.";
  Format.printf "Popup tags:  0=Settings 1=Inventory 2=Hacking@.";
  Format.printf "Custom sections: affinescript.ownership, affinescript.tea_layout@.";
  `Ok ()

(** Generate the CharacterSelect TEA Bridge Wasm module.

    Produces a WebAssembly 1.0 module that implements the AffineScript TEA
    ABI for CharacterSelectScreen.  The model holds the player's background
    selection as a single i32 selected_tag (0=none, 1-6=background, 7=confirmed).

    The exported API surface is identical to the TitleScreen bridge, so the
    same AffineTEA.js / AffineTEA.res bindings work without modification.

    Msg tags (input to affinescript_update):
      0=SelectAssault  1=SelectRecon  2=SelectEngineer
      3=SelectSignals  4=SelectMedic  5=SelectLogistics  6=Confirm

    selected_tag (output from affinescript_get_selected):
      0=none  1=Assault  2=Recon  3=Engineer  4=Signals  5=Medic  6=Logistics
      7=confirmed (navigate to JessicaCustomise)

    No source file needed — generated from the CharacterSelect TEA ABI.
    Write to a .wasm file, copy to IDApTIK's public/assets/wasm/ directory. *)
let cs_bridge_cmd_fn output =
  let m = Affinescript.Tea_cs_bridge.generate () in
  (* Auto-verify ownership constraints before writing. *)
  (match Affinescript.Tw_verify.verify_from_module m with
  | Ok () ->
    Format.printf "typed-wasm ownership verification: OK@."
  | Error errs ->
    Affinescript.Tw_verify.pp_report Format.std_formatter errs);
  Affinescript.Wasm_encode.write_module_to_file output m;
  Format.printf "CharacterSelect TEA bridge written to %s@." output;
  Format.printf "  affinescript_init()                     — initialise CharacterSelectModel@.";
  Format.printf "  affinescript_update(msg: i32)           — 0-5=SelectClass 6=Confirm@.";
  Format.printf "  affinescript_get_selected() -> i32      — 0=none 1-6=class 7=confirmed@.";
  Format.printf "  affinescript_get_screen_w/h() -> i32    — current screen dimensions@.";
  Format.printf "  affinescript_set_screen(w: i32, h: i32) — handle resize events@.";
  Format.printf "  memory                                   — exported linear memory (model at offset 64)@.";
  Format.printf "Custom sections: affinescript.ownership, affinescript.tea_layout@.";
  `Ok ()

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
  let face = resolve_face ~quiet:json face path in
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
          (match Affinescript.Borrow.check_program resolve_ctx.symbols prog with
          | Error e ->
            add (Affinescript.Json_output.of_borrow_error e)
          | Ok () ->
            (* Stage 1: QTT quantity enforcement *)
            (match Affinescript.Quantity.check_program_quantities prog with
            | Error (err, span) ->
              add (Affinescript.Json_output.of_quantity_error (err, span))
            | Ok () ->
            let is_julia = Filename.check_suffix output ".jl" in
            let is_js = Filename.check_suffix output ".js" in
            let is_c = Filename.check_suffix output ".c" in
            let is_wgsl = Filename.check_suffix output ".wgsl" in
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
            end else if is_js then begin
              match Affinescript.Js_codegen.codegen_js prog resolve_ctx.symbols with
              | Error msg ->
                add { severity = Error; code = "E0803";
                      message = Printf.sprintf "JS codegen error: %s" msg;
                      span = Affinescript.Span.dummy; help = None; labels = [] }
              | Ok js_code ->
                let oc = open_out output in
                output_string oc js_code;
                close_out oc
            end else if is_c then begin
              match Affinescript.C_codegen.codegen_c prog resolve_ctx.symbols with
              | Error msg ->
                add { severity = Error; code = "E0804";
                      message = Printf.sprintf "C codegen error: %s" msg;
                      span = Affinescript.Span.dummy; help = None; labels = [] }
              | Ok c_code ->
                let oc = open_out output in
                output_string oc c_code;
                close_out oc
            end else if is_wgsl then begin
              match Affinescript.Wgsl_codegen.codegen_wgsl prog resolve_ctx.symbols with
              | Error msg ->
                add { severity = Error; code = "E0805";
                      message = Printf.sprintf "WGSL codegen error: %s" msg;
                      span = Affinescript.Span.dummy; help = None; labels = [] }
              | Ok wgsl_code ->
                let oc = open_out output in
                output_string oc wgsl_code;
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
            end))))
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
          (Affinescript.Face.format_resolve_error face e);
        `Error (false, "Resolution error")
      | Ok (resolve_ctx, _type_ctx) ->
        (match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
        | Error e ->
          Format.eprintf "@[<v>%s@]@."
            (Affinescript.Face.format_type_error face e);
          `Error (false, "Type error")
        | Ok _type_ctx ->
          (match Affinescript.Borrow.check_program resolve_ctx.symbols prog with
          | Error e ->
            Format.eprintf "@[<v>Borrow error: %s@]@."
              (Affinescript.Face.format_borrow_error face e);
            `Error (false, "Borrow error")
          | Ok () ->
            (* Stage 1: QTT quantity enforcement *)
            (match Affinescript.Quantity.check_program_quantities prog with
            | Error (err, _span) ->
              Format.eprintf "@[<v>Quantity error: %s@]@."
                (Affinescript.Face.format_quantity_error face err);
              `Error (false, "Quantity error")
            | Ok () ->
          begin
            let is_julia = Filename.check_suffix output ".jl" in
            let is_js = Filename.check_suffix output ".js" in
            let is_c = Filename.check_suffix output ".c" in
            let is_wgsl = Filename.check_suffix output ".wgsl" in
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
            else if is_js then
              (match Affinescript.Js_codegen.codegen_js prog resolve_ctx.symbols with
              | Error e ->
                Format.eprintf "@[<v>JS codegen error: %s@]@." e;
                `Error (false, "JS codegen error")
              | Ok js_code ->
                let oc = open_out output in
                output_string oc js_code;
                close_out oc;
                Format.printf "Compiled %s -> %s (JS)@." path output;
                `Ok ())
            else if is_c then
              (match Affinescript.C_codegen.codegen_c prog resolve_ctx.symbols with
              | Error e ->
                Format.eprintf "@[<v>C codegen error: %s@]@." e;
                `Error (false, "C codegen error")
              | Ok c_code ->
                let oc = open_out output in
                output_string oc c_code;
                close_out oc;
                Format.printf "Compiled %s -> %s (C)@." path output;
                `Ok ())
            else if is_wgsl then
              (match Affinescript.Wgsl_codegen.codegen_wgsl prog resolve_ctx.symbols with
              | Error e ->
                Format.eprintf "@[<v>WGSL codegen error: %s@]@." e;
                `Error (false, "WGSL codegen error")
              | Ok wgsl_code ->
                let oc = open_out output in
                output_string oc wgsl_code;
                close_out oc;
                Format.printf "Compiled %s -> %s (WGSL)@." path output;
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
                (* Stage 8: auto-verify typed-wasm ownership constraints.
                   Printed as informational; does not block compilation. *)
                (match Affinescript.Tw_verify.verify_from_module wasm_module with
                | Ok () -> ()
                | Error errs ->
                  Affinescript.Tw_verify.pp_report Format.err_formatter errs);
                Affinescript.Wasm_encode.write_module_to_file output wasm_module;
                Format.printf "Compiled %s -> %s (WASM)@." path output;
                `Ok ())
          end))))
    with
    | Affinescript.Lexer.Lexer_error (msg, pos) ->
        Format.eprintf "@[<v>%s:%d:%d: lexer error: %s@]@." path pos.line pos.col msg;
        `Error (false, "Lexer error")
    | Affinescript.Parse_driver.Parse_error (msg, span) ->
        Format.eprintf "@[<v>%a: parse error: %s@]@."
          Affinescript.Span.pp_short span msg;
        `Error (false, "Parse error")
  end

(** Format a file.  Only canonical face is supported — non-canonical face
    formatting requires a reverse transform that is not yet implemented. *)
let fmt_file face path =
  let face = resolve_face face path in
  (match face with
  | Affinescript.Face.Python ->
    Format.eprintf "fmt --face python is not yet supported \
                    (reverse Python transform is pending).@."; ()
  | Affinescript.Face.Js ->
    Format.eprintf "fmt --face js is not yet supported \
                    (reverse JS transform is pending).@."; ()
  | Affinescript.Face.Pseudocode ->
    Format.eprintf "fmt --face pseudocode is not yet supported \
                    (reverse pseudocode transform is pending).@."; ()
  | Affinescript.Face.Lucid ->
    Format.eprintf "fmt --face lucid is not yet supported \
                    (reverse Lucid/PureScript transform is pending).@."; ()
  | Affinescript.Face.Cafe ->
    Format.eprintf "fmt --face cafe is not yet supported \
                    (reverse Cafe/CoffeeScript transform is pending).@."; ()
  | Affinescript.Face.Canonical -> ());
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
  let face = resolve_face ~quiet:json face path in
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
          (Affinescript.Face.format_resolve_error face e);
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

(** {1 Stage 7: typed-wasm ownership verifier subcommand} *)

(** Compile a source file through the full frontend pipeline
    (parse → resolve → typecheck → borrow → quantity → codegen) and return
    the in-memory [Wasm.wasm_module] on success. On any stage failure,
    formats the diagnostic to stderr in the requested face's vocabulary
    and returns [Error stage_label].

    Shared by the intra-module [verify] subcommand and the cross-module
    [verify-boundary] subcommand so the ownership custom section emitted
    during codegen is the same one consumed by both verifiers. *)
let compile_to_wasm_module face path
  : (Affinescript.Wasm.wasm_module, string) Result.t =
  try
    let prog = parse_with_face face path in
    let loader_config = Affinescript.Module_loader.default_config () in
    let loader = Affinescript.Module_loader.create loader_config in
    match Affinescript.Resolve.resolve_program_with_loader prog loader with
    | Error (e, _span) ->
      Format.eprintf "%s: resolution error: %s@." path
        (Affinescript.Face.format_resolve_error face e);
      Error "Resolution error"
    | Ok (resolve_ctx, _type_ctx) ->
      match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
      | Error e ->
        Format.eprintf "%s: %s@." path
          (Affinescript.Face.format_type_error face e);
        Error "Type error"
      | Ok _type_ctx ->
        match Affinescript.Borrow.check_program resolve_ctx.symbols prog with
        | Error e ->
          Format.eprintf "%s: borrow error: %s@." path
            (Affinescript.Face.format_borrow_error face e);
          Error "Borrow error"
        | Ok () ->
          match Affinescript.Quantity.check_program_quantities prog with
          | Error (err, _span) ->
            Format.eprintf "%s: quantity error: %s@." path
              (Affinescript.Face.format_quantity_error face err);
            Error "Quantity error"
          | Ok () ->
            let optimized_prog = Affinescript.Opt.fold_constants_program prog in
            (match Affinescript.Codegen.generate_module optimized_prog with
            | Error e ->
              Format.eprintf "%s: codegen error: %s@." path
                (Affinescript.Codegen.show_codegen_error e);
              Error "Codegen error"
            | Ok wasm_mod -> Ok wasm_mod)
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
    Format.eprintf "%s:%d:%d: lexer error: %s@." path pos.line pos.col msg;
    Error "Lexer error"
  | Affinescript.Parse_driver.Parse_error (msg, span) ->
    Format.eprintf "%a: parse error: %s@."
      Affinescript.Span.pp_short span msg;
    Error "Parse error"

(** Verify typed-wasm Level 7/10 ownership constraints on a compiled AffineScript
    source file.

    Runs the full frontend pipeline (parse → resolve → typecheck → borrow →
    quantity → codegen) and then passes the resulting Wasm module through
    [Tw_verify.verify_from_module], which reads the [affinescript.ownership]
    custom section and checks each function body for linearity (Level 10) and
    aliasing-safety (Level 7) violations.

    Exit code 0 = clean.  Exit code 1 = violations found.

    Usage: affinescript verify FILE *)
let verify_file face path =
  let face = resolve_face face path in
  try
    let prog = parse_with_face face path in
    let loader_config = Affinescript.Module_loader.default_config () in
    let loader = Affinescript.Module_loader.create loader_config in
    (match Affinescript.Resolve.resolve_program_with_loader prog loader with
    | Error (e, _span) ->
      Format.eprintf "Resolution error: %s@."
        (Affinescript.Face.format_resolve_error face e);
      `Error (false, "Resolution error")
    | Ok (resolve_ctx, _type_ctx) ->
      (match Affinescript.Typecheck.check_program resolve_ctx.symbols prog with
      | Error e ->
        Format.eprintf "%s@."
          (Affinescript.Face.format_type_error face e);
        `Error (false, "Type error")
      | Ok _type_ctx ->
        (match Affinescript.Borrow.check_program resolve_ctx.symbols prog with
        | Error e ->
          Format.eprintf "Borrow error: %s@."
            (Affinescript.Face.format_borrow_error face e);
          `Error (false, "Borrow error")
        | Ok () ->
          (match Affinescript.Quantity.check_program_quantities prog with
          | Error (err, _span) ->
            Format.eprintf "Quantity error: %s@."
              (Affinescript.Face.format_quantity_error face err);
            `Error (false, "Quantity error")
          | Ok () ->
            let optimized_prog = Affinescript.Opt.fold_constants_program prog in
            (match Affinescript.Codegen.generate_module optimized_prog with
            | Error e ->
              Format.eprintf "Codegen error: %s@."
                (Affinescript.Codegen.show_codegen_error e);
              `Error (false, "Codegen error")
            | Ok wasm_mod ->
              (match Affinescript.Tw_verify.verify_from_module wasm_mod with
              | Ok () ->
                Format.printf "typed-wasm ownership verification: OK@.";
                `Ok ()
              | Error errs ->
                Affinescript.Tw_verify.pp_report Format.std_formatter errs;
                `Error (false, "Ownership violations found")))))))
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
    Format.eprintf "%s:%d:%d: lexer error: %s@." path pos.line pos.col msg;
    `Error (false, "Lexer error")
  | Affinescript.Parse_driver.Parse_error (msg, span) ->
    Format.eprintf "%a: parse error: %s@."
      Affinescript.Span.pp_short span msg;
    `Error (false, "Parse error")

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
  let faces = Arg.enum [
    (* Generic names. *)
    ("canonical",    Affinescript.Face.Canonical);
    ("python",       Affinescript.Face.Python);
    ("js",           Affinescript.Face.Js);
    ("javascript",   Affinescript.Face.Js);
    ("pseudocode",   Affinescript.Face.Pseudocode);
    ("pseudo",       Affinescript.Face.Pseudocode);
    ("lucid",        Affinescript.Face.Lucid);
    ("purescript",   Affinescript.Face.Lucid);
    ("cafe",         Affinescript.Face.Cafe);
    ("coffee",       Affinescript.Face.Cafe);
    ("coffeescript", Affinescript.Face.Cafe);
    (* Brand names from README.adoc — "Different faces, same cube". *)
    ("affinescript", Affinescript.Face.Canonical);
    ("rattle",       Affinescript.Face.Python);
    ("rattlescript", Affinescript.Face.Python);
    ("jaffa",        Affinescript.Face.Js);
    ("jaffascript",  Affinescript.Face.Js);
    ("pseudoscript", Affinescript.Face.Pseudocode);
    ("lucidscript",  Affinescript.Face.Lucid);
    ("cafescripto",  Affinescript.Face.Cafe);
  ] in
  Arg.(value & opt faces Affinescript.Face.Canonical & info ["face"]
    ~docv:"FACE"
    ~doc:"Parser face (surface-syntax variant). The established faces \
          (per README.adoc — 'different faces, same cube'): \
          $(b,canonical)/$(b,affinescript) (default) — the canonical face. \
          $(b,python)/$(b,rattle)/$(b,rattlescript) — Python-style \
          ($(b,def)/indentation/$(b,True)/$(b,None)/etc.). \
          $(b,js)/$(b,javascript)/$(b,jaffa)/$(b,jaffascript) — JavaScript-style \
          ($(b,const)/$(b,let)/$(b,function)/$(b,=>)/$(b,null)/$(b,===)/import-from). \
          $(b,pseudocode)/$(b,pseudo)/$(b,pseudoscript) — natural-language pseudocode \
          ($(b,function)/$(b,set...to)/$(b,if...then)/$(b,end)/$(b,is)/$(b,and)/etc.). \
          $(b,lucid)/$(b,lucidscript)/$(b,purescript) — PureScript-style \
          ($(b,module … where)/$(b,data)/$(b,class)/$(b,instance)/$(b,case … of)/\
           $(b,\\x ->)/$(b,let … in)/$(b,True)/$(b,--) comments). \
          $(b,cafe)/$(b,cafescripto)/$(b,coffee)/$(b,coffeescript) — CoffeeScript-style \
          ($(b,->)/$(b,=>)/$(b,@)/$(b,unless)/$(b,until)/postfix-if/postfix-unless/\
           $(b,Yes)/$(b,No)/$(b,On)/$(b,Off)/$(b,#) comments/$(b,###) blocks). \
          All faces compile to the same canonical AST and produce identical \
          typed-wasm output; errors are reported in face-appropriate \
          vocabulary. Resolution order when this flag is omitted: \
          (1) a $(b,face:) pragma in the source file's leading comment lines \
          (e.g. $(b,'# face: rattlescript') or $(b,'-- face: lucidscript')); \
          (2) a deprecated face-implying extension \
          ($(b,.rattle), $(b,.pyaff), $(b,.jsaff), $(b,.pseudoaff)) — \
          warns and is scheduled for removal; \
          (3) Canonical. The recommended layout is $(b,.affine) everywhere \
          with a pragma on the first line of non-canonical files.")

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

let tea_bridge_cmd =
  let doc = "Generate the AffineScript TEA bridge Wasm module for IDApTIK" in
  let info = Cmd.info "tea-bridge" ~doc in
  Cmd.v info Term.(ret (const tea_bridge_cmd_fn $ output_arg))

let router_bridge_cmd =
  let doc = "Generate the AffineScript Cadre Router Wasm module for IDApTIK" in
  let info = Cmd.info "router-bridge" ~doc in
  Cmd.v info Term.(ret (const router_bridge_cmd_fn $ output_arg))

let cs_bridge_cmd =
  let doc = "Generate the AffineScript CharacterSelect TEA bridge Wasm module for IDApTIK" in
  let info = Cmd.info "cs-bridge" ~doc in
  Cmd.v info Term.(ret (const cs_bridge_cmd_fn $ output_arg))

let verify_cmd =
  let doc = "Verify typed-wasm Level 7/10 ownership constraints on compiled output" in
  let man = [
    `S "DESCRIPTION";
    `P "Compiles FILE through the full AffineScript pipeline and then verifies \
        that the emitted Wasm module satisfies typed-wasm ownership contracts:";
    `P "Level 10 (Linearity): each parameter annotated [own] must be consumed \
        exactly once in the function body.";
    `P "Level 7 (Aliasing safety): each parameter annotated [mut] must be \
        referenced at most once simultaneously.";
    `P "Exit 0 if all constraints are satisfied; exit 1 if violations are found.";
  ] in
  let info = Cmd.info "verify" ~doc ~man in
  Cmd.v info Term.(ret (const verify_file $ face_arg $ path_arg))

(** {1 Stage 10: typed-wasm interface extraction subcommand} *)

(** Print the ownership-annotated export interface of a generated bridge module.

    Generates the selected module ([tea-bridge] or [router]) in-memory, extracts
    its [affinescript.ownership] section, and prints the per-export ownership
    contract: which parameters are [own] (Linear), [ref] (SharedBorrow), [mut]
    (ExclBorrow), or plain [val] (Unrestricted).

    This is the machine-readable boundary contract that any caller (JS bridge,
    another Wasm module, or a future Wasm-to-Wasm linker) must honour to preserve
    the typed-wasm Level 7/10 guarantees across the module boundary.

    Usage: affinescript interface [--which tea-bridge|router|all] *)
let interface_cmd_fn which =
  let show_for name m =
    let iface = Affinescript.Tw_interface.extract_exports m in
    Format.printf "=== %s ===@." name;
    Affinescript.Tw_interface.pp_interface Format.std_formatter iface;
    Format.printf "@."
  in
  (match which with
  | `TeaBridge ->
    show_for "tea-bridge" (Affinescript.Tea_bridge.generate ())
  | `Router ->
    show_for "router" (Affinescript.Tea_router.generate ())
  | `All ->
    show_for "tea-bridge" (Affinescript.Tea_bridge.generate ());
    show_for "router"     (Affinescript.Tea_router.generate ()));
  `Ok ()

(** {1 Stage 10: typed-wasm cross-module boundary verifier subcommand} *)

(** Build a synthetic well-formed caller module that imports a single
    function (type: [i32 → ()]) at import slot 0 and calls it exactly once
    from its sole local function.

    This is the minimal "correct caller" used to demonstrate that the
    cross-module verifier accepts a Linear-param import called once per path.
    The import is named [fn_name] and sourced from module [mod_name]. *)
let make_linear_caller mod_name fn_name : Affinescript.Wasm.wasm_module =
  let import = Affinescript.Wasm.{
    i_module = mod_name;
    i_name   = fn_name;
    i_desc   = ImportFunc 0;
  } in
  let caller_fn = Affinescript.Wasm.{
    f_type   = 0;
    f_locals = [];
    f_body   = [ I32Const 0l; Call 0 ];
  } in
  { (Affinescript.Wasm.empty_module ()) with
    Affinescript.Wasm.types   = [{ ft_params  = [I32]; ft_results = [] }];
    Affinescript.Wasm.imports = [import];
    Affinescript.Wasm.funcs   = [caller_fn];
  }

(** Verify typed-wasm Level 7/10 cross-module boundary constraints.

    Generates the selected callee module, extracts its ownership-annotated
    export interface, then verifies a synthetic caller module against it.
    The synthetic caller imports each Linear-param export and calls it exactly
    once — this is the correct usage pattern that the verifier must accept.

    For the [router] callee, additionally verifies that [fn_push]'s explicit
    else-drop (Stage 9 fix) is reflected cleanly in the interface.

    Exit 0 = no violations.  Exit 1 = violations found.

    Usage: affinescript verify-bridge [--which tea-bridge|router|all] *)
let verify_boundary_fn which =
  (* Accumulates total violations so the exit code reflects them. Previously
     this function always returned `Ok (), so `verify-bridge` silently
     claimed success even when `verify_cross_module` found violations — the
     pretty-printed report was visible but the CI contract (exit 1) never
     triggered. *)
  let total_violations = ref 0 in
  let verify_one name callee_mod fn_name =
    let iface = Affinescript.Tw_interface.extract_exports callee_mod in
    let caller = make_linear_caller name fn_name in
    Format.printf "=== %s — boundary check for '%s' ===@." name fn_name;
    (match Affinescript.Tw_interface.verify_cross_module iface caller with
    | Ok () ->
      Affinescript.Tw_interface.pp_cross_report Format.std_formatter [];
    | Error errs ->
      total_violations := !total_violations + List.length errs;
      Affinescript.Tw_interface.pp_cross_report Format.std_formatter errs)
  in
  (match which with
  | `TeaBridge ->
    verify_one "tea-bridge"
      (Affinescript.Tea_bridge.generate ())
      "affinescript_tea_update"
  | `Router ->
    verify_one "router"
      (Affinescript.Tea_router.generate ())
      "affinescript_router_push"
  | `All ->
    verify_one "tea-bridge"
      (Affinescript.Tea_bridge.generate ())
      "affinescript_tea_update";
    verify_one "router"
      (Affinescript.Tea_router.generate ())
      "affinescript_router_push");
  if !total_violations = 0 then `Ok ()
  else `Error (false, Printf.sprintf "%d boundary violation(s)" !total_violations)

(** Verify typed-wasm Level 7/10 cross-module boundary constraints between
    two user-compiled AffineScript modules.

    Compiles [callee_path] and [caller_path] through the full frontend
    pipeline, extracts [callee]'s ownership-annotated export interface from
    its [affinescript.ownership] custom section, then checks that every
    function in [caller] invokes each Linear-param import with consistent
    per-path call counts:
      - max_calls > 1 on any path → LinearImportCalledMultiple
      - min_calls = 0 ∧ max_calls ≥ 1 → LinearImportDroppedOnSomePath

    Unlike [verify-bridge] (which exercises the two internal IDApTIK
    bridges against a synthetic one-call caller), this subcommand takes
    user modules so any AffineScript consumer can validate its own
    multi-module composition. The callee exposes [Linear] params via
    [pub fn f(x: own T)]; the caller imports those exports and its body
    becomes the subject of the path-sensitive call-count analysis.

    Exit code 0 = clean.  Exit code 1 = violations found. *)
let verify_boundary_files_fn face callee_path caller_path =
  let callee_face = resolve_face face callee_path in
  let caller_face = resolve_face face caller_path in
  match compile_to_wasm_module callee_face callee_path with
  | Error label -> `Error (false, "callee: " ^ label)
  | Ok callee_mod ->
    match compile_to_wasm_module caller_face caller_path with
    | Error label -> `Error (false, "caller: " ^ label)
    | Ok caller_mod ->
      let iface = Affinescript.Tw_interface.extract_exports callee_mod in
      Format.printf "=== boundary check: callee=%s caller=%s ===@."
        callee_path caller_path;
      match Affinescript.Tw_interface.verify_cross_module iface caller_mod with
      | Ok () ->
        Affinescript.Tw_interface.pp_cross_report Format.std_formatter [];
        `Ok ()
      | Error errs ->
        Affinescript.Tw_interface.pp_cross_report Format.std_formatter errs;
        `Error (false, Printf.sprintf "%d boundary violation(s)"
                         (List.length errs))

let which_arg =
  let which = Arg.enum [
    ("tea-bridge", `TeaBridge);
    ("router",     `Router);
    ("all",        `All);
  ] in
  Arg.(value & opt which `All & info ["which"]
    ~docv:"MODULE"
    ~doc:"Which bridge module to operate on: $(b,tea-bridge), $(b,router), or $(b,all) (default).")

let interface_cmd =
  let doc = "Print the ownership-annotated export interface of a generated bridge module" in
  let man = [
    `S "DESCRIPTION";
    `P "Extracts the $(b,affinescript.ownership) custom section from the selected \
        generated Wasm module and prints the per-export ownership contract.";
    `P "Parameters are annotated: $(b,own) = Linear (consumed exactly once), \
        $(b,ref) = SharedBorrow (read-only), $(b,mut) = ExclBorrow (exclusive \
        mutable reference), $(b,val) = Unrestricted (unconstrained).";
    `P "This is the typed-wasm boundary contract that any caller — JS bridge, \
        a Wasm linker, or a future multi-module composition tool — must honour \
        to preserve Level 7/10 guarantees across the module boundary.";
  ] in
  let info = Cmd.info "interface" ~doc ~man in
  Cmd.v info Term.(ret (const interface_cmd_fn $ which_arg))

let verify_bridge_cmd =
  let doc = "Verify cross-module typed-wasm boundary constraints (internal bridges)" in
  let man = [
    `S "DESCRIPTION";
    `P "Generates the selected internal bridge module (tea-bridge, router, or \
        both), extracts its ownership-annotated export interface, and verifies \
        that a well-formed synthetic caller module (one that imports a \
        Linear-param export and calls it exactly once per execution path) \
        passes Level 7/10 cross-module boundary checking.";
    `P "Use $(b,verify-boundary) instead when the caller and callee are \
        user-authored AffineScript modules rather than the internal IDApTIK \
        bridges.";
    `P "Exit 0 = boundary clean.  Exit 1 = violations found.";
  ] in
  let info = Cmd.info "verify-bridge" ~doc ~man in
  Cmd.v info Term.(ret (const verify_boundary_fn $ which_arg))

(** Positional args for [verify-boundary]: callee first, caller second. *)
let verify_boundary_callee_arg =
  Arg.(required & pos 0 (some file) None
    & info [] ~docv:"CALLEE"
       ~doc:"AffineScript source for the exporter: its ownership-annotated \
             $(b,pub fn) declarations define the contract the caller must \
             honour.")

let verify_boundary_caller_arg =
  Arg.(required & pos 1 (some file) None
    & info [] ~docv:"CALLER"
       ~doc:"AffineScript source for the importer: every local function body \
             is checked against the callee interface using per-path call-count \
             analysis.")

let verify_boundary_cmd =
  let doc = "Verify cross-module typed-wasm boundary constraints between two source files" in
  let man = [
    `S "DESCRIPTION";
    `P "Compiles $(b,CALLEE) and $(b,CALLER) through the full frontend pipeline, \
        extracts $(b,CALLEE)'s ownership-annotated export interface from its \
        $(b,affinescript.ownership) custom section, and then checks every \
        function body in $(b,CALLER) against that interface.";
    `P "For each import that corresponds to a callee export with at least one \
        $(b,own) (Linear) parameter, the verifier counts per-execution-path \
        invocations and reports:";
    `P "$(b,LinearImportCalledMultiple): the import is called more than once \
        on some path. The Linear argument would be duplicated, violating \
        exclusive ownership.";
    `P "$(b,LinearImportDroppedOnSomePath): the import is called on some paths \
        but not others (min=0, max≥1). The Linear argument is silently dropped \
        on the zero-call paths.";
    `P "Imports that never appear in a function body are not flagged — there \
        is no obligation for every function to invoke every import.";
    `P "Exit 0 = boundary clean.  Exit 1 = violations found.";
    `S "EXAMPLES";
    `P "  affinescript verify-boundary lib.affine app.affine";
  ] in
  let info = Cmd.info "verify-boundary" ~doc ~man in
  Cmd.v info Term.(ret (const verify_boundary_files_fn
                          $ face_arg
                          $ verify_boundary_callee_arg
                          $ verify_boundary_caller_arg))

let repl_cmd =
  let doc = "Start the interactive REPL" in
  let info = Cmd.info "repl" ~doc in
  Cmd.v info Term.(ret (const repl_cmd_fn $ const ()))

let compile_cmd =
  let doc = "Compile a file to WebAssembly (1.0 or GC proposal), Julia (.jl), JavaScript (.js), C (.c), or a WGSL kernel (.wgsl)" in
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

let preview_js_cmd =
  let doc = "Preview the JS-face text transform (debug)" in
  let info = Cmd.info "preview-js" ~doc in
  Cmd.v info Term.(ret (const preview_js_transform $ path_arg))

let preview_pseudocode_cmd =
  let doc = "Preview the pseudocode-face text transform (debug)" in
  let info = Cmd.info "preview-pseudocode" ~doc in
  Cmd.v info Term.(ret (const preview_pseudocode_transform $ path_arg))

let preview_lucid_cmd =
  let doc = "Preview the Lucid-face (PureScript) text transform (debug)" in
  let info = Cmd.info "preview-lucid" ~doc in
  Cmd.v info Term.(ret (const preview_lucid_transform $ path_arg))

let preview_cafe_cmd =
  let doc = "Preview the Cafe-face (CoffeeScript) text transform (debug)" in
  let info = Cmd.info "preview-cafe" ~doc in
  Cmd.v info Term.(ret (const preview_cafe_transform $ path_arg))

(** {1 Phase B: hover and goto-definition subcommands}

    Both commands run the full pipeline (parse → resolve → typecheck)
    on the given file, locate the symbol at the cursor position, and
    emit a JSON result on stdout.

    Usage:
      affinescript hover  FILE LINE COL
      affinescript goto-def FILE LINE COL

    Line and column are 1-based integers matching LSP convention.
    Exit 0 whether or not a symbol was found; the [found] field in
    the JSON response indicates presence. *)

(** Shared pipeline runner for hover / goto-def.

    Returns [(symbols, refs)] on success so the caller can query them,
    or [None] if the pipeline failed (in which case an error is printed). *)
let run_pipeline_for_query face path =
  try
    let prog = parse_with_face face path in
    let loader_config = Affinescript.Module_loader.default_config () in
    let loader = Affinescript.Module_loader.create loader_config in
    match Affinescript.Resolve.resolve_program_with_loader prog loader with
    | Error (e, _span) ->
      Format.eprintf "Resolution error: %s@."
        (Affinescript.Resolve.show_resolve_error e);
      None
    | Ok (resolve_ctx, _) ->
      (* Run type checking to populate sym_type fields on the symbol table.
         We intentionally ignore the type error here — hover/goto-def should
         still work on partially-correct programs. *)
      let _tc_result = Affinescript.Typecheck.check_program resolve_ctx.symbols prog in
      let refs = List.rev resolve_ctx.references
                 |> List.map (fun (r : Affinescript.Resolve.reference) ->
                      Affinescript.Json_output.{
                        ref_symbol_id = r.ref_symbol_id;
                        ref_span      = r.ref_span;
                      })
      in
      Some (resolve_ctx.symbols, refs)
  with
  | Affinescript.Lexer.Lexer_error (msg, pos) ->
    Format.eprintf "Lexer error at %d:%d: %s@." pos.line pos.col msg;
    None
  | Affinescript.Parse_driver.Parse_error (msg, _span) ->
    Format.eprintf "Parse error: %s@." msg;
    None

(** Hover subcommand handler. *)
let hover_file face path line col =
  let face = resolve_face ~quiet:true face path in
  (match run_pipeline_for_query face path with
   | None ->
     Affinescript.Json_output.emit_hover None
   | Some (symbols, refs) ->
     let sym = Affinescript.Json_output.find_symbol_at symbols refs line col in
     Affinescript.Json_output.emit_hover sym);
  `Ok ()

(** Goto-definition subcommand handler. *)
let goto_def_file face path line col =
  let face = resolve_face ~quiet:true face path in
  (match run_pipeline_for_query face path with
   | None ->
     Affinescript.Json_output.emit_goto_def None
   | Some (symbols, refs) ->
     let sym = Affinescript.Json_output.find_symbol_at symbols refs line col in
     Affinescript.Json_output.emit_goto_def sym);
  `Ok ()

(** Shared line and column arguments (1-based, LSP convention). *)
let line_arg =
  Arg.(required & pos 1 (some int) None & info [] ~docv:"LINE"
    ~doc:"Cursor line (1-based).")

let col_arg =
  Arg.(required & pos 2 (some int) None & info [] ~docv:"COL"
    ~doc:"Cursor column (1-based).")

(** [hover FILE LINE COL] — return hover info for the symbol at the cursor. *)
let hover_cmd =
  let doc = "Return type information for the symbol at a cursor position" in
  let man = [
    `S Manpage.s_description;
    `P "Runs the full pipeline on FILE, finds the symbol at (LINE, COL), \
        and prints a JSON object on stdout.  If no symbol is found, \
        prints {\"found\": false}.";
    `P "Lines and columns are 1-based integers (LSP convention).";
  ] in
  let info = Cmd.info "hover" ~doc ~man in
  Cmd.v info Term.(ret (const hover_file $ face_arg $ path_arg $ line_arg $ col_arg))

(** [goto-def FILE LINE COL] — return the definition location of the symbol. *)
let goto_def_cmd =
  let doc = "Return the definition location of the symbol at a cursor position" in
  let man = [
    `S Manpage.s_description;
    `P "Runs the full pipeline on FILE, finds the symbol at (LINE, COL), \
        and prints a JSON object with the definition span on stdout.";
    `P "Lines and columns are 1-based integers (LSP convention).";
  ] in
  let info = Cmd.info "goto-def" ~doc ~man in
  Cmd.v info Term.(ret (const goto_def_file $ face_arg $ path_arg $ line_arg $ col_arg))

(** {1 Phase D: LSP server subcommand} *)

(** Start the JSON-RPC LSP server on stdin/stdout.

    Handles the full lifecycle: initialize → document sync → hover /
    definition / completion → shutdown / exit.  The server runs
    entirely in-process; no child processes are spawned. *)
let server_file () =
  Affinescript.Lsp_server.run ();
  `Ok ()

let server_cmd =
  let doc = "Start the JSON-RPC LSP server on stdin/stdout" in
  let man = [
    `S Manpage.s_description;
    `P "Starts the AffineScript Language Server Protocol server. \
        Communicates over stdin/stdout using JSON-RPC 2.0 with \
        Content-Length framing (LSP 3.17 subset).";
    `P "Capabilities: full document sync, hover, go-to-definition, \
        completion with keyword candidates.";
    `P "Configure your editor's LSP client to run: \
        $(b,affinescript server --stdio)";
  ] in
  let info = Cmd.info "server" ~doc ~man in
  Cmd.v info Term.(ret (const server_file $ const ()))

(** {1 Phase C: completion subcommand} *)

(** Complete subcommand handler.

    Reads the source file, extracts the identifier prefix at (line, col),
    resolves the program to build the symbol table, and emits a JSON array
    of completion candidates on stdout.  Emits an empty array on pipeline
    failure so the editor doesn't break. *)
let complete_file face path line col =
  let face = resolve_face ~quiet:true face path in
  let source = read_file path in
  (match run_pipeline_for_query face path with
   | None ->
     Affinescript.Json_output.emit_completions []
   | Some (symbols, _refs) ->
     let (prefix, dot_ctx) =
       Affinescript.Json_output.extract_prefix_at source line col
     in
     let items =
       Affinescript.Json_output.collect_completions symbols prefix dot_ctx
     in
     Affinescript.Json_output.emit_completions items);
  `Ok ()

(** [complete FILE LINE COL] — return completion candidates at cursor. *)
let complete_cmd =
  let doc = "Return completion candidates at a cursor position" in
  let man = [
    `S Manpage.s_description;
    `P "Extracts the identifier prefix at (LINE, COL), filters the symbol \
        table by prefix match, and prints a JSON array of completion \
        candidates on stdout.  Each item has {name, kind, type, detail}.";
    `P "Lines and columns are 1-based integers (LSP convention).";
  ] in
  let info = Cmd.info "complete" ~doc ~man in
  Cmd.v info Term.(ret (const complete_file $ face_arg $ path_arg $ line_arg $ col_arg))

let default_cmd =
  let doc = "The AffineScript compiler" in
  let info = Cmd.info "affinescript" ~version ~doc in
  let default = Term.(ret (const (`Help (`Pager, None)))) in
  Cmd.group info ~default [
    lex_cmd; parse_cmd; check_cmd; eval_cmd; repl_cmd; compile_cmd;
    fmt_cmd; lint_cmd;
    tea_bridge_cmd; router_bridge_cmd; cs_bridge_cmd; verify_cmd;
    interface_cmd; verify_bridge_cmd; verify_boundary_cmd;
    hover_cmd; goto_def_cmd; complete_cmd; server_cmd;
    preview_python_cmd; preview_js_cmd; preview_pseudocode_cmd;
    preview_lucid_cmd; preview_cafe_cmd
  ]

let () = exit (Cmd.eval default_cmd)
