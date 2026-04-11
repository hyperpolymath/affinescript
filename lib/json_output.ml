(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2024-2026 Jonathan D.A. Jewell (hyperpolymath) *)

(** JSON output for machine-readable compiler diagnostics.

    Phase A of the AffineScript LSP integration: structured JSON output
    from the compiler CLI via [--json], enabling the LSP (and other tools)
    to consume diagnostics without fragile regex parsing.

    Output schema (one JSON object per line on stderr):
    {[
      {
        "version": 1,
        "diagnostics": [
          {
            "severity": "error" | "warning" | "hint" | "info",
            "code": "E0001",
            "message": "...",
            "file": "path/to/file.affine",
            "start_line": 1,
            "start_col": 1,
            "end_line": 1,
            "end_col": 10,
            "help": "..." | null,
            "labels": [
              { "file": "...", "start_line": 1, "start_col": 1,
                "end_line": 1, "end_col": 5, "message": "..." }
            ]
          }
        ],
        "success": true | false
      }
    ]}
*)

(** {1 Span serialization} *)

(** Convert a [Span.pos] to a JSON object. *)
let pos_to_json (p : Span.pos) : Yojson.Basic.t =
  `Assoc [
    ("line", `Int p.line);
    ("col", `Int p.col);
    ("offset", `Int p.offset);
  ]

(** Convert a [Span.t] to a JSON object with flat start/end fields. *)
let span_to_json (s : Span.t) : (string * Yojson.Basic.t) list =
  [
    ("file", `String s.file);
    ("start_line", `Int s.start_pos.line);
    ("start_col", `Int s.start_pos.col);
    ("end_line", `Int s.end_pos.line);
    ("end_col", `Int s.end_pos.col);
  ]

(** {1 Unified diagnostic type} *)

(** Severity levels matching the LSP spec. *)
type severity = Error | Warning | Hint | Info

(** A single compiler diagnostic in a tool-friendly format. *)
type diagnostic = {
  severity : severity;
  code : string;
  message : string;
  span : Span.t;
  help : string option;
  labels : (Span.t * string) list;
}

(** Convert severity to its JSON string representation. *)
let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Hint -> "hint"
  | Info -> "info"

(** Serialize a single diagnostic to JSON. *)
let diagnostic_to_json (d : diagnostic) : Yojson.Basic.t =
  let labels_json = `List (List.map (fun (s, msg) ->
    `Assoc (span_to_json s @ [("message", `String msg)])
  ) d.labels) in
  `Assoc (
    [
      ("severity", `String (severity_to_string d.severity));
      ("code", `String d.code);
      ("message", `String d.message);
    ]
    @ span_to_json d.span
    @ [
      ("help", match d.help with Some h -> `String h | None -> `Null);
      ("labels", labels_json);
    ]
  )

(** {1 Diagnostic envelope} *)

(** Serialize a complete diagnostic report (the top-level JSON object). *)
let report_to_json ~(success : bool) (diags : diagnostic list) : Yojson.Basic.t =
  `Assoc [
    ("version", `Int 1);
    ("diagnostics", `List (List.map diagnostic_to_json diags));
    ("success", `Bool success);
  ]

(** Write the JSON report to stderr. *)
let emit_report ~(success : bool) (diags : diagnostic list) : unit =
  let json = report_to_json ~success diags in
  Format.eprintf "%s@." (Yojson.Basic.to_string json)

(** {1 Converters from compiler error types} *)

(** Convert a lexer error to a diagnostic. *)
let of_lexer_error (msg : string) (pos : Span.pos) (file : string) : diagnostic =
  let span = Span.make ~file
    ~start_pos:pos
    ~end_pos:{ pos with col = pos.col + 1 }
  in
  {
    severity = Error;
    code = "E0001";
    message = msg;
    span;
    help = None;
    labels = [];
  }

(** Convert a parse error to a diagnostic. *)
let of_parse_error (msg : string) (span : Span.t) : diagnostic =
  {
    severity = Error;
    code = "E0100";
    message = msg;
    span;
    help = None;
    labels = [];
  }

(** Convert a resolve error to a diagnostic.

    [Resolve.resolve_error] variants carry [Ast.ident] values which
    contain both a name and a span.  We use the ident's span when
    available (it points to the exact token), falling back to the
    caller-supplied [span] for variants that carry only a [string]. *)
let of_resolve_error (e : Resolve.resolve_error) (span : Span.t) : diagnostic =
  let code, message, help, diag_span = match e with
    | Resolve.UndefinedVariable id ->
      "E0500", Printf.sprintf "Undefined variable: %s" id.Ast.name,
      Some "Check spelling or add an import", id.Ast.span
    | Resolve.UndefinedType id ->
      "E0501", Printf.sprintf "Undefined type: %s" id.Ast.name,
      Some "Check spelling or add an import", id.Ast.span
    | Resolve.UndefinedEffect id ->
      "E0502", Printf.sprintf "Undefined effect: %s" id.Ast.name,
      None, id.Ast.span
    | Resolve.UndefinedModule id ->
      "E0503", Printf.sprintf "Undefined module: %s" id.Ast.name,
      Some "Check that the module file exists", id.Ast.span
    | Resolve.DuplicateDefinition id ->
      "E0504", Printf.sprintf "Duplicate definition: %s" id.Ast.name,
      None, id.Ast.span
    | Resolve.VisibilityError (id, reason) ->
      "E0505", Printf.sprintf "Visibility error for %s: %s" id.Ast.name reason,
      None, id.Ast.span
    | Resolve.ImportError msg ->
      "E0506", Printf.sprintf "Import error: %s" msg, None, span
  in
  {
    severity = Error;
    code;
    message;
    span = diag_span;
    help;
    labels = [];
  }

(** Convert a type error to a diagnostic.

    Type errors currently lack span information in the compiler, so we
    use [Span.dummy] as a placeholder. Phase B will thread spans through
    the type checker. *)
let of_type_error (e : Typecheck.type_error) : diagnostic =
  let code, message, help = match e with
    | Typecheck.UnboundVariable v ->
      "E0100", Printf.sprintf "Unbound variable: %s" v,
      Some "Check spelling or add an import"
    | Typecheck.TypeMismatch { expected; got } ->
      "E0101",
      Printf.sprintf "Type mismatch: expected %s, got %s"
        (Types.show_ty expected) (Types.show_ty got),
      None
    | Typecheck.OccursCheck (v, ty) ->
      "E0102",
      Printf.sprintf "Infinite type: %s occurs in %s" v (Types.show_ty ty),
      Some "This usually means a recursive type without proper boxing"
    | Typecheck.NotImplemented msg ->
      "E0199", Printf.sprintf "Not implemented: %s" msg, None
    | Typecheck.ArityMismatch { name; expected; got } ->
      "E0103", Printf.sprintf "Function %s expects %d arguments, got %d" name expected got, None
    | Typecheck.NotAFunction ty ->
      "E0104", Printf.sprintf "Expected a function, got %s" (Types.show_ty ty), None
    | Typecheck.FieldNotFound { field; record_ty } ->
      "E0105", Printf.sprintf "Field '%s' not found in %s" field (Types.show_ty record_ty), None
    | Typecheck.TupleIndexOutOfBounds { index; length } ->
      "E0106", Printf.sprintf "Tuple index %d out of bounds (length %d)" index length, None
    | Typecheck.DuplicateField f ->
      "E0107", Printf.sprintf "Duplicate field: %s" f, None
    | Typecheck.UnificationError ue ->
      "E0108", Printf.sprintf "Type error: %s" (Unify.show_unify_error ue), None
    | Typecheck.PatternTypeMismatch msg ->
      "E0109", Printf.sprintf "Pattern mismatch: %s" msg, None
    | Typecheck.BranchTypeMismatch { then_ty; else_ty } ->
      "E0110", Printf.sprintf "Branch mismatch: then %s, else %s"
        (Types.show_ty then_ty) (Types.show_ty else_ty), None
    | Typecheck.QuantityError (qerr, _span) ->
      begin match qerr with
      | Quantity.LinearVariableUnused id ->
        "E0300",
        Printf.sprintf "Linear variable '%s' must be used exactly once, but was never used"
          id.Ast.name,
        Some "Remove the quantity annotation or use the variable"
      | Quantity.LinearVariableUsedMultiple id ->
        "E0301",
        Printf.sprintf "Linear variable '%s' must be used exactly once, but was used multiple times"
          id.Ast.name,
        Some "Clone the value or change the quantity to omega"
      | Quantity.ErasedVariableUsed id ->
        "E0302",
        Printf.sprintf "Erased variable '%s' (quantity 0) must not be used at runtime"
          id.Ast.name,
        Some "Erased parameters exist only for type-level reasoning"
      | Quantity.QuantityMismatch _ ->
        "E0303",
        Quantity.format_quantity_error qerr,
        None
      end
  in
  {
    severity = Error;
    code;
    message;
    span = Span.dummy;
    help;
    labels = [];
  }

(** Convert a borrow error to a diagnostic. *)
let of_borrow_error (e : Borrow.borrow_error) : diagnostic =
  let code, message, help = match e with
    | Borrow.UseAfterMove _ ->
      "E0501", Borrow.format_borrow_error e,
      Some "Each owned value may be used only once; clone it if you need a copy"
    | Borrow.ConflictingBorrow _ ->
      "E0502", Borrow.format_borrow_error e,
      Some "Cannot have both a mutable and an immutable borrow active at the same time"
    | Borrow.BorrowOutlivesOwner _ ->
      "E0503", Borrow.format_borrow_error e,
      Some "The borrow must not outlive the variable it borrows from"
    | Borrow.MoveWhileBorrowed _ ->
      "E0504", Borrow.format_borrow_error e,
      Some "End all borrows before moving the value"
    | Borrow.CannotMoveOutOfBorrow _ ->
      "E0505", Borrow.format_borrow_error e,
      Some "Dereference the reference and clone the value, or take ownership another way"
    | Borrow.CannotBorrowAsMutable _ ->
      "E0506", Borrow.format_borrow_error e,
      Some "Declare the binding with `let mut` to allow mutable borrows"
  in
  { severity = Error; code; message; span = Span.dummy; help; labels = [] }

(** Convert an eval error to a diagnostic. *)
let of_eval_error (e : Value.eval_error) : diagnostic =
  let code, message = match e with
    | Value.UnboundVariable v ->
      "E0700", Printf.sprintf "Runtime: unbound variable %s" v
    | Value.TypeMismatch msg ->
      "E0701", Printf.sprintf "Runtime type mismatch: %s" msg
    | Value.DivisionByZero ->
      "E0702", "Division by zero"
    | Value.IndexOutOfBounds (idx, len) ->
      "E0703", Printf.sprintf "Index %d out of bounds (length %d)" idx len
    | Value.FieldNotFound f ->
      "E0704", Printf.sprintf "Field not found: %s" f
    | Value.PatternMatchFailure ->
      "E0705", "Non-exhaustive pattern match"
    | Value.AffineViolation msg ->
      "E0706", Printf.sprintf "Affine violation: %s" msg
    | Value.RuntimeError msg ->
      "E0799", Printf.sprintf "Runtime error: %s" msg
    | Value.PerformEffect (name, _args) ->
      "E0710", Printf.sprintf "Unhandled effect: %s" name
  in
  {
    severity = Error;
    code;
    message;
    span = Span.dummy;
    help = None;
    labels = [];
  }

(** Convert a quantity error to a diagnostic.

    Quantity errors use error codes E0300-E0399 (QTT checking range). *)
let of_quantity_error ((err, span) : Quantity.quantity_error * Span.t) : diagnostic =
  let code, message, help = match err with
    | Quantity.LinearVariableUnused id ->
      "E0300",
      Printf.sprintf "Linear variable '%s' must be used exactly once, but was never used"
        id.name,
      Some "Remove the quantity annotation or use the variable"
    | Quantity.LinearVariableUsedMultiple id ->
      "E0301",
      Printf.sprintf "Linear variable '%s' must be used exactly once, but was used multiple times"
        id.name,
      Some "Clone the value or change the quantity to omega"
    | Quantity.ErasedVariableUsed id ->
      "E0302",
      Printf.sprintf "Erased variable '%s' (quantity 0) must not be used at runtime"
        id.name,
      Some "Erased parameters exist only for type-level reasoning"
    | Quantity.QuantityMismatch (_id, _q, _u) ->
      "E0303",
      Quantity.format_quantity_error err,
      None
  in
  {
    severity = Error;
    code;
    message;
    span;
    help;
    labels = [];
  }

(** Convert a linter diagnostic to our unified format. *)
let of_lint_diagnostic (d : Linter.diagnostic) : diagnostic =
  let severity = match d.severity with
    | Linter.Error -> Error
    | Linter.Warning -> Warning
    | Linter.Hint -> Hint
    | Linter.Info -> Info
  in
  {
    severity;
    code = d.code;
    message = d.message;
    span = d.span;
    help = d.help;
    labels = [];
  }

(** {1 Phase B: Symbol table and references for goto-def/find-refs}

    When [version >= 2], the JSON output includes a [symbols] array
    and a [references] map, enabling the LSP to provide goto-definition,
    find-references, and rename without a second compiler invocation.

    Schema additions:
    {[
      {
        "version": 2,
        ...existing fields...,
        "symbols": [
          {
            "id": 0,
            "name": "foo",
            "kind": "function",
            "file": "path/to/file.affine",
            "start_line": 1, "start_col": 4,
            "end_line": 1, "end_col": 7,
            "type": "Int -> Int" | null,
            "quantity": "linear" | "affine" | "unrestricted" | null
          }
        ],
        "references": {
          "0": [
            { "file": "...", "start_line": 5, "start_col": 10,
              "end_line": 5, "end_col": 13 }
          ]
        }
      }
    ]}
*)

(** Convert a symbol kind to its JSON string representation. *)
let symbol_kind_to_string (kind : Symbol.symbol_kind) : string =
  match kind with
  | Symbol.SKVariable -> "variable"
  | Symbol.SKFunction -> "function"
  | Symbol.SKType -> "type"
  | Symbol.SKTypeVar -> "type_variable"
  | Symbol.SKEffect -> "effect"
  | Symbol.SKEffectOp -> "effect_operation"
  | Symbol.SKTrait -> "trait"
  | Symbol.SKModule -> "module"
  | Symbol.SKConstructor -> "constructor"

(** Serialize a symbol to JSON. *)
let symbol_to_json (sym : Symbol.symbol) : Yojson.Basic.t =
  let type_json = match sym.sym_type with
    | Some ty -> `String (Ast.show_type_expr ty)
    | None -> `Null
  in
  let quantity_json = match sym.sym_quantity with
    | Some q -> `String (Ast.show_quantity q)
    | None -> `Null
  in
  `Assoc (
    [
      ("id", `Int sym.sym_id);
      ("name", `String sym.sym_name);
      ("kind", `String (symbol_kind_to_string sym.sym_kind));
    ]
    @ span_to_json sym.sym_span
    @ [
      ("type", type_json);
      ("quantity", quantity_json);
    ]
  )

(** Serialize all symbols from a symbol table to a JSON array. *)
let symbols_to_json (table : Symbol.t) : Yojson.Basic.t =
  let syms = ref [] in
  Hashtbl.iter (fun _id sym -> syms := sym :: !syms) table.all_symbols;
  let sorted = List.sort (fun a b -> compare a.Symbol.sym_id b.Symbol.sym_id) !syms in
  `List (List.map symbol_to_json sorted)

(** A reference is a use-site span for a symbol. *)
type reference = {
  ref_symbol_id : int;
  ref_span : Span.t;
}

(** Serialize references to JSON: { "symbol_id": [ spans... ] } *)
let references_to_json (refs : reference list) : Yojson.Basic.t =
  let by_id = Hashtbl.create 64 in
  List.iter (fun r ->
    let existing = try Hashtbl.find by_id r.ref_symbol_id with Not_found -> [] in
    Hashtbl.replace by_id r.ref_symbol_id (r.ref_span :: existing)
  ) refs;
  let entries = ref [] in
  Hashtbl.iter (fun id spans ->
    let span_jsons = List.map (fun s ->
      `Assoc (span_to_json s)
    ) (List.rev spans) in
    entries := (string_of_int id, `List span_jsons) :: !entries
  ) by_id;
  `Assoc (List.sort compare !entries)

(** Phase B report: diagnostics + symbol table + references. *)
let report_v2_to_json ~(success : bool) (diags : diagnostic list)
    (symbols : Symbol.t) (refs : reference list) : Yojson.Basic.t =
  `Assoc [
    ("version", `Int 2);
    ("diagnostics", `List (List.map diagnostic_to_json diags));
    ("success", `Bool success);
    ("symbols", symbols_to_json symbols);
    ("references", references_to_json refs);
  ]

(** Emit a Phase B JSON report to stderr. *)
let emit_report_v2 ~(success : bool) (diags : diagnostic list)
    (symbols : Symbol.t) (refs : reference list) : unit =
  let json = report_v2_to_json ~success diags symbols refs in
  Format.eprintf "%s@." (Yojson.Basic.to_string json)

(** {1 Phase B: Hover and goto-definition queries}

    These entry points power the LSP hover and go-to-definition features.
    Given a file path and cursor position (1-based line/col), the compiler
    re-runs the pipeline and answers the query from the resolved symbol table.

    Output schema for hover:
    {[
      { "found": true,
        "name": "foo",
        "kind": "function",
        "type": "Int -> Int" | null,
        "quantity": "linear" | null,
        "def_file": "src/lib.affine",
        "def_start_line": 3, "def_start_col": 1,
        "def_end_line":   3, "def_end_col":   4 }
      | { "found": false }
    ]}

    Output schema for goto-definition:
    {[
      { "found": true,
        "file": "src/lib.affine",
        "start_line": 3, "start_col": 1,
        "end_line":   3, "end_col":   4 }
      | { "found": false }
    ]}
*)

(** Check whether a span contains a given (line, col) position.
    Lines and columns are 1-based, matching LSP convention. *)
let span_contains (span : Span.t) (line : int) (col : int) : bool =
  let sl = span.start_pos.Span.line in
  let sc = span.start_pos.Span.col  in
  let el = span.end_pos.Span.line   in
  let ec = span.end_pos.Span.col    in
  if sl = el then
    (* Single-line span *)
    sl = line && sc <= col && col <= ec
  else
    (* Multi-line span *)
    (line = sl && col >= sc)
    || (line > sl && line < el)
    || (line = el && col <= ec)

(** Find the symbol whose definition or use-site span covers the
    given position.  References are checked first (they are smaller
    spans and therefore more precise); then definition spans. *)
let find_symbol_at
    (symbols : Symbol.t)
    (refs    : reference list)
    (line    : int)
    (col     : int)
    : Symbol.symbol option =
  (* 1. Search references (use-sites). *)
  let via_ref =
    List.find_opt (fun (r : reference) -> span_contains r.ref_span line col) refs
  in
  begin match via_ref with
  | Some r ->
    Hashtbl.find_opt symbols.Symbol.all_symbols r.ref_symbol_id
  | None ->
    (* 2. Fallback: search definition spans. *)
    let result = ref None in
    Hashtbl.iter (fun _id (sym : Symbol.symbol) ->
      if !result = None && span_contains sym.sym_span line col then
        result := Some sym
    ) symbols.Symbol.all_symbols;
    !result
  end

(** Serialize a hover result to JSON. *)
let hover_to_json (sym : Symbol.symbol) : Yojson.Basic.t =
  let type_str = match sym.sym_type with
    | Some te -> `String (Ast.show_type_expr te)
    | None    -> `Null
  in
  let qty_str = match sym.sym_quantity with
    | Some q -> `String (Ast.show_quantity q)
    | None   -> `Null
  in
  (* Prefix each span field with "def_" so the definition location is
     distinguished from the cursor position in the response. *)
  let def_span_fields =
    List.map (fun (k, v) -> ("def_" ^ k, v)) (span_to_json sym.sym_span)
  in
  `Assoc (
    [ ("found",    `Bool true);
      ("name",     `String sym.sym_name);
      ("kind",     `String (symbol_kind_to_string sym.sym_kind));
      ("type",     type_str);
      ("quantity", qty_str);
    ]
    @ def_span_fields
  )

(** Serialize a goto-definition result to JSON. *)
let goto_def_to_json (sym : Symbol.symbol) : Yojson.Basic.t =
  `Assoc (
    [("found", `Bool true)]
    @ span_to_json sym.sym_span
  )

(** Serialize a "not found" result. *)
let not_found_json : Yojson.Basic.t = `Assoc [("found", `Bool false)]

(** Emit a hover JSON response on stdout. *)
let emit_hover (sym_opt : Symbol.symbol option) : unit =
  let json = match sym_opt with
    | Some sym -> hover_to_json sym
    | None     -> not_found_json
  in
  print_string (Yojson.Basic.to_string json);
  print_newline ()

(** Emit a goto-definition JSON response on stdout. *)
let emit_goto_def (sym_opt : Symbol.symbol option) : unit =
  let json = match sym_opt with
    | Some sym -> goto_def_to_json sym
    | None     -> not_found_json
  in
  print_string (Yojson.Basic.to_string json);
  print_newline ()

(** {1 Phase C: Completion candidates}

    Given a file path and cursor position (1-based), the [complete] subcommand
    extracts the identifier prefix before the cursor, filters the symbol table
    by that prefix, and emits a JSON array of completion candidates.

    Output schema:
    {[
      [
        { "name": "add",
          "kind": "function",
          "type": "Int -> Int -> Int" | null,
          "detail": "function" }
      ]
    ]}

    Keywords are included as candidates with kind ["keyword"] and null type
    unless the cursor is in a dot-access context (e.g. [record.fie|]).
*)

(** AffineScript keywords eligible for completion. *)
let affine_keywords : string list = [
  "fn"; "let"; "match"; "if"; "else"; "return";
  "effect"; "handle"; "trait"; "impl"; "type"; "where";
  "forall"; "use"; "pub"; "mut"; "true"; "false";
  "Self"; "Int"; "Bool"; "Float"; "String"; "Unit";
]

(** Extract the identifier prefix ending at column [col] on [line] in [source].

    Scans backward from the character just before [col] (1-based) collecting
    [a-zA-Z0-9_] characters.  Returns [(prefix, dot_ctx)] where [dot_ctx] is
    [true] when the character immediately preceding the prefix is ['.'],
    indicating a field-access / method completion context. *)
let extract_prefix_at (source : string) (line : int) (col : int) : string * bool =
  let lines = String.split_on_char '\n' source in
  match List.nth_opt lines (line - 1) with
  | None -> ("", false)
  | Some line_text ->
    (* col is 1-based; the character to the left of the cursor is at index col-2. *)
    let end_idx = min (col - 2) (String.length line_text - 1) in
    if end_idx < 0 then ("", false)
    else begin
      let is_ident_char c =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9') || c = '_'
      in
      (* Collect identifier chars in reverse order. *)
      let buf = Buffer.create 16 in
      let i = ref end_idx in
      while !i >= 0 && is_ident_char line_text.[!i] do
        Buffer.add_char buf line_text.[!i];
        decr i
      done;
      (* Reverse to get forward order. *)
      let rev_s = Buffer.contents buf in
      let n = String.length rev_s in
      let prefix = String.init n (fun k -> rev_s.[n - 1 - k]) in
      let preceded_by_dot = !i >= 0 && line_text.[!i] = '.' in
      (prefix, preceded_by_dot)
    end

(** A single completion candidate. *)
type completion_item = {
  comp_name   : string;
  comp_kind   : string;
  comp_type   : string option;
  comp_detail : string;
}

(** Serialize a completion item to JSON. *)
let completion_item_to_json (item : completion_item) : Yojson.Basic.t =
  `Assoc [
    ("name",   `String item.comp_name);
    ("kind",   `String item.comp_kind);
    ("type",   (match item.comp_type with Some t -> `String t | None -> `Null));
    ("detail", `String item.comp_detail);
  ]

(** Collect completion candidates from [symbols] whose name begins with [prefix].

    All symbols match when [prefix] is empty.  AffineScript keywords are
    appended after symbol candidates unless [dot_ctx] is [true] (field-access
    position — keywords don't apply there). *)
let collect_completions
    (symbols  : Symbol.t)
    (prefix   : string)
    (dot_ctx  : bool)
    : completion_item list =
  let prefix_len = String.length prefix in
  let starts_with name =
    String.length name >= prefix_len
    && String.sub name 0 prefix_len = prefix
  in
  (* Symbol candidates. *)
  let sym_items = ref [] in
  Hashtbl.iter (fun _id (sym : Symbol.symbol) ->
    if starts_with sym.sym_name then begin
      let kind = symbol_kind_to_string sym.sym_kind in
      let type_str = match sym.sym_type with
        | Some te -> Some (Ast.show_type_expr te)
        | None    -> None
      in
      sym_items := {
        comp_name   = sym.sym_name;
        comp_kind   = kind;
        comp_type   = type_str;
        comp_detail = kind;
      } :: !sym_items
    end
  ) symbols.Symbol.all_symbols;
  let sym_sorted =
    List.sort (fun a b -> compare a.comp_name b.comp_name) !sym_items
  in
  (* Keyword candidates (not in dot-access context). *)
  let keyword_items =
    if dot_ctx then []
    else
      List.filter_map (fun kw ->
        if starts_with kw then
          Some { comp_name = kw; comp_kind = "keyword";
                 comp_type = None; comp_detail = "keyword" }
        else None
      ) affine_keywords
  in
  sym_sorted @ keyword_items

(** Emit a completion JSON array on stdout. *)
let emit_completions (items : completion_item list) : unit =
  let json = `List (List.map completion_item_to_json items) in
  print_string (Yojson.Basic.to_string json);
  print_newline ()
