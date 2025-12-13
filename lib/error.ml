(** Error handling and diagnostics for AffineScript *)

(** Error severity levels *)
type severity =
  | Error
  | Warning
  | Info
  | Hint
[@@deriving show, eq]

(** Error codes *)
type code =
  (* Lexer errors: E0001-E0099 *)
  | E0001  (** Unexpected character *)
  | E0002  (** Unterminated string *)
  | E0003  (** Unterminated comment *)
  | E0004  (** Invalid escape sequence *)
  | E0005  (** Invalid character literal *)

  (* Parser errors: E0100-E0199 *)
  | E0101  (** Unexpected token *)
  | E0102  (** Expected expression *)
  | E0103  (** Expected type *)
  | E0104  (** Expected pattern *)
  | E0105  (** Expected identifier *)
  | E0106  (** Unclosed delimiter *)

  (* Name resolution errors: E0200-E0299 *)
  | E0201  (** Undefined variable *)
  | E0202  (** Undefined type *)
  | E0203  (** Undefined module *)
  | E0204  (** Duplicate definition *)
  | E0205  (** Private item *)

  (* Type errors: E0300-E0399 *)
  | E0301  (** Type mismatch *)
  | E0302  (** Cannot infer type *)
  | E0303  (** Invalid type application *)
  | E0304  (** Missing type annotation *)
  | E0305  (** Refinement not satisfied *)
  | E0306  (** Kind mismatch *)
  | E0307  (** Row field missing *)
  | E0308  (** Row field duplicate *)

  (* Trait errors: E0400-E0499 *)
  | E0401  (** Trait not implemented *)
  | E0402  (** Missing trait method *)
  | E0403  (** Conflicting implementations *)
  | E0404  (** Orphan implementation *)

  (* Ownership errors: E0500-E0599 *)
  | E0501  (** Use after move *)
  | E0502  (** Cannot borrow as mutable *)
  | E0503  (** Cannot borrow while mutable borrow exists *)
  | E0504  (** Value does not live long enough *)
  | E0505  (** Cannot move out of borrowed reference *)
  | E0506  (** Linearity violation *)

  (* Effect errors: E0600-E0699 *)
  | E0601  (** Effect not handled *)
  | E0602  (** Effect mismatch *)
  | E0603  (** Invalid effect in total function *)

  (* Totality errors: E0700-E0799 *)
  | E0701  (** Non-terminating recursion *)
  | E0702  (** Non-exhaustive patterns *)

  (* Warnings: W0001-W0999 *)
  | W0001  (** Unused variable *)
  | W0002  (** Unused import *)
  | W0003  (** Unreachable code *)
  | W0004  (** Deprecated feature *)
  | W0601  (** Owned resource may leak on exception *)
  | W0701  (** Effectful refinement predicate *)
[@@deriving show, eq]

(** Convert error code to string *)
let code_to_string = function
  | E0001 -> "E0001" | E0002 -> "E0002" | E0003 -> "E0003"
  | E0004 -> "E0004" | E0005 -> "E0005"
  | E0101 -> "E0101" | E0102 -> "E0102" | E0103 -> "E0103"
  | E0104 -> "E0104" | E0105 -> "E0105" | E0106 -> "E0106"
  | E0201 -> "E0201" | E0202 -> "E0202" | E0203 -> "E0203"
  | E0204 -> "E0204" | E0205 -> "E0205"
  | E0301 -> "E0301" | E0302 -> "E0302" | E0303 -> "E0303"
  | E0304 -> "E0304" | E0305 -> "E0305" | E0306 -> "E0306"
  | E0307 -> "E0307" | E0308 -> "E0308"
  | E0401 -> "E0401" | E0402 -> "E0402" | E0403 -> "E0403"
  | E0404 -> "E0404"
  | E0501 -> "E0501" | E0502 -> "E0502" | E0503 -> "E0503"
  | E0504 -> "E0504" | E0505 -> "E0505" | E0506 -> "E0506"
  | E0601 -> "E0601" | E0602 -> "E0602" | E0603 -> "E0603"
  | E0701 -> "E0701" | E0702 -> "E0702"
  | W0001 -> "W0001" | W0002 -> "W0002" | W0003 -> "W0003"
  | W0004 -> "W0004" | W0601 -> "W0601" | W0701 -> "W0701"

(** A labeled span for error messages *)
type label = {
  span : Span.t;
  message : string;
  primary : bool;  (** Is this the primary span? *)
}

(** A diagnostic message *)
type diagnostic = {
  severity : severity;
  code : code;
  message : string;
  labels : label list;
  notes : string list;
  help : string list;
}

(** Create a new error *)
let error ~code ~message ~span =
  {
    severity = Error;
    code;
    message;
    labels = [{ span; message = ""; primary = true }];
    notes = [];
    help = [];
  }

(** Create a new warning *)
let warning ~code ~message ~span =
  {
    severity = Warning;
    code;
    message;
    labels = [{ span; message = ""; primary = true }];
    notes = [];
    help = [];
  }

(** Add a secondary label *)
let with_label ~span ~message diag =
  { diag with labels = diag.labels @ [{ span; message; primary = false }] }

(** Add a note *)
let with_note note diag =
  { diag with notes = diag.notes @ [note] }

(** Add help text *)
let with_help help diag =
  { diag with help = diag.help @ [help] }

(** Format a diagnostic for terminal output *)
let format_diagnostic ~source diag =
  let severity_str = match diag.severity with
    | Error -> "\027[1;31merror\027[0m"
    | Warning -> "\027[1;33mwarning\027[0m"
    | Info -> "\027[1;34minfo\027[0m"
    | Hint -> "\027[1;36mhint\027[0m"
  in
  let code_str = code_to_string diag.code in
  let buf = Buffer.create 256 in

  (* Header *)
  Buffer.add_string buf (Printf.sprintf "%s[%s]: %s\n" severity_str code_str diag.message);

  (* Labels *)
  List.iter (fun label ->
    let span = label.span in
    Buffer.add_string buf (Printf.sprintf "  --> %s:%d:%d\n"
      span.file span.start_pos.line span.start_pos.col);

    (* Show source line if available *)
    (match source with
    | Some src ->
      let lines = String.split_on_char '\n' src in
      if span.start_pos.line <= List.length lines then begin
        let line = List.nth lines (span.start_pos.line - 1) in
        let line_num = string_of_int span.start_pos.line in
        let padding = String.make (String.length line_num) ' ' in
        Buffer.add_string buf (Printf.sprintf "   %s |\n" padding);
        Buffer.add_string buf (Printf.sprintf " %s | %s\n" line_num line);
        Buffer.add_string buf (Printf.sprintf "   %s | %s%s"
          padding
          (String.make (span.start_pos.col - 1) ' ')
          (String.make (max 1 (span.end_pos.col - span.start_pos.col)) '^'));
        if label.message <> "" then
          Buffer.add_string buf (Printf.sprintf " %s" label.message);
        Buffer.add_char buf '\n';
      end
    | None -> ());
  ) diag.labels;

  (* Notes *)
  List.iter (fun note ->
    Buffer.add_string buf (Printf.sprintf "   = note: %s\n" note)
  ) diag.notes;

  (* Help *)
  List.iter (fun help ->
    Buffer.add_string buf (Printf.sprintf "   = help: %s\n" help)
  ) diag.help;

  Buffer.contents buf

(** Diagnostic collector *)
type collector = {
  mutable diagnostics : diagnostic list;
  mutable has_errors : bool;
}

let create_collector () = { diagnostics = []; has_errors = false }

let emit collector diag =
  collector.diagnostics <- collector.diagnostics @ [diag];
  if diag.severity = Error then collector.has_errors <- true

let has_errors collector = collector.has_errors

let get_diagnostics collector = collector.diagnostics
