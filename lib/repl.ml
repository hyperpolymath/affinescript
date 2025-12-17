(** Interactive REPL for AffineScript *)

(** REPL state *)
type state = {
  env: Value.env;
  mutable counter: int;
  mutable debug: bool;
  mutable multiline: bool;
  mutable buffer: string;
}

let create_state () = {
  env = Value.empty_env ();
  counter = 0;
  debug = false;
  multiline = false;
  buffer = "";
}

(** Built-in functions for the REPL environment *)
let add_builtins env =
  (* print: any -> () *)
  Value.bind env "print" (Value.VBuiltin ("print", fun args ->
    List.iter (fun v -> Format.printf "%s@." (Value.show v)) args;
    Value.VUnit
  )) ~mutable_:false ~linear:false;

  (* println: any -> () *)
  Value.bind env "println" (Value.VBuiltin ("println", fun args ->
    List.iter (fun v -> Format.printf "%s@." (Value.show v)) args;
    Value.VUnit
  )) ~mutable_:false ~linear:false;

  (* str: any -> String *)
  Value.bind env "str" (Value.VBuiltin ("str", fun args ->
    match args with
    | [v] -> Value.VString (Value.show v)
    | _ -> failwith "str expects 1 argument"
  )) ~mutable_:false ~linear:false;

  (* len: array|string|tuple -> Int *)
  Value.bind env "len" (Value.VBuiltin ("len", fun args ->
    match args with
    | [Value.VArray a] -> Value.VInt (Array.length a)
    | [Value.VString s] -> Value.VInt (String.length s)
    | [Value.VTuple t] -> Value.VInt (List.length t)
    | [Value.VRecord r] -> Value.VInt (List.length r)
    | _ -> failwith "len expects an array, string, tuple, or record"
  )) ~mutable_:false ~linear:false;

  (* type_of: any -> String *)
  Value.bind env "type_of" (Value.VBuiltin ("type_of", fun args ->
    match args with
    | [v] ->
        let ty = match v with
          | Value.VUnit -> "Unit"
          | Value.VBool _ -> "Bool"
          | Value.VInt _ -> "Int"
          | Value.VFloat _ -> "Float"
          | Value.VChar _ -> "Char"
          | Value.VString _ -> "String"
          | Value.VTuple _ -> "Tuple"
          | Value.VArray _ -> "Array"
          | Value.VRecord _ -> "Record"
          | Value.VVariant (ty, _, _) -> ty
          | Value.VClosure _ -> "Function"
          | Value.VBuiltin _ -> "Builtin"
          | Value.VRef _ -> "Ref"
        in
        Value.VString ty
    | _ -> failwith "type_of expects 1 argument"
  )) ~mutable_:false ~linear:false;

  (* range: Int -> Int -> Array[Int] *)
  Value.bind env "range" (Value.VBuiltin ("range", fun args ->
    match args with
    | [Value.VInt start; Value.VInt stop] ->
        Value.VArray (Array.init (max 0 (stop - start)) (fun i -> Value.VInt (start + i)))
    | [Value.VInt stop] ->
        Value.VArray (Array.init (max 0 stop) (fun i -> Value.VInt i))
    | _ -> failwith "range expects 1 or 2 integer arguments"
  )) ~mutable_:false ~linear:false;

  (* push: Array[T] -> T -> Array[T] (returns new array) *)
  Value.bind env "push" (Value.VBuiltin ("push", fun args ->
    match args with
    | [Value.VArray arr; elem] ->
        Value.VArray (Array.append arr [|elem|])
    | _ -> failwith "push expects an array and an element"
  )) ~mutable_:false ~linear:false;

  (* head: Array[T] -> T *)
  Value.bind env "head" (Value.VBuiltin ("head", fun args ->
    match args with
    | [Value.VArray arr] when Array.length arr > 0 -> arr.(0)
    | [Value.VArray _] -> failwith "head: empty array"
    | _ -> failwith "head expects a non-empty array"
  )) ~mutable_:false ~linear:false;

  (* tail: Array[T] -> Array[T] *)
  Value.bind env "tail" (Value.VBuiltin ("tail", fun args ->
    match args with
    | [Value.VArray arr] when Array.length arr > 0 ->
        Value.VArray (Array.sub arr 1 (Array.length arr - 1))
    | [Value.VArray _] -> failwith "tail: empty array"
    | _ -> failwith "tail expects a non-empty array"
  )) ~mutable_:false ~linear:false;

  (* map: (T -> U) -> Array[T] -> Array[U] *)
  Value.bind env "map" (Value.VBuiltin ("map", fun args ->
    match args with
    | [f; Value.VArray arr] ->
        Value.VArray (Array.map (fun x -> Eval.apply f [x]) arr)
    | _ -> failwith "map expects a function and an array"
  )) ~mutable_:false ~linear:false;

  (* filter: (T -> Bool) -> Array[T] -> Array[T] *)
  Value.bind env "filter" (Value.VBuiltin ("filter", fun args ->
    match args with
    | [f; Value.VArray arr] ->
        let filtered = Array.to_list arr |> List.filter (fun x ->
          match Eval.apply f [x] with
          | Value.VBool true -> true
          | _ -> false
        ) in
        Value.VArray (Array.of_list filtered)
    | _ -> failwith "filter expects a function and an array"
  )) ~mutable_:false ~linear:false;

  (* fold: (A -> T -> A) -> A -> Array[T] -> A *)
  Value.bind env "fold" (Value.VBuiltin ("fold", fun args ->
    match args with
    | [f; init; Value.VArray arr] ->
        Array.fold_left (fun acc x -> Eval.apply f [acc; x]) init arr
    | _ -> failwith "fold expects a function, initial value, and an array"
  )) ~mutable_:false ~linear:false;

  (* assert: Bool -> () *)
  Value.bind env "assert" (Value.VBuiltin ("assert", fun args ->
    match args with
    | [Value.VBool true] -> Value.VUnit
    | [Value.VBool false] -> failwith "Assertion failed"
    | _ -> failwith "assert expects a boolean"
  )) ~mutable_:false ~linear:false;

  (* panic: String -> Never *)
  Value.bind env "panic" (Value.VBuiltin ("panic", fun args ->
    match args with
    | [Value.VString msg] -> failwith ("panic: " ^ msg)
    | _ -> failwith "panic expects a string message"
  )) ~mutable_:false ~linear:false;

  ()

(** REPL commands *)
type command =
  | CmdHelp
  | CmdQuit
  | CmdClear
  | CmdEnv
  | CmdDebug
  | CmdLoad of string
  | CmdType of string
  | CmdAst of string
  | CmdNone
  | CmdInput of string

let parse_command input =
  let input = String.trim input in
  if String.length input = 0 then CmdNone
  else if input.[0] = ':' then
    let cmd = String.sub input 1 (String.length input - 1) in
    let parts = String.split_on_char ' ' cmd in
    match parts with
    | ["help"] | ["h"] | ["?"] -> CmdHelp
    | ["quit"] | ["q"] | ["exit"] -> CmdQuit
    | ["clear"] | ["c"] -> CmdClear
    | ["env"] | ["e"] -> CmdEnv
    | ["debug"] | ["d"] -> CmdDebug
    | ["load"; file] | ["l"; file] -> CmdLoad file
    | "type" :: rest | "t" :: rest -> CmdType (String.concat " " rest)
    | "ast" :: rest | "a" :: rest -> CmdAst (String.concat " " rest)
    | _ -> CmdInput input  (* Unknown command, treat as expression *)
  else CmdInput input

(** Check if input is complete (balanced braces/parens) *)
let is_complete input =
  let depth = ref 0 in
  String.iter (fun c ->
    match c with
    | '(' | '[' | '{' -> incr depth
    | ')' | ']' | '}' -> decr depth
    | _ -> ()
  ) input;
  !depth <= 0

(** Print REPL help *)
let print_help () =
  Format.printf "@[<v>";
  Format.printf "AffineScript REPL Commands:@.";
  Format.printf "  :help, :h, :?    Show this help@.";
  Format.printf "  :quit, :q        Exit the REPL@.";
  Format.printf "  :clear, :c       Clear the environment@.";
  Format.printf "  :env, :e         Show bound variables@.";
  Format.printf "  :debug, :d       Toggle debug mode@.";
  Format.printf "  :load <file>     Load and execute a file@.";
  Format.printf "  :type <expr>     Show the AST of an expression@.";
  Format.printf "  :ast <code>      Parse and show AST@.";
  Format.printf "@.";
  Format.printf "Examples:@.";
  Format.printf "  > let x = 42@.";
  Format.printf "  > x + 1@.";
  Format.printf "  > fn add(a: Int, b: Int) -> Int { a + b }@.";
  Format.printf "  > add(1, 2)@.";
  Format.printf "@]"

(** Print environment *)
let print_env env =
  Format.printf "@[<v>Bound variables:@.";
  Hashtbl.iter (fun name binding ->
    let prefix = if binding.Value.mutable_ then "mut " else "" in
    Format.printf "  %s%s = %s@." prefix name (Value.show binding.Value.value)
  ) env.Value.bindings;
  Format.printf "@]"

(** Load and execute a file *)
let load_file state filename =
  try
    let ic = open_in_bin filename in
    let n = in_channel_length ic in
    let source = really_input_string ic n in
    close_in ic;
    let prog = Parse_driver.parse_string ~file:filename source in
    Eval.eval_program state.env prog;
    Format.printf "Loaded %s@." filename
  with
  | Sys_error msg -> Format.eprintf "Error: %s@." msg
  | Parse_driver.Parse_error (msg, span) ->
      Format.eprintf "%a: parse error: %s@." Span.pp_short span msg
  | Lexer.Lexer_error (msg, pos) ->
      Format.eprintf "%s:%d:%d: lexer error: %s@." filename pos.Span.line pos.Span.col msg
  | Eval.Runtime_error (msg, _) ->
      Format.eprintf "Runtime error: %s@." msg

(** Try to parse and evaluate as expression *)
let try_eval_expr state input =
  try
    let expr = Parse_driver.parse_expr ~file:"<repl>" input in
    if state.debug then
      Format.printf "AST: %s@." (Ast.show_expr expr);
    let result = Eval.eval state.env expr in
    state.counter <- state.counter + 1;
    let var_name = Printf.sprintf "_%d" state.counter in
    Value.bind state.env var_name result ~mutable_:false ~linear:false;
    Format.printf "%s = %s@." var_name (Value.show result);
    true
  with _ -> false

(** Try to parse and evaluate as statement/declaration *)
let try_eval_decl state input =
  try
    (* Wrap in a module to parse as program *)
    let prog = Parse_driver.parse_string ~file:"<repl>" input in
    if state.debug then
      Format.printf "AST: %s@." (Ast.show_program prog);
    Eval.eval_program state.env prog;
    true
  with _ -> false

(** Evaluate REPL input *)
let eval_input state input =
  (* First try as expression *)
  if try_eval_expr state input then ()
  (* Then try as declaration/statement *)
  else if try_eval_decl state input then ()
  else
    (* If both fail, show parse error *)
    try
      let _ = Parse_driver.parse_expr ~file:"<repl>" input in
      ()
    with
    | Parse_driver.Parse_error (msg, span) ->
        Format.eprintf "%a: %s@." Span.pp_short span msg
    | Lexer.Lexer_error (msg, pos) ->
        Format.eprintf "<repl>:%d:%d: %s@." pos.Span.line pos.Span.col msg
    | Eval.Runtime_error (msg, span_opt) ->
        (match span_opt with
         | Some span -> Format.eprintf "%a: %s@." Span.pp_short span msg
         | None -> Format.eprintf "Error: %s@." msg)
    | Failure msg ->
        Format.eprintf "Error: %s@." msg

(** Process a single line of input *)
let process_line state line =
  match parse_command line with
  | CmdHelp -> print_help (); true
  | CmdQuit -> false
  | CmdClear ->
      Hashtbl.clear state.env.Value.bindings;
      add_builtins state.env;
      state.counter <- 0;
      Format.printf "Environment cleared.@.";
      true
  | CmdEnv -> print_env state.env; true
  | CmdDebug ->
      state.debug <- not state.debug;
      Format.printf "Debug mode: %s@." (if state.debug then "on" else "off");
      true
  | CmdLoad file -> load_file state file; true
  | CmdType input ->
      (try
         let expr = Parse_driver.parse_expr ~file:"<repl>" input in
         Format.printf "%s@." (Ast.show_expr expr)
       with
       | Parse_driver.Parse_error (msg, span) ->
           Format.eprintf "%a: %s@." Span.pp_short span msg
       | Lexer.Lexer_error (msg, pos) ->
           Format.eprintf "<repl>:%d:%d: %s@." pos.Span.line pos.Span.col msg);
      true
  | CmdAst input ->
      (try
         let prog = Parse_driver.parse_string ~file:"<repl>" input in
         Format.printf "%s@." (Ast.show_program prog)
       with
       | Parse_driver.Parse_error (msg, span) ->
           Format.eprintf "%a: %s@." Span.pp_short span msg
       | Lexer.Lexer_error (msg, pos) ->
           Format.eprintf "<repl>:%d:%d: %s@." pos.Span.line pos.Span.col msg);
      true
  | CmdNone -> true
  | CmdInput input ->
      (* Handle multiline input *)
      if state.multiline then begin
        state.buffer <- state.buffer ^ "\n" ^ input;
        if is_complete state.buffer then begin
          eval_input state state.buffer;
          state.buffer <- "";
          state.multiline <- false
        end
      end else if not (is_complete input) then begin
        state.buffer <- input;
        state.multiline <- true
      end else
        eval_input state input;
      true

(** Main REPL loop *)
let run () =
  Format.printf "@[<v>";
  Format.printf "AffineScript REPL v0.1.0@.";
  Format.printf "Type :help for commands, :quit to exit@.";
  Format.printf "@]@.";

  let state = create_state () in
  add_builtins state.env;

  let rec loop () =
    let prompt = if state.multiline then "... " else ">>> " in
    Format.printf "%s@?" prompt;
    match In_channel.input_line In_channel.stdin with
    | None -> Format.printf "@.Goodbye!@."
    | Some line ->
        if process_line state line then loop ()
        else Format.printf "Goodbye!@."
  in
  loop ()

(** Run REPL with initial file *)
let run_with_file filename =
  let state = create_state () in
  add_builtins state.env;
  load_file state filename;
  Format.printf "@[<v>";
  Format.printf "AffineScript REPL v0.1.0 (loaded %s)@." filename;
  Format.printf "Type :help for commands, :quit to exit@.";
  Format.printf "@]@.";

  let rec loop () =
    let prompt = if state.multiline then "... " else ">>> " in
    Format.printf "%s@?" prompt;
    match In_channel.input_line In_channel.stdin with
    | None -> Format.printf "@.Goodbye!@."
    | Some line ->
        if process_line state line then loop ()
        else Format.printf "Goodbye!@."
  in
  loop ()

(** Evaluate a string and return the result (for testing) *)
let eval_string ?(env = Value.empty_env ()) input =
  add_builtins env;
  try
    (* Try as expression first *)
    let expr = Parse_driver.parse_expr ~file:"<eval>" input in
    Ok (Eval.eval env expr)
  with
  | _ ->
      (* Try as program *)
      try
        let prog = Parse_driver.parse_string ~file:"<eval>" input in
        Eval.eval_program env prog;
        Ok Value.VUnit
      with
      | Parse_driver.Parse_error (msg, _) -> Error ("Parse error: " ^ msg)
      | Lexer.Lexer_error (msg, _) -> Error ("Lexer error: " ^ msg)
      | Eval.Runtime_error (msg, _) -> Error ("Runtime error: " ^ msg)
      | Failure msg -> Error msg
