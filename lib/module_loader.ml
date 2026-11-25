(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2025 hyperpolymath *)

(** Module loading and resolution.

    This module handles finding, loading, and caching modules from the file system.
*)

open Ast

(** Module loading errors *)
type load_error =
  | ModuleNotFound of string list  (** Module path not found *)
  | ModuleParseError of string * string  (** File path and parse error *)
  | ModuleCycle of string list  (** Circular dependency detected *)
  | InvalidModulePath of string list
  | ModuleDeclMismatch of string list * string list * string
      (** Requested path, the path the file declares, and the file.  A file
          found for [A.B] that declares [module A;] is not module [A.B]:
          the language has no nested module declarations, so accepting it
          would bind [A]'s exports under a name that does not exist. *)
[@@deriving show]

(** Why the declarations of a program's imports cannot be flattened into one
    program. Raised as {!Flatten_error} by {!flatten_imports}; the CLI reports
    it as a diagnostic. *)
type flatten_error =
  | ImportCollision of string * string list * string list
      (** One flattened name, carried by two different modules' declarations. *)
  | MutableAlias of string * string * string list
      (** A mutable module binding, the alias it was imported under, and its
          module. An alias is a second declaration, so it would be a second,
          separate copy of the binding. *)
[@@deriving show]

exception Flatten_error of flatten_error

(** Helper for Result bind *)
let ( let* ) = Result.bind

(** Module loader configuration *)
type config = {
  stdlib_path : string;  (** Path to standard library *)
  search_paths : string list;  (** Additional module search paths *)
  current_dir : string;  (** Current working directory *)
}

(** Loaded module information *)
type loaded_module = {
  mod_path : string list;  (** Module path (e.g., ["Math", "Geometry"]) *)
  mod_program : program;  (** Parsed AST *)
  mod_file : string;  (** Source file path *)
}

(** Module loader state *)
type t = {
  config : config;
  loaded : (string list, loaded_module) Hashtbl.t;  (** Cache of loaded modules *)
  loading : (string list, unit) Hashtbl.t;  (** Currently loading (for cycle detection) *)
}

(** Create a new module loader *)
let create (config : config) : t =
  {
    config;
    loaded = Hashtbl.create 32;
    loading = Hashtbl.create 16;
  }

(** Test whether a candidate stdlib directory contains the prelude. *)
let stdlib_dir_valid path =
  Sys.file_exists (Filename.concat path "prelude.affine")

(** Walk up from [start_dir] looking for an ancestor whose [stdlib/]
    sub-directory contains the prelude. Returns [Some path] on hit, [None]
    after exhausting the parent chain. Used by [default_config] to find
    the stdlib when [affinescript check] is invoked from a sub-directory
    of an affinescript repo (issue #415). *)
let find_stdlib_walking_up start_dir =
  let rec walk dir =
    let candidate = Filename.concat dir "stdlib" in
    if stdlib_dir_valid candidate then Some candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then None
      else walk parent
  in
  walk start_dir

(** Discover the stdlib directory by trying, in order:
    1. [$AFFINESCRIPT_STDLIB] — explicit operator override
    2. [./stdlib] — current behaviour, for in-repo dev
    3. Walk up from CWD looking for [stdlib/prelude.affine] — catches
       sub-directory invocations of [affinescript check]
    4. [<binary_dir>/../share/affinescript/stdlib/] — XDG-style installed
       location alongside an installed binary
    5. [$HOME/.local/share/affinescript/stdlib/] — user-local install
    Falls back to ["./stdlib"] (and lets the loader's own
    [ModuleNotFound] surface for a clean error) if nothing matches.
    Issue #415. *)
let discover_stdlib () =
  match Sys.getenv_opt "AFFINESCRIPT_STDLIB" with
  | Some p -> p
  | None ->
    let cwd = Sys.getcwd () in
    let cwd_local = Filename.concat cwd "stdlib" in
    if stdlib_dir_valid cwd_local then cwd_local
    else match find_stdlib_walking_up cwd with
    | Some p -> p
    | None ->
      let binary_share =
        let bin = Sys.executable_name in
        if bin <> "" then
          Filename.concat
            (Filename.dirname (Filename.dirname bin))
            "share/affinescript/stdlib"
        else ""
      in
      if binary_share <> "" && stdlib_dir_valid binary_share then binary_share
      else
        let user_share =
          match Sys.getenv_opt "HOME" with
          | Some h -> Filename.concat h ".local/share/affinescript/stdlib"
          | None -> ""
        in
        if user_share <> "" && stdlib_dir_valid user_share then user_share
        else "./stdlib"  (* preserves the historical default error path *)

(** Directories listed in [$AFFINESCRIPT_PATH] (colon-separated, empty
    entries ignored): where third-party packages such as affinescript-tea
    live. Searched after the current directory and the stdlib. *)
let env_search_paths () : string list =
  match Sys.getenv_opt "AFFINESCRIPT_PATH" with
  | None -> []
  | Some v -> List.filter (fun d -> d <> "") (String.split_on_char ':' v)

(** Create a configuration using the discovered stdlib, [$AFFINESCRIPT_PATH]
    and the current working directory.

    @raise Sys_error if the current working directory cannot be obtained. *)
let default_config () : config =
  {
    stdlib_path = discover_stdlib ();
    search_paths = env_search_paths ();
    current_dir = Sys.getcwd ();
  }

(** [dir] with `.` segments and empty segments removed and `..` resolved
    lexically, so `/p/src/./Kyaml` and `/p/src/Kyaml` match the same
    module path. *)
let normalise_dir (dir : string) : string =
  let absolute = String.length dir > 0 && dir.[0] = '/' in
  let segs =
    List.fold_left (fun acc seg ->
        match seg with
        | "" | "." -> acc
        | ".." -> (match acc with _ :: rest -> rest | [] -> acc)
        | s -> s :: acc)
      [] (String.split_on_char '/' dir)
    |> List.rev
  in
  let body = String.concat "/" segs in
  if absolute then "/" ^ body else if body = "" then "." else body

(** The directory module paths are resolved from, for a root file in
    [file_dir] that declares [mod_path].

    A file that declares [module A.B.C;] and sits in [.../A/B/] belongs to
    the package rooted two directories up, so [use A.D;] must find
    [.../A/D.affine], not [.../A/B/A/D.affine].  When the declared path's
    leading segments match the file's trailing directories the root is the
    directory above them; otherwise (no declaration, a one-segment name, or
    a layout that does not match) it is [file_dir], which keeps sibling
    imports working (#642). *)
let module_root_dir ~(file_dir : string) (mod_path : string list option)
    : string =
  let file_dir = normalise_dir file_dir in
  match mod_path with
  | None | Some [] | Some [_] -> file_dir
  | Some segs ->
    let parents = List.rev (List.tl (List.rev segs)) in
    let rec strip dir = function
      | [] -> Some dir
      | seg :: rest ->
        if Filename.basename dir = seg then strip (Filename.dirname dir) rest
        else None
    in
    match strip file_dir (List.rev parents) with
    | Some root -> root
    | None -> file_dir

(** Loader configuration for a root file at [file_path] that declares
    [mod_path].  The package root (see {!module_root_dir}) resolves full
    paths (`use A.C;` from `A/B.affine`); the file's own directory stays
    searchable, so a short sibling import (`use Helper;` for
    `A/Helper.affine`) keeps working (#642). *)
let config_for_file ~(file_path : string) (mod_path : string list option)
    : config =
  let absolute_path =
    if Filename.is_relative file_path
    then Filename.concat (Sys.getcwd ()) file_path
    else file_path
  in
  let file_dir = normalise_dir (Filename.dirname absolute_path) in
  let root = module_root_dir ~file_dir mod_path in
  let config = default_config () in
  { config with
    current_dir = root;
    search_paths =
      (if root = file_dir then config.search_paths
       else file_dir :: config.search_paths) }

(** Whether [prog] itself declares `main`.  `eval` auto-calls `main` only
    then: flattening inlines imported declarations, so a dependency's
    `pub fn main` must not run because the root has none. *)
let declares_main (prog : program) : bool =
  List.exists (function
      | TopFn fd -> fd.fd_name.name = "main"
      | _ -> false)
    prog.prog_decls

(** Convert module path to file path candidates

    For module path ["Math", "Geometry"], try:
    1. Math/Geometry.affine (nested module)
    2. Math.affine (check if it exports Geometry)
*)
let path_to_candidates (mod_path : string list) : string list =
  match mod_path with
  | [] -> []
  | [name] -> [name ^ ".affine"]
  | _ ->
    (* Try nested path: A/B/C.affine *)
    let nested = String.concat "/" mod_path ^ ".affine" in
    (* Try parent with submodule: A/B.affine (if B declares module C) *)
    let parent_parts = List.rev mod_path |> List.tl |> List.rev in
    let parent = String.concat "/" parent_parts ^ ".affine" in
    [nested; parent]

(** Search for a module file in search paths *)
let find_module_file (loader : t) (mod_path : string list) : string option =
  let candidates = path_to_candidates mod_path in
  let search_in_dir cands dir =
    List.find_map (fun candidate ->
      let full_path = Filename.concat dir candidate in
      if Sys.file_exists full_path then Some full_path else None
    ) cands
  in
  (* Search order: 1) current dir, 2) stdlib, 3) additional search paths *)
  let all_paths =
    loader.config.current_dir ::
    loader.config.stdlib_path ::
    loader.config.search_paths
  in
  (* Every directory is searched for the exact file (`A/B.affine`) before
     any falls back to the parent file (`A.affine`): a parent file that
     declares a different module is rejected when loaded
     (ModuleDeclMismatch), so it must not shadow a real `A/B.affine` in a
     later search directory. *)
  let exact, parent = match candidates with
    | [ nested; up ] -> ([ nested ], [ up ])
    | cands -> (cands, [])
  in
  match List.find_map (search_in_dir exact) all_paths with
  | Some _ as found -> found
  | None -> List.find_map (search_in_dir parent) all_paths

(** Load and parse a module file *)
let parse_module_file (file_path : string) : (program, load_error) result =
  try
    let prog = Parse_driver.parse_file file_path in
    Ok prog
  with
  | Lexer.Lexer_error (msg, _pos) ->
    Error (ModuleParseError (file_path, "Lexer error: " ^ msg))
  | Parse_driver.Parse_error (msg, _span) ->
    Error (ModuleParseError (file_path, "Parse error: " ^ msg))
  | Sys_error msg ->
    Error (ModuleParseError (file_path, "System error: " ^ msg))

(** Load a module and all its dependencies

    Returns the loaded module or an error if loading fails.
    Note: Does NOT resolve symbols - that's done by the caller.
*)
let rec load_module (loader : t) (mod_path : string list) : (loaded_module, load_error) result =
  (* Check if already loaded *)
  match Hashtbl.find_opt loader.loaded mod_path with
  | Some m -> Ok m
  | None ->
    (* Check for circular dependency *)
    if Hashtbl.mem loader.loading mod_path then
      Error (ModuleCycle mod_path)
    else begin
      (* Mark as loading *)
      Hashtbl.add loader.loading mod_path ();

      (* Find the module file *)
      match find_module_file loader mod_path with
      | None -> Hashtbl.remove loader.loading mod_path; Error (ModuleNotFound mod_path)
      | Some file_path ->
        (* Parse the module *)
        match parse_module_file file_path with
        | Error e -> Hashtbl.remove loader.loading mod_path; Error e
        | Ok prog ->
          let declared =
            Option.map (List.map (fun (id : ident) -> id.name)) prog.prog_module
          in
          match declared with
          | Some d when d <> mod_path ->
            Hashtbl.remove loader.loading mod_path;
            Error (ModuleDeclMismatch (mod_path, d, file_path))
          | _ ->
          (* Load dependencies first (imports).  On failure, clear this
             module's loading marker, or a later load of it would be
             reported as a cycle. *)
          let* () =
            match load_dependencies loader prog.prog_imports with
            | Ok () -> Ok ()
            | Error e -> Hashtbl.remove loader.loading mod_path; Error e
          in

          let loaded_mod = {
            mod_path;
            mod_program = prog;
            mod_file = file_path;
          } in

          (* Cache the loaded module *)
          Hashtbl.add loader.loaded mod_path loaded_mod;
          Hashtbl.remove loader.loading mod_path;

          Ok loaded_mod
    end

(** Load all dependencies for a module *)
and load_dependencies (loader : t) (imports : import_decl list) : (unit, load_error) result =
  List.fold_left (fun acc import ->
    match acc with
    | Error e -> Error e
    | Ok () ->
      let mod_paths = match import with
        | ImportSimple (path, _alias) -> [List.map (fun id -> id.name) path]
        | ImportList (path, _items) -> [List.map (fun id -> id.name) path]
        | ImportGlob path -> [List.map (fun id -> id.name) path]
      in
      List.fold_left (fun acc' mod_path ->
        match acc' with
        | Error e -> Error e
        | Ok () ->
          match load_module loader mod_path with
          | Ok _ -> Ok ()
          | Error e -> Error e
      ) acc mod_paths
  ) (Ok ()) imports

(** Get a loaded module by path *)
let get_module (loader : t) (mod_path : string list) : loaded_module option =
  Hashtbl.find_opt loader.loaded mod_path

(** Get the loaded module's program *)
let get_program (loaded_mod : loaded_module) : program =
  loaded_mod.mod_program

(** Pretty-print module path *)
let show_module_path (path : string list) : string =
  String.concat "::" path

(** Check if a module is loaded *)
let is_loaded (loader : t) (mod_path : string list) : bool =
  Hashtbl.mem loader.loaded mod_path

(** Clear the module cache (for testing/reloading) *)
let clear_cache (loader : t) : unit =
  Hashtbl.clear loader.loaded;
  Hashtbl.clear loader.loading

(* ============================================================================
   Import flattening (#801)

   The non-Wasm backends compile one flattened program, so every module's
   top-level declarations share one namespace there.  Flattening used to keep
   the source names of the declarations it carried and resolve clashes by
   dropping one, which made one module's code call another module's function
   of the same name with no diagnostic (#801: `Lib.A.a_label` printed
   `Lib.B.label`'s text).  The scheme below makes a clash impossible by
   construction instead of resolving it:

   - Every function and constant of an imported module, public or private,
     is carried under its module-qualified name ({!qualified_name}), and every
     reference to it, in its own module and in the modules that import it, is
     rewritten to that name.  The encoding is injective, so declarations of
     different modules never share a name.
   - The root program's own declarations keep their source names: they are
     the compiled unit's host-facing surface (`main`, `activate`, the
     exports a harness imports).  A name the root imports is also present
     under the name the root binds it to, as a copy of the qualified
     declaration, so the flattened program keeps the shape it had.
   - Extern functions keep their names everywhere: the host binds them by
     name (the Vscode, Bun, Deno and MCP-shim stdlib modules declare them).
     An alias of an extern rewrites its references to the extern's own name.
   - Enum types and constructors keep their names; constructors are values
     a backend preamble may already define, and an enum's constructors are
     carried with the enum.

   {!check_unique} then refuses a flattened program in which two different
   top-level bindings still share a name, so a regression in the scheme
   fails loudly instead of silently picking one.
   ============================================================================ *)

(** The name a module's function or constant carries once flattened.  Each
    path component is prefixed by its length, so the encoding is injective:
    joining with `_` made `A_B.C` and `A.B_C` the same.  Underscores and
    alphanumerics keep the name valid in every backend's target language. *)
let qualified_name (mod_path : string list) (name : string) : string =
  "__mod_"
  ^ String.concat ""
      (List.map (fun c -> string_of_int (String.length c) ^ c) mod_path)
  ^ "__" ^ name

(** The diagnostic text of a {!flatten_error}. *)
let flatten_error_message = function
  | ImportCollision (name, a, b) ->
    Printf.sprintf
      "cannot flatten imports: two different top-level bindings named `%s` \
       (from %s and from %s) would share one name in the compiled program"
      name
      (match a with [] -> "the root module" | a -> String.concat "::" a)
      (match b with [] -> "the root module" | b -> String.concat "::" b)
  | MutableAlias (name, alias, m) ->
    Printf.sprintf
      "cannot import the mutable binding `%s` from %s as `%s`: an alias would \
       be a separate copy of it; import it under its own name" name
      (String.concat "::" m) alias

(** Map, through [look], the free references of a top-level function,
    constant or impl block's methods; parameters shadow.  The declaration's
    own name is not touched. *)
let map_top_level_free (look : string -> string option) (decl : top_level)
    : top_level =
  let map_fn (fd : fn_decl) =
    let inner =
      shadow look (List.map (fun (p : param) -> p.p_name.name) fd.fd_params)
    in
    let body = match fd.fd_body with
      | FnExpr e -> FnExpr (map_free inner e)
      | FnBlock b -> FnBlock (map_block inner b)
      | FnExtern -> FnExtern
    in
    { fd with fd_body = body }
  in
  match decl with
  | TopFn fd -> TopFn (map_fn fd)
  | TopConst c -> TopConst { c with tc_value = map_free look c.tc_value }
  | TopImpl ib ->
    TopImpl { ib with ib_items = List.map (function
        | ImplFn fd -> ImplFn (map_fn fd)
        | other -> other) ib.ib_items }
  | other -> other

(** Rename, by [ren] (old name to new), a top-level function or constant's
    own name and the free references to the renamed names in its body; a
    parameter that shadows a renamed name is left alone. *)
let rename_top_level (ren : (string * string) list) (decl : top_level)
    : top_level =
  if ren = [] then decl
  else
    let rename_ident (id : ident) =
      match List.assoc_opt id.name ren with
      | Some n -> { id with name = n }
      | None -> id
    in
    match map_top_level_free (fun n -> List.assoc_opt n ren) decl with
    | TopFn fd -> TopFn { fd with fd_name = rename_ident fd.fd_name }
    | TopConst c -> TopConst { c with tc_name = rename_ident c.tc_name }
    | other -> other

(** The free names of a top-level declaration (see {!Ast.free_names}). *)
let top_level_free_names (decl : top_level) : string list =
  let acc = ref [] in
  ignore (map_top_level_free (fun n -> acc := n :: !acc; None) decl);
  !acc

(** What a name a module exports binds, as an importer sees it. *)
type export =
  | XValue of string   (** a function or constant, by its qualified name *)
  | XExtern of string  (** an extern function, by its (host) name *)
  | XEnum of string    (** an enum type or one of its constructors, by the
                           enum's name *)

let exported_vis = function Public | PubCrate -> true | _ -> false

(* Every non-Wasm backend's runtime preamble defines these constructors. *)
let preamble_ctors = [ "Some"; "None"; "Ok"; "Err" ]

(** An enum the flattener carries: one with a constructor the preamble does
    not already supply. A variant with a preamble name but another arity is
    not the preamble's, so it is carried and the emitter refuses it. *)
let carried_enum (td : type_decl) : bool =
  match td.td_body with
  | TyEnum variants ->
    List.exists (fun (vd : variant_decl) ->
        let n = vd.vd_name.name in
        not (List.mem n preamble_ctors)
        || List.length vd.vd_fields <> (if n = "None" then 0 else 1)) variants
  | _ -> false

let is_extern = function
  | TopFn { fd_body = FnExtern; _ } | TopExternFn _ -> true
  | _ -> false

(** The name a value-level declaration binds: functions (extern or not),
    extern functions and constants. *)
let value_name = function
  | TopFn fd -> Some fd.fd_name.name
  | TopExternFn { ef_name; _ } -> Some ef_name.name
  | TopConst { tc_name; _ } -> Some tc_name.name
  | _ -> None

(** The names module [mod_path] exports, read from its own declarations. *)
let exports_of (mod_path : string list) (prog : program)
    : (string * export) list =
  List.concat_map (function
      | TopFn fd when exported_vis fd.fd_vis ->
        let n = fd.fd_name.name in
        [ (n, if fd.fd_body = FnExtern then XExtern n
               else XValue (qualified_name mod_path n)) ]
      | TopExternFn { ef_name; _ } -> [ (ef_name.name, XExtern ef_name.name) ]
      | TopConst { tc_vis; tc_name; _ } when exported_vis tc_vis ->
        [ (tc_name.name, XValue (qualified_name mod_path tc_name.name)) ]
      | TopType ({ td_body = TyEnum variants; _ } as td)
        when exported_vis td.td_vis && carried_enum td ->
        (td.td_name.name, XEnum td.td_name.name)
        :: List.map (fun (vd : variant_decl) ->
            (vd.vd_name.name, XEnum td.td_name.name)) variants
      | _ -> [])
    prog.prog_decls

let import_path = function
  | ImportSimple (p, _) | ImportList (p, _) | ImportGlob p ->
    List.map (fun (id : ident) -> id.name) p

(** The names [imports] bind in a module whose own value names are [local]
    (an own declaration shadows an import of the same name), each with the
    module it comes from and what it binds, in import order.  A glob or a
    plain `use` binds every export; an item list binds each item under its
    alias or its own name.  One name bound to two different things is an
    {!ImportCollision}: the resolver rejects such a program first, and the
    flattener does not pick one. *)
let bindings_of (loader : t) ~(local : string list) (imports : import_decl list)
    : (string * (string list * export)) list =
  let tbl = Hashtbl.create 32 in
  let order = ref [] in
  List.iter (fun imp ->
      let path = import_path imp in
      match Hashtbl.find_opt loader.loaded path with
      | None -> ()
      | Some lm ->
        let ex = exports_of path lm.mod_program in
        let bound = match imp with
          | ImportGlob _ | ImportSimple _ -> ex
          | ImportList (_, items) ->
            List.filter_map (fun item ->
                List.assoc_opt item.ii_name.name ex
                |> Option.map (fun x ->
                    ((match item.ii_alias with
                        | Some a -> a.name
                        | None -> item.ii_name.name), x)))
              items
        in
        List.iter (fun (b, x) ->
            if not (List.mem b local) then
              match Hashtbl.find_opt tbl b with
              | Some (p', x') when (p', x') <> (path, x) ->
                raise (Flatten_error (ImportCollision (b, p', path)))
              | Some _ -> ()
              | None ->
                Hashtbl.replace tbl b (path, x);
                order := b :: !order)
          bound)
    imports;
  List.rev_map (fun b -> (b, Hashtbl.find tbl b)) !order

(** The renaming that realises [bindings] in the importing module's code:
    a value is referred to by its qualified name, an aliased extern by the
    extern's own name. *)
let binding_renames bindings =
  List.filter_map (fun (b, (_, x)) ->
      match x with
      | XValue q -> Some (b, q)
      | XExtern x when x <> b -> Some (b, x)
      | XExtern _ | XEnum _ -> None)
    bindings

(** A declaration in the flattened program, with the module it came from
    ([[]] for an anonymous root). *)
type entry = { origin : string list; decl : top_level }

(** Module [mod_path]'s own carried declarations, qualified: its functions
    and constants under {!qualified_name}, its references to them and to
    what it imports rewritten accordingly.  Externs and enums keep their
    names; other declarations (structs, traits, effects, impls) are not
    carried. *)
let qualify_module (loader : t) (mod_path : string list) (prog : program)
    : entry list =
  let local = List.filter_map value_name prog.prog_decls in
  let own =
    List.filter_map (function
        | TopFn fd when fd.fd_body <> FnExtern ->
          Some (fd.fd_name.name, qualified_name mod_path fd.fd_name.name)
        | TopConst { tc_name; _ } ->
          Some (tc_name.name, qualified_name mod_path tc_name.name)
        | _ -> None)
      prog.prog_decls
  in
  let ren =
    own @ binding_renames (bindings_of loader ~local prog.prog_imports) in
  List.filter_map (fun decl ->
      match decl with
      | TopFn _ | TopConst _ | TopExternFn _ ->
        Some { origin = mod_path; decl = rename_top_level ren decl }
      | TopType td when carried_enum td -> Some { origin = mod_path; decl }
      | _ -> None)
    prog.prog_decls

(** Keep the first of each declaration.  A function or constant reached
    twice (a diamond: `D` imports `P` and `J`, and `J` imports `P`) is one
    declaration; two different ones under one name are an
    {!ImportCollision}, which qualification makes unreachable short of a
    bug.  Externs of one name are one host binding, so later ones are
    dropped.  Enums are identified by module and name. *)
let merge (entries : entry list) : entry list =
  let values = Hashtbl.create 64 in
  let types = Hashtbl.create 16 in
  List.filter (fun e ->
      match e.decl, value_name e.decl with
      | TopType td, _ ->
        let k = (e.origin, td.td_name.name) in
        if Hashtbl.mem types k then false
        else (Hashtbl.replace types k (); true)
      | d, Some n ->
        (match Hashtbl.find_opt values n with
         | None -> Hashtbl.replace values n e; true
         | Some prev when is_extern prev.decl && is_extern d -> false
         | Some prev when prev.decl == d || prev.decl = d -> false
         | Some prev ->
           raise (Flatten_error (ImportCollision (n, prev.origin, e.origin))))
      | _, None -> true)
    entries

(** Module [mod_path] flattened as a dependency: the qualified declarations
    of everything it imports (transitively, dependencies first), then its
    own.  [cache] memoises per module for one flattening; [visiting] stops
    an import cycle, whose modules the outer frames carry. *)
let rec flatten_module cache visiting (loader : t) (mod_path : string list)
    : entry list =
  match Hashtbl.find_opt cache mod_path with
  | Some es -> es
  | None ->
    (match Hashtbl.find_opt loader.loaded mod_path with
     | None -> []
     | Some lm ->
       let prog = lm.mod_program in
       if List.mem mod_path visiting then qualify_module loader mod_path prog
       else begin
         let deps =
           List.concat_map (fun imp ->
               flatten_module cache (mod_path :: visiting) loader
                 (import_path imp))
             prog.prog_imports
         in
         let es = merge (deps @ qualify_module loader mod_path prog) in
         Hashtbl.replace cache mod_path es;
         es
       end)

(** Refuse a flattened program in which two different top-level value
    bindings share a name, or a function or constant has an extern's name
    (a backend would lower calls to it as host calls).  Several externs of
    one name are one host binding.  This is the loud backstop behind the
    qualification scheme: it holds for every backend that compiles the
    flattened program. *)
let check_unique (entries : entry list) : unit =
  let values = Hashtbl.create 64 in
  let externs = Hashtbl.create 32 in
  List.iter (fun e ->
      match value_name e.decl with
      | Some n when is_extern e.decl ->
        if not (Hashtbl.mem externs n) then Hashtbl.replace externs n e.origin
      | Some n ->
        (match Hashtbl.find_opt values n with
         | Some o -> raise (Flatten_error (ImportCollision (n, o, e.origin)))
         | None -> Hashtbl.replace values n e.origin)
      | None -> ())
    entries;
  Hashtbl.iter (fun n o ->
      match Hashtbl.find_opt externs n with
      | Some xo -> raise (Flatten_error (ImportCollision (n, o, xo)))
      | None -> ())
    values

(** Inline the declarations [prog]'s imports need (transitively) into [prog],
    for the backends that compile one flattened program (see the section
    comment above for the naming scheme).  Uses only modules already loaded
    in [loader] (by {!Resolve.resolve_program_with_loader}); a module that
    is not loaded is skipped, the resolver having reported it.

    The result is: the imported declarations the root reaches, dependencies
    first and each module's in its own order (declarations must precede
    their uses: the interpreter binds each function in an environment that
    holds only what came before it); then a copy, under its bound name, of
    each function and immutable constant the root imports (not of a function
    bound as [main]: only the root's own [main] is an entry point, so
    references to that binding are rewritten to the qualified name); then the root's
    own declarations.  The copy of an immutable constant keeps a literal
    value as-is (C emits [static const T K = <expr>;], which needs a constant
    initialiser) and otherwise refers to the qualified declaration, so the
    root and the module share one runtime value.  A mutable constant is not copied (a copy would be
    separate storage): the root's references to it are rewritten to its
    qualified name.  A local declaration shadows an import of its name.
    [prog.prog_imports] is retained.

    Raises {!Flatten_error} on a mutable binding imported under an alias, and
    on two different top-level bindings that would share a name. *)
let flatten_imports (loader : t) (prog : program) : program =
  let root =
    match prog.prog_module with
    | Some p -> List.map (fun (id : ident) -> id.name) p
    | None -> []
  in
  let cache = Hashtbl.create 8 in
  let visiting = if root = [] then [] else [ root ] in
  let pool =
    merge
      (List.concat_map (fun imp ->
           flatten_module cache visiting loader (import_path imp))
          prog.prog_imports)
  in
  let local = List.filter_map value_name prog.prog_decls in
  let bindings = bindings_of loader ~local prog.prog_imports in
  let pool_value = Hashtbl.create 64 in
  List.iter (fun e ->
      match value_name e.decl with
      | Some n when not (Hashtbl.mem pool_value n) -> Hashtbl.replace pool_value n e
      | _ -> ())
    pool;
  (* Realise the root's bindings. *)
  let copies = ref [] and ren = ref [] and enum_seeds = ref [] in
  List.iter (fun (b, (path, x)) ->
      match x with
      | XValue q ->
        (match Hashtbl.find_opt pool_value q with
         | Some { decl = TopFn _; _ } when b = "main" ->
           (* A top-level [main] is the program's entry point (the JS
              backends call it on load).  Only the root's own [main] is one:
              an imported function bound as [main] gets no copy under that
              name, and the root's references to it are rewritten to its
              qualified name (#805). *)
           ren := (b, q) :: !ren
         | Some { decl = TopFn fd; _ } ->
           copies := { origin = path;
                       decl = TopFn { fd with fd_name = { fd.fd_name with name = b } } }
                     :: !copies
         | Some { decl = TopConst ({ tc_mut = false; _ } as c); _ } ->
           (* A literal is copied as-is (some backends need a constant
              initialiser).  Any other value refers to the source
              declaration, so the root and the module share one runtime
              value: a second array or record would compare unequal under a
              reference equality. *)
           let value = match c.tc_value with
             | ExprLit _ -> c.tc_value
             | _ -> ExprVar { c.tc_name with name = q }
           in
           copies := { origin = path;
                       decl = TopConst { c with tc_name = { c.tc_name with name = b };
                                                tc_value = value } }
                     :: !copies
         | Some { decl = TopConst ({ tc_mut = true; _ } as c); _ } ->
           if q <> qualified_name path b then begin
             let own =
               let pre = qualified_name path "" in
               let pl = String.length pre in
               String.sub c.tc_name.name pl (String.length c.tc_name.name - pl)
             in
             raise (Flatten_error (MutableAlias (own, b, path)))
           end;
           ren := (b, q) :: !ren
         | _ -> ())
      | XExtern xn -> if xn <> b then ren := (b, xn) :: !ren
      | XEnum ty -> enum_seeds := (path, ty) :: !enum_seeds)
    bindings;
  let copies = List.rev !copies in
  let root_entries =
    List.map (fun decl -> { origin = root; decl = rename_top_level !ren decl })
      prog.prog_decls
  in
  (* Reachability: carry the pool entries the root reaches.  A constructor
     or enum-type name resolves in the scope of the module whose code names
     it: its own enums, then the enums it imports; failing that (a name the
     scopes do not account for), every carried enum that declares it. *)
  let enums = Hashtbl.create 16 in
  List.iter (fun e ->
      match e.decl with
      | TopType td -> Hashtbl.replace enums (e.origin, td.td_name.name) e
      | _ -> ())
    pool;
  let enum_names (td : type_decl) =
    td.td_name.name
    :: (match td.td_body with
        | TyEnum vs -> List.map (fun (vd : variant_decl) -> vd.vd_name.name) vs
        | _ -> [])
  in
  let scopes = Hashtbl.create 16 in
  let scope_of m =
    match Hashtbl.find_opt scopes m with
    | Some s -> s
    | None ->
      let s = Hashtbl.create 16 in
      let imports =
        if m = root then prog.prog_imports
        else match Hashtbl.find_opt loader.loaded m with
          | Some lm -> lm.mod_program.prog_imports
          | None -> []
      in
      List.iter (fun (b, (path, x)) ->
          match x with
          | XEnum ty -> Hashtbl.replace s b (path, ty)
          | XValue _ | XExtern _ -> ())
        (try bindings_of loader ~local:[] imports
         with Flatten_error _ -> []);
      Hashtbl.iter (fun (o, ty) e ->
          match e.decl with
          | TopType td when o = m ->
            List.iter (fun n -> Hashtbl.replace s n (o, ty)) (enum_names td)
          | _ -> ())
        enums;
      Hashtbl.replace scopes m s;
      s
  in
  let key (e : entry) =
    match e.decl with
    | TopType td -> (e.origin, td.td_name.name, true)
    | d -> ([], Option.value (value_name d) ~default:"", false)
  in
  let reached = Hashtbl.create 64 in
  let rec reach (e : entry) =
    if not (Hashtbl.mem reached (key e)) then begin
      Hashtbl.replace reached (key e) ();
      visit e
    end
  and visit (e : entry) =
    List.iter (fun n ->
        match Hashtbl.find_opt pool_value n with
        | Some d -> reach d
        | None ->
          (match Hashtbl.find_opt (scope_of e.origin) n with
           | Some k ->
             Option.iter reach (Hashtbl.find_opt enums k)
           | None ->
             Hashtbl.iter (fun _ en ->
                 match en.decl with
                 | TopType td when List.mem n (enum_names td) -> reach en
                 | _ -> ())
               enums))
      (top_level_free_names e.decl)
  in
  List.iter visit copies;
  List.iter visit root_entries;
  List.iter (fun k -> Option.iter reach (Hashtbl.find_opt enums k))
    (List.rev !enum_seeds);
  (* An extern the root declares itself is not carried a second time. *)
  let root_externs =
    List.filter_map (fun e -> if is_extern e.decl then value_name e.decl else None)
      root_entries
  in
  let carried =
    List.filter (fun e ->
        Hashtbl.mem reached (key e)
        && not (is_extern e.decl
                && List.mem (Option.get (value_name e.decl)) root_externs))
      pool
  in
  let all = carried @ copies @ root_entries in
  check_unique all;
  { prog with prog_decls = List.map (fun e -> e.decl) all }
