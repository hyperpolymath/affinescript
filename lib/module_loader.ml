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

(** The name a module's private helper carries once flattened.  Flattening
    puts the declarations of every imported module into one program, so two
    modules' private helpers of the same name (kyq: `parse_int` in both
    `Kyaml` and `Kyaml.Parser`) would otherwise collide: one would be
    dropped, or the importer's own local of that name would win, and a
    module's code would call another module's helper.  Underscores keep the
    name valid in every backend's target language. *)
let qualified_private_name (mod_path : string list) (name : string) : string =
  (* Each path component is prefixed by its length, so the encoding is
     injective: joining with `_` made `A_B.C` and `A.B_C` the same. *)
  "__priv_"
  ^ String.concat ""
      (List.map (fun c -> string_of_int (String.length c) ^ c) mod_path)
  ^ "__" ^ name

(** The diagnostic text of a {!flatten_error}. *)
let flatten_error_message = function
  | ImportCollision (name, a, b) ->
    Printf.sprintf
      "cannot flatten imports: `%s` is defined in both %s and %s" name
      (String.concat "::" a) (String.concat "::" b)
  | MutableAlias (name, alias, m) ->
    Printf.sprintf
      "cannot import the mutable binding `%s` from %s as `%s`: an alias would \
       be a separate copy of it; import it under its own name" name
      (String.concat "::" m) alias

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
    match decl with
    | TopFn fd ->
      let inner =
        without_names ren
          (List.map (fun (p : param) -> p.p_name.name) fd.fd_params)
      in
      let body = match fd.fd_body with
        | FnExpr e -> FnExpr (rename_free inner e)
        | FnBlock b -> FnBlock (rename_block inner b)
        | FnExtern -> FnExtern
      in
      TopFn { fd with fd_name = rename_ident fd.fd_name; fd_body = body }
    | TopConst c ->
      TopConst { c with tc_name = rename_ident c.tc_name;
                        tc_value = rename_free ren c.tc_value }
    | other -> other

(** Give [prog]'s own private functions and constants (those of [own], the
    module's unflattened declarations) their {!qualified_private_name}, and
    rewrite the references to them in the module's own declarations.
    Declarations [prog] carries from other modules are left alone: their
    private helpers were qualified when those modules were flattened, and
    they cannot refer to this module's private names.  Extern functions keep
    their names (the host binds them by name). *)
let qualify_private (mod_path : string list) (own : top_level list)
    (prog : program) : program =
  let ren =
    List.filter_map (function
        | TopFn fd when fd.fd_vis = Private && fd.fd_body <> FnExtern ->
          Some (fd.fd_name.name, qualified_private_name mod_path fd.fd_name.name)
        | TopConst { tc_vis = Private; tc_name; _ } ->
          Some (tc_name.name, qualified_private_name mod_path tc_name.name)
        | _ -> None) own
  in
  if ren = [] then prog
  else
    let rename_decl = rename_top_level ren in
    let is_own d = List.exists (fun o -> o == d) own in
    { prog with
      prog_decls =
        List.map (fun d -> if is_own d then rename_decl d else d)
          prog.prog_decls }

(** Flatten cross-module imports by inlining the public top-level decls of
    each imported module into [prog.prog_decls], with deduplication by name.

    The WASM backend handles cross-module imports via a separate Wasm-import
    section ([Codegen.gen_imports]). Every other codegen ([Julia / JS / C /
    Rust / OCaml / ReScript / Lua / Bash / Nickel / LLVM / ...]) iterates
    [prog.prog_decls] only and would otherwise fail to find imported
    functions at codegen time. Inlining here is the simplest correct
    semantics for those targets and saves implementing per-backend module
    systems.

    The loader must already have been used by [Resolve.resolve_program_with_loader]
    so that the cache is populated; this function does NOT re-load on
    demand. If a referenced module isn't cached, its imports are silently
    skipped — the resolver would have reported the error already.

    Imports are processed in declaration order; later imports replace
    earlier imported declarations with the same name. This mirrors the
    resolver's name-table order and keeps the two paths deterministic.
    Glob/glob collisions are rejected by the resolver before code generation;
    this function retains the same last-import policy as a defensive fallback
    for callers that flatten an already-loaded program directly. Local decls
    in [prog.prog_decls] always win over imported ones.

    Selective imports carry referenced helpers, including private ones, when
    [find_free_vars] detects them. That walker is partial: dependencies
    referenced only in record spreads, shorthand record fields or qualified
    constructors may be omitted. Public enums named directly by type or
    constructor in a selective import
    are included; enums containing only [Some], [None], [Ok] and [Err] are
    excluded from this direct selection because the runtime supplies them.

    [cache] memoises flattened dependencies for this traversal and is updated
    in place. [visiting] lists module paths already being traversed; a cycle
    uses the cached module's original declarations without further recursion.
    Imported declarations are prepended; [prog.prog_imports] is retained. *)
let rec flatten_imports_from
    (cache : (string list, program) Hashtbl.t)
    (visiting : string list list) (loader : t)
    (prog : program) : program =
  (* Local-decl names suppress same-named imports of any kind. *)
  let local_name_list =
    List.filter_map (function
      | TopFn fd -> Some fd.fd_name.name
      | TopConst { tc_name; _ } -> Some tc_name.name
      | TopType td -> Some td.td_name.name
      | _ -> None
    ) prog.prog_decls
  in
  let local_names = Hashtbl.create 32 in
  List.iter (fun n -> Hashtbl.replace local_names n ()) local_name_list;
  (* Keep imported declaration order stable while replacing the declaration
     stored for a later duplicate.  A plain `already_in` set used to keep the
     FIRST declaration here, while resolve.ml kept the LAST one (#743). *)
  let imported_by_name = Hashtbl.create 32 in
  let imported_origin = Hashtbl.create 32 in
  let imported_order = ref [] in
  let is_private = function
    | `Fn fd -> fd.fd_vis = Private
    | `Const (TopConst { tc_vis; _ }) -> tc_vis = Private
    | `Type (TopType td) -> td.td_vis = Private
    | `Const _ | `Type _ -> false
  in
  (* The same declaration reaches here more than once in a diamond (`D`
     imports `P` and `J`, and `J` imports `P`): flattening is recursive, so
     `P`'s helpers arrive both directly and inside `J`. That is one helper,
     not a collision. *)
  let same_decl a b = match a, b with
    | `Fn x, `Fn y -> x == y || x = y
    | `Const x, `Const y | `Type x, `Type y -> x == y || x = y
    | _ -> false
  in
  let add_imported origin name decl_kind =
    if not (Hashtbl.mem local_names name) then begin
      (match Hashtbl.find_opt imported_origin name,
             Hashtbl.find_opt imported_by_name name with
       | Some other, Some prev
         when other <> origin
              && (is_private prev || is_private decl_kind)
              && not (same_decl prev decl_kind) ->
         (* Two modules each carry a private helper of this name: one
            flattened program can hold only one, and keeping the later would
            make the other module call the wrong function.  (Public
            collisions are rejected by the resolver; #743's last-wins rule
            still applies to them.) *)
         raise (Flatten_error (ImportCollision (name, other, origin)))
       | _ -> ());
      if not (Hashtbl.mem imported_by_name name) then
        imported_order := name :: !imported_order;
      Hashtbl.replace imported_by_name name decl_kind;
      Hashtbl.replace imported_origin name origin
    end
  in
  List.iter (fun imp ->
      let path_strs path =
        List.map (fun (id : ident) -> id.name) path
      in
      let mod_path = match imp with
        | ImportSimple (p, _) | ImportList (p, _) | ImportGlob p -> path_strs p
      in
      match Hashtbl.find_opt loader.loaded mod_path with
      | None -> ()
      | Some lm0 ->
        (* Flatten the imported module's own imports first, so the closure
           below can reach what *it* uses from other modules: `Nav` importing
           `Router.on_url_change`, whose body calls `Tea.subs`, emitted an
           undefined `subs`. [visiting] stops an import cycle. *)
        let own = lm0.mod_program.prog_decls in
        let lm =
          if List.mem mod_path visiting then
            { lm0 with mod_program = qualify_private mod_path own lm0.mod_program }
          else
            (* Memoised per flatten call: a module shared by several import
               paths (a diamond) is flattened once, not once per path. *)
            let flat = match Hashtbl.find_opt cache mod_path with
              | Some p -> p
              | None ->
                let p = flatten_imports_from cache (mod_path :: visiting) loader
                    lm0.mod_program in
                let p = qualify_private mod_path own p in
                Hashtbl.replace cache mod_path p;
                p
            in
            { lm0 with mod_program = flat }
        in
        let public_decls = List.filter_map (fun decl ->
          match decl with
          | TopFn fd when fd.fd_vis = Public || fd.fd_vis = PubCrate ->
            Some (fd.fd_name.name, `Fn fd)
          | TopConst { tc_vis; tc_name; _ }
            when tc_vis = Public || tc_vis = PubCrate ->
            Some (tc_name.name, `Const decl)
          | _ -> None
        ) lm.mod_program.prog_decls in
        (* Transitive closure. A selected declaration's body may call
           helpers of the same module that the import list never names:
           `use Dom::{div}` pulls in `div`, whose body calls `h`. Leaving
           `h` out of the flattened program produced
           `ReferenceError: h is not defined` in the emitted module
           (tests/codegen-deno/dom_startup_error). Private helpers are
           pulled in too — a public wrapper may delegate to one. *)
        let module_value_decls =
          List.filter_map (fun decl ->
            match decl with
            | TopFn fd when fd.fd_body <> FnExtern ->
              Some (fd.fd_name.name, `Fn fd)
            | TopConst { tc_name; _ } as d ->
              Some (tc_name.name, `Const d)
            | _ -> None
          ) lm.mod_program.prog_decls
        in
        (* Constructors of this module's own enums, keyed by constructor
           name. An inlined wrapper body can name a constructor
           (`text` returns `VText(content)`) and the constructor exists
           only where the enum is declared. Option/Result constructors
           are excluded: every non-wasm preamble already defines
           Some/None/Ok/Err, and re-emitting them is the duplicate-`const`
           crash that reverted type-carrying in #138 — this narrows the
           carry to user enums a backend preamble cannot supply. *)
        let preamble_ctors = [ "Some"; "None"; "Ok"; "Err" ] in
        let ctor_decls = Hashtbl.create 16 in
        List.iter (fun decl ->
          match decl with
          | TopType td ->
            (match td.td_body with
             | TyEnum variants ->
               List.iter (fun (vd : variant_decl) ->
                 if not (List.mem vd.vd_name.name preamble_ctors) then
                   Hashtbl.replace ctor_decls vd.vd_name.name
                     (td.td_name.name, decl)
               ) variants
             | _ -> ())
          | _ -> ()) lm.mod_program.prog_decls;
        let by_name = Hashtbl.create 16 in
        List.iter (fun (n, d) -> Hashtbl.replace by_name n d) module_value_decls;
        (* Close a selection over the helpers its bodies reference, then emit
           it in this module's own declaration order.  Declarations must
           precede their uses (the checker rejects a forward reference, and
           the interpreter binds each function in an environment that holds
           only what came before it), so the order a selection was gathered
           in is not an order a consumer can use: `api` selected first and
           its helper `h` carried second left `h` unbound under `eval`.
           Every import kind needs the closure: a glob or a plain `use`
           carries the module's public declarations, whose bodies call
           private helpers just as a selected item's do. *)
        let close_over direct =
          let seen = Hashtbl.create 16 in
          List.iter (fun (n, _) -> Hashtbl.replace seen n ()) direct;
          let rec close pending acc =
            match pending with
            | [] -> List.rev acc
            | (_, dk) :: rest ->
              let free = match dk with
                | `Fn fd ->
                  let params =
                    List.map (fun (p : param) -> p.p_name.name) fd.fd_params
                  in
                  (match fd.fd_body with
                   | FnExtern -> []
                   | FnExpr e -> find_free_vars params e
                   | FnBlock b -> find_free_vars params (ExprBlock b))
                | `Const (TopConst { tc_value; _ }) -> find_free_vars [] tc_value
                | `Const _ | `Type _ -> []
              in
              let (pending, acc) =
                List.fold_left (fun (pending, acc) name ->
                  if Hashtbl.mem seen name then (pending, acc)
                  else begin
                    Hashtbl.replace seen name ();
                    match Hashtbl.find_opt by_name name with
                    | Some dep -> ((name, dep) :: pending, (name, dep) :: acc)
                    | None ->
                      (* A constructor name: carry the enum it belongs to,
                         once per type. *)
                      (match Hashtbl.find_opt ctor_decls name with
                       | Some (td_name, decl) when not (Hashtbl.mem seen td_name) ->
                         Hashtbl.replace seen td_name ();
                         ((td_name, `Type decl) :: pending,
                          (td_name, `Type decl) :: acc)
                       | _ -> (pending, acc))
                  end
                ) (rest, acc) free
              in
              close pending acc
          in
          let extras = close direct [] in
          let position = Hashtbl.create 32 in
          List.iteri (fun i decl ->
            let name = match decl with
              | TopFn fd -> Some fd.fd_name.name
              | TopConst { tc_name; _ } -> Some tc_name.name
              | TopType td -> Some td.td_name.name
              | _ -> None
            in
            Option.iter (fun n ->
              if not (Hashtbl.mem position n) then Hashtbl.replace position n i)
              name) lm.mod_program.prog_decls;
          let pos (n, _) =
            Option.value (Hashtbl.find_opt position n) ~default:max_int
          in
          List.stable_sort (fun a b -> compare (pos a) (pos b)) (direct @ extras)
        in
        let select = match imp with
          | ImportGlob _ -> close_over public_decls
          | ImportSimple _ ->
            (* `use Foo` brings the namespace into scope but doesn't import
               specific symbols. For codegens that need them inlined we still
               include all public top-level bindings — same as glob. The
               resolver determines what's referenceable; codegen just needs
               the bodies present. *)
            close_over public_decls
          | ImportList (_, items) ->
            (* Every alias an item is imported under. *)
            let aliases_of target =
              List.filter_map (fun item ->
                  match item.ii_alias with
                  | Some a when item.ii_name.name = target -> Some a.name
                  | _ -> None) items
            in
            (* Names the importer binds under their own name. *)
            let exposed =
              List.filter_map (fun item ->
                  match item.ii_alias with
                  | None -> Some item.ii_name.name
                  | Some _ -> None) items
            in
            (* An alias is a second declaration: for a mutable binding that
               would be a second, separate copy. *)
            List.iter (fun item ->
                match item.ii_alias,
                      List.assoc_opt item.ii_name.name public_decls with
                | Some a, Some (`Const (TopConst { tc_mut = true; _ })) ->
                  raise (Flatten_error
                           (MutableAlias (item.ii_name.name, a.name, mod_path)))
                | _ -> ()) items;
            let rename_decl bound_name found =
              match found with
              | `Fn fd ->
                `Fn { fd with fd_name = { fd.fd_name with name = bound_name } }
              | `Const (TopConst { tc_vis; tc_mut; tc_name; tc_ty; tc_value }) ->
                `Const (TopConst {
                  tc_vis;
                  tc_mut;
                  tc_name = { tc_name with name = bound_name };
                  tc_ty;
                  tc_value;
                })
              | `Const _ | `Type _ ->
                (* `Const` here is unreachable: public_decls only stores
                   TopConst under `Const`. `Type` decls are never renamed —
                   they are carried under their own name. *)
                found
            in
            let direct =
              List.filter_map (fun item ->
                let target = item.ii_name.name in
                List.find_opt (fun (n, _) -> n = target) public_decls
                |> Option.map (fun (_, found) -> (target, found))
              ) items
            in
            let closed = close_over direct in
            (* This module's own public functions and immutable constants that
               the closure carries but the importer does not bind under their
               own name (`helper` in `use M::{helper as h, api}`, or a public
               helper only `api` calls) get an internal name, with the
               references to them rewritten. Left under their own name, two
               modules' `helper`s would replace one another and `api` would
               call the other module's (CodeRabbit on #797). A mutable binding
               keeps its name: it may be imported under that name elsewhere,
               and a renamed copy would be separate storage. *)
            let hidden =
              List.filter_map (function
                  | TopFn fd
                    when (fd.fd_vis = Public || fd.fd_vis = PubCrate)
                         && fd.fd_body <> FnExtern
                         && not (List.mem fd.fd_name.name exposed)
                         && List.mem_assoc fd.fd_name.name closed ->
                    Some (fd.fd_name.name,
                          qualified_private_name mod_path fd.fd_name.name)
                  | TopConst { tc_vis = (Public | PubCrate); tc_mut = false;
                               tc_name; _ }
                    when not (List.mem tc_name.name exposed)
                         && List.mem_assoc tc_name.name closed ->
                    Some (tc_name.name,
                          qualified_private_name mod_path tc_name.name)
                  | _ -> None) lm0.mod_program.prog_decls
            in
            let internalise = function
              | `Fn fd ->
                (match rename_top_level hidden (TopFn fd) with
                 | TopFn fd' -> `Fn fd'
                 | _ -> `Fn fd)
              | `Const d -> `Const (rename_top_level hidden d)
              | `Type _ as t -> t
            in
            (* An aliased item (`use M::{helper as h, api}`) is bound under its
               alias for the importer; the original stays, under its internal
               name, because the module's other declarations (`api`) call it. *)
            List.concat_map (fun (n, dk) ->
                let dk = internalise dk in
                let n' = Option.value (List.assoc_opt n hidden) ~default:n in
                (n', dk) :: List.map (fun b -> (b, rename_decl b dk)) (aliases_of n))
              closed
        in
        (* Public enums the importer names directly. A constructor in the
           import list (`use Geometry::{Up, Down}`) or a type name the
           importer constructs was only carried when some carried *function*
           happened to construct it, so `Some(Up)` in the importer compiled
           to a reference to an undefined `Up`. Enums made only of the
           preamble's Option/Result constructors are never carried (every
           non-Wasm backend's runtime preamble already defines those). *)
        let preamble_only = [ "Some"; "None"; "Ok"; "Err" ] in
        let public_enums = List.filter_map (fun decl ->
          match decl with
          | TopType ({ td_body = TyEnum variants; _ } as td)
            when (td.td_vis = Public || td.td_vis = PubCrate)
                 && List.exists (fun (vd : variant_decl) ->
                      not (List.mem vd.vd_name.name preamble_only)) variants ->
            Some (td.td_name.name,
                  List.map (fun (vd : variant_decl) -> vd.vd_name.name) variants,
                  decl)
          | _ -> None) lm.mod_program.prog_decls in
        let named = match imp with
          | ImportList (_, items) ->
            List.filter (fun (ty, ctors, _) ->
              List.exists (fun item ->
                let n = item.ii_name.name in n = ty || List.mem n ctors) items)
              public_enums
          | ImportSimple _ | ImportGlob _ -> public_enums
        in
        List.iter (fun (ty, _, decl) -> add_imported mod_path ty (`Type decl)) named;
        List.iter (fun (name, decl_kind) -> add_imported mod_path name decl_kind) select
    ) prog.prog_imports;
  let imported_decls =
    List.rev !imported_order
    |> List.filter_map (fun name ->
         match Hashtbl.find_opt imported_by_name name with
         | Some (`Fn fd) -> Some (TopFn fd)
         | Some (`Const decl) -> Some decl
         | Some (`Type decl) -> Some decl
         | None -> None)
  in
  (* #138 follow-up: imported TYPE decls are intentionally NOT inlined here.
     An earlier #138 revision carried imported public [TopType]s so the
     prog_decls-iterating backends could register their constructors — but the
     non-Wasm backends (Deno-ESM / JS / Julia / C / Rust / ...) already emit
     the Option/Result constructors from a built-in runtime preamble, so
     carrying the prelude types declared `Some`/`None`/`Ok`/`Err` twice
     (`SyntaxError: Identifier 'Some' has already been declared` when the
     emitted Deno-ESM module is run under node). The Wasm backend learns
     imported variant tags natively via [Codegen.gen_imports] (it consumes the
     original, un-flattened [prog]); the other backends rely on their preamble.
     Re-introducing type-carrying for *user-defined* cross-module enums would
     need per-backend constructor dedup first. *)
  { prog with prog_decls = imported_decls @ prog.prog_decls }

(** Inline the declarations [prog]'s imports need (transitively) into [prog],
    for the backends that compile one flattened program. Uses only modules
    already loaded in [loader]; missing modules are silently skipped.
    See [flatten_imports_from] for selection and name precedence. *)
let flatten_imports (loader : t) (prog : program) : program =
  flatten_imports_from (Hashtbl.create 8) [] loader prog
