(* SPDX-License-Identifier: MIT OR AGPL-3.0-or-later *)
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
[@@deriving show]

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

(** Create default configuration *)
let default_config () : config =
  let stdlib_path =
    try Sys.getenv "AFFINESCRIPT_STDLIB"
    with Not_found -> "./stdlib"
  in
  {
    stdlib_path;
    search_paths = [];
    current_dir = Sys.getcwd ();
  }

(** Convert module path to file path candidates

    For module path ["Math", "Geometry"], try:
    1. Math/Geometry.as (nested module)
    2. Math.as (check if it exports Geometry)
*)
let path_to_candidates (mod_path : string list) : string list =
  match mod_path with
  | [] -> []
  | [name] -> [name ^ ".as"]
  | _ ->
    (* Try nested path: A/B/C.as *)
    let nested = String.concat "/" mod_path ^ ".as" in
    (* Try parent with submodule: A/B.as (if B declares module C) *)
    let parent_parts = List.rev mod_path |> List.tl |> List.rev in
    let parent = String.concat "/" parent_parts ^ ".as" in
    [nested; parent]

(** Search for a module file in search paths *)
let find_module_file (loader : t) (mod_path : string list) : string option =
  let candidates = path_to_candidates mod_path in
  let search_in_dir dir =
    List.find_map (fun candidate ->
      let full_path = Filename.concat dir candidate in
      if Sys.file_exists full_path then Some full_path else None
    ) candidates
  in
  (* Search order: 1) current dir, 2) stdlib, 3) additional search paths *)
  let all_paths =
    loader.config.current_dir ::
    loader.config.stdlib_path ::
    loader.config.search_paths
  in
  List.find_map search_in_dir all_paths

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
      | None -> Error (ModuleNotFound mod_path)
      | Some file_path ->
        (* Parse the module *)
        match parse_module_file file_path with
        | Error e -> Error e
        | Ok prog ->
          (* Load dependencies first (imports) *)
          let* () = load_dependencies loader prog.prog_imports in

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
