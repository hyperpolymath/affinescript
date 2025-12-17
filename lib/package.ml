(** Package manager for AffineScript *)

(** Semantic version *)
type version = {
  major: int;
  minor: int;
  patch: int;
  prerelease: string option;
}
[@@deriving show]

let parse_version s =
  let parts = String.split_on_char '.' s in
  match parts with
  | [major; minor; patch] ->
      (try
         let patch, pre = match String.split_on_char '-' patch with
           | [p] -> (p, None)
           | [p; pre] -> (p, Some pre)
           | _ -> (patch, None)
         in
         Some { major = int_of_string major;
                minor = int_of_string minor;
                patch = int_of_string patch;
                prerelease = pre }
       with _ -> None)
  | [major; minor] ->
      (try Some { major = int_of_string major;
                  minor = int_of_string minor;
                  patch = 0;
                  prerelease = None }
       with _ -> None)
  | _ -> None

let string_of_version v =
  let base = Printf.sprintf "%d.%d.%d" v.major v.minor v.patch in
  match v.prerelease with
  | Some pre -> base ^ "-" ^ pre
  | None -> base

let compare_version v1 v2 =
  let c = compare v1.major v2.major in
  if c <> 0 then c
  else let c = compare v1.minor v2.minor in
    if c <> 0 then c
    else let c = compare v1.patch v2.patch in
      if c <> 0 then c
      else match v1.prerelease, v2.prerelease with
        | None, None -> 0
        | None, Some _ -> 1  (* Release > prerelease *)
        | Some _, None -> -1
        | Some p1, Some p2 -> String.compare p1 p2

(** Version constraint *)
type version_constraint =
  | VExact of version           (** = 1.2.3 *)
  | VGreater of version         (** > 1.2.3 *)
  | VGreaterEq of version       (** >= 1.2.3 *)
  | VLess of version            (** < 1.2.3 *)
  | VLessEq of version          (** <= 1.2.3 *)
  | VCaret of version           (** ^1.2.3 (compatible) *)
  | VTilde of version           (** ~1.2.3 (patch-level) *)
  | VAny                        (** * *)
  | VAnd of version_constraint * version_constraint
  | VOr of version_constraint * version_constraint
[@@deriving show]

let rec satisfies_constraint v = function
  | VExact v2 -> compare_version v v2 = 0
  | VGreater v2 -> compare_version v v2 > 0
  | VGreaterEq v2 -> compare_version v v2 >= 0
  | VLess v2 -> compare_version v v2 < 0
  | VLessEq v2 -> compare_version v v2 <= 0
  | VCaret v2 ->
      (* ^1.2.3 means >=1.2.3, <2.0.0 (or <0.2.0 if major=0, <0.0.4 if minor=0) *)
      compare_version v v2 >= 0 &&
      (if v2.major > 0 then v.major = v2.major
       else if v2.minor > 0 then v.major = 0 && v.minor = v2.minor
       else v.major = 0 && v.minor = 0 && v.patch = v2.patch)
  | VTilde v2 ->
      (* ~1.2.3 means >=1.2.3, <1.3.0 *)
      compare_version v v2 >= 0 &&
      v.major = v2.major && v.minor = v2.minor
  | VAny -> true
  | VAnd (c1, c2) -> satisfies_constraint v c1 && satisfies_constraint v c2
  | VOr (c1, c2) -> satisfies_constraint v c1 || satisfies_constraint v c2

(** Dependency specification *)
type dependency = {
  dep_name: string;
  dep_version: version_constraint;
  dep_optional: bool;
  dep_features: string list;
}
[@@deriving show]

(** Package target *)
type target_type =
  | TargetLib
  | TargetBin
  | TargetTest
  | TargetBench
[@@deriving show]

type target = {
  tgt_name: string;
  tgt_type: target_type;
  tgt_path: string;
  tgt_deps: string list;  (** Internal deps *)
}
[@@deriving show]

(** Package manifest (afs.toml) *)
type manifest = {
  pkg_name: string;
  pkg_version: version;
  pkg_authors: string list;
  pkg_license: string option;
  pkg_description: string option;
  pkg_repository: string option;
  pkg_homepage: string option;
  pkg_keywords: string list;
  pkg_categories: string list;
  pkg_edition: string;  (** AffineScript edition *)
  pkg_dependencies: dependency list;
  pkg_dev_dependencies: dependency list;
  pkg_build_dependencies: dependency list;
  pkg_features: (string * string list) list;  (** Feature flags *)
  pkg_default_features: string list;
  pkg_targets: target list;
}
[@@deriving show]

let empty_manifest name = {
  pkg_name = name;
  pkg_version = { major = 0; minor = 1; patch = 0; prerelease = None };
  pkg_authors = [];
  pkg_license = None;
  pkg_description = None;
  pkg_repository = None;
  pkg_homepage = None;
  pkg_keywords = [];
  pkg_categories = [];
  pkg_edition = "2024";
  pkg_dependencies = [];
  pkg_dev_dependencies = [];
  pkg_build_dependencies = [];
  pkg_features = [];
  pkg_default_features = [];
  pkg_targets = [{ tgt_name = name; tgt_type = TargetLib; tgt_path = "lib"; tgt_deps = [] }];
}

(** Lock file entry *)
type lock_entry = {
  le_name: string;
  le_version: version;
  le_source: string;  (** registry, git, path *)
  le_checksum: string option;
  le_dependencies: (string * version) list;
}
[@@deriving show]

(** Lock file *)
type lock_file = {
  lf_version: int;
  lf_entries: lock_entry list;
}
[@@deriving show]

(** Package source *)
type package_source =
  | SourceRegistry of string     (** Official registry *)
  | SourceGit of string * string (** URL, ref *)
  | SourcePath of string         (** Local path *)
[@@deriving show]

(** Resolved package *)
type resolved_package = {
  rp_manifest: manifest;
  rp_source: package_source;
  rp_path: string;  (** Local path after fetch *)
}

(** Package registry *)
type registry = {
  reg_url: string;
  reg_name: string;
}

let default_registry = {
  reg_url = "https://packages.affinescript.org";
  reg_name = "afs";
}

(** Package manager state *)
type pm_state = {
  pm_root: string;              (** Project root *)
  pm_cache_dir: string;         (** Global cache directory *)
  pm_manifest: manifest option;
  pm_lock: lock_file option;
  pm_registries: registry list;
}

let init_state root =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  {
    pm_root = root;
    pm_cache_dir = Filename.concat home ".afs/cache";
    pm_manifest = None;
    pm_lock = None;
    pm_registries = [default_registry];
  }

(** Parse manifest from TOML (simplified) *)
let parse_manifest_string content =
  (* Simplified TOML parsing - would use a real parser in production *)
  let lines = String.split_on_char '\n' content in
  let manifest = ref (empty_manifest "unknown") in
  let current_section = ref "package" in
  List.iter (fun line ->
    let line = String.trim line in
    if String.length line > 0 && line.[0] = '[' then
      current_section := String.sub line 1 (String.length line - 2)
    else if String.contains line '=' then
      let parts = String.split_on_char '=' line in
      match parts with
      | [key; value] ->
          let key = String.trim key in
          let value = String.trim value in
          let value = if String.length value > 1 && value.[0] = '"' then
            String.sub value 1 (String.length value - 2)
          else value in
          (match !current_section, key with
           | "package", "name" -> manifest := { !manifest with pkg_name = value }
           | "package", "version" ->
               (match parse_version value with
                | Some v -> manifest := { !manifest with pkg_version = v }
                | None -> ())
           | "package", "license" -> manifest := { !manifest with pkg_license = Some value }
           | "package", "description" -> manifest := { !manifest with pkg_description = Some value }
           | _ -> ())
      | _ -> ()
  ) lines;
  !manifest

(** Read manifest from file *)
let read_manifest path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some (parse_manifest_string content)
  with _ -> None

(** Write manifest to file *)
let write_manifest path manifest =
  let oc = open_out path in
  Printf.fprintf oc "[package]\n";
  Printf.fprintf oc "name = \"%s\"\n" manifest.pkg_name;
  Printf.fprintf oc "version = \"%s\"\n" (string_of_version manifest.pkg_version);
  Option.iter (Printf.fprintf oc "license = \"%s\"\n") manifest.pkg_license;
  Option.iter (Printf.fprintf oc "description = \"%s\"\n") manifest.pkg_description;
  Printf.fprintf oc "edition = \"%s\"\n" manifest.pkg_edition;
  if manifest.pkg_dependencies <> [] then begin
    Printf.fprintf oc "\n[dependencies]\n";
    List.iter (fun dep ->
      Printf.fprintf oc "%s = \"%s\"\n" dep.dep_name
        (show_version_constraint dep.dep_version)
    ) manifest.pkg_dependencies
  end;
  close_out oc

(** Dependency resolution *)

type resolution_error =
  | PackageNotFound of string
  | VersionNotFound of string * version_constraint
  | ConflictingVersions of string * version * version
  | CyclicDependency of string list
[@@deriving show]

(** Build dependency graph *)
type dep_graph = {
  dg_nodes: (string, manifest) Hashtbl.t;
  dg_edges: (string, string list) Hashtbl.t;
}

let create_dep_graph () = {
  dg_nodes = Hashtbl.create 32;
  dg_edges = Hashtbl.create 32;
}

(** Topological sort of dependencies *)
let topo_sort graph =
  let visited = Hashtbl.create 32 in
  let temp = Hashtbl.create 32 in
  let result = ref [] in
  let rec visit name =
    if Hashtbl.mem temp name then
      Error (CyclicDependency [name])
    else if not (Hashtbl.mem visited name) then begin
      Hashtbl.replace temp name true;
      let deps = Hashtbl.find_opt graph.dg_edges name |> Option.value ~default:[] in
      let errors = List.filter_map (fun dep ->
        match visit dep with
        | Error e -> Some e
        | Ok () -> None
      ) deps in
      match errors with
      | e :: _ -> Error e
      | [] ->
          Hashtbl.remove temp name;
          Hashtbl.replace visited name true;
          result := name :: !result;
          Ok ()
    end
    else Ok ()
  in
  let nodes = Hashtbl.fold (fun name _ acc -> name :: acc) graph.dg_nodes [] in
  let errors = List.filter_map (fun name ->
    match visit name with
    | Error e -> Some e
    | Ok () -> None
  ) nodes in
  match errors with
  | e :: _ -> Error e
  | [] -> Ok !result

(** Package manager commands *)

let cmd_init state name =
  let manifest = empty_manifest name in
  let manifest_path = Filename.concat state.pm_root "afs.toml" in
  write_manifest manifest_path manifest;
  (* Create directory structure *)
  let _ = Sys.command (Printf.sprintf "mkdir -p %s/lib" state.pm_root) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s/bin" state.pm_root) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s/test" state.pm_root) in
  (* Create main.afs *)
  let main_path = Filename.concat state.pm_root "lib/main.afs" in
  let oc = open_out main_path in
  Printf.fprintf oc "// %s - AffineScript project\n\n" name;
  Printf.fprintf oc "pub fn hello() -> String {\n";
  Printf.fprintf oc "  \"Hello from %s!\"\n" name;
  Printf.fprintf oc "}\n";
  close_out oc;
  Ok ()

let cmd_add state dep_name version_str =
  match state.pm_manifest with
  | None -> Error "No manifest found. Run 'afs init' first."
  | Some manifest ->
      let version_constraint = match version_str with
        | "*" -> VAny
        | s when String.length s > 0 && s.[0] = '^' ->
            (match parse_version (String.sub s 1 (String.length s - 1)) with
             | Some v -> VCaret v
             | None -> VAny)
        | s ->
            (match parse_version s with
             | Some v -> VCaret v  (* Default to caret *)
             | None -> VAny)
      in
      let dep = {
        dep_name;
        dep_version = version_constraint;
        dep_optional = false;
        dep_features = [];
      } in
      let manifest' = { manifest with
        pkg_dependencies = dep :: manifest.pkg_dependencies
      } in
      let manifest_path = Filename.concat state.pm_root "afs.toml" in
      write_manifest manifest_path manifest';
      Ok ()

let cmd_remove state dep_name =
  match state.pm_manifest with
  | None -> Error "No manifest found."
  | Some manifest ->
      let deps' = List.filter (fun d -> d.dep_name <> dep_name) manifest.pkg_dependencies in
      let manifest' = { manifest with pkg_dependencies = deps' } in
      let manifest_path = Filename.concat state.pm_root "afs.toml" in
      write_manifest manifest_path manifest';
      Ok ()

let cmd_build _state =
  (* Would invoke compiler on all targets *)
  Ok ()

let cmd_test _state =
  (* Would run test targets *)
  Ok ()

let cmd_publish _state =
  (* Would publish to registry *)
  Ok ()
