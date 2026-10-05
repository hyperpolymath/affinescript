(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <jonathan.jewell@open.ac.uk> *)
(** [$AFFINESCRIPT_PATH]: colon-separated directories the module loader
    searches after the current directory and the stdlib, so third-party
    packages (affinescript-tea, ...) can be imported from outside the
    importing program's directory. *)

open Affinescript

(** A fresh temp directory holding one module file [name].affine. *)
let module_dir name body =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "as_path_%s_%d" name (Unix.getpid ())) in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let oc = open_out (Filename.concat dir (name ^ ".affine")) in
  output_string oc body;
  close_out oc;
  dir

(** Run [f] with [$AFFINESCRIPT_PATH] set to [v], restoring it afterwards. *)
let with_path v f =
  let old = Sys.getenv_opt "AFFINESCRIPT_PATH" in
  Unix.putenv "AFFINESCRIPT_PATH" v;
  Fun.protect f ~finally:(fun () ->
    Unix.putenv "AFFINESCRIPT_PATH" (Option.value old ~default:""))

let parses_entries () =
  with_path "/a::/b:" (fun () ->
    Alcotest.(check (list string)) "empty entries dropped" [ "/a"; "/b" ]
      (Module_loader.env_search_paths ()))

let finds_module_on_path () =
  let dir = module_dir "PathLib" "module PathLib;\npub fn seven() -> Int = 7;\n" in
  let loader () = Module_loader.create (Module_loader.default_config ()) in
  with_path "" (fun () ->
    Alcotest.(check bool) "absent without the path" true
      (Module_loader.find_module_file (loader ()) [ "PathLib" ] = None));
  with_path dir (fun () ->
    Alcotest.(check bool) "found via AFFINESCRIPT_PATH" true
      (Module_loader.find_module_file (loader ()) [ "PathLib" ] <> None))

let tests =
  [
    Alcotest.test_case "parses entries" `Quick parses_entries;
    Alcotest.test_case "finds module on path" `Quick finds_module_on_path;
  ]
