(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Regression: every `b "name"` registered in lib/codegen_deno.ml's
    `deno_builtins` must have a matching `extern fn name` declaration
    somewhere under `stdlib/*.affine`. Two production bugs of exactly
    this shape landed within two PRs on 2026-05-31 (#504 STEP 3 — Deno
    bytesLength / bytesByteAt / bytesAsciiSlice; #509 STEP 4-B — Deno
    math_random / random_u32 / random_in_range / performance_now). In
    both cases the codegen lowering was wired AND a fixture referenced
    the name, but the stdlib `pub extern fn` was missing, leaving the
    resolver to fail with `undefined value: X` at compile time. Both
    landed on main because nothing cross-checked the two registries.

    The codegen builtin space spans multiple stdlib modules (Deno,
    Canvas, Ipc, Pixi*, Motion, plus base prelude names like
    `string_length`, `int_to_string`), so the test reads every
    `stdlib/*.affine` (not just Deno.affine) and asserts subset. *)

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* OCaml's `Str` module does not interpret backslash escapes inside
   `[...]` character classes — `[ \t]+` would match space-or-backslash-
   or-`t`, then consume the `t` of `targetPostMessage` and report
   `argetPostMessage` as the capture. The source files use plain spaces
   (no tabs) so a `[ ]+` class is precise and correct. *)
let codegen_builtin_re =
  Str.regexp {|b[ ]+"\([A-Za-z_][A-Za-z0-9_]*\)"|}

let stdlib_extern_re =
  Str.regexp {|extern[ ]+fn[ ]+\([A-Za-z_][A-Za-z0-9_]*\)|}

let extract_names re text =
  let rec loop pos acc =
    match (try Some (Str.search_forward re text pos) with Not_found -> None) with
    | None -> List.rev acc
    | Some _ ->
        let name = Str.matched_group 1 text in
        loop (Str.match_end ()) (name :: acc)
  in
  loop 0 []

let stdlib_dir =
  let candidates =
    (match Sys.getenv_opt "AFFINESCRIPT_STDLIB" with
     | Some d -> [ d ] | None -> [])
    @ [ "../stdlib"; "stdlib"; "../../stdlib" ]
  in
  match
    List.find_opt
      (fun d -> Sys.file_exists (Filename.concat d "prelude.affine"))
      candidates
  with
  | Some d -> d
  | None ->
      failwith "test_deno_builtins_consistency: cannot locate stdlib/ (no prelude.affine)"

let codegen_path =
  let candidates =
    [ "../lib/codegen_deno.ml"; "lib/codegen_deno.ml"; "../../lib/codegen_deno.ml" ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some p -> p
  | None ->
      failwith "test_deno_builtins_consistency: cannot locate lib/codegen_deno.ml"

let list_stdlib_affine () =
  Sys.readdir stdlib_dir
  |> Array.to_list
  |> List.filter (fun n -> Filename.check_suffix n ".affine")
  |> List.map (Filename.concat stdlib_dir)

(* Compiler-synthesised JS helpers and runtime intrinsics don't have
   per-name `extern fn` decls; they're wired in `lib/resolve.ml` /
   `lib/typecheck.ml` directly. Same for the JSON-FFI bridge surface
   (one umbrella binding header, no per-call extern). *)
let codegen_only_names =
  [ "len"; "panic"; "get"; "set"; "slice"; "show"; "error"; "make_ref";
    "int_to_string"; "float_to_string"; "string_to_int"; "parse_int";
    "parse_float"; "int_to_char"; "char_to_int";
    "string_length"; "string_sub"; "string_get"; "string_find";
    "string_char_code_at"; "string_from_char_code";
    "to_lowercase"; "to_uppercase"; "trim";
    "http_request";
    "hpm_json_array_get"; "hpm_json_array_len"; "hpm_json_bool";
    "hpm_json_escape_string"; "hpm_json_float"; "hpm_json_free";
    "hpm_json_int"; "hpm_json_object_get"; "hpm_json_parse";
    "hpm_json_string"; "hpm_json_type";
  ]

let test_codegen_subset_of_stdlib () =
  let codegen_src = read_file codegen_path in
  let start_marker = "let deno_builtins" in
  let s_idx =
    try Str.search_forward (Str.regexp_string start_marker) codegen_src 0
    with Not_found ->
      failwith "`let deno_builtins` not found in lib/codegen_deno.ml"
  in
  let after = String.sub codegen_src s_idx (String.length codegen_src - s_idx) in
  let codegen_names = extract_names codegen_builtin_re after in

  let stdlib_names =
    list_stdlib_affine ()
    |> List.concat_map (fun p -> extract_names stdlib_extern_re (read_file p))
  in
  let stdlib_set = List.sort_uniq compare stdlib_names in
  let codegen_only_set = List.sort_uniq compare codegen_only_names in

  let missing =
    List.filter
      (fun n -> not (List.mem n stdlib_set) && not (List.mem n codegen_only_set))
      codegen_names
    |> List.sort_uniq compare
  in
  match missing with
  | [] -> ()
  | xs ->
      let msg =
        Printf.sprintf
          "codegen registers Deno-ESM builtins with no matching `extern fn` under stdlib/*.affine and no entry in the codegen-only allowlist: %s\n\
           For each name: either add `pub extern fn NAME(...)` to the relevant `stdlib/*.affine` module, OR add it to `codegen_only_names` in this test if it is a true compiler-internal name."
          (String.concat ", " xs)
      in
      Alcotest.fail msg

let tests =
  [ ("codegen builtins subset of stdlib extern decls",
     `Quick,
     test_codegen_subset_of_stdlib) ]
