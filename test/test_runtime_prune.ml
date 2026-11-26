(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Runtime pruning ([Codegen_deno.prune_runtime]) relies on the runtime's
    shape: a sequence of top-level [const] declarations whose continuation
    lines are indented or close a bracket, with no helper reached by a
    computed name. These tests hold the runtime to that shape, so a helper
    written another way fails here instead of being pruned wrongly. *)

open Affinescript

let runtime = Codegen_deno.bun_host_prelude ^ Codegen_deno.common_prelude

let test_shape () =
  String.split_on_char '\n' runtime
  |> List.iteri (fun i line ->
         let t = String.trim line in
         let ok =
           t = ""
           || (String.length t >= 2 && String.sub t 0 2 = "//")
           || Codegen_deno.declared_name line <> None
           || line.[0] = ' ' || line.[0] = '\t'
           || line.[0] = '}' || line.[0] = ')' || line.[0] = ']'
         in
         if not ok then
           Alcotest.failf "runtime line %d is neither a const declaration nor a continuation: %s" (i + 1) line)

let test_no_computed_access () =
  List.iter (fun bad ->
      let n = String.length bad in
      let rec find i =
        i + n <= String.length runtime && (String.sub runtime i n = bad || find (i + 1)) in
      if find 0 then Alcotest.failf "runtime reaches something by computed name: %s" bad)
    [ "eval("; "new Function"; "globalThis[\"__as"; "globalThis['__as"; "\"__as_\" +"; "'__as_' +" ]

let kept body name =
  let out = Codegen_deno.prune_runtime runtime ~body in
  List.mem name (List.filter_map Codegen_deno.declared_name (String.split_on_char '\n' out))

let test_reachability () =
  Alcotest.(check bool) "direct use kept" true (kept "x = Some(1);" "Some");
  Alcotest.(check bool) "transitive use kept" true (kept "__as_hpmJsonParse(s)" "None");
  Alcotest.(check bool) "unused DOM sink dropped" false (kept "x = Some(1);" "__as_bodySetInnerHTML");
  Alcotest.(check bool) "used DOM sink kept" true (kept "__as_bodySetInnerHTML(h)" "__as_bodySetInnerHTML")

let test_order_preserved () =
  let names s = List.filter_map Codegen_deno.declared_name (String.split_on_char '\n' s) in
  let all = names runtime in
  let pruned = names (Codegen_deno.prune_runtime runtime ~body:"__as_hpmJsonParse(s); __as_urlParse(u)") in
  let rec subseq xs ys = match xs, ys with
    | [], _ -> true | _, [] -> false
    | x :: xs', y :: ys' -> if x = y then subseq xs' ys' else subseq xs ys' in
  Alcotest.(check bool) "kept helpers keep their order" true (subseq pruned all)

let tests =
  [ ("runtime is const declarations and continuations", `Quick, test_shape);
    ("runtime reaches no helper by computed name", `Quick, test_no_computed_access);
    ("pruning keeps what the body reaches, transitively", `Quick, test_reachability);
    ("pruning preserves declaration order", `Quick, test_order_preserved) ]
