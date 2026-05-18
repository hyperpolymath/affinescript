(* #{ record-syntax codemod (affinescript#218). Parses each .affine with
   the instrumented origin/main grammar, which records the byte offset of
   every record-literal LBRACE in Affinescript.Codemod_hook.brace_offsets.
   We then insert '#' immediately before each such '{', so `{` -> `#{` and
   `Foo {` -> `Foo #{`. Only true record literals are touched.

   Usage:
     codemod --check FILE...   # report offsets, write nothing
     codemod FILE...           # rewrite in place (only if it parses &
                               # every offset really points at '{') *)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic; s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s; close_out oc

let process ~check path =
  Affinescript.Codemod_hook.brace_offsets := [];
  match
    (try Affinescript.Parse.parse_file path
     with e -> Error (Printexc.to_string e, Affinescript.Span.dummy))
  with
  | Error (msg, _) ->
    Printf.eprintf "SKIP %s -- parse error: %s\n" path msg; false
  | Ok _ ->
    let offsets =
      !Affinescript.Codemod_hook.brace_offsets
      |> List.sort_uniq (fun a b -> compare b a)  (* descending, deduped *)
    in
    let src = read_file path in
    let len = String.length src in
    let bad =
      List.find_opt
        (fun o -> o < 0 || o >= len || src.[o] <> '{') offsets
    in
    (match bad with
     | Some o ->
       Printf.eprintf
         "SKIP %s -- offset %d is not '{'; not rewriting\n" path o;
       false
     | None ->
       if check then begin
         Printf.printf "%s: %d record literal(s) at %s\n" path
           (List.length offsets)
           (String.concat "," (List.map string_of_int offsets));
         true
       end else if offsets = [] then begin
         Printf.printf "OK %s -- no record literals\n" path; true
       end else begin
         let out =
           List.fold_left
             (fun acc o ->
                String.sub acc 0 o ^ "#" ^
                String.sub acc o (String.length acc - o))
             src offsets
         in
         write_file path out;
         Printf.printf "REWROTE %s -- %d record literal(s)\n"
           path (List.length offsets);
         true
       end)

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let check, files =
    match args with
    | "--check" :: rest -> true, rest
    | rest -> false, rest
  in
  let ok = ref 0 and skip = ref 0 in
  List.iter
    (fun f -> if process ~check f then incr ok else incr skip)
    files;
  Printf.eprintf "done: %d processed, %d skipped\n" !ok !skip
