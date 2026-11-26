(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(** Same cube (#601): one program written in every face compiles to
    byte-identical core-WASM.

    The faces keep a block's trailing call as their author wrote it: a
    statement `println("Hello");` (canonical, jaffa) or a tail expression
    `println("Hello")` (rattle, pseudo, lucid, and cafe written without a
    `;`). For a Unit-returning call the WASM emitter now gives both forms
    the same code, so the faces cannot split into two wasm classes. *)

open Affinescript

let effect_io = "effect IO {\n  fn println(s: String) -> ();\n}\n\n"

(* (face, parser, source) for the one-call greet program. *)
let greet =
  [ ("canonical", Parse_driver.parse_file,
     effect_io ^ "fn main() -{IO}-> () {\n  println(\"Hello\");\n}\n");
    ("rattle", Python_face.parse_file_python,
     "effect IO:\n    fn println(s: String) -> ();\n\n\
      def main() -{IO}-> ():\n    println(\"Hello\")\n");
    ("jaffa", Js_face.parse_file_js,
     effect_io ^ "function main() -{IO}-> () {\n  println(\"Hello\");\n}\n");
    ("cafe", Cafe_face.parse_file_cafe,
     effect_io ^ "fn main() -{IO}-> () {\n  println(\"Hello\")\n}\n");
    ("pseudo", Pseudocode_face.parse_file_pseudocode,
     effect_io ^ "function main() -{IO}-> () do\n    output \"Hello\"\nend\n");
    ("lucid", Lucid_face.parse_file_lucid,
     effect_io ^ "main :: -{IO}-> ()\nmain () = println(\"Hello\")\n") ]

let read_all path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic; s

let with_tmp suffix contents f =
  let path = Filename.temp_file "same_cube" suffix in
  Fun.protect ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let oc = open_out_bin path in
      output_string oc contents; close_out oc;
      f path)

let wasm_bytes_of_prog prog =
  match Codegen.generate_module (Opt.fold_constants_program prog) with
  | Error e -> Alcotest.failf "codegen: %s" (Codegen.show_codegen_error e)
  | Ok m ->
      with_tmp ".wasm" "" (fun out ->
        Wasm_encode.write_module_to_file out m;
        read_all out)

let wasm_of (face, parse, src) =
  with_tmp ".affine" src (fun path ->
    match parse path with
    | prog -> wasm_bytes_of_prog prog
    | exception e -> Alcotest.failf "%s face did not parse: %s" face (Printexc.to_string e))

let test_greet_one_class () =
  let reference = wasm_of (List.hd greet) in
  List.iter
    (fun ((face, _, _) as g) ->
      if not (String.equal (wasm_of g) reference) then
        Alcotest.failf "the %s face compiles greet to different wasm than canonical" face)
    (List.tl greet)

(* A value-returning tail is not Unit: it keeps its value, so its wasm
   differs from the statement form (which would not type-check as Int). *)
let test_value_tail_untouched () =
  let src tail =
    "fn k() -> Int { 7 }\n\npub fn f() -> Int {\n  " ^ tail ^ "\n}\n" in
  let a = wasm_of ("canonical", Parse_driver.parse_file, src "k()") in
  let b = wasm_of ("canonical", Parse_driver.parse_file, src "k();") in
  Alcotest.(check bool) "an Int tail is not dropped" false (String.equal a b)

let tests = [
  Alcotest.test_case "greet: six faces, one wasm" `Quick test_greet_one_class;
  Alcotest.test_case "a value-returning tail keeps its value" `Quick
    test_value_tail_untouched;
]
