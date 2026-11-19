(* SPDX-License-Identifier: MPL-2.0 *)
open Affinescript

(* Exercise emission directly: the frontend currently rejects nested
   statement bindings that shadow an enclosing binding with another type. *)
let check_divisions src =
  let prog = Parse_driver.parse_string ~file:"<int_scope>" src in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  let ctx = match Resolve.resolve_program_with_loader prog loader with
    | Ok (ctx, _) -> ctx
    | Error (e, _) -> Alcotest.fail (Resolve.show_resolve_error e)
  in
  match Codegen_deno.codegen_bun prog ctx.symbols with
  | Error e -> Alcotest.fail e
  | Ok js ->
      Alcotest.(check int) "only the Int division truncates" 1
        (Test_int_div_js.count_occurrences ~needle:"Math.trunc(" js)

let tests = [
  Alcotest.test_case "inner Int preserves outer Float" `Quick (fun () ->
    check_divisions {|pub fn f() -> Float {
      let x = 7.0; let y = 2.0;
      { let x = 9; let y = 2; x / y; };
      x / y
    }|});
  Alcotest.test_case "inner Float preserves outer Int" `Quick (fun () ->
    check_divisions {|pub fn f() -> Int {
      let x = 7;
      { let x = 9.0; x / 2.0; };
      x / 2
    }|});
]
