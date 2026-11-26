(* SPDX-License-Identifier: MPL-2.0 *)
open Affinescript

(* Exercise emission directly: the frontend currently rejects nested
   statement bindings that shadow an enclosing binding with another type. *)
let count_truncs src =
  let prog = Parse_driver.parse_string ~file:"<int_scope>" src in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  let ctx = match Resolve.resolve_program_with_loader prog loader with
    | Ok (ctx, _) -> ctx
    | Error (e, _) -> Alcotest.fail (Resolve.show_resolve_error e)
  in
  match Codegen_deno.codegen_bun prog ctx.symbols with
  | Error e -> Alcotest.fail e
  | Ok js -> Test_int_div_js.count_occurrences ~needle:"Math.trunc(" js

let expect_truncs what n src =
  Alcotest.(check int) what n (count_truncs src)

let check_divisions = expect_truncs "only the Int division truncates" 1

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
  (* An assignment in a nested block changes the enclosing variable, so the
     Int it records outlives the block. *)
  Alcotest.test_case "Int assigned in a block to an enclosing var" `Quick (fun () ->
    expect_truncs "the division after the block truncates" 1
      {|pub fn f(flag: Bool) -> Int {
        let mut n = match flag { true => 5, false => 7 };
        { n = 5; };
        n / 2
      }|});
  (* ... but not when the block declared its own binding of that name. *)
  Alcotest.test_case "Int assigned to a block's own binding" `Quick (fun () ->
    expect_truncs "the outer Float division does not truncate" 0
      {|pub fn f() -> Float {
        let mut n = 7.0;
        { let mut n = 0.0; n = 5; };
        n / 2.0
      }|});
  (* A lambda parameter is scoped to the lambda: an Int assigned to it does
     not classify a same-named outer Float. *)
  Alcotest.test_case "Int assigned to a lambda parameter" `Quick (fun () ->
    expect_truncs "the outer Float division does not truncate" 0
      {|pub fn f() -> Float {
        let k = 7.0;
        let g = |k| { k = 4; 0 };
        k / 2.0
      }|});
  (* A callable local is classified by its own declared return type. *)
  Alcotest.test_case "Int-returning callable parameter" `Quick (fun () ->
    expect_truncs "len(s) / 2 truncates" 1
      {|pub fn f(len: fn(String) -> Int) -> Int { len("abcde") / 2 }|});
  Alcotest.test_case "Float-returning callable shadows intrinsic" `Quick (fun () ->
    expect_truncs "len(s) / 2.0 does not truncate" 0
      {|pub fn f(len: fn(String) -> Float) -> Float { len("abcde") / 2.0 }|});
  Alcotest.test_case "Int-returning callable let" `Quick (fun () ->
    expect_truncs "w(s) / 2 truncates" 1
      {|pub fn f() -> Int {
        let w: fn(String) -> Int = |s| 5;
        w("abcde") / 2
      }|});
]
