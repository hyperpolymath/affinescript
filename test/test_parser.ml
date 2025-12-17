(** Parser tests *)

open Affinescript

(** Helper to parse an expression *)
let parse_expr source =
  Parse_driver.parse_expr ~file:"<test>" source

(** Helper to parse a program *)
let parse source =
  Parse_driver.parse_string ~file:"<test>" source

(** Check that parsing succeeds *)
let parses source =
  try
    let _ = parse source in
    true
  with _ -> false

(** Check that parsing an expression succeeds *)
let parses_expr source =
  try
    let _ = parse_expr source in
    true
  with _ -> false

(* ========== Expression Tests ========== *)

let test_literal_int () =
  let expr = parse_expr "42" in
  match expr with
  | Ast.ExprLit (Ast.LitInt (42, _)) -> ()
  | _ -> Alcotest.fail "Expected integer literal"

let test_literal_float () =
  let expr = parse_expr "3.14" in
  match expr with
  | Ast.ExprLit (Ast.LitFloat (f, _)) when abs_float (f -. 3.14) < 0.001 -> ()
  | _ -> Alcotest.fail "Expected float literal"

let test_literal_string () =
  let expr = parse_expr {|"hello"|} in
  match expr with
  | Ast.ExprLit (Ast.LitString ("hello", _)) -> ()
  | _ -> Alcotest.fail "Expected string literal"

let test_literal_bool () =
  let expr = parse_expr "true" in
  match expr with
  | Ast.ExprLit (Ast.LitBool (true, _)) -> ()
  | _ -> Alcotest.fail "Expected bool literal";
  let expr = parse_expr "false" in
  match expr with
  | Ast.ExprLit (Ast.LitBool (false, _)) -> ()
  | _ -> Alcotest.fail "Expected bool literal"

let test_literal_unit () =
  let expr = parse_expr "()" in
  match expr with
  | Ast.ExprLit (Ast.LitUnit _) -> ()
  | _ -> Alcotest.fail "Expected unit literal"

let test_variable () =
  let expr = parse_expr "foo" in
  match expr with
  | Ast.ExprVar { name = "foo"; _ } -> ()
  | _ -> Alcotest.fail "Expected variable"

let test_binary_add () =
  let expr = parse_expr "1 + 2" in
  match expr with
  | Ast.ExprBinary (
      Ast.ExprLit (Ast.LitInt (1, _)),
      Ast.OpAdd,
      Ast.ExprLit (Ast.LitInt (2, _))
    ) -> ()
  | _ -> Alcotest.fail "Expected binary add"

let test_binary_precedence () =
  (* 1 + 2 * 3 should parse as 1 + (2 * 3) *)
  let expr = parse_expr "1 + 2 * 3" in
  match expr with
  | Ast.ExprBinary (
      Ast.ExprLit (Ast.LitInt (1, _)),
      Ast.OpAdd,
      Ast.ExprBinary (
        Ast.ExprLit (Ast.LitInt (2, _)),
        Ast.OpMul,
        Ast.ExprLit (Ast.LitInt (3, _))
      )
    ) -> ()
  | _ -> Alcotest.fail "Expected correct precedence"

let test_binary_associativity () =
  (* 1 - 2 - 3 should parse as (1 - 2) - 3 *)
  let expr = parse_expr "1 - 2 - 3" in
  match expr with
  | Ast.ExprBinary (
      Ast.ExprBinary (
        Ast.ExprLit (Ast.LitInt (1, _)),
        Ast.OpSub,
        Ast.ExprLit (Ast.LitInt (2, _))
      ),
      Ast.OpSub,
      Ast.ExprLit (Ast.LitInt (3, _))
    ) -> ()
  | _ -> Alcotest.fail "Expected left associativity"

let test_comparison () =
  let expr = parse_expr "x == y" in
  match expr with
  | Ast.ExprBinary (
      Ast.ExprVar { name = "x"; _ },
      Ast.OpEq,
      Ast.ExprVar { name = "y"; _ }
    ) -> ()
  | _ -> Alcotest.fail "Expected comparison"

let test_logical () =
  let expr = parse_expr "a && b || c" in
  match expr with
  | Ast.ExprBinary (
      Ast.ExprBinary (_, Ast.OpAnd, _),
      Ast.OpOr,
      _
    ) -> ()
  | _ -> Alcotest.fail "Expected logical operators"

let test_unary_neg () =
  let expr = parse_expr "-x" in
  match expr with
  | Ast.ExprUnary (Ast.OpNeg, Ast.ExprVar { name = "x"; _ }) -> ()
  | _ -> Alcotest.fail "Expected unary negation"

let test_unary_not () =
  let expr = parse_expr "!x" in
  match expr with
  | Ast.ExprUnary (Ast.OpNot, Ast.ExprVar { name = "x"; _ }) -> ()
  | _ -> Alcotest.fail "Expected unary not"

let test_function_call () =
  let expr = parse_expr "foo(1, 2, 3)" in
  match expr with
  | Ast.ExprApp (
      Ast.ExprVar { name = "foo"; _ },
      [Ast.ExprLit (Ast.LitInt (1, _));
       Ast.ExprLit (Ast.LitInt (2, _));
       Ast.ExprLit (Ast.LitInt (3, _))]
    ) -> ()
  | _ -> Alcotest.fail "Expected function call"

let test_field_access () =
  let expr = parse_expr "foo.bar" in
  match expr with
  | Ast.ExprField (Ast.ExprVar { name = "foo"; _ }, { name = "bar"; _ }) -> ()
  | _ -> Alcotest.fail "Expected field access"

let test_index_access () =
  let expr = parse_expr "arr[0]" in
  match expr with
  | Ast.ExprIndex (
      Ast.ExprVar { name = "arr"; _ },
      Ast.ExprLit (Ast.LitInt (0, _))
    ) -> ()
  | _ -> Alcotest.fail "Expected index access"

let test_tuple () =
  let expr = parse_expr "(1, 2, 3)" in
  match expr with
  | Ast.ExprTuple [_; _; _] -> ()
  | _ -> Alcotest.fail "Expected tuple"

let test_array () =
  let expr = parse_expr "[1, 2, 3]" in
  match expr with
  | Ast.ExprArray [_; _; _] -> ()
  | _ -> Alcotest.fail "Expected array"

let test_record () =
  let expr = parse_expr "{ x: 1, y: 2 }" in
  match expr with
  | Ast.ExprRecord { er_fields = [(_, Some _); (_, Some _)]; er_spread = None } -> ()
  | _ -> Alcotest.fail "Expected record"

let test_if_expr () =
  let expr = parse_expr "if x { 1 } else { 2 }" in
  match expr with
  | Ast.ExprIf { ei_cond = _; ei_then = _; ei_else = Some _ } -> ()
  | _ -> Alcotest.fail "Expected if expression"

let test_match_expr () =
  let expr = parse_expr "match x { Some(y) => y, None => 0 }" in
  match expr with
  | Ast.ExprMatch { em_scrutinee = _; em_arms = [_; _] } -> ()
  | _ -> Alcotest.fail "Expected match expression"

let test_lambda () =
  let expr = parse_expr "|x| x + 1" in
  match expr with
  | Ast.ExprLambda { elam_params = [_]; elam_body = _ } -> ()
  | _ -> Alcotest.fail "Expected lambda"

let test_block () =
  let expr = parse_expr "{ let x = 1; x + 1 }" in
  match expr with
  | Ast.ExprBlock { blk_stmts = [_]; blk_expr = Some _ } -> ()
  | _ -> Alcotest.fail "Expected block"

(* ========== Pattern Tests ========== *)

let test_pattern_wildcard () =
  let prog = parse "fn test() { match x { _ => 1 } }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_pattern_variable () =
  let prog = parse "fn test() { match x { y => y } }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_pattern_constructor () =
  let prog = parse "fn test() { match x { Some(y) => y, None => 0 } }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_pattern_tuple () =
  let prog = parse "fn test() { match x { (a, b, c) => a + b + c } }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_pattern_or () =
  let prog = parse "fn test() { match x { 1 | 2 | 3 => true, _ => false } }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

(* ========== Statement Tests ========== *)

let test_let_stmt () =
  let prog = parse "fn test() { let x = 1; }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_let_mut_stmt () =
  let prog = parse "fn test() { let mut x = 1; x = 2; }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_while_stmt () =
  let prog = parse "fn test() { while x { x = x - 1; } }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_for_stmt () =
  let prog = parse "fn test() { for i in items { process(i); } }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

(* ========== Declaration Tests ========== *)

let test_fn_decl_simple () =
  let prog = parse "fn add(x: Int, y: Int) -> Int { x + y }" in
  match prog.prog_decls with
  | [Ast.TopFn { fd_name = { name = "add"; _ }; fd_params = [_; _]; _ }] -> ()
  | _ -> Alcotest.fail "Expected function declaration"

let test_fn_decl_total () =
  let prog = parse "total fn safe(n: Nat) -> Nat { n }" in
  match prog.prog_decls with
  | [Ast.TopFn { fd_total = true; _ }] -> ()
  | _ -> Alcotest.fail "Expected total function"

let test_fn_decl_with_effect () =
  let prog = parse "fn greet() -{IO}-> () { println(\"hello\"); }" in
  match prog.prog_decls with
  | [Ast.TopFn { fd_eff = Some _; _ }] -> ()
  | _ -> Alcotest.fail "Expected function with effect"

let test_fn_decl_generic () =
  let prog = parse "fn identity[T](x: T) -> T { x }" in
  match prog.prog_decls with
  | [Ast.TopFn { fd_type_params = [_]; _ }] -> ()
  | _ -> Alcotest.fail "Expected generic function"

let test_struct_decl () =
  let prog = parse "struct Point { x: Int, y: Int }" in
  match prog.prog_decls with
  | [Ast.TopType { td_name = { name = "Point"; _ };
                   td_body = Ast.TyStruct [_; _] }] -> ()
  | _ -> Alcotest.fail "Expected struct declaration"

let test_enum_decl () =
  let prog = parse "enum Option[T] { Some(T), None }" in
  match prog.prog_decls with
  | [Ast.TopType { td_name = { name = "Option"; _ };
                   td_body = Ast.TyEnum [_; _] }] -> ()
  | _ -> Alcotest.fail "Expected enum declaration"

let test_type_alias () =
  let prog = parse "type Id = Int;" in
  match prog.prog_decls with
  | [Ast.TopType { td_body = Ast.TyAlias _; _ }] -> ()
  | _ -> Alcotest.fail "Expected type alias"

let test_trait_decl () =
  let prog = parse "trait Eq { fn eq(self: ref Self, other: ref Self) -> Bool; }" in
  match prog.prog_decls with
  | [Ast.TopTrait { trd_name = { name = "Eq"; _ }; trd_items = [_] }] -> ()
  | _ -> Alcotest.fail "Expected trait declaration"

let test_impl_block () =
  let prog = parse "impl Point { fn new(x: Int, y: Int) -> Point { Point { x: x, y: y } } }" in
  match prog.prog_decls with
  | [Ast.TopImpl { ib_self_ty = _; ib_items = [_] }] -> ()
  | _ -> Alcotest.fail "Expected impl block"

let test_impl_trait () =
  let prog = parse "impl Eq for Point { fn eq(self: ref Self, other: ref Self) -> Bool { true } }" in
  match prog.prog_decls with
  | [Ast.TopImpl { ib_trait_ref = Some _; _ }] -> ()
  | _ -> Alcotest.fail "Expected trait impl"

let test_effect_decl () =
  let prog = parse "effect Console { fn print(msg: String) -> (); fn read() -> String; }" in
  match prog.prog_decls with
  | [Ast.TopEffect { ed_name = { name = "Console"; _ }; ed_ops = [_; _] }] -> ()
  | _ -> Alcotest.fail "Expected effect declaration"

(* ========== Type Expression Tests ========== *)

let test_type_simple () =
  let prog = parse "fn test(x: Int) -> Bool { true }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_type_generic () =
  let prog = parse "fn test(x: Option[Int]) -> Bool { true }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_type_tuple () =
  let prog = parse "fn test(x: (Int, String, Bool)) -> () { () }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_type_function () =
  let prog = parse "fn test(f: Int -> Int) -> Int { f(1) }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_type_record () =
  let prog = parse "fn test(p: {x: Int, y: Int}) -> Int { p.x }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_type_row_poly () =
  let prog = parse "fn test[r](p: {x: Int, ..r}) -> Int { p.x }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

let test_type_ownership () =
  let prog = parse "fn test(x: own String, y: ref Int, z: mut Bool) -> () { () }" in
  Alcotest.(check bool) "parses" true (prog.prog_decls <> [])

(* ========== Import Tests ========== *)

let test_import_simple () =
  let prog = parse "use std.io; fn main() { () }" in
  match prog.prog_imports with
  | [Ast.ImportSimple (_, None)] -> ()
  | _ -> Alcotest.fail "Expected simple import"

let test_import_alias () =
  let prog = parse "use std.collections.HashMap as Map; fn main() { () }" in
  match prog.prog_imports with
  | [Ast.ImportSimple (_, Some _)] -> ()
  | _ -> Alcotest.fail "Expected aliased import"

let test_import_list () =
  let prog = parse "use std.collections::{Vec, HashMap}; fn main() { () }" in
  match prog.prog_imports with
  | [Ast.ImportList (_, [_; _])] -> ()
  | _ -> Alcotest.fail "Expected list import"

let test_import_glob () =
  let prog = parse "use std.prelude::*; fn main() { () }" in
  match prog.prog_imports with
  | [Ast.ImportGlob _] -> ()
  | _ -> Alcotest.fail "Expected glob import"

(* ========== Complex Examples ========== *)

let test_fibonacci () =
  Alcotest.(check bool) "fibonacci" true
    (parses {|
      fn fib(n: Int) -> Int {
        if n <= 1 {
          n
        } else {
          fib(n - 1) + fib(n - 2)
        }
      }
    |})

let test_linked_list () =
  Alcotest.(check bool) "linked list" true
    (parses {|
      enum List[T] {
        Nil,
        Cons(T, own List[T])
      }

      fn length[T](list: ref List[T]) -> Int {
        match list {
          Nil => 0,
          Cons(_, tail) => 1 + length(tail)
        }
      }
    |})

let test_effect_handler () =
  Alcotest.(check bool) "effect handler" true
    (parses {|
      effect State[S] {
        fn get() -> S;
        fn put(s: S) -> ();
      }

      fn counter() -{State[Int]}-> Int {
        let x = get();
        put(x + 1);
        x
      }

      fn run_state() -> Int {
        handle counter() {
          return(x) => x,
          get() => resume(0),
          put(s) => resume(())
        }
      }
    |})

let test_trait_bounds () =
  Alcotest.(check bool) "trait bounds" true
    (parses {|
      fn compare[T](a: T, b: T) -> Bool
        where T: Eq + Ord
      {
        a == b
      }
    |})

(* ========== Test Suite ========== *)

let tests =
  [
    (* Literals *)
    Alcotest.test_case "literal int" `Quick test_literal_int;
    Alcotest.test_case "literal float" `Quick test_literal_float;
    Alcotest.test_case "literal string" `Quick test_literal_string;
    Alcotest.test_case "literal bool" `Quick test_literal_bool;
    Alcotest.test_case "literal unit" `Quick test_literal_unit;

    (* Expressions *)
    Alcotest.test_case "variable" `Quick test_variable;
    Alcotest.test_case "binary add" `Quick test_binary_add;
    Alcotest.test_case "binary precedence" `Quick test_binary_precedence;
    Alcotest.test_case "binary associativity" `Quick test_binary_associativity;
    Alcotest.test_case "comparison" `Quick test_comparison;
    Alcotest.test_case "logical" `Quick test_logical;
    Alcotest.test_case "unary neg" `Quick test_unary_neg;
    Alcotest.test_case "unary not" `Quick test_unary_not;
    Alcotest.test_case "function call" `Quick test_function_call;
    Alcotest.test_case "field access" `Quick test_field_access;
    Alcotest.test_case "index access" `Quick test_index_access;
    Alcotest.test_case "tuple" `Quick test_tuple;
    Alcotest.test_case "array" `Quick test_array;
    Alcotest.test_case "record" `Quick test_record;
    Alcotest.test_case "if expr" `Quick test_if_expr;
    Alcotest.test_case "match expr" `Quick test_match_expr;
    Alcotest.test_case "lambda" `Quick test_lambda;
    Alcotest.test_case "block" `Quick test_block;

    (* Patterns *)
    Alcotest.test_case "pattern wildcard" `Quick test_pattern_wildcard;
    Alcotest.test_case "pattern variable" `Quick test_pattern_variable;
    Alcotest.test_case "pattern constructor" `Quick test_pattern_constructor;
    Alcotest.test_case "pattern tuple" `Quick test_pattern_tuple;
    Alcotest.test_case "pattern or" `Quick test_pattern_or;

    (* Statements *)
    Alcotest.test_case "let stmt" `Quick test_let_stmt;
    Alcotest.test_case "let mut stmt" `Quick test_let_mut_stmt;
    Alcotest.test_case "while stmt" `Quick test_while_stmt;
    Alcotest.test_case "for stmt" `Quick test_for_stmt;

    (* Declarations *)
    Alcotest.test_case "fn decl simple" `Quick test_fn_decl_simple;
    Alcotest.test_case "fn decl total" `Quick test_fn_decl_total;
    Alcotest.test_case "fn decl effect" `Quick test_fn_decl_with_effect;
    Alcotest.test_case "fn decl generic" `Quick test_fn_decl_generic;
    Alcotest.test_case "struct decl" `Quick test_struct_decl;
    Alcotest.test_case "enum decl" `Quick test_enum_decl;
    Alcotest.test_case "type alias" `Quick test_type_alias;
    Alcotest.test_case "trait decl" `Quick test_trait_decl;
    Alcotest.test_case "impl block" `Quick test_impl_block;
    Alcotest.test_case "impl trait" `Quick test_impl_trait;
    Alcotest.test_case "effect decl" `Quick test_effect_decl;

    (* Type expressions *)
    Alcotest.test_case "type simple" `Quick test_type_simple;
    Alcotest.test_case "type generic" `Quick test_type_generic;
    Alcotest.test_case "type tuple" `Quick test_type_tuple;
    Alcotest.test_case "type function" `Quick test_type_function;
    Alcotest.test_case "type record" `Quick test_type_record;
    Alcotest.test_case "type row poly" `Quick test_type_row_poly;
    Alcotest.test_case "type ownership" `Quick test_type_ownership;

    (* Imports *)
    Alcotest.test_case "import simple" `Quick test_import_simple;
    Alcotest.test_case "import alias" `Quick test_import_alias;
    Alcotest.test_case "import list" `Quick test_import_list;
    Alcotest.test_case "import glob" `Quick test_import_glob;

    (* Complex examples *)
    Alcotest.test_case "fibonacci" `Quick test_fibonacci;
    Alcotest.test_case "linked list" `Quick test_linked_list;
    Alcotest.test_case "effect handler" `Quick test_effect_handler;
    Alcotest.test_case "trait bounds" `Quick test_trait_bounds;
  ]
