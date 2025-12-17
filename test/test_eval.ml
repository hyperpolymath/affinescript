(** Tests for the AffineScript interpreter *)

open Affinescript

let eval s =
  let env = Value.empty_env () in
  Stdlib.load_prelude env;
  match Repl.eval_string ~env s with
  | Ok v -> v
  | Error msg -> failwith msg

let eval_to_int s =
  match eval s with
  | Value.VInt i -> i
  | v -> failwith (Printf.sprintf "Expected int, got %s" (Value.show v))

let eval_to_bool s =
  match eval s with
  | Value.VBool b -> b
  | v -> failwith (Printf.sprintf "Expected bool, got %s" (Value.show v))

let eval_to_string s =
  match eval s with
  | Value.VString s -> s
  | v -> failwith (Printf.sprintf "Expected string, got %s" (Value.show v))

(* ========== Literal Tests ========== *)

let test_int_literal () =
  Alcotest.(check int) "int literal" 42 (eval_to_int "42")

let test_negative_int () =
  Alcotest.(check int) "negative int" (-5) (eval_to_int "-5")

let test_bool_literal () =
  Alcotest.(check bool) "true" true (eval_to_bool "true");
  Alcotest.(check bool) "false" false (eval_to_bool "false")

let test_string_literal () =
  Alcotest.(check string) "string" "hello" (eval_to_string "\"hello\"")

(* ========== Arithmetic Tests ========== *)

let test_addition () =
  Alcotest.(check int) "1 + 2" 3 (eval_to_int "1 + 2")

let test_subtraction () =
  Alcotest.(check int) "5 - 3" 2 (eval_to_int "5 - 3")

let test_multiplication () =
  Alcotest.(check int) "4 * 5" 20 (eval_to_int "4 * 5")

let test_division () =
  Alcotest.(check int) "10 / 3" 3 (eval_to_int "10 / 3")

let test_modulo () =
  Alcotest.(check int) "10 % 3" 1 (eval_to_int "10 % 3")

let test_precedence () =
  Alcotest.(check int) "1 + 2 * 3" 7 (eval_to_int "1 + 2 * 3");
  Alcotest.(check int) "(1 + 2) * 3" 9 (eval_to_int "(1 + 2) * 3")

let test_complex_expr () =
  Alcotest.(check int) "complex" 14 (eval_to_int "2 * 3 + 4 * 2")

(* ========== Comparison Tests ========== *)

let test_equality () =
  Alcotest.(check bool) "1 == 1" true (eval_to_bool "1 == 1");
  Alcotest.(check bool) "1 == 2" false (eval_to_bool "1 == 2")

let test_inequality () =
  Alcotest.(check bool) "1 != 2" true (eval_to_bool "1 != 2");
  Alcotest.(check bool) "1 != 1" false (eval_to_bool "1 != 1")

let test_less_than () =
  Alcotest.(check bool) "1 < 2" true (eval_to_bool "1 < 2");
  Alcotest.(check bool) "2 < 1" false (eval_to_bool "2 < 1")

let test_greater_than () =
  Alcotest.(check bool) "2 > 1" true (eval_to_bool "2 > 1");
  Alcotest.(check bool) "1 > 2" false (eval_to_bool "1 > 2")

(* ========== Logical Tests ========== *)

let test_and () =
  Alcotest.(check bool) "true && true" true (eval_to_bool "true && true");
  Alcotest.(check bool) "true && false" false (eval_to_bool "true && false")

let test_or () =
  Alcotest.(check bool) "false || true" true (eval_to_bool "false || true");
  Alcotest.(check bool) "false || false" false (eval_to_bool "false || false")

let test_not () =
  Alcotest.(check bool) "!true" false (eval_to_bool "!true");
  Alcotest.(check bool) "!false" true (eval_to_bool "!false")

(* ========== Let Binding Tests ========== *)

let test_let_binding () =
  Alcotest.(check int) "let x = 5; x" 5 (eval_to_int "{ let x = 5; x }")

let test_let_shadowing () =
  Alcotest.(check int) "shadowing" 10
    (eval_to_int "{ let x = 5; let x = 10; x }")

let test_let_in_expr () =
  Alcotest.(check int) "let in expr" 15
    (eval_to_int "{ let x = 5; let y = 10; x + y }")

(* ========== If Expression Tests ========== *)

let test_if_true () =
  Alcotest.(check int) "if true" 1 (eval_to_int "if true { 1 } else { 2 }")

let test_if_false () =
  Alcotest.(check int) "if false" 2 (eval_to_int "if false { 1 } else { 2 }")

let test_nested_if () =
  Alcotest.(check int) "nested if" 3
    (eval_to_int "if false { 1 } else if false { 2 } else { 3 }")

(* ========== Function Tests ========== *)

let test_function_def () =
  Alcotest.(check int) "fn call" 5
    (eval_to_int "{ fn add(a: Int, b: Int) -> Int { a + b } add(2, 3) }")

let test_recursive_fn () =
  Alcotest.(check int) "recursive" 120
    (eval_to_int {|{
      fn fact(n: Int) -> Int {
        if n <= 1 { 1 } else { n * fact(n - 1) }
      }
      fact(5)
    }|})

let test_higher_order () =
  Alcotest.(check int) "higher order" 10
    (eval_to_int {|{
      fn apply(f: fn(Int) -> Int, x: Int) -> Int { f(x) }
      fn double(x: Int) -> Int { x * 2 }
      apply(double, 5)
    }|})

let test_lambda () =
  Alcotest.(check int) "lambda" 15
    (eval_to_int {|{
      let f = \x: Int -> x * 3;
      f(5)
    }|})

(* ========== Tuple Tests ========== *)

let test_tuple () =
  Alcotest.(check int) "tuple.0" 1 (eval_to_int "(1, 2, 3).0");
  Alcotest.(check int) "tuple.1" 2 (eval_to_int "(1, 2, 3).1")

(* ========== Array Tests ========== *)

let test_array_index () =
  Alcotest.(check int) "array[1]" 20 (eval_to_int "[10, 20, 30][1]")

let test_array_len () =
  Alcotest.(check int) "len([1,2,3])" 3 (eval_to_int "len([1, 2, 3])")

(* ========== Record Tests ========== *)

let test_record () =
  Alcotest.(check int) "record.x" 10 (eval_to_int "{x: 10, y: 20}.x")

let test_record_shorthand () =
  Alcotest.(check int) "shorthand" 5 (eval_to_int "{ let x = 5; {x}.x }")

(* ========== Match Tests ========== *)

let test_match_int () =
  Alcotest.(check int) "match int" 10
    (eval_to_int "match 1 { 1 => 10, 2 => 20, _ => 0 }")

let test_match_tuple () =
  Alcotest.(check int) "match tuple" 3
    (eval_to_int "match (1, 2) { (a, b) => a + b }")

(* ========== Loop Tests ========== *)

let test_while_loop () =
  Alcotest.(check int) "while" 10
    (eval_to_int {|{
      let mut x = 0;
      while x < 10 { x += 1; }
      x
    }|})

let test_for_loop () =
  Alcotest.(check int) "for" 6
    (eval_to_int {|{
      let mut sum = 0;
      for i in [1, 2, 3] { sum += i; }
      sum
    }|})

(* ========== Builtin Tests ========== *)

let test_builtin_range () =
  Alcotest.(check int) "range len" 5 (eval_to_int "len(range(5))")

let test_builtin_map () =
  Alcotest.(check int) "map" 4
    (eval_to_int "map(\\x: Int -> x * 2, [1, 2])[1]")

let test_builtin_filter () =
  Alcotest.(check int) "filter" 2
    (eval_to_int "len(filter(\\x: Int -> x > 1, [1, 2, 3]))")

let test_builtin_fold () =
  Alcotest.(check int) "fold" 6
    (eval_to_int "fold(\\acc: Int, x: Int -> acc + x, 0, [1, 2, 3])")

(* ========== Test Suite ========== *)

let tests = [
  (* Literals *)
  ("int literal", `Quick, test_int_literal);
  ("negative int", `Quick, test_negative_int);
  ("bool literal", `Quick, test_bool_literal);
  ("string literal", `Quick, test_string_literal);

  (* Arithmetic *)
  ("addition", `Quick, test_addition);
  ("subtraction", `Quick, test_subtraction);
  ("multiplication", `Quick, test_multiplication);
  ("division", `Quick, test_division);
  ("modulo", `Quick, test_modulo);
  ("precedence", `Quick, test_precedence);
  ("complex expr", `Quick, test_complex_expr);

  (* Comparison *)
  ("equality", `Quick, test_equality);
  ("inequality", `Quick, test_inequality);
  ("less than", `Quick, test_less_than);
  ("greater than", `Quick, test_greater_than);

  (* Logical *)
  ("and", `Quick, test_and);
  ("or", `Quick, test_or);
  ("not", `Quick, test_not);

  (* Let bindings *)
  ("let binding", `Quick, test_let_binding);
  ("let shadowing", `Quick, test_let_shadowing);
  ("let in expr", `Quick, test_let_in_expr);

  (* If expressions *)
  ("if true", `Quick, test_if_true);
  ("if false", `Quick, test_if_false);
  ("nested if", `Quick, test_nested_if);

  (* Functions *)
  ("function def", `Quick, test_function_def);
  ("recursive fn", `Quick, test_recursive_fn);
  ("higher order", `Quick, test_higher_order);
  ("lambda", `Quick, test_lambda);

  (* Data structures *)
  ("tuple", `Quick, test_tuple);
  ("array index", `Quick, test_array_index);
  ("array len", `Quick, test_array_len);
  ("record", `Quick, test_record);
  ("record shorthand", `Quick, test_record_shorthand);

  (* Match *)
  ("match int", `Quick, test_match_int);
  ("match tuple", `Quick, test_match_tuple);

  (* Loops *)
  ("while loop", `Quick, test_while_loop);
  ("for loop", `Quick, test_for_loop);

  (* Builtins *)
  ("builtin range", `Quick, test_builtin_range);
  ("builtin map", `Quick, test_builtin_map);
  ("builtin filter", `Quick, test_builtin_filter);
  ("builtin fold", `Quick, test_builtin_fold);
]
