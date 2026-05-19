(* SPDX-License-Identifier: PMPL-1.0-or-later *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(* ADR-016 / #234 S2a: the shared call-site numbering must be total,
   deterministic, and pure (the keying contract for the effect
   side-table — typecheck and codegen both call it on the same prog and
   MUST agree). *)

open Affinescript

let parse src = Parse_driver.parse_string ~file:"<test_effect_sites>" src

(* Calls: helper(1) helper(2) helper(3) helper(4) — across a let value,
   an if-condition, and both if branches. `>` and `+` are ExprBinary,
   not calls. *)
let prog_src =
  {|
fn helper(x: Int) -> Int { x }

fn main() -> Int {
  let a = helper(1);
  let b = if helper(2) > 0 { helper(3) } else { helper(4) };
  a + b
}
|}

let test_count () =
  let p = parse prog_src in
  Alcotest.(check int) "four call sites" 4 (Effect_sites.count p)

let test_ordinals_contiguous_preorder () =
  let p = parse prog_src in
  let ords = List.map fst (Effect_sites.to_list p) in
  (* 0..3 in order — pre-order, contiguous, no gaps/dupes. *)
  Alcotest.(check (list int)) "ordinals 0..3 in order" [ 0; 1; 2; 3 ] ords

let test_deterministic_and_pure () =
  let p = parse prog_src in
  let run () = Effect_sites.to_list p |> List.map fst in
  Alcotest.(check (list int)) "two runs identical" (run ()) (run ());
  (* Re-parsing the same source yields the same numbering. *)
  let p2 = parse prog_src in
  Alcotest.(check int) "stable across parses"
    (Effect_sites.count p) (Effect_sites.count p2)

let test_no_calls () =
  let p = parse {|
fn main() -> Int {
  let a = 1;
  let b = a + 2;
  b
}
|} in
  Alcotest.(check int) "no call sites" 0 (Effect_sites.count p)

let test_calls_in_many_positions () =
  (* arg-nested, block stmt, while, for, match arm, lambda body. *)
  let p = parse {|
fn f(x: Int) -> Int { x }

fn main() -> Int {
  let mut s = 0;
  s = f(f(1));
  while f(0) > 9 { s = s + f(2); }
  for y in [f(3)] { s = s + y; }
  let g = |z: Int| -> Int { f(z) };
  s = g(f(4));
  match f(5) {
    _ => f(6)
  }
}
|} in
  (* f(f(1))=2, f(0)=1, f(2)=1, f(3)=1, g(f(4))=2, f(5)=1, f(6)=1
     => 10 call sites (g(...) is itself a call). *)
  Alcotest.(check int) "ten call sites across positions" 10
    (Effect_sites.count p)

(* ── #234 S2b: typecheck populates the ordinal→effect side-table ── *)

let rec eff_mentions (name : string) (e : Types.eff) : bool =
  match e with
  | Types.EPure | Types.EVar _ -> false
  | Types.ESingleton s -> s = name
  | Types.EUnion es -> List.exists (eff_mentions name) es

let typecheck_ctx (src : string) : Typecheck.context =
  let prog = parse src in
  let loader = Module_loader.create (Module_loader.default_config ()) in
  match Resolve.resolve_program_with_loader prog loader with
  | Error (e, _) -> Alcotest.failf "resolve: %s" (Resolve.show_resolve_error e)
  | Ok (rc, _) ->
    (match Typecheck.check_program rc.Resolve.symbols prog with
     | Ok ctx -> ctx
     | Error e -> Alcotest.failf "typecheck: %s" (Typecheck.format_type_error e))

(* helper(1)=ord0 (pure, no eff), prim(2)=ord1 (declared /{Net,Async}).
   `a + b` is ExprBinary, not a call. *)
let s2b_src =
  {|
extern fn prim(x: Int) -> Int / { Net, Async };

fn helper(x: Int) -> Int { x }

fn main() -> Int {
  let a = helper(1);
  let b = prim(2);
  a + b
}
|}

let test_s2b_table_built () =
  let ctx = typecheck_ctx s2b_src in
  Alcotest.(check int) "two table entries" 2
    (Hashtbl.length ctx.Typecheck.call_effects);
  let e0 = Hashtbl.find ctx.Typecheck.call_effects 0 in
  let e1 = Hashtbl.find ctx.Typecheck.call_effects 1 in
  Alcotest.(check bool) "helper(1) carries no Async" false
    (eff_mentions "Async" e0);
  Alcotest.(check bool) "prim(2) carries Async" true
    (eff_mentions "Async" e1)

let test_s2b_keyed_by_effect_sites_ordinals () =
  (* Producer/consumer agreement contract: every Effect_sites ordinal
     has an entry in the table. *)
  let ctx = typecheck_ctx s2b_src in
  let p = parse s2b_src in
  List.iter
    (fun (ord, _) ->
      Alcotest.(check bool)
        (Printf.sprintf "ordinal %d present" ord)
        true
        (Hashtbl.mem ctx.Typecheck.call_effects ord))
    (Effect_sites.to_list p)

let tests =
  [
    Alcotest.test_case "count" `Quick test_count;
    Alcotest.test_case "ordinals contiguous pre-order" `Quick
      test_ordinals_contiguous_preorder;
    Alcotest.test_case "deterministic & pure" `Quick
      test_deterministic_and_pure;
    Alcotest.test_case "no calls" `Quick test_no_calls;
    Alcotest.test_case "calls in many positions" `Quick
      test_calls_in_many_positions;
    Alcotest.test_case "S2b: typecheck builds the effect side-table"
      `Quick test_s2b_table_built;
    Alcotest.test_case "S2b: keyed by Effect_sites ordinals" `Quick
      test_s2b_keyed_by_effect_sites_ordinals;
  ]
