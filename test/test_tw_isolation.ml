(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 hyperpolymath *)

(* #10 / typed-wasm contract widening: L13 module-isolation (negative
   form) enforced on emitted wasm by [Tw_verify.verify_module_isolation]
   / [verify_from_module]. Additive — no ownership-section ABI change. *)

open Affinescript

let mem () : Wasm.memory = { Wasm.mem_type = { Wasm.lim_min = 1; lim_max = None } }

let imp m n d : Wasm.import = { Wasm.i_module = m; i_name = n; i_desc = d }

(* count=0 ownership payload (u32le 0) — present so verify_from_module
   does not short-circuit on "no ownership section ⇒ Ok". *)
let own_section = ("typedwasm.ownership", Bytes.make 4 '\000')

let is_isolation_err = function
  | Tw_verify.ModuleNotIsolated _ -> true
  | _ -> false

let test_clean_module_ok () =
  let m =
    { (Wasm.empty_module ()) with
      Wasm.mems = [ mem () ];
      imports = [ imp "Other" "callee" (Wasm.ImportFunc 0) ];
      custom_sections = [ own_section ] }
  in
  match Tw_verify.verify_from_module m with
  | Ok () -> ()
  | Error errs ->
    Alcotest.failf "clean isolated module flagged: %d errs" (List.length errs)

let test_imported_memory_flagged () =
  let m =
    { (Wasm.empty_module ()) with
      Wasm.mems = [ mem () ];
      imports = [ imp "Host" "memory" Wasm.ImportMemory ];
      custom_sections = [ own_section ] }
  in
  match Tw_verify.verify_from_module m with
  | Ok () -> Alcotest.fail "imported memory + own memory must violate L13"
  | Error errs ->
    Alcotest.(check bool) "ModuleNotIsolated reported" true
      (List.exists is_isolation_err errs)

let test_imported_table_flagged () =
  let m =
    { (Wasm.empty_module ()) with
      Wasm.mems = [ mem () ];
      imports = [ imp "Host" "tbl" Wasm.ImportTable ];
      custom_sections = [ own_section ] }
  in
  Alcotest.(check bool) "imported table flagged" true
    (List.exists is_isolation_err (Tw_verify.verify_module_isolation m))

let test_no_own_memory_not_in_scope () =
  (* A pure consumer shim (no own memory) importing a memory is not the
     negative-L13 shape AffineScript emits; not flagged. *)
  let m =
    { (Wasm.empty_module ()) with
      Wasm.mems = [];
      imports = [ imp "Host" "memory" Wasm.ImportMemory ];
      custom_sections = [ own_section ] }
  in
  Alcotest.(check int) "no isolation errors" 0
    (List.length (Tw_verify.verify_module_isolation m))

let test_no_ownership_section_contract () =
  (* Even a would-be violation: no ownership section ⇒ Ok (the
     pre-existing contract is preserved; isolation is gated behind it). *)
  let m =
    { (Wasm.empty_module ()) with
      Wasm.mems = [ mem () ];
      imports = [ imp "Host" "memory" Wasm.ImportMemory ];
      custom_sections = [] }
  in
  match Tw_verify.verify_from_module m with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "no ownership section must remain Ok"

let tests =
  [
    Alcotest.test_case "clean isolated module is Ok" `Quick
      test_clean_module_ok;
    Alcotest.test_case "imported memory + own memory violates L13" `Quick
      test_imported_memory_flagged;
    Alcotest.test_case "imported table violates L13" `Quick
      test_imported_table_flagged;
    Alcotest.test_case "no-own-memory consumer not in scope" `Quick
      test_no_own_memory_not_in_scope;
    Alcotest.test_case "no ownership section ⇒ Ok (contract)" `Quick
      test_no_ownership_section_contract;
  ]
