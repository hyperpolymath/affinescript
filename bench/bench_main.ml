(* SPDX-License-Identifier: MPL-2.0 *)
(* AffineScript microbenchmark runner.

   Visibility-only — no merge gate (docs/standards/TESTING.adoc §"Bench
   standards"). The numbers are wall-clock and printed directly to stdout.

   NOTE (2026-06-16): the previous runner wrapped each bench in [Alcotest.run],
   which CAPTURES per-test stdout into a log file — so the measurements were
   computed but never reached the console ("visibility-only" benches that were
   not actually visible). This runner calls each [run ()] directly. *)

let () =
  Printf.printf "\n════════════════════════════════════════════════════\n";
  Printf.printf "  AffineScript microbench\n";
  Printf.printf "════════════════════════════════════════════════════\n%!";
  Bench_lex.run ();
  Bench_parse.run ();
  Bench_typecheck.run ();
  Bench_codegen.run ();
  Bench_scaling.run ();
  Bench_vm.run ();
  Printf.printf "\n%!"
