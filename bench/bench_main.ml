(* SPDX-License-Identifier: MPL-2.0 *)
(* AffineScript microbenchmark runner.

   Visibility-only — no merge gate.  See
   docs/standards/TESTING.adoc §"Bench standards" for the policy.

   We register each phase under an alcotest "bench" suite so the
   harness gives consistent setup/teardown framing.  The actual
   measurements are wall-clock and emitted via `Printf.printf` to
   stdout — alcotest assertions check only that the bench ran to
   completion (the assertion is `pass`, the value is in the
   printout).  Switch to alcotest-bench / Bechamel when a calibrated
   baseline + ratchet policy lands. *)

let with_section name f =
  Printf.printf "\n";
  Printf.printf "════════════════════════════════════════════════════\n";
  Printf.printf "  AffineScript microbench — %s\n" name;
  Printf.printf "════════════════════════════════════════════════════\n%!";
  f ();
  Printf.printf "\n%!"

let bench_case label fn =
  Alcotest.test_case label `Slow (fun () ->
    fn ();
    Alcotest.(check pass) (Printf.sprintf "%s ran to completion" label) () ())

let () =
  with_section "phases" (fun () ->
    Alcotest.run ~and_exit:false "affinescript-bench" [
      ("lex",       [ bench_case "lex sweep"       Bench_lex.run ]);
      ("parse",     [ bench_case "parse sweep"     Bench_parse.run ]);
      ("typecheck", [ bench_case "typecheck sweep" Bench_typecheck.run ]);
      ("codegen",   [ bench_case "codegen sweep"   Bench_codegen.run ]);
    ])
