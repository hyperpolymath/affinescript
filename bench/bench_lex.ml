(* SPDX-License-Identifier: MPL-2.0 *)
(* Lexer-only sweep — drain the token stream to EOF.

   Reads the shared fixtures and repeatedly tokenises them, then
   prints the elapsed wall-clock and tokens/second.  Visibility-only:
   a regression flags itself in the run log, no merge gate.  See
   docs/standards/TESTING.adoc §"Bench standards". *)

open Affinescript

let iterations = 200

let drain_tokens stream =
  let n = ref 0 in
  let rec loop () =
    let (tok, _span) = stream () in
    incr n;
    match tok with
    | Token.EOF -> ()
    | _ -> loop ()
  in
  loop ();
  !n

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  (1000.0 *. (t1 -. t0), r)

let bench_one (label, src) =
  let elapsed, total_tokens =
    time_ms (fun () ->
      let total = ref 0 in
      for _ = 1 to iterations do
        let stream = Lexer.from_string ~file:"<bench>" src in
        total := !total + drain_tokens stream
      done;
      !total)
  in
  let toks_per_sec =
    if elapsed > 0.0 then
      float_of_int total_tokens /. (elapsed /. 1000.0)
    else 0.0
  in
  Printf.printf
    "  lex[%s]: %d iters, %d tokens total, %.2fms, %.0f tok/s\n%!"
    label iterations total_tokens elapsed toks_per_sec

let run () =
  print_endline "── bench: lex ─────────────────────────────────";
  List.iter bench_one Bench_fixtures.all
