(* SPDX-License-Identifier: MPL-2.0 *)
(* Parser sweep — full parse to AST, repeatedly. *)

open Affinescript

let iterations = 100

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  (1000.0 *. (t1 -. t0), r)

let bench_one (label, src) =
  let elapsed, ok =
    time_ms (fun () ->
      let succeeded = ref 0 in
      for _ = 1 to iterations do
        match (try Some (Parse_driver.parse_string ~file:"<bench>" src)
               with _ -> None) with
        | Some _ -> incr succeeded
        | None -> ()
      done;
      !succeeded)
  in
  let per_iter = if iterations > 0 then elapsed /. float_of_int iterations else 0.0 in
  Printf.printf
    "  parse[%s]: %d iters (%d ok), %.2fms total, %.3fms/iter\n%!"
    label iterations ok elapsed per_iter

let run () =
  print_endline "── bench: parse ───────────────────────────────";
  List.iter bench_one Bench_fixtures.all
