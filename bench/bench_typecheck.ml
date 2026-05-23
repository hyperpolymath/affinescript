(* SPDX-License-Identifier: MPL-2.0 *)
(* Typecheck (incl. quantity pass) sweep.

   The typecheck phase requires a resolved program (symbols + import
   types are produced by [Resolve.resolve_program_with_loader]).  We
   resolve once outside the timing loop so the measurement is purely
   typecheck + quantity. *)

open Affinescript

let iterations = 60

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  (1000.0 *. (t1 -. t0), r)

let resolve_once prog =
  let cfg = Module_loader.default_config () in
  let loader = Module_loader.create cfg in
  Resolve.resolve_program_with_loader prog loader

(* Wrap each phase in a swallow-everything boolean so the bench
   loop is failure-tolerant — visibility-only: errors don't fail
   the gate, they reduce the "ok" counter. *)
let run_tc resolve_ctx (type_ctx : Typecheck.context) prog =
  try
    match Typecheck.check_program
            ~import_types:type_ctx.Typecheck.name_types
            resolve_ctx.Resolve.symbols prog
    with
    | Ok _ -> true
    | Error _ -> false
  with _ -> false

let run_qty prog =
  try
    match Quantity.check_program_quantities prog with
    | Ok _ -> true
    | Error _ -> false
  with _ -> false

let bench_one (label, src) =
  match (try Some (Parse_driver.parse_string ~file:"<bench>" src)
         with _ -> None) with
  | None ->
    Printf.printf "  typecheck[%s]: parse failed, skipping\n%!" label
  | Some prog ->
    (match resolve_once prog with
     | Error _ ->
       Printf.printf "  typecheck[%s]: resolve failed, skipping\n%!" label
     | Ok (resolve_ctx, type_ctx) ->
       let elapsed, (ok_t, ok_q) =
         time_ms (fun () ->
           let ok_t = ref 0 and ok_q = ref 0 in
           for _ = 1 to iterations do
             if run_tc resolve_ctx type_ctx prog then begin
               incr ok_t;
               if run_qty prog then incr ok_q
             end
           done;
           (!ok_t, !ok_q))
       in
       let per_iter = if iterations > 0 then elapsed /. float_of_int iterations else 0.0 in
       Printf.printf
         "  typecheck[%s]: %d iters (tc=%d, qty=%d), %.2fms total, %.3fms/iter\n%!"
         label iterations ok_t ok_q elapsed per_iter)

let run () =
  print_endline "── bench: typecheck (+ quantity) ──────────────";
  List.iter bench_one Bench_fixtures.all
