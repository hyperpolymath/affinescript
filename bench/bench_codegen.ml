(* SPDX-License-Identifier: MPL-2.0 *)
(* WASM codegen sweep.

   Like the typecheck bench, we resolve+typecheck once outside the
   timing loop so the measurement is purely codegen. *)

open Affinescript

let iterations = 80

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  (1000.0 *. (t1 -. t0), r)

let prep src =
  match (try Some (Parse_driver.parse_string ~file:"<bench>" src)
         with _ -> None) with
  | None -> None
  | Some prog ->
    let cfg = Module_loader.default_config () in
    let loader = Module_loader.create cfg in
    (match Resolve.resolve_program_with_loader prog loader with
     | Error _ -> None
     | Ok _ -> Some prog)

let bench_one (label, src) =
  match prep src with
  | None -> Printf.printf "  codegen[%s]: prep failed, skipping\n%!" label
  | Some prog ->
    let elapsed, ok =
      time_ms (fun () ->
        let ok = ref 0 in
        for _ = 1 to iterations do
          match (try Codegen.generate_module prog with _ -> Error (Codegen.UnsupportedFeature "<bench raised>")) with
          | Ok _ -> incr ok
          | Error _ -> ()
        done;
        !ok)
    in
    let per_iter = if iterations > 0 then elapsed /. float_of_int iterations else 0.0 in
    Printf.printf
      "  codegen[%s]: %d iters (%d ok), %.2fms total, %.3fms/iter\n%!"
      label iterations ok elapsed per_iter

let run () =
  print_endline "── bench: codegen (wasm) ──────────────────────";
  List.iter bench_one Bench_fixtures.all
