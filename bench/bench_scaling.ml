(* SPDX-License-Identifier: MPL-2.0 *)
(* Compile-time SCALING on generated large inputs.

   Closes the "large-input/scaling performance unmeasured" gap (TEST-NEEDS.md):
   the fixed corpus maxes at ~114 lines, so this generates N-function programs
   and times the parse → resolve → wasm-codegen pipeline as N grows, reporting
   µs/function so a super-linear blow-up in any phase shows up immediately.
   Mirrors the estate k9-bench `benchmark_scaling()` (sizes ramp by 10×). *)

open Affinescript

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  (1000.0 *. (t1 -. t0), r)

(** A program of [n] independent integer functions + a main. Linear in n by
    construction, so a non-linear timing curve indicates a phase-level defect. *)
let gen (n : int) : string =
  let b = Buffer.create (n * 40) in
  for i = 0 to n - 1 do
    Buffer.add_string b (Printf.sprintf "fn f%d(x: Int) -> Int { x + %d }\n" i i)
  done;
  Buffer.add_string b "fn main() -> Int { f0(1) }\n";
  Buffer.contents b

(* Phase-split timing so a super-linear phase is localised (issue-draft 07). *)
let one (n : int) : unit =
  let src = gen n in
  let (t_parse, parsed) =
    time_ms (fun () -> try Some (Parse_driver.parse_string ~file:"<scaling>" src) with _ -> None) in
  match parsed with
  | None -> Printf.printf "  n=%-6d PARSE FAILED\n%!" n
  | Some prog ->
    let cfg = Module_loader.default_config () in
    let loader = Module_loader.create cfg in
    let (t_resolve, rok) =
      time_ms (fun () -> match Resolve.resolve_program_with_loader prog loader with
        | Ok _ -> true | Error _ -> false) in
    let (t_codegen, cok) =
      time_ms (fun () -> match (try Codegen.generate_module prog
                                with _ -> Error (Codegen.UnsupportedFeature "<bench raised>")) with
        | Ok _ -> true | Error _ -> false) in
    let up f = f *. 1000.0 /. float_of_int n in   (* µs/func *)
    Printf.printf
      "  n=%-6d  parse %8.2f (%5.1fµ/f)  resolve %8.2f (%5.1fµ/f)  codegen %8.2f (%5.1fµ/f) ms  [r=%b c=%b]\n%!"
      n t_parse (up t_parse) t_resolve (up t_resolve) t_codegen (up t_codegen) rok cok

let run () =
  print_endline "── bench: compile-time SCALING (phase-split, generated) ──";
  List.iter one [ 100; 1000; 3000; 5000 ]
