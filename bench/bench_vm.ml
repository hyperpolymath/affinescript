(* SPDX-License-Identifier: MPL-2.0 *)
(* VM M1 (Solo-core CESK) step-rate bench.

   Closes the "no VM step-rate bench" gap (TEST-NEEDS.md). Builds a Solo term
   that reduces in O(n) machine steps (a chain of n single-use lets — each binds
   the previous value, used exactly once, so affine enforcement stays ON) and
   reports steps/second. The tropical cost-meter's accumulated [cost] IS the
   step count, so this also exercises the metering on by default. *)

open Affinescript

let time_ms f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let t1 = Unix.gettimeofday () in
  (1000.0 *. (t1 -. t0), r)

(** n nested lets: let x = (… let x = () …) in x. Each binding used once. *)
let chain (n : int) : Solo_cesk.term =
  let open Solo_cesk in
  let rec build i acc = if i <= 0 then acc else build (i - 1) (Let (One, acc, Var 0)) in
  build n TUnit

let one (n : int) : unit =
  let t = chain n in
  let (ms, (_v, cost)) = time_ms (fun () -> Solo_cesk.run ~fuel:(10 * n + 100) t) in
  let rate = if ms > 0.0 then float_of_int cost /. (ms /. 1000.0) else infinity in
  Printf.printf "  n=%-7d  %9d steps  %8.3f ms  %.3e steps/sec\n%!" n cost ms rate

let run () =
  print_endline "── bench: VM M1 (Solo CESK) step-rate ──────────────";
  List.iter one [ 1_000; 10_000; 100_000 ]
