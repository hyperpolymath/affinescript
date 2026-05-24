(* SPDX-License-Identifier: MPL-2.0 *)
(* Shared input strings for the bench/ microbenchmarks.

   Kept small and self-contained so each phase-bench (lex, parse,
   typecheck, codegen) can be measured against a comparable load.
   Not exhaustive — these are *representative* inputs, not a corpus.
   The goal is to detect order-of-magnitude regressions in a phase,
   not to be a full performance suite. *)

(** A small program exercising arithmetic + branching + a function
    call.  No imports, no traits, no effects — keeps every phase
    in the simple path. *)
let tiny_arith = {|
fn add(x: Int, y: Int) -> Int { x + y }

fn main() -> Int {
  let a = add(1, 2);
  let b = add(3, 4);
  if a < b { add(a, b) } else { add(b, a) }
}
|}

(** A medium program with a struct, a function over the struct, and
    a match.  Exercises type resolution + pattern compilation. *)
let medium_struct_match = {|
type Point = { x: Int, y: Int }

fn manhattan(p: Point) -> Int {
  let dx = if p.x < 0 { 0 - p.x } else { p.x };
  let dy = if p.y < 0 { 0 - p.y } else { p.y };
  dx + dy
}

fn classify(n: Int) -> Int {
  match n {
    0 => 100,
    1 => 200,
    _ => 300,
  }
}

fn main() -> Int {
  let p = Point #{ x: 3, y: 4 };
  let m = manhattan(p);
  classify(m)
}
|}

(** A larger program with multiple functions, an enum, a match over
    it, and arithmetic — representative of mid-sized stdlib files. *)
let larger_enum = {|
enum Shape {
  Circle(Int),
  Square(Int),
  Rect(Int, Int),
}

fn area(s: Shape) -> Int {
  match s {
    Circle(r) => r * r * 3,
    Square(s) => s * s,
    Rect(w, h) => w * h,
  }
}

fn perimeter(s: Shape) -> Int {
  match s {
    Circle(r) => r * 6,
    Square(s) => s * 4,
    Rect(w, h) => (w + h) * 2,
  }
}

fn main() -> Int {
  let a = area(Circle(5));
  let b = area(Square(4));
  let c = area(Rect(3, 6));
  let p = perimeter(Circle(5));
  let q = perimeter(Square(4));
  let r = perimeter(Rect(3, 6));
  a + b + c + p + q + r
}
|}

(** All three programs together, useful for codegen-throughput
    measurements when a single program is too small to register. *)
let all = [
  ("tiny_arith", tiny_arith);
  ("medium_struct_match", medium_struct_match);
  ("larger_enum", larger_enum);
]
