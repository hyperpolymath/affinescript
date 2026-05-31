// SPDX-License-Identifier: MIT
// #488 partial-port fixture: module-top-level functions -> `fn` skeletons
// with switch->match and best-effort expression translation. Output is NOT
// expected to type-check; it must parse, with un-translatable forms as
// `() /* TODO */` holes.

let classify = x => switch x {
| Some(n) => n + 1
| None => 0
}

let area = (w, h) => w *. h

let greet = name => "hi " ++ name

let log2 = msg => Js.log(msg)

// pipe-first has no AffineScript equivalent -> must become a TODO hole.
let piped = x => x->doStuff(1)
