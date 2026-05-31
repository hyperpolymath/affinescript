// SPDX-License-Identifier: MIT
// #488 partial-port fixture: module-top-level functions -> `fn` skeletons
// with switch->match, pipe desugaring, if/else, blocks, and best-effort
// expression translation. Output is NOT expected to type-check; it must
// parse, with un-translatable forms (e.g. an array literal) as TODO holes.

let classify = x => switch x {
| Some(n) => n + 1
| None => 0
}

let area = (w, h) => w *. h

let greet = name => "hi " ++ name

let log2 = msg => Js.log(msg)

// pipe-first desugars: x->doStuff(1)  ->  doStuff(x, 1)
let piped = x => x->doStuff(1)

// pipe chain desugars left-to-right: x->f->g(2)  ->  g(f(x), 2)
let chain = x => x->f->g(2)

// if/else
let clamp = x => if x > 0 { x } else { 0 }

// block with a let statement
let scaled = x => { let y = x + 1; y * 2 }

// array literal has no handler yet -> must become a TODO hole.
let pair = x => [x, x]
