(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Deno-ESM Emit Mode (issue #122, Refs #35 #103).

    A *direct* AffineScript-AST → ES-module transpiler. Unlike
    {!Codegen_node} (which wraps a compiled [Wasm.wasm_module] in a CJS
    shim), this backend emits standalone ES2020 module source — no wasm,
    no [require], no handle table — so the output is a drop-in [.js] /
    [.mjs] ES module a Deno (or Node ESM) consumer can [import] directly.

    Why a direct transpiler and not a wasm-wrapping ESM shim: the
    motivating consumer ([hyperpolymath/ubicity]'s [storage.ts] /
    [wasm-bridge.ts]) is pure JS-value orchestration — it [JSON.stringify]s
    arbitrary opaque objects, [JSON.parse]s file contents back into JS
    objects, and returns those objects to JS callers. AffineScript's wasm
    ABI is i32-only ([Codegen]: params/ret all [I32]); an opaque JS object
    and [JSON.stringify(obj)] cannot cross that boundary. There is
    essentially no computation here that belongs in wasm. The correct tool
    is therefore source-to-source emission, where [extern fn]s lower to
    direct host calls ([Deno.writeTextFileSync], [JSON.stringify], ...) and
    a struct + its [impl] block lower to a real [export class].

    Relationship to the other JS-ish backends:
    - {!Js_codegen} — standalone ES2020 from AST, but CommonJS-flavoured,
      uses [require("fs")], and *erases* [impl] blocks
      ([// impl block (erased)]). This module reuses its proven
      expression/statement/pattern lowering shape but: emits ESM
      ([export] / no [require]); emits [export class] for struct+impl;
      and lowers [extern fn] to direct Deno/JS host calls.
    - {!Codegen_node} — wasm-wrapping CJS shim (issue #35). Distinct track.

    Out of scope (documented future work on #122 / #103): an async-extern
    ABI. Not needed here — every Deno FS op used by the motivating consumer
    has a synchronous form ([*Sync]); [await] on a synchronously-returned
    value is valid JS so [async]-looking consumer code keeps working. *)

open Ast

(* ============================================================================
   Code-generation context
   ============================================================================ *)

type codegen_ctx = {
  output : Buffer.t;
  indent : int;
  symbols : Symbol.t;
  (* Names declared as [extern fn] (after import flattening these arrive as
     [TopExternFn]). A call to one of these lowers via {!deno_builtin}
     rather than as an ordinary AffineScript function call. *)
  externs : (string, unit) Hashtbl.t;
  (* When emitting a method body, the receiver parameter is rewritten to
     [this]; this holds that parameter's source name (if any). *)
  self_name : string option;
  (* Associated-function name -> emitted method name. A call to one of
     these whose first argument is the active receiver lowers to
     [this.<method>(rest)]; with any other first argument it lowers to
     [(<arg0>).<method>(rest)] (a call on another instance). Lets the
     receiver-first free-function source style (the only form the current
     grammar accepts — no [self]/inherent-[impl]) read as real methods. *)
  assoc : (string, string) Hashtbl.t;
  (* Top-level functions/consts defined in this program. A user
     definition shadows a same-named host intrinsic ({!deno_builtins}),
     so e.g. a user `fn len(xs: IntList)` is NOT lowered to `.length`. *)
  local_fns : (string, unit) Hashtbl.t;
  (* Top-level functions declared with an `Async` effect row. A call to
     one of these is a Promise; from an async context the call site must
     `await` it (e.g. `get(u).status` would otherwise read `.status` off
     a pending Promise). Populated in {!generate}. *)
  async_fns : (string, unit) Hashtbl.t;
  (* Top-level functions whose declared return type is [Int]. Used by
     {!expr_is_int} so a call like [gcd(a, b)] counts as an integer
     operand — needed to truncate e.g. [abs(a*b) / gcd(a,b)]. Populated
     in {!generate}. *)
  int_fns : (string, unit) Hashtbl.t;
  (* Names bound to a provably-[Int] value in the *current function*:
     [Int]-typed params plus [let]/assignments whose value is an integer
     expression. Mutated in source order as statements are emitted (the
     existing per-statement {!List.iter} makes this order-correct), and
     reset per function so names never leak across functions. Drives the
     [Int / Int -> Math.trunc(a / b)] lowering (issue #478); an unknown
     operand keeps plain [/], so float division is never affected. *)
  mutable int_vars : (string, unit) Hashtbl.t;
  (* Names provably bound to an [Array<Int>] (i.e. `[Int]`) in the
     *current function*: array-typed params. Lets a `for x in xs` over an
     int array seed [x] as [Int], and an `xs[i]` element read count as an
     integer operand, so divisions over them truncate (#478). Reset per
     function alongside {!int_vars}. *)
  int_array_vars : (string, unit) Hashtbl.t;
  (* True while emitting a synthesised `async` method body. The
     expression-position IIFE wrappers (block/try/match/let/return) must
     then be `async` and awaited, because they may contain an awaited
     associated call (`(await this.m(...))`) — `await` inside a plain
     `(() => {...})()` is a SyntaxError ("Unexpected reserved word"). *)
  in_async : bool;
}

let create_ctx symbols = {
  output = Buffer.create 1024;
  indent = 0;
  symbols;
  externs = Hashtbl.create 32;
  self_name = None;
  assoc = Hashtbl.create 32;
  local_fns = Hashtbl.create 64;
  async_fns = Hashtbl.create 32;
  int_fns = Hashtbl.create 64;
  int_vars = Hashtbl.create 16;
  int_array_vars = Hashtbl.create 16;
  in_async = false;
}

(* Expression-position IIFE wrapper. In an async method body it must be
   `(await (async () => { ... })())` so a contained `await` is legal;
   elsewhere a plain `(() => { ... })()`. *)
let iife ctx (inner : string) : string =
  if ctx.in_async then "(await (async () => { " ^ inner ^ " })())"
  else "(() => { " ^ inner ^ " })()"

let emit ctx str = Buffer.add_string ctx.output str

let emit_line ctx str =
  Buffer.add_string ctx.output (String.make (ctx.indent * 2) ' ');
  Buffer.add_string ctx.output str;
  Buffer.add_char ctx.output '\n'

let increase_indent ctx = { ctx with indent = ctx.indent + 1 }

(* True if an effect row mentions `Async`. A function whose declared
   effect row includes Async compiles to an `async function`, and its
   body is emitted in async context (`in_async`) so an awaited host
   call (e.g. the `http_request` -> `await fetch(...)` lowering for
   issue #160) is legal JS. This is the free-function analogue of the
   unconditionally-async synthesised methods (`gen_class`). *)
let rec eff_has_async : effect_expr -> bool = function
  | EffVar id | EffCon (id, _) -> id.name = "Async"
  | EffUnion (a, b) -> eff_has_async a || eff_has_async b

let fd_is_async (fd : fn_decl) : bool =
  match fd.fd_eff with Some e -> eff_has_async e | None -> false

(* ============================================================================
   Runtime prelude (ESM, Deno-flavoured)

   Emitted once at the top of every module. No library deps, no [require]:
   `deno run foo.js` / an ESM `import` is enough. `print` uses Deno.stdout
   (Node ESM also exposes a `Deno` global only under compat, so consumers
   that need Node should target the .cjs backend instead).
   ============================================================================ *)

let prelude = {|// ---- AffineScript Deno-ESM runtime ----
const Some = (value) => ({ tag: "Some", value });
const None = { tag: "None" };
const Ok   = (value) => ({ tag: "Ok",  value });
const Err  = (error) => ({ tag: "Err", error });
const Unit = null;
const print   = (s) => { Deno.stdout.writeSync(new TextEncoder().encode(String(s))); };
const println = (s) => { console.log(String(s)); };
// ---- Deno host shims (extern fn lowering targets, issue #122) ----
// Kept tiny + inlined so emitted modules are genuinely drop-in (no extra
// package to publish or resolve). The same surface is mirrored, for
// standalone `deno test`, by packages/affine-deno/mod.js.
const __as_ensureDir = (p) => {
  try { Deno.mkdirSync(p, { recursive: true }); }
  catch (e) { if (!(e instanceof Deno.errors.AlreadyExists)) throw e; }
};
const __as_pathJoin = (a, b) => {
  if (a.length === 0) return b;
  const sep = a.endsWith("/") || a.endsWith("\\") ? "" : "/";
  return a + sep + b;
};
const __as_readDirNames = (p) => {
  const names = [];
  for (const entry of Deno.readDirSync(p)) {
    if (entry.isFile) names.push(entry.name);
  }
  return names;
};
const __as_isNotFound = (e) => (e instanceof Deno.errors.NotFound);
const __as_walkRecursive = (root) => {
  const out = [];
  const rec = (dir) => {
    for (const entry of Deno.readDirSync(dir)) {
      const full = (dir.endsWith("/") ? dir : dir + "/") + entry.name;
      if (entry.isFile) out.push(full);
      else if (entry.isDirectory) rec(full);
    }
  };
  rec(root);
  return out;
};
const __as_regexMatch = (s, pat) => new RegExp(pat).test(String(s));
const __as_wasmInstance = (bytes) =>
  new WebAssembly.Instance(new WebAssembly.Module(bytes)).exports;
const __as_wasmCall = (exports, name, args) => Number(exports[name](...(args || [])));
// ---- WasmValue (Deno.affine #455 — Tier 1 #5, Option B) ----
// Opaque tagged value crossing the AS/JS boundary as `{ kind, v }`.
// `kind` is one of "i32" | "i64" | "f32" | "f64". The `v` payload is
// `BigInt` for i64 (preserves precision beyond 2^53), `Number` otherwise.
const __as_wv_i32 = (n) => ({ kind: "i32", v: (Number(n) | 0) });
const __as_wv_i64 = (n) => ({ kind: "i64", v: BigInt(n) });
const __as_wv_f32 = (f) => ({ kind: "f32", v: Math.fround(Number(f)) });
const __as_wv_f64 = (f) => ({ kind: "f64", v: Number(f) });
const __as_wv_as_int = (v) => {
  if (v == null) return 0;
  if (typeof v.v === "bigint") {
    // i64: truncate to safe-integer Number; caller's responsibility for
    // precision-sensitive paths (use wv_kind to detect).
    return Number(v.v);
  }
  // i32 / f32 / f64: truncate toward zero per AS Int semantics.
  return (Number(v.v) | 0);
};
const __as_wv_as_float = (v) => {
  if (v == null) return 0;
  return typeof v.v === "bigint" ? Number(v.v) : Number(v.v);
};
const __as_wv_kind = (v) => (v && typeof v.kind === "string") ? v.kind : "";
const __as_wasm_export_call = (exports, name, args) => {
  // Unmarshal AS-side [WasmValue] to raw JS scalars for the wasm call.
  const rawArgs = (args || []).map((wv) => {
    if (wv == null) return 0;
    // i64 payload is BigInt; wasm i64 imports accept BigInt directly.
    return wv.v;
  });
  const result = exports[name](...rawArgs);
  // Wrap return as f64 (lossless for any numeric; callers expecting i32/i64
  // can rebuild via wv_i32(wv_as_int(result)) or inspect wv_kind).
  if (typeof result === "bigint") {
    return { kind: "i64", v: result };
  }
  return { kind: "f64", v: Number(result) };
};
// ---- motion (bindings #4): consumer-provided import ----
// Host JS environment must expose globalThis.__as_motion (the motion
// library or a compatible mock). Tests set it in the harness before
// importing the generated module; production consumers typically do
// `import * as m from "motion"; globalThis.__as_motion = m;` once at
// module-init time. The AffineScript-side externs (stdlib/Motion.affine)
// don't see this indirection — they call __as_motion* helpers directly.
const __as_motionAnimate = (target, keyframes, options) =>
  globalThis.__as_motion.animate(target, keyframes, options);
const __as_motionAwait = (controls) =>
  Promise.resolve(controls).then(() => 0);
const __as_motionCancel = (controls) => {
  if (controls && typeof controls.cancel === "function") controls.cancel();
  return 0;
};
// `animateMini` / `tween` / `spring` / `ease` — bindings #4 follow-up
// surface. Each helper resolves the host method on globalThis.__as_motion
// at call time so a mock that only stubs a subset still works for the
// rest (the smoke harness exercises every variant).
const __as_motionAnimateMini = (target, keyframes, options) =>
  globalThis.__as_motion.animateMini(target, keyframes, options);
const __as_motionTween = (target, from, to, options) =>
  globalThis.__as_motion.tween(target, from, to, options);
const __as_motionSpring = (target, keyframes, springConfig) =>
  globalThis.__as_motion.spring(target, keyframes, springConfig);
const __as_motionEase = (name) =>
  globalThis.__as_motion.ease(name);
// ---- pixi.js (bindings #1): consumer-provided import ----
// Host JS environment exposes globalThis.__as_pixi (the PIXI namespace
// from `import * as PIXI from "pixi.js"`). Tests set it in the harness
// before importing the generated module.
const __as_pixiAppInit = async (options) => {
  const app = new globalThis.__as_pixi.Application();
  await app.init(options);
  return app;
};
const __as_pixiAppCanvas = (app) => app.canvas;
const __as_pixiAppStage = (app) => app.stage;
const __as_pixiAppTicker = (app) => app.ticker;
const __as_pixiAppDestroy = (app) => { app.destroy(); return 0; };
const __as_pixiContainerNew = () => new globalThis.__as_pixi.Container();
const __as_pixiContainerAddChild = (p, c) => { p.addChild(c); return 0; };
const __as_pixiContainerRemoveChild = (p, c) => { p.removeChild(c); return 0; };
const __as_pixiContainerSetPosition = (c, x, y) => { c.x = x; c.y = y; return 0; };
const __as_pixiContainerSetScale = (c, x, y) => { c.scale.set(x, y); return 0; };
const __as_pixiContainerSetPivot = (c, x, y) => { c.pivot.set(x, y); return 0; };
const __as_pixiContainerSetRotation = (c, rad) => { c.rotation = rad; return 0; };
const __as_pixiContainerSetAlpha = (c, a) => { c.alpha = a; return 0; };
const __as_pixiContainerSetZIndex = (c, z) => { c.zIndex = z; return 0; };
const __as_pixiContainerSetSortableChildren = (c, v) => { c.sortableChildren = v; return 0; };
const __as_pixiContainerSetEventMode = (c, mode) => { c.eventMode = mode; return 0; };
const __as_pixiContainerSetCursor = (c, cursor) => { c.cursor = cursor; return 0; };
const __as_pixiContainerSetVisible = (c, v) => { c.visible = v; return 0; };
const __as_pixiContainerOn = (c, event, handler) => { c.on(event, handler); return 0; };
const __as_pixiContainerOff = (c, event, handler) => { c.off(event, handler); return 0; };
const __as_pixiContainerDestroy = (c) => { c.destroy(); return 0; };
const __as_pixiSpriteFrom = (t) => new globalThis.__as_pixi.Sprite(t);
const __as_pixiSpriteSetAnchor = (s, x, y) => { s.anchor.set(x, y); return 0; };
// Upcasts are identity — PIXI's class hierarchy makes Sprite/Graphics/
// Text actual Container subclasses, so the JS object is the same.
const __as_pixiSpriteAsContainer = (s) => s;
const __as_pixiTextureFromUrl = (url) => globalThis.__as_pixi.Texture.from(url);
const __as_pixiGraphicsNew = () => new globalThis.__as_pixi.Graphics();
const __as_pixiGraphicsRect = (g, x, y, w, h) => { g.rect(x, y, w, h); return 0; };
const __as_pixiGraphicsFill = (g, color) => { g.fill({ color }); return 0; };
const __as_pixiGraphicsClear = (g) => { g.clear(); return 0; };
const __as_pixiGraphicsAsContainer = (g) => g;
const __as_pixiTextNew = (options) => new globalThis.__as_pixi.Text(options);
const __as_pixiTextSetText = (t, content) => { t.text = content; return 0; };
const __as_pixiTextAsContainer = (t) => t;
const __as_pixiTickerAdd = (t, cb) => { t.add(cb); return 0; };
const __as_pixiTickerStart = (t) => { t.start(); return 0; };
const __as_pixiTickerStop = (t) => { t.stop(); return 0; };
// ---- @pixi/ui (bindings #3): consumer-provided import ----
// Host JS environment exposes globalThis.__as_pixi_ui (the namespace
// from `import * as PixiUI from "@pixi/ui"`). Tests set it in the
// harness before importing the generated module; production
// consumers typically do once at module-init time. The
// AffineScript-side externs (stdlib/PixiUI.affine) don't see this
// indirection — they call __as_pixiUi* helpers directly.
//
// Upcasts to Container are identity — @pixi/ui's Button /
// FancyButton / Slider / Switch are all real PIXI.Container
// subclasses, so the JS object is the same.
const __as_pixiUiButtonNew         = (options) => new globalThis.__as_pixi_ui.Button(options);
const __as_pixiUiButtonOnPress     = (b, cb)   => { b.onPress.connect(cb); return 0; };
const __as_pixiUiButtonAsContainer = (b)       => b;
const __as_pixiUiFancyButtonNew         = (options) => new globalThis.__as_pixi_ui.FancyButton(options);
const __as_pixiUiFancyButtonAsContainer = (b)       => b;
const __as_pixiUiSliderNew         = (options) => new globalThis.__as_pixi_ui.Slider(options);
const __as_pixiUiSliderOnUpdate    = (s, cb)   => { s.onUpdate.connect(cb); return 0; };
const __as_pixiUiSliderAsContainer = (s)       => s;
const __as_pixiUiSwitchNew         = (options) => new globalThis.__as_pixi_ui.Switch(options);
const __as_pixiUiSwitchOnChange    = (sw, cb)  => { sw.onChange.connect(cb); return 0; };
const __as_pixiUiSwitchAsContainer = (sw)      => sw;
// ---- @pixi/sound (bindings #2): consumer-provided import ----
// Host JS environment exposes globalThis.__as_pixi_sound (the `Sound`
// named export from `@pixi/sound`). Tests set it in the harness before
// importing the generated module; production consumers typically do
// `import { Sound } from "@pixi/sound"; globalThis.__as_pixi_sound = Sound;`
// once at module-init time. The AffineScript-side externs
// (stdlib/PixiSound.affine) don't see this indirection — they call
// __as_pixiSound* helpers directly.
const __as_pixiSoundFrom = (url) => globalThis.__as_pixi_sound.from(url);
const __as_pixiSoundPlay = (s) => { s.play(); return 0; };
const __as_pixiSoundStop = (s) => { s.stop(); return 0; };
const __as_pixiSoundPause = (s) => { s.pause(); return 0; };
const __as_pixiSoundResume = (s) => { s.resume(); return 0; };
const __as_pixiSoundSetVolume = (s, vol) => { s.volume = vol; return 0; };
const __as_pixiSoundSetLoop = (s, loop) => { s.loop = loop; return 0; };
// ---- Ipc (bindings #9): web-platform MessageChannel/MessagePort ----
// Uses standard web globals (MessageChannel, structuredClone) — no
// consumer-side init required. Available unmodified in Deno, Node 16+,
// browsers, and Web Workers.
const __as_messageChannelNew = () => new MessageChannel();
const __as_messageChannelPort1 = (ch) => ch.port1;
const __as_messageChannelPort2 = (ch) => ch.port2;
const __as_messagePortPostMessage = (p, data) => { p.postMessage(data); return 0; };
const __as_messagePortOnMessage = (p, handler) => { p.onmessage = handler; return 0; };
const __as_messagePortStart = (p) => { p.start(); return 0; };
const __as_messagePortClose = (p) => { p.close(); return 0; };
const __as_targetPostMessage = (t, msg) => { t.postMessage(msg); return 0; };
const __as_structuredCloneValue = (v) => structuredClone(v);
// ---- Canvas (bindings #8): HTML5 Canvas 2D rendering context ----
// `canvas` arg is the consumer-supplied HTMLCanvasElement; helpers
// dispatch directly to the standard CanvasRenderingContext2D
// methods. Available unmodified in browsers, jsdom-under-Deno,
// idaptik's WebView host, and any DOM emulator.
const __as_canvasGetContext2D = (canvas) => canvas.getContext("2d");
const __as_canvasFillStyle = (ctx, color) => { ctx.fillStyle = color; return 0; };
const __as_canvasStrokeStyle = (ctx, color) => { ctx.strokeStyle = color; return 0; };
const __as_canvasLineWidth = (ctx, w) => { ctx.lineWidth = w; return 0; };
const __as_canvasGlobalAlpha = (ctx, a) => { ctx.globalAlpha = a; return 0; };
const __as_canvasFillRect = (ctx, x, y, w, h) => { ctx.fillRect(x, y, w, h); return 0; };
const __as_canvasStrokeRect = (ctx, x, y, w, h) => { ctx.strokeRect(x, y, w, h); return 0; };
const __as_canvasClearRect = (ctx, x, y, w, h) => { ctx.clearRect(x, y, w, h); return 0; };
const __as_canvasBeginPath = (ctx) => { ctx.beginPath(); return 0; };
const __as_canvasClosePath = (ctx) => { ctx.closePath(); return 0; };
const __as_canvasMoveTo = (ctx, x, y) => { ctx.moveTo(x, y); return 0; };
const __as_canvasLineTo = (ctx, x, y) => { ctx.lineTo(x, y); return 0; };
const __as_canvasArc = (ctx, x, y, r, s, e) => { ctx.arc(x, y, r, s, e); return 0; };
const __as_canvasFill = (ctx) => { ctx.fill(); return 0; };
const __as_canvasStroke = (ctx) => { ctx.stroke(); return 0; };
const __as_canvasSave = (ctx) => { ctx.save(); return 0; };
const __as_canvasRestore = (ctx) => { ctx.restore(); return 0; };
const __as_canvasTranslate = (ctx, x, y) => { ctx.translate(x, y); return 0; };
const __as_canvasRotate = (ctx, rad) => { ctx.rotate(rad); return 0; };
const __as_canvasScale = (ctx, x, y) => { ctx.scale(x, y); return 0; };
const __as_canvasFont = (ctx, font) => { ctx.font = font; return 0; };
const __as_canvasTextAlign = (ctx, align) => { ctx.textAlign = align; return 0; };
const __as_canvasTextBaseline = (ctx, baseline) => { ctx.textBaseline = baseline; return 0; };
const __as_canvasFillText = (ctx, text, x, y) => { ctx.fillText(text, x, y); return 0; };
const __as_canvasStrokeText = (ctx, text, x, y) => { ctx.strokeText(text, x, y); return 0; };
const __as_canvasMeasureText = (ctx, text) => ctx.measureText(text);
const __as_canvasDrawImage = (ctx, img, x, y) => { ctx.drawImage(img, x, y); return 0; };
const __as_canvasDrawImageScaled = (ctx, img, x, y, w, h) => { ctx.drawImage(img, x, y, w, h); return 0; };
// `++` is overloaded (string concat / array concat); `a + b` would
// stringify arrays. Dispatch on shape so stdlib/string.affine's
// `result ++ [x]` and `a ++ b` are both correct.
const __as_concat = (a, b) => Array.isArray(a) ? a.concat(b) : (a + b);
// Honest host/runtime primitives underpinning the AffineScript-level
// stdlib/string.affine (its is_empty/starts_with/ends_with/split/join/
// replace/... are real AffineScript on top of these).
const __as_strSub = (s, start, n) => String(s).slice(start, start + n);
const __as_strGet = (s, i) => String(s)[i];
const __as_strFind = (s, n) => String(s).indexOf(n);
const __as_charToInt = (c) => String(c).codePointAt(0);
const __as_intToChar = (n) => String.fromCodePoint(n);
const __as_strCharCodeAt = (s, i) => (i >= 0 && i < s.length ? s.charCodeAt(i) : -1);
const __as_strFromCharCode = (n) => String.fromCharCode(n & 0xff);
const __as_parseInt = (s) => {
  const n = parseInt(String(s), 10);
  return Number.isNaN(n) ? None : Some(n);
};
const __as_parseFloat = (s) => {
  const n = parseFloat(String(s));
  return Number.isNaN(n) ? None : Some(n);
};
const __as_show = (v) => (typeof v === "string" ? v : JSON.stringify(v));
// ---- Http (issue #160): portable fetch round-trip ----
// `headers` crosses the boundary as an AffineScript [(String, String)]
// assoc list == JS array of [name, value] pairs. `body` is an
// AffineScript Option<String> == { tag: "Some", value } | { tag: "None" }.
// The result is the `Response` record shape { status, headers, body }.
const __as_httpHeadersToObject = (pairs) => {
  const o = {};
  for (const kv of (pairs || [])) o[kv[0]] = kv[1];
  return o;
};
const __as_httpHeadersFromResponse = (res) => {
  const out = [];
  res.headers.forEach((value, key) => out.push([key, value]));
  return out;
};
// ---- hpm-json-rsr Zig FFI shims (stdlib/json.affine v0.3) ----
// `HpmJsonValue` is opaque to AffineScript; on Deno-ESM it's just the
// underlying JS value from JSON.parse. The shims mirror the sentinel
// conventions of the Zig exports so the AffineScript-side wrappers
// (`to_json`, `parse`) behave identically across backends.
const __as_hpmJsonParse = (s) => {
  try { return Some(JSON.parse(String(s))); } catch (_e) { return None; }
};
const __as_hpmJsonFree = (_v) => 0;
const __as_hpmJsonType = (v) => {
  if (v === null || v === undefined) return 0;
  if (typeof v === "boolean") return 1;
  if (typeof v === "number")  return Number.isInteger(v) ? 2 : 3;
  if (typeof v === "string")  return 4;
  if (Array.isArray(v))       return 5;
  if (typeof v === "object")  return 6;
  return -1;
};
const __as_hpmJsonBool = (v) => (typeof v === "boolean" ? (v ? 1 : 0) : -1);
const __as_hpmJsonInt = (v) =>
  (typeof v === "number" ? Math.trunc(v) : Number.MIN_SAFE_INTEGER);
const __as_hpmJsonFloat = (v) => (typeof v === "number" ? v : NaN);
const __as_hpmJsonString = (v) => (typeof v === "string" ? v : "");
const __as_hpmJsonObjectGet = (v, k) => {
  if (v === null || typeof v !== "object" || Array.isArray(v)) return None;
  return Object.prototype.hasOwnProperty.call(v, String(k))
    ? Some(v[String(k)]) : None;
};
const __as_hpmJsonArrayLen = (v) => (Array.isArray(v) ? v.length : 0);
const __as_hpmJsonArrayGet = (v, i) => {
  if (!Array.isArray(v)) return None;
  const idx = Number(i);
  return (idx >= 0 && idx < v.length) ? Some(v[idx]) : None;
};
const __as_hpmJsonEscapeString = (s) => {
  let out = "";
  const src = String(s);
  for (let i = 0; i < src.length; i++) {
    const c = src.charCodeAt(i);
    if (c === 0x22) out += "\\\"";
    else if (c === 0x5c) out += "\\\\";
    else if (c === 0x0a) out += "\\n";
    else if (c === 0x0d) out += "\\r";
    else if (c === 0x09) out += "\\t";
    else if (c === 0x08) out += "\\b";
    else if (c === 0x0c) out += "\\f";
    else if (c < 0x20) out += "\\u00" + c.toString(16).padStart(2, "0");
    else out += src[i];
  }
  return out;
};
const __as_httpFetch = async (url, method, headers, bodyOpt) => {
  const init = { method, headers: __as_httpHeadersToObject(headers) };
  if (bodyOpt && bodyOpt.tag === "Some") init.body = bodyOpt.value;
  // `globalThis.fetch` explicitly: the stdlib `Http.fetch` compiles to a
  // module-level `function fetch`, which would otherwise shadow the host.
  const res = await globalThis.fetch(url, init);
  const text = await res.text();
  return {
    status: res.status,
    headers: __as_httpHeadersFromResponse(res),
    body: text,
  };
};
// ---- end runtime ----

|}

(* Lowering table: extern-fn name -> a function from rendered-arg list to a
   JS expression string. Covers the Deno FS / JSON / Wasm / string surface
   declared by stdlib/Deno.affine (issue #122). An extern not in this table
   is assumed to be a host symbol of the same name already in scope (e.g.
   imported by a hand-written shim); it lowers as a plain call. *)
let deno_builtins :
  (string, string list -> string) Hashtbl.t = Hashtbl.create 32

let () =
  let b name f = Hashtbl.replace deno_builtins name f in
  let arg n a = List.nth a n in
  (* ---- filesystem (synchronous; see module docstring re: async) ---- *)
  b "writeTextFile" (fun a -> Printf.sprintf "Deno.writeTextFileSync(%s, %s)" (arg 0 a) (arg 1 a));
  b "readTextFile"  (fun a -> Printf.sprintf "Deno.readTextFileSync(%s)" (arg 0 a));
  b "readFileBytes" (fun a -> Printf.sprintf "Deno.readFileSync(%s)" (arg 0 a));
  b "removePath"    (fun a -> Printf.sprintf "Deno.removeSync(%s)" (arg 0 a));
  b "mkdirRecursive" (fun a -> Printf.sprintf "Deno.mkdirSync(%s, { recursive: true })" (arg 0 a));
  b "ensureDir"     (fun a -> Printf.sprintf "__as_ensureDir(%s)" (arg 0 a));
  b "readDirNames"  (fun a -> Printf.sprintf "__as_readDirNames(%s)" (arg 0 a));
  b "statSize"      (fun a -> Printf.sprintf "Deno.statSync(%s).size" (arg 0 a));
  b "pathJoin"      (fun a -> Printf.sprintf "__as_pathJoin(%s, %s)" (arg 0 a) (arg 1 a));
  b "isNotFound"    (fun a -> Printf.sprintf "__as_isNotFound(%s)" (arg 0 a));
  (* ---- JSON ---- *)
  b "jsonStringify"       (fun a -> Printf.sprintf "JSON.stringify(%s)" (arg 0 a));
  b "jsonStringifyPretty" (fun a -> Printf.sprintf "JSON.stringify(%s, null, 2)" (arg 0 a));
  b "jsonParse"           (fun a -> Printf.sprintf "JSON.parse(%s)" (arg 0 a));
  b "jsonNull"            (fun _ -> "null");
  (* Opaque-object field/index read (the boundary primitive for treating
     an arbitrary JS value as data — ubicity's `experience.id` etc.). *)
  b "jsonGet"     (fun a -> Printf.sprintf "(%s)[%s]" (arg 0 a) (arg 1 a));
  b "jsonGetStr"  (fun a -> Printf.sprintf "String((%s)[%s])" (arg 0 a) (arg 1 a));
  (* Nullish default — preserves a JS default parameter when the arg is
     omitted (`new ExperienceStorage()` -> './ubicity-data'). *)
  b "orDefault"   (fun a -> Printf.sprintf "(%s ?? %s)" (arg 0 a) (arg 1 a));
  (* Kilobyte display string: `(n/1024).toFixed(2)` — runtime number
     formatting is an honest host primitive in every language. *)
  b "kbString"    (fun a -> Printf.sprintf "(Number(%s) / 1024).toFixed(2)" (arg 0 a));
  (* ---- misc host ---- *)
  b "dateNow"     (fun _ -> "Date.now()");
  (* `new Date().toISOString()` — UTC ISO-8601 timestamp string. Distinct
     from `dateNow()` which returns epoch millis as Int. *)
  b "dateNowIso"  (fun _ -> "(new Date().toISOString())");
  (* `Deno.args` — CLI argument vector (excludes argv[0]). *)
  b "args"        (fun _ -> "Deno.args");
  (* `Deno.exit(code)` — terminate process. Never returns. *)
  b "exit"        (fun a -> Printf.sprintf "Deno.exit(%s)" (arg 0 a));
  (* `console.error(s)` — stderr write. Returns 0 for chaining; the
     comma-expression preserves the AffineScript Int contract. *)
  b "consoleError" (fun a -> Printf.sprintf "(console.error(%s), 0)" (arg 0 a));
  (* Recursive file walk — depth-first, returns every file path under
     `root`. Filtering by extension is the caller's responsibility. *)
  b "walkRecursive" (fun a -> Printf.sprintf "__as_walkRecursive(%s)" (arg 0 a));
  (* `new RegExp(pat).test(s)` — minimal regex surface. Invalid `pat`
     throws at call time (RegExp constructor error). *)
  b "regexMatch"   (fun a -> Printf.sprintf "__as_regexMatch(%s, %s)" (arg 0 a) (arg 1 a));
  b "wasmInstance" (fun a -> Printf.sprintf "__as_wasmInstance(%s)" (arg 0 a));
  b "wasmCall"     (fun a -> Printf.sprintf "__as_wasmCall(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  (* WasmValue constructors / accessors / typed export call — #455. *)
  b "wv_i32"           (fun a -> Printf.sprintf "__as_wv_i32(%s)" (arg 0 a));
  b "wv_i64"           (fun a -> Printf.sprintf "__as_wv_i64(%s)" (arg 0 a));
  b "wv_f32"           (fun a -> Printf.sprintf "__as_wv_f32(%s)" (arg 0 a));
  b "wv_f64"           (fun a -> Printf.sprintf "__as_wv_f64(%s)" (arg 0 a));
  b "wv_as_int"        (fun a -> Printf.sprintf "__as_wv_as_int(%s)" (arg 0 a));
  b "wv_as_float"      (fun a -> Printf.sprintf "__as_wv_as_float(%s)" (arg 0 a));
  b "wv_kind"          (fun a -> Printf.sprintf "__as_wv_kind(%s)" (arg 0 a));
  b "wasm_export_call" (fun a -> Printf.sprintf "__as_wasm_export_call(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  (* ---- motion (bindings #4) ---- *)
  b "motionAnimate" (fun a -> Printf.sprintf "__as_motionAnimate(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "motionAwait"   (fun a -> Printf.sprintf "(await __as_motionAwait(%s))" (arg 0 a));
  b "motionCancel"  (fun a -> Printf.sprintf "__as_motionCancel(%s)" (arg 0 a));
  (* ---- pixi.js (bindings #1) ---- *)
  b "pixiAppInit"              (fun a -> Printf.sprintf "(await __as_pixiAppInit(%s))" (arg 0 a));
  b "pixiAppCanvas"            (fun a -> Printf.sprintf "__as_pixiAppCanvas(%s)" (arg 0 a));
  b "pixiAppStage"             (fun a -> Printf.sprintf "__as_pixiAppStage(%s)" (arg 0 a));
  b "pixiAppTicker"            (fun a -> Printf.sprintf "__as_pixiAppTicker(%s)" (arg 0 a));
  b "pixiAppDestroy"           (fun a -> Printf.sprintf "__as_pixiAppDestroy(%s)" (arg 0 a));
  b "pixiContainerNew"         (fun _ -> "__as_pixiContainerNew()");
  b "pixiContainerAddChild"    (fun a -> Printf.sprintf "__as_pixiContainerAddChild(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerRemoveChild" (fun a -> Printf.sprintf "__as_pixiContainerRemoveChild(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerSetPosition" (fun a -> Printf.sprintf "__as_pixiContainerSetPosition(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "pixiContainerSetScale"    (fun a -> Printf.sprintf "__as_pixiContainerSetScale(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "pixiContainerSetPivot"    (fun a -> Printf.sprintf "__as_pixiContainerSetPivot(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "pixiContainerSetRotation" (fun a -> Printf.sprintf "__as_pixiContainerSetRotation(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerSetAlpha"    (fun a -> Printf.sprintf "__as_pixiContainerSetAlpha(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerSetZIndex"   (fun a -> Printf.sprintf "__as_pixiContainerSetZIndex(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerSetSortableChildren" (fun a -> Printf.sprintf "__as_pixiContainerSetSortableChildren(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerSetEventMode" (fun a -> Printf.sprintf "__as_pixiContainerSetEventMode(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerSetCursor"   (fun a -> Printf.sprintf "__as_pixiContainerSetCursor(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerSetVisible"  (fun a -> Printf.sprintf "__as_pixiContainerSetVisible(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiContainerOn"          (fun a -> Printf.sprintf "__as_pixiContainerOn(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "pixiContainerOff"         (fun a -> Printf.sprintf "__as_pixiContainerOff(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "pixiContainerDestroy"     (fun a -> Printf.sprintf "__as_pixiContainerDestroy(%s)" (arg 0 a));
  b "pixiSpriteFrom"           (fun a -> Printf.sprintf "__as_pixiSpriteFrom(%s)" (arg 0 a));
  b "pixiSpriteSetAnchor"      (fun a -> Printf.sprintf "__as_pixiSpriteSetAnchor(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "pixiSpriteAsContainer"    (fun a -> Printf.sprintf "__as_pixiSpriteAsContainer(%s)" (arg 0 a));
  b "pixiTextureFromUrl"       (fun a -> Printf.sprintf "__as_pixiTextureFromUrl(%s)" (arg 0 a));
  b "pixiGraphicsNew"          (fun _ -> "__as_pixiGraphicsNew()");
  b "pixiGraphicsRect"         (fun a -> Printf.sprintf "__as_pixiGraphicsRect(%s, %s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a) (arg 4 a));
  b "pixiGraphicsFill"         (fun a -> Printf.sprintf "__as_pixiGraphicsFill(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiGraphicsClear"        (fun a -> Printf.sprintf "__as_pixiGraphicsClear(%s)" (arg 0 a));
  b "pixiGraphicsAsContainer"  (fun a -> Printf.sprintf "__as_pixiGraphicsAsContainer(%s)" (arg 0 a));
  b "pixiTextNew"              (fun a -> Printf.sprintf "__as_pixiTextNew(%s)" (arg 0 a));
  b "pixiTextSetText"          (fun a -> Printf.sprintf "__as_pixiTextSetText(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiTextAsContainer"      (fun a -> Printf.sprintf "__as_pixiTextAsContainer(%s)" (arg 0 a));
  b "pixiTickerAdd"            (fun a -> Printf.sprintf "__as_pixiTickerAdd(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiTickerStart"          (fun a -> Printf.sprintf "__as_pixiTickerStart(%s)" (arg 0 a));
  b "pixiTickerStop"           (fun a -> Printf.sprintf "__as_pixiTickerStop(%s)" (arg 0 a));
  (* ---- @pixi/ui (bindings #3) ---- *)
  b "pixiUiButtonNew"              (fun a -> Printf.sprintf "__as_pixiUiButtonNew(%s)" (arg 0 a));
  b "pixiUiButtonOnPress"          (fun a -> Printf.sprintf "__as_pixiUiButtonOnPress(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiUiButtonAsContainer"      (fun a -> Printf.sprintf "__as_pixiUiButtonAsContainer(%s)" (arg 0 a));
  b "pixiUiFancyButtonNew"         (fun a -> Printf.sprintf "__as_pixiUiFancyButtonNew(%s)" (arg 0 a));
  b "pixiUiFancyButtonAsContainer" (fun a -> Printf.sprintf "__as_pixiUiFancyButtonAsContainer(%s)" (arg 0 a));
  b "pixiUiSliderNew"              (fun a -> Printf.sprintf "__as_pixiUiSliderNew(%s)" (arg 0 a));
  b "pixiUiSliderOnUpdate"         (fun a -> Printf.sprintf "__as_pixiUiSliderOnUpdate(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiUiSliderAsContainer"      (fun a -> Printf.sprintf "__as_pixiUiSliderAsContainer(%s)" (arg 0 a));
  b "pixiUiSwitchNew"              (fun a -> Printf.sprintf "__as_pixiUiSwitchNew(%s)" (arg 0 a));
  b "pixiUiSwitchOnChange"         (fun a -> Printf.sprintf "__as_pixiUiSwitchOnChange(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiUiSwitchAsContainer"      (fun a -> Printf.sprintf "__as_pixiUiSwitchAsContainer(%s)" (arg 0 a));
  (* ---- motion extras (bindings #4 follow-up) ---- *)
  b "motionAnimateMini" (fun a -> Printf.sprintf "__as_motionAnimateMini(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "motionTween"   (fun a -> Printf.sprintf "__as_motionTween(%s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a));
  b "motionSpring"  (fun a -> Printf.sprintf "__as_motionSpring(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "motionEase"    (fun a -> Printf.sprintf "__as_motionEase(%s)" (arg 0 a));
  (* ---- @pixi/sound (bindings #2) ---- *)
  b "pixiSoundFrom"      (fun a -> Printf.sprintf "__as_pixiSoundFrom(%s)" (arg 0 a));
  b "pixiSoundPlay"      (fun a -> Printf.sprintf "__as_pixiSoundPlay(%s)" (arg 0 a));
  b "pixiSoundStop"      (fun a -> Printf.sprintf "__as_pixiSoundStop(%s)" (arg 0 a));
  b "pixiSoundPause"     (fun a -> Printf.sprintf "__as_pixiSoundPause(%s)" (arg 0 a));
  b "pixiSoundResume"    (fun a -> Printf.sprintf "__as_pixiSoundResume(%s)" (arg 0 a));
  b "pixiSoundSetVolume" (fun a -> Printf.sprintf "__as_pixiSoundSetVolume(%s, %s)" (arg 0 a) (arg 1 a));
  b "pixiSoundSetLoop"   (fun a -> Printf.sprintf "__as_pixiSoundSetLoop(%s, %s)" (arg 0 a) (arg 1 a));
  (* ---- Ipc (bindings #9): MessageChannel/MessagePort + structuredClone ---- *)
  b "messageChannelNew"        (fun _ -> "__as_messageChannelNew()");
  b "messageChannelPort1"      (fun a -> Printf.sprintf "__as_messageChannelPort1(%s)" (arg 0 a));
  b "messageChannelPort2"      (fun a -> Printf.sprintf "__as_messageChannelPort2(%s)" (arg 0 a));
  b "messagePortPostMessage"   (fun a -> Printf.sprintf "__as_messagePortPostMessage(%s, %s)" (arg 0 a) (arg 1 a));
  b "messagePortOnMessage"     (fun a -> Printf.sprintf "__as_messagePortOnMessage(%s, %s)" (arg 0 a) (arg 1 a));
  b "messagePortStart"         (fun a -> Printf.sprintf "__as_messagePortStart(%s)" (arg 0 a));
  b "messagePortClose"         (fun a -> Printf.sprintf "__as_messagePortClose(%s)" (arg 0 a));
  b "targetPostMessage"        (fun a -> Printf.sprintf "__as_targetPostMessage(%s, %s)" (arg 0 a) (arg 1 a));
  b "structuredCloneValue"     (fun a -> Printf.sprintf "__as_structuredCloneValue(%s)" (arg 0 a));
  (* ---- Canvas (bindings #8): HTML5 Canvas 2D rendering context ---- *)
  b "canvasGetContext2D"   (fun a -> Printf.sprintf "__as_canvasGetContext2D(%s)" (arg 0 a));
  b "canvasFillStyle"      (fun a -> Printf.sprintf "__as_canvasFillStyle(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasStrokeStyle"    (fun a -> Printf.sprintf "__as_canvasStrokeStyle(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasLineWidth"      (fun a -> Printf.sprintf "__as_canvasLineWidth(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasGlobalAlpha"    (fun a -> Printf.sprintf "__as_canvasGlobalAlpha(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasFillRect"       (fun a -> Printf.sprintf "__as_canvasFillRect(%s, %s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a) (arg 4 a));
  b "canvasStrokeRect"     (fun a -> Printf.sprintf "__as_canvasStrokeRect(%s, %s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a) (arg 4 a));
  b "canvasClearRect"      (fun a -> Printf.sprintf "__as_canvasClearRect(%s, %s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a) (arg 4 a));
  b "canvasBeginPath"      (fun a -> Printf.sprintf "__as_canvasBeginPath(%s)" (arg 0 a));
  b "canvasClosePath"      (fun a -> Printf.sprintf "__as_canvasClosePath(%s)" (arg 0 a));
  b "canvasMoveTo"         (fun a -> Printf.sprintf "__as_canvasMoveTo(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "canvasLineTo"         (fun a -> Printf.sprintf "__as_canvasLineTo(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "canvasArc"            (fun a -> Printf.sprintf "__as_canvasArc(%s, %s, %s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a) (arg 4 a) (arg 5 a));
  b "canvasFill"           (fun a -> Printf.sprintf "__as_canvasFill(%s)" (arg 0 a));
  b "canvasStroke"         (fun a -> Printf.sprintf "__as_canvasStroke(%s)" (arg 0 a));
  b "canvasSave"           (fun a -> Printf.sprintf "__as_canvasSave(%s)" (arg 0 a));
  b "canvasRestore"        (fun a -> Printf.sprintf "__as_canvasRestore(%s)" (arg 0 a));
  b "canvasTranslate"      (fun a -> Printf.sprintf "__as_canvasTranslate(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "canvasRotate"         (fun a -> Printf.sprintf "__as_canvasRotate(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasScale"          (fun a -> Printf.sprintf "__as_canvasScale(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "canvasFont"           (fun a -> Printf.sprintf "__as_canvasFont(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasTextAlign"      (fun a -> Printf.sprintf "__as_canvasTextAlign(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasTextBaseline"   (fun a -> Printf.sprintf "__as_canvasTextBaseline(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasFillText"       (fun a -> Printf.sprintf "__as_canvasFillText(%s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a));
  b "canvasStrokeText"     (fun a -> Printf.sprintf "__as_canvasStrokeText(%s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a));
  b "canvasMeasureText"    (fun a -> Printf.sprintf "__as_canvasMeasureText(%s, %s)" (arg 0 a) (arg 1 a));
  b "canvasDrawImage"      (fun a -> Printf.sprintf "__as_canvasDrawImage(%s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a));
  b "canvasDrawImageScaled" (fun a -> Printf.sprintf "__as_canvasDrawImageScaled(%s, %s, %s, %s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a) (arg 4 a) (arg 5 a));
  (* Generic JS array push helper (returns the array, fluent). *)
  b "arrayPush" (fun a -> Printf.sprintf "(%s.push(%s), %s)" (arg 0 a) (arg 1 a) (arg 0 a));
  (* ---- honest string/number primitives underpinning the
     AffineScript-level stdlib/string.affine. These are intrinsics (no
     AffineScript definition exists; the interpreter binds them too),
     not externs — endsWith/stripSuffix/pathJoin/etc. are NOT here:
     they are real AffineScript built on `ends_with`/`substring`/`++`. *)
  b "len"            (fun a -> Printf.sprintf "((%s).length)" (arg 0 a));
  b "slice"          (fun a -> Printf.sprintf "((%s).slice(%s, %s))"
                                 (arg 0 a) (arg 1 a) (arg 2 a));
  b "string_length"  (fun a -> Printf.sprintf "((%s).length)" (arg 0 a));
  b "string_get"     (fun a -> Printf.sprintf "__as_strGet(%s, %s)" (arg 0 a) (arg 1 a));
  b "string_sub"     (fun a -> Printf.sprintf "__as_strSub(%s, %s, %s)" (arg 0 a) (arg 1 a) (arg 2 a));
  b "string_find"    (fun a -> Printf.sprintf "__as_strFind(%s, %s)" (arg 0 a) (arg 1 a));
  b "to_lowercase"   (fun a -> Printf.sprintf "String(%s).toLowerCase()" (arg 0 a));
  b "to_uppercase"   (fun a -> Printf.sprintf "String(%s).toUpperCase()" (arg 0 a));
  b "trim"           (fun a -> Printf.sprintf "String(%s).trim()" (arg 0 a));
  b "int_to_string"  (fun a -> Printf.sprintf "String(%s)" (arg 0 a));
  b "float_to_string" (fun a -> Printf.sprintf "String(%s)" (arg 0 a));
  (* STDLIB-04e (Refs #332): `string_to_int` is the typed-alias of
     `parse_int` declared in stdlib/effects.affine. Same `__as_parseInt`
     host shim, returns Option<Int>. *)
  b "string_to_int"  (fun a -> Printf.sprintf "__as_parseInt(%s)" (arg 0 a));
  b "parse_int"      (fun a -> Printf.sprintf "__as_parseInt(%s)" (arg 0 a));
  b "parse_float"    (fun a -> Printf.sprintf "__as_parseFloat(%s)" (arg 0 a));
  b "char_to_int"    (fun a -> Printf.sprintf "__as_charToInt(%s)" (arg 0 a));
  b "int_to_char"    (fun a -> Printf.sprintf "__as_intToChar(%s)" (arg 0 a));
  b "string_char_code_at"
    (fun a -> Printf.sprintf "__as_strCharCodeAt(%s, %s)" (arg 0 a) (arg 1 a));
  b "string_from_char_code"
    (fun a -> Printf.sprintf "__as_strFromCharCode(%s)" (arg 0 a));
  b "show"           (fun a -> Printf.sprintf "__as_show(%s)" (arg 0 a));
  b "panic"          (fun a -> Printf.sprintf "(() => { throw new Error(%s); })()" (arg 0 a));
  (* STDLIB-04b (Refs #329): `error<T>` is panic's polymorphic sibling.
     Same divergent runtime semantics (throw); the polymorphic return
     type is unobservable. *)
  b "error"          (fun a -> Printf.sprintf "(() => { throw new Error(%s); })()" (arg 0 a));
  (* Mut effect builtins (STDLIB-04a, Refs #328) — runtime mutable cells.
     Distinct from borrow-checker [&]/[&mut] references: these back the
     [stdlib/effects.affine] [Ref<T>] type declared `/ Mut`. Lowered as
     a single-field object so [get]/[set] are O(1) field access; comma-
     expression in [set] returns Unit (null) to match the extern's
     signature `(Ref<T>, T) -> Unit / Mut`. *)
  b "make_ref"       (fun a -> Printf.sprintf "({__cell: %s})" (arg 0 a));
  b "get"            (fun a -> Printf.sprintf "((%s).__cell)" (arg 0 a));
  b "set"            (fun a -> Printf.sprintf "(((%s).__cell = %s), null)"
                                 (arg 0 a) (arg 1 a));
  (* ---- Http (issue #160) ---- *)
  (* `await` is legal: every caller of `http_request` is declared
     `/ Net, Async` and so is emitted as an `async function`
     (see {!fd_is_async}). *)
  b "http_request" (fun a ->
    Printf.sprintf "(await __as_httpFetch(%s, %s, %s, %s))"
      (arg 0 a) (arg 1 a) (arg 2 a) (arg 3 a));
  (* ---- hpm-json-rsr Zig FFI surface (stdlib/json.affine v0.3) ---- *)
  b "hpm_json_parse"         (fun a -> Printf.sprintf "__as_hpmJsonParse(%s)" (arg 0 a));
  b "hpm_json_free"          (fun a -> Printf.sprintf "__as_hpmJsonFree(%s)" (arg 0 a));
  b "hpm_json_type"          (fun a -> Printf.sprintf "__as_hpmJsonType(%s)" (arg 0 a));
  b "hpm_json_bool"          (fun a -> Printf.sprintf "__as_hpmJsonBool(%s)" (arg 0 a));
  b "hpm_json_int"           (fun a -> Printf.sprintf "__as_hpmJsonInt(%s)" (arg 0 a));
  b "hpm_json_float"         (fun a -> Printf.sprintf "__as_hpmJsonFloat(%s)" (arg 0 a));
  b "hpm_json_string"        (fun a -> Printf.sprintf "__as_hpmJsonString(%s)" (arg 0 a));
  b "hpm_json_object_get"    (fun a -> Printf.sprintf "__as_hpmJsonObjectGet(%s, %s)" (arg 0 a) (arg 1 a));
  b "hpm_json_array_len"     (fun a -> Printf.sprintf "__as_hpmJsonArrayLen(%s)" (arg 0 a));
  b "hpm_json_array_get"     (fun a -> Printf.sprintf "__as_hpmJsonArrayGet(%s, %s)" (arg 0 a) (arg 1 a));
  b "hpm_json_escape_string" (fun a -> Printf.sprintf "__as_hpmJsonEscapeString(%s)" (arg 0 a))

(* ============================================================================
   Identifier sanitisation (JS reserved words -> trailing underscore)
   ============================================================================ *)

(* Real ECMAScript reserved words + strict-mode (ES modules are strict)
   future-reserved words + restricted names. Deliberately excludes the
   Java-ism primitives ([double], [int], [boolean], [byte], [char],
   [float], [long], [short], [abstract], [final], [native], ...) that
   {!Js_codegen} carries: those are valid JS identifiers, and mangling
   them would silently corrupt a consumer's required ESM export surface
   (issue #122 requires the *exact* surface, e.g. ubicity). *)
let js_reserved = [
  "arguments"; "await"; "break"; "case"; "catch"; "class"; "const";
  "continue"; "debugger"; "default"; "delete"; "do"; "else"; "enum";
  "eval"; "export"; "extends"; "false"; "finally"; "for"; "function";
  "if"; "implements"; "import"; "in"; "instanceof"; "interface"; "let";
  "new"; "null"; "package"; "private"; "protected"; "public"; "return";
  "static"; "super"; "switch"; "this"; "throw"; "true"; "try"; "typeof";
  "var"; "void"; "while"; "with"; "yield";
]

let mangle (name : string) : string =
  if List.mem name js_reserved then name ^ "_" else name

(* Resolve a variable reference, honouring the active [self] rename. *)
let resolve_var ctx (name : string) : string =
  match ctx.self_name with
  | Some s when s = name -> "this"
  | _ -> mangle name

(* ============================================================================
   Integer-operand inference for division (issue #478)

   The Deno-ESM backend is type-erased, but JS `/` is floating-point, so a
   naive `OpDiv -> "/"` turns `255 / 16` into `15.9375`. We lower an
   `Int / Int` to `Math.trunc(a / b)` (truncate-toward-zero, matching the
   interpreter's OCaml `/` and wasm's `i32.div_s`) and leave every other
   `/` as plain float division. The classifier below is deliberately
   conservative: it reports [true] only when an operand is *provably* an
   integer, so a value of unknown type keeps `/` and float division is
   never silently truncated.
   ============================================================================ *)

(* [Int] head of a (possibly ref/own/mut) type expression. Only the
   nominal [Int] constructor counts; type variables / applications do not. *)
let rec type_head_is_int : type_expr -> bool = function
  | TyCon id -> id.name = "Int"
  | TyOwn t | TyRef t | TyMut t -> type_head_is_int t
  | _ -> false

(* Is [t] an [Array<Int>] (surface `[Int]`, which the parser desugars to
   `Array[Int]`)? Used to seed for-loop variables and recognise indexed
   element reads as integers (#478). *)
let rec type_is_int_array : type_expr -> bool = function
  | TyApp (id, [ TyArg elem ]) -> id.name = "Array" && type_head_is_int elem
  | TyOwn t | TyRef t | TyMut t -> type_is_int_array t
  | _ -> false

(* Simple-variable pattern name, if [pat] binds exactly one name. *)
let pat_var_name : pattern -> string option = function
  | PatVar id -> Some id.name
  | _ -> None

(* Builtins whose return type is unambiguously [Int]. Calls to these count
   as integer operands. (Excludes e.g. [parse_int], which is Option<Int>.) *)
let int_returning_builtins =
  [ "len"; "string_find"; "string_char_code_at"; "char_to_int";
    "string_length" ]

(* Conservative "is this expression provably an [Int]?" Used only to decide
   whether a `/` should truncate; a [false] is always safe (keeps `/`). *)
let rec expr_is_int ctx (e : expr) : bool =
  match e with
  | ExprLit (LitInt _) -> true
  | ExprLit _ -> false
  | ExprSpan (e, _) -> expr_is_int ctx e
  | ExprVar id -> Hashtbl.mem ctx.int_vars id.name
  (* Integer-closed arithmetic: result is [Int] iff both operands are.
     [OpDiv] is included because the emission below makes `Int / Int`
     truncate, so it too yields an [Int]. *)
  | ExprBinary (a, (OpAdd | OpSub | OpMul | OpDiv | OpMod), b) ->
      expr_is_int ctx a && expr_is_int ctx b
  (* JS bitwise operators coerce to a 32-bit integer regardless of input,
     so the result is always an integer. *)
  | ExprBinary (_, (OpBitAnd | OpBitOr | OpBitXor | OpShl | OpShr), _) -> true
  | ExprUnary (OpNeg, e) -> expr_is_int ctx e
  | ExprUnary (OpBitNot, _) -> true
  | ExprIf { ei_then; ei_else = Some e; _ } ->
      expr_is_int ctx ei_then && expr_is_int ctx e
  | ExprApp (ExprVar id, _) ->
      Hashtbl.mem ctx.int_fns id.name
      || List.mem id.name int_returning_builtins
  (* An element read from a provably-[Array<Int>] value is an [Int]
     (covers `xs[i] / 2` where xs: [Int]) — issue #478 finding 2. *)
  | ExprIndex (arr, _) -> expr_is_int_array ctx arr
  | _ -> false

(* Conservative "is this expression provably an [Array<Int>]?" Recognises
   int-array params/locals and array literals whose every element is an
   integer. As with {!expr_is_int}, a [false] is always safe. *)
and expr_is_int_array ctx (e : expr) : bool =
  match e with
  | ExprSpan (e, _) -> expr_is_int_array ctx e
  | ExprVar id -> Hashtbl.mem ctx.int_array_vars id.name
  | ExprArray elems -> elems <> [] && List.for_all (expr_is_int ctx) elems
  | _ -> false

(* Record/forget whether [name] currently holds an [Int], after a binding
   or assignment of [value] to it. Keeps {!ctx.int_vars} in sync as a
   function body is emitted top-to-bottom. *)
let track_int_binding ctx (name : string) (value : expr) : unit =
  if expr_is_int ctx value then Hashtbl.replace ctx.int_vars name ()
  else Hashtbl.remove ctx.int_vars name

(* Reset [int_vars] / [int_array_vars] for a new function body and seed
   them from [Int]- and [Array<Int>]-typed params. Returns [ctx] with the
   fresh tables installed. *)
let enter_fn_scope ctx (params : param list) : codegen_ctx =
  let tbl = Hashtbl.create 16 in
  let arr_tbl = Hashtbl.create 16 in
  List.iter
    (fun (p : param) ->
      if type_head_is_int p.p_ty then Hashtbl.replace tbl p.p_name.name ()
      else if type_is_int_array p.p_ty then
        Hashtbl.replace arr_tbl p.p_name.name ())
    params;
  { ctx with int_vars = tbl; int_array_vars = arr_tbl }

(* ============================================================================
   Expression code generation

   Shape adapted from {!Js_codegen}; the divergences are: variable
   resolution goes through {!resolve_var} (for [self] -> [this]); and a
   call whose head is a known [extern fn] lowers via {!deno_builtins}.
   ============================================================================ *)

let rec gen_expr ctx (expr : expr) : string =
  match expr with
  | ExprLit lit -> gen_literal lit
  | ExprVar name -> resolve_var ctx name.name
  | ExprApp (func, args) ->
      (match func with
       | ExprVar id when Hashtbl.mem ctx.assoc id.name && args <> [] ->
           (* Receiver-first associated call -> method call. *)
           let m = Hashtbl.find ctx.assoc id.name in
           let recv =
             match List.hd args with
             | ExprVar v
               when (match ctx.self_name with Some s -> s = v.name | None -> false) ->
                 "this"
             | other -> "(" ^ gen_expr ctx other ^ ")"
           in
           let rest = List.map (gen_expr ctx) (List.tl args) in
           (* Synthesised methods are all [async]; an associated call is
              an expression sub-term, so await it (valid: it only occurs
              inside the [async] method bodies we emit). *)
           "(await " ^ recv ^ "." ^ m ^ "(" ^ String.concat ", " rest ^ "))"
       | ExprVar id
         when Hashtbl.mem deno_builtins id.name
              && not (Hashtbl.mem ctx.local_fns id.name) ->
           (* Honest host/runtime intrinsic (FS/JSON/Date/Wasm extern or
              a string/number primitive underpinning stdlib/string.affine).
              Applied to ANY matching call head, not only declared externs,
              so AffineScript-level stdlib compiled here resolves — but a
              same-named user definition shadows it (e.g. a user `len`). *)
           (Hashtbl.find deno_builtins id.name) (List.map (gen_expr ctx) args)
       | ExprVar id when Hashtbl.mem ctx.externs id.name ->
           (* Declared extern with no intrinsic lowering: assume a
              same-named host symbol is in scope. *)
           let arg_strs = List.map (gen_expr ctx) args in
           mangle id.name ^ "(" ^ String.concat ", " arg_strs ^ ")"
       | _ ->
           let arg_strs = List.map (gen_expr ctx) args in
           let call =
             gen_expr ctx func ^ "(" ^ String.concat ", " arg_strs ^ ")" in
           (match func with
            | ExprVar id
              when Hashtbl.mem ctx.async_fns id.name && ctx.in_async ->
                (* Async free fn returns a Promise; await at the call
                   site so `get(u).status` reads the resolved value. *)
                "(await " ^ call ^ ")"
            | _ -> call))
  | ExprBinary (e1, OpConcat, e2) ->
      (* `++` is string- OR array-concat; dispatch on shape at runtime so
         `a ++ b` (string) and `acc ++ [x]` (array) are both correct. *)
      "__as_concat(" ^ gen_expr ctx e1 ^ ", " ^ gen_expr ctx e2 ^ ")"
  | ExprBinary (e1, OpDiv, e2) when expr_is_int ctx e1 && expr_is_int ctx e2 ->
      (* Issue #478: integer `/` truncates toward zero; JS `/` is float.
         Both operands are provably [Int] here, so emit a truncating
         divide that matches the interpreter and wasm backends. *)
      "Math.trunc((" ^ gen_expr ctx e1 ^ ") / (" ^ gen_expr ctx e2 ^ "))"
  | ExprBinary (e1, op, e2) ->
      let op_str = match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/"
        | OpMod -> "%" | OpEq -> "===" | OpNe -> "!==" | OpLt -> "<"
        | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">=" | OpAnd -> "&&"
        | OpOr -> "||" | OpBitAnd -> "&" | OpBitOr -> "|" | OpBitXor -> "^"
        | OpShl -> "<<" | OpShr -> ">>" | OpConcat -> "+" (* unreachable *)
      in
      "(" ^ gen_expr ctx e1 ^ " " ^ op_str ^ " " ^ gen_expr ctx e2 ^ ")"
  | ExprUnary (op, e) ->
      (match op with
       | OpNeg    -> "(-" ^ gen_expr ctx e ^ ")"
       | OpNot    -> "(!" ^ gen_expr ctx e ^ ")"
       | OpBitNot -> "(~" ^ gen_expr ctx e ^ ")"
       | OpRef    -> "({ get: () => " ^ gen_expr ctx e ^ ", set: (_) => {} })"
       | OpDeref  -> "(" ^ gen_expr ctx e ^ ".get())")
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let else_str = match ei_else with
        | Some e -> gen_expr ctx e | None -> "Unit"
      in
      "(" ^ gen_expr ctx ei_cond ^ " ? " ^ gen_expr ctx ei_then ^ " : "
      ^ else_str ^ ")"
  | ExprLet { el_pat; el_value; el_body; el_mut; el_quantity = _; el_ty = _ } ->
      let pat_str = gen_pattern ctx el_pat in
      let val_str = gen_expr ctx el_value in
      let kw = if el_mut then "let" else "const" in
      (* `let x = v in body` is lexically scoped, so track [x]'s int-ness
         only for [body] and restore afterward — a leaked binding could
         wrongly truncate a same-named outer Float (#478). *)
      let body_str =
        match el_body with
        | Some body ->
            let restore =
              match pat_var_name el_pat with
              | Some n ->
                  let had = Hashtbl.mem ctx.int_vars n in
                  track_int_binding ctx n el_value;
                  fun () ->
                    if had then Hashtbl.replace ctx.int_vars n ()
                    else Hashtbl.remove ctx.int_vars n
              | None -> fun () -> ()
            in
            let b = gen_expr ctx body in
            restore ();
            kw ^ " " ^ pat_str ^ " = " ^ val_str ^ "; return " ^ b ^ ";"
        | None -> kw ^ " " ^ pat_str ^ " = " ^ val_str ^ "; return Unit;"
      in
      iife ctx body_str
  | ExprTuple exprs | ExprArray exprs ->
      "[" ^ String.concat ", " (List.map (gen_expr ctx) exprs) ^ "]"
  | ExprIndex (arr, idx) ->
      gen_expr ctx arr ^ "[" ^ gen_expr ctx idx ^ "]"
  | ExprTupleIndex (e, n) ->
      gen_expr ctx e ^ "[" ^ string_of_int n ^ "]"
  | ExprRecord { er_fields; er_spread } ->
      let field_strs = List.map (fun (name, e_opt) ->
        let v = match e_opt with
          | Some e -> gen_expr ctx e
          | None   -> resolve_var ctx name.name
        in
        mangle name.name ^ ": " ^ v
      ) er_fields in
      let spread_str = match er_spread with
        | Some e -> "...(" ^ gen_expr ctx e ^ "), " | None -> ""
      in
      "({ " ^ spread_str ^ String.concat ", " field_strs ^ " })"
  | ExprField (record, field) ->
      gen_expr ctx record ^ "." ^ mangle field.name
  | ExprMatch { em_scrutinee; em_arms } ->
      gen_match ctx em_scrutinee em_arms
  | ExprBlock block -> gen_block_expr ctx block
  | ExprReturn (Some e) -> iife ctx ("return " ^ gen_expr ctx e ^ ";")
  | ExprReturn None     -> iife ctx "return Unit;"
  (* #459: break/continue lower to the corresponding JS keywords. The
     wrapping IIFE pattern used for `return` doesn't work here — JS's
     `break`/`continue` only target the nearest enclosing loop and an
     IIFE wraps the keyword in a new function frame. Emit a bare
     statement and rely on the parent block-flatten machinery. *)
  | ExprBreak _    -> iife ctx "break;"
  | ExprContinue _ -> iife ctx "continue;"
  | ExprLambda { elam_params; elam_body; elam_ret_ty = _ } ->
      let ps = List.map (fun (p : param) -> mangle p.p_name.name) elam_params in
      "((" ^ String.concat ", " ps ^ ") => " ^ gen_expr ctx elam_body ^ ")"
  | ExprTry { et_body; et_catch; et_finally } ->
      gen_try ctx et_body et_catch et_finally
  | ExprVariant (ty, ctor) ->
      (match ty.name, ctor.name with
       | _, "None" -> "None" | _, "Some" -> "Some"
       | _, "Ok"   -> "Ok"   | _, "Err"  -> "Err"
       | _, name   -> Printf.sprintf "({ tag: %S })" name)
  | ExprSpan (inner, _) -> gen_expr ctx inner
  | ExprRowRestrict (e, _) -> gen_expr ctx e
  | ExprHandle { eh_body; eh_handlers = _ } -> gen_expr ctx eh_body
  | ExprResume (Some e) -> gen_expr ctx e
  | ExprResume None     -> "Unit"
  | ExprUnsafe _ ->
      iife ctx "throw new Error('unsafe op not supported in Deno-ESM backend');"

and gen_literal (lit : literal) : string =
  match lit with
  | LitInt (n, _)      -> string_of_int n
  | LitFloat (f, _)    ->
      let s = string_of_float f in
      if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _)  -> "true"
  | LitBool (false, _) -> "false"
  | LitString (s, _)   -> Js_codegen.js_string_lit s
  | LitChar (c, _)     -> Js_codegen.js_string_lit (String.make 1 c)
  | LitUnit _          -> "Unit"

and gen_pattern ctx (pat : pattern) : string =
  match pat with
  | PatWildcard _ -> "_"
  | PatVar name   -> mangle name.name
  | PatLit _      -> "_"
  | PatTuple pats ->
      "[" ^ String.concat ", " (List.map (gen_pattern ctx) pats) ^ "]"
  | PatRecord (fields, _) ->
      let strs = List.map (fun (n, sub) ->
        match sub with
        | None     -> mangle n.name
        | Some sub -> mangle n.name ^ ": " ^ gen_pattern ctx sub
      ) fields in
      "{ " ^ String.concat ", " strs ^ " }"
  | PatAs (id, _)  -> mangle id.name
  | PatCon (id, _) -> mangle id.name
  | PatOr (p, _)   -> gen_pattern ctx p

and gen_match ctx scrutinee arms =
  let scrutinee_str = gen_expr ctx scrutinee in
  let scrut_var = "__scrut" in
  let rec gen_arms = function
    | [] -> "throw new Error(\"non-exhaustive match\");"
    | arm :: rest ->
        let cond = gen_pattern_test scrut_var arm.ma_pat in
        let bindings = gen_pattern_bindings scrut_var arm.ma_pat in
        let body = gen_expr ctx arm.ma_body in
        let prefix =
          if bindings = "" then
            let guard = match arm.ma_guard with
              | Some g -> " && (" ^ gen_expr ctx g ^ ")" | None -> ""
            in
            "if (" ^ cond ^ guard ^ ") { return " ^ body ^ "; }"
          else if arm.ma_guard = None then
            "if (" ^ cond ^ ") { " ^ bindings ^ " return " ^ body ^ "; }"
          else
            "if (" ^ cond ^ ") { " ^ bindings ^ " if ("
            ^ (match arm.ma_guard with Some g -> gen_expr ctx g | None -> "true")
            ^ ") { return " ^ body ^ "; } }"
        in
        prefix ^ " " ^ gen_arms rest
  in
  if ctx.in_async then
    "(await (async (" ^ scrut_var ^ ") => { " ^ gen_arms arms ^ " })("
    ^ scrutinee_str ^ "))"
  else
    "((" ^ scrut_var ^ ") => { " ^ gen_arms arms ^ " })(" ^ scrutinee_str ^ ")"

and gen_pattern_test scrut pat =
  match pat with
  | PatWildcard _ | PatVar _ -> "true"
  | PatLit lit -> scrut ^ " === " ^ gen_literal lit
  | PatCon (id, _) -> scrut ^ ".tag === " ^ Printf.sprintf "%S" id.name
  | PatTuple pats ->
      let conds = List.mapi (fun i p ->
        gen_pattern_test (scrut ^ "[" ^ string_of_int i ^ "]") p) pats in
      String.concat " && " (("Array.isArray(" ^ scrut ^ ")") :: conds)
  | PatRecord (fields, _) ->
      let conds = List.map (fun (n, sub) ->
        match sub with
        | None -> "true"
        | Some sub -> gen_pattern_test (scrut ^ "." ^ mangle n.name) sub
      ) fields in
      String.concat " && " conds
  | PatAs (_, p) -> gen_pattern_test scrut p
  | PatOr (p1, p2) ->
      "((" ^ gen_pattern_test scrut p1 ^ ") || ("
      ^ gen_pattern_test scrut p2 ^ "))"

and gen_pattern_bindings scrut pat =
  let buf = Buffer.create 64 in
  let rec walk path = function
    | PatWildcard _ | PatLit _ -> ()
    | PatVar id ->
        Buffer.add_string buf ("const " ^ mangle id.name ^ " = " ^ path ^ "; ")
    | PatTuple pats ->
        List.iteri (fun i p -> walk (path ^ "[" ^ string_of_int i ^ "]") p) pats
    | PatRecord (fields, _) ->
        List.iter (fun (n, sub) ->
          let sub_path = path ^ "." ^ mangle n.name in
          match sub with
          | None -> Buffer.add_string buf
                      ("const " ^ mangle n.name ^ " = " ^ sub_path ^ "; ")
          | Some sub -> walk sub_path sub
        ) fields
    | PatCon (_, args) ->
        (match args with
         | [] -> ()
         | [single] -> walk (path ^ ".value") single
         | many -> List.iteri (fun i p ->
             walk (path ^ ".values[" ^ string_of_int i ^ "]") p) many)
    | PatAs (id, sub) ->
        Buffer.add_string buf ("const " ^ mangle id.name ^ " = " ^ path ^ "; ");
        walk path sub
    | PatOr (p, _) -> walk path p
  in
  walk scrut pat;
  Buffer.contents buf

and gen_block_expr ctx block =
  let body = Buffer.create 64 in
  List.iter (fun s ->
    Buffer.add_string body (gen_stmt ctx s);
    Buffer.add_string body " ") block.blk_stmts;
  let result = match block.blk_expr with
    | Some e -> "return " ^ gen_expr ctx e ^ ";"
    | None   -> "return Unit;"
  in
  iife ctx (Buffer.contents body ^ result)

and gen_try ctx body catch finally =
  let body_str = gen_block_expr ctx body in
  let catch_str = match catch with
    | None | Some [] -> "catch (__e) { throw __e; }"
    | Some (arm :: _) ->
        let bind = match arm.ma_pat with
          | PatVar id -> "const " ^ mangle id.name ^ " = __e; "
          | _ -> ""
        in
        "catch (__e) { " ^ bind ^ "return " ^ gen_expr ctx arm.ma_body ^ "; }"
  in
  let finally_str = match finally with
    | None -> ""
    | Some blk -> " finally { " ^ gen_block_expr ctx blk ^ "; }"
  in
  iife ctx ("try { return " ^ body_str ^ "; } " ^ catch_str ^ finally_str)

(* Unwrap span markers to inspect an expression's real head. *)
and unspan = function
  | ExprSpan (e, _) -> unspan e
  | e -> e

(* Lower an expression in STATEMENT position, where `return` and control
   flow are real JS statements (not IIFE-wrapped — that was the inherited
   js_codegen bug: a statement-position `return e;` became
   `(() => { return e; })();`, discarding the value, so the enclosing
   function returned undefined). [gen_expr] keeps the IIFE form for the
   genuine expression-position cases. *)
and gen_stmt_expr ctx (e : expr) : string =
  match unspan e with
  | ExprReturn (Some e) -> "return " ^ gen_expr ctx e ^ ";"
  | ExprReturn None     -> "return;"
  | ExprBreak _    -> "break;"
  | ExprContinue _ -> "continue;"
  | ExprIf { ei_cond; ei_then; ei_else } ->
      let elseb = match ei_else with
        | Some e -> " else { " ^ gen_branch ctx e ^ " }"
        | None   -> ""
      in
      "if (" ^ gen_expr ctx ei_cond ^ ") { " ^ gen_branch ctx ei_then
      ^ " }" ^ elseb
  | ExprBlock blk -> gen_stmt_seq ctx blk
  | ExprMatch { em_scrutinee; em_arms } ->
      gen_match_stmt ctx em_scrutinee em_arms
  | ExprTry { et_body; et_catch; et_finally } ->
      gen_try_stmt ctx et_body et_catch et_finally
  | other -> gen_expr ctx other ^ ";"

(* An if/try/match branch body: splice a block's statements, else treat
   the expression as a single statement. *)
and gen_branch ctx e =
  match unspan e with
  | ExprBlock blk -> gen_stmt_seq ctx blk
  | other -> gen_stmt_expr ctx other

and gen_stmt_seq ctx blk =
  let b = Buffer.create 64 in
  List.iter (fun s ->
    Buffer.add_string b (gen_stmt ctx s);
    Buffer.add_char b ' ') blk.blk_stmts;
  (match blk.blk_expr with
   | Some e -> Buffer.add_string b (gen_stmt_expr ctx e)
   | None   -> ());
  Buffer.contents b

and gen_match_stmt ctx scrut arms =
  let sv = "__scrut" in
  let rec arms_js = function
    | [] -> "throw new Error(\"non-exhaustive match\");"
    | arm :: rest ->
        let cond = gen_pattern_test sv arm.ma_pat in
        let binds = gen_pattern_bindings sv arm.ma_pat in
        let guard = match arm.ma_guard with
          | Some g -> " && (" ^ gen_expr ctx g ^ ")" | None -> "" in
        "if (" ^ cond ^ guard ^ ") { " ^ binds
        ^ gen_branch ctx arm.ma_body ^ " } else " ^ arms_js rest
  in
  "{ const " ^ sv ^ " = " ^ gen_expr ctx scrut ^ "; " ^ arms_js arms ^ " }"

and gen_try_stmt ctx body catch finally =
  let b = gen_stmt_seq ctx body in
  let c = match catch with
    | None | Some [] -> "catch (__e) { throw __e; }"
    | Some (arm :: _) ->
        let bind = match arm.ma_pat with
          | PatVar id -> "const " ^ mangle id.name ^ " = __e; " | _ -> ""
        in
        "catch (__e) { " ^ bind ^ gen_branch ctx arm.ma_body ^ " }"
  in
  let f = match finally with
    | None -> "" | Some blk -> " finally { " ^ gen_stmt_seq ctx blk ^ " }"
  in
  "try { " ^ b ^ " } " ^ c ^ f

and gen_stmt ctx (stmt : stmt) : string =
  match stmt with
  | StmtLet { sl_pat; sl_value; sl_mut; sl_quantity = _; sl_ty = _ } ->
      let kw = if sl_mut then "let" else "const" in
      let js = kw ^ " " ^ gen_pattern ctx sl_pat ^ " = "
               ^ gen_expr ctx sl_value ^ ";" in
      (* Track an [Int]-bound name for the rest of this body (#478). A
         block statement's scope is the enclosing function, which
         {!enter_fn_scope} already bounds, so no restore is needed here. *)
      (match pat_var_name sl_pat with
       | Some n -> track_int_binding ctx n sl_value
       | None -> ());
      js
  | StmtExpr e -> gen_stmt_expr ctx e
  | StmtAssign (lhs, op, rhs) ->
      (* #478: `x /= y` over integers must truncate, like `x = x / y`. *)
      let div_int =
        op = AssignDiv && expr_is_int ctx lhs && expr_is_int ctx rhs in
      let js =
        if div_int then
          let t = gen_expr ctx lhs in
          t ^ " = Math.trunc(" ^ t ^ " / (" ^ gen_expr ctx rhs ^ "));"
        else
          let op_str = match op with
            | AssignEq -> "=" | AssignAdd -> "+=" | AssignSub -> "-="
            | AssignMul -> "*=" | AssignDiv -> "/="
          in
          gen_expr ctx lhs ^ " " ^ op_str ^ " " ^ gen_expr ctx rhs ^ ";"
      in
      (* Keep int-tracking current across reassignment of a simple var. *)
      (match lhs, op with
       | ExprVar id, AssignEq -> track_int_binding ctx id.name rhs
       | ExprVar id, AssignDiv ->
           if div_int then Hashtbl.replace ctx.int_vars id.name ()
           else Hashtbl.remove ctx.int_vars id.name
       | ExprVar id, (AssignAdd | AssignSub | AssignMul) ->
           if not (expr_is_int ctx rhs) then
             Hashtbl.remove ctx.int_vars id.name
       | _ -> ());
      js
  | StmtWhile (cond, body) ->
      "while (" ^ gen_expr ctx cond ^ ") { "
      ^ String.concat " " (List.map (gen_stmt ctx) body.blk_stmts)
      ^ (match body.blk_expr with
         | Some e -> " " ^ gen_stmt_expr ctx e | None -> "")
      ^ " }"
  | StmtFor (pat, iter, body) ->
      (* The iterable is evaluated in the outer scope, so emit it first. *)
      let iter_str = gen_expr ctx iter in
      let pat_str = gen_pattern ctx pat in
      (* #478: a `for x in xs` over a provably-[Array<Int>] binds [x] to an
         [Int] each iteration, so `x / 2` in the body must truncate. The
         loop variable is loop-scoped, so save/restore its [int_vars] entry
         to avoid leaking onto a same-named outer binding after the loop. *)
      let restore =
        match pat_var_name pat with
        | Some n when expr_is_int_array ctx iter ->
            let had = Hashtbl.mem ctx.int_vars n in
            Hashtbl.replace ctx.int_vars n ();
            fun () ->
              if had then Hashtbl.replace ctx.int_vars n ()
              else Hashtbl.remove ctx.int_vars n
        | _ -> fun () -> ()
      in
      let body_js =
        String.concat " " (List.map (gen_stmt ctx) body.blk_stmts)
        ^ (match body.blk_expr with
           | Some e -> " " ^ gen_stmt_expr ctx e | None -> "")
      in
      restore ();
      "for (const " ^ pat_str ^ " of " ^ iter_str ^ ") { " ^ body_js ^ " }"

(* ============================================================================
   Top-level declarations + class emission
   ============================================================================ *)

let visibility_is_public = function
  | Public | PubCrate | PubSuper | PubIn _ -> true
  | Private -> false

(* Render a method/function body (block or expr) into [ctx]. *)
let gen_body ctx (fb : fn_body) : unit =
  match fb with
  | FnExpr e ->
      emit_line ctx ("return " ^ gen_expr ctx e ^ ";")
  | FnBlock block ->
      List.iter (fun s -> emit_line ctx (gen_stmt ctx s)) block.blk_stmts;
      (match block.blk_expr with
       | Some e -> emit_line ctx ("return " ^ gen_expr ctx e ^ ";")
       | None   -> ())
  | FnExtern -> ()

let gen_function ctx (fd : fn_decl) : unit =
  let name = mangle fd.fd_name.name in
  let params =
    List.map (fun (p : param) -> mangle p.p_name.name) fd.fd_params in
  let is_async = fd_is_async fd in
  let async_kw = if is_async then "async " else "" in
  let kw =
    if visibility_is_public fd.fd_vis
    then "export " ^ async_kw ^ "function"
    else async_kw ^ "function" in
  emit_line ctx
    (Printf.sprintf "%s %s(%s) {" kw name (String.concat ", " params));
  let body_ctx = enter_fn_scope (increase_indent ctx) fd.fd_params in
  let body_ctx =
    if is_async then { body_ctx with in_async = true } else body_ctx in
  gen_body body_ctx fd.fd_body;
  emit_line ctx "}";
  emit ctx "\n"

(* Head name of a (possibly ref/own/mut/applied) type expression. *)
let rec type_expr_name : type_expr -> string option = function
  | TyCon id | TyVar id -> Some id.name
  | TyApp (id, _)       -> Some id.name
  | TyOwn t | TyRef t | TyMut t -> type_expr_name t
  | _ -> None

(* The struct (if any, among [known]) that [fd]'s first parameter is typed
   as — i.e. [fd] is a receiver-first method of that struct. *)
let receiver_struct ~(known : (string, 'a) Hashtbl.t) (fd : fn_decl)
  : (string * string) option =
  match fd.fd_params with
  | p :: _ ->
      (match type_expr_name p.p_ty with
       | Some s when Hashtbl.mem known s -> Some (s, p.p_name.name)
       | _ -> None)
  | [] -> None

(* The struct (if any, among [known]) that [fd] returns — a constructor
   candidate for that struct. *)
let returns_struct ~(known : (string, 'a) Hashtbl.t) (fd : fn_decl)
  : string option =
  match fd.fd_ret_ty with
  | Some t ->
      (match type_expr_name t with
       | Some s when Hashtbl.mem known s -> Some s | _ -> None)
  | None -> None

let ctor_name_hint (n : string) : bool =
  let n = String.lowercase_ascii n in
  List.exists (fun k ->
    let kl = String.length k and nl = String.length n in
    n = k
    || (nl > kl && String.sub n 0 (kl + 1) = k ^ "_")
    || (nl > kl && String.sub n (nl - kl - 1) (kl + 1) = "_" ^ k))
    ["new"; "make"; "create"; "init"; "from"]

(* Emitted method name: strip a leading "<Struct>_" prefix (case-insensitive
   on the struct name) from the source fn name if present. *)
let method_js_name ~(struct_name : string) (fn : string) : string =
  let pfx = struct_name ^ "_" in
  let lc = String.lowercase_ascii in
  if String.length fn > String.length pfx
     && lc (String.sub fn 0 (String.length pfx)) = lc pfx then
    String.sub fn (String.length pfx) (String.length fn - String.length pfx)
  else fn

(* Emit a class method from a receiver-first free function: the receiver
   parameter is dropped from the JS signature and rewritten to [this].
   All methods are emitted [async] — the motivating consumer's surface is
   entirely [async] and callers [await]; [await] on a synchronously
   returned value is valid JS, so this preserves the exact consumer API
   with no async ABI (issue #122 scope; async-extern #103 not required). *)
let gen_method ctx ~(recv_name : string) ~(js_name : string)
    (fd : fn_decl) : unit =
  let rest_params = match fd.fd_params with _ :: t -> t | [] -> [] in
  let params =
    List.map (fun (p : param) -> mangle p.p_name.name) rest_params in
  let ctx_m = { ctx with self_name = Some recv_name; in_async = true } in
  emit_line ctx_m
    (Printf.sprintf "async %s(%s) {" (mangle js_name)
       (String.concat ", " params));
  gen_body (enter_fn_scope (increase_indent ctx_m) fd.fd_params) fd.fd_body;
  emit_line ctx_m "}";
  emit ctx_m ""

(* Constructor from a free function returning the struct: a returned record
   literal becomes [this.f = e;] assignments. *)
let gen_constructor ctx (fd : fn_decl) : unit =
  let params =
    List.map (fun (p : param) -> mangle p.p_name.name) fd.fd_params in
  emit_line ctx
    (Printf.sprintf "constructor(%s) {" (String.concat ", " params));
  let body_ctx = enter_fn_scope (increase_indent ctx) fd.fd_params in
  let rec assign_record e =
    match e with
    | ExprSpan (inner, _) -> assign_record inner
    | ExprRecord { er_fields; er_spread = _ } ->
        List.iter (fun (name, e_opt) ->
          let v = match e_opt with
            | Some e -> gen_expr body_ctx e
            | None   -> resolve_var body_ctx name.name
          in
          emit_line body_ctx
            (Printf.sprintf "this.%s = %s;" (mangle name.name) v)
        ) er_fields;
        true
    | _ -> false
  in
  let handled =
    match fd.fd_body with
    | FnExpr e -> assign_record e
    | FnBlock { blk_stmts; blk_expr = Some e } ->
        List.iter (fun s -> emit_line body_ctx (gen_stmt body_ctx s)) blk_stmts;
        assign_record e
    | _ -> false
  in
  if not handled then gen_body body_ctx fd.fd_body;
  emit_line ctx "}";
  emit ctx ""

(* Synthesised constructor when the struct has methods but no fn returns
   it: one parameter per field, assigned positionally. *)
let gen_default_constructor ctx (fields : struct_field list) : unit =
  let ps = List.map (fun (sf : struct_field) -> mangle sf.sf_name.name) fields in
  emit_line ctx (Printf.sprintf "constructor(%s) {" (String.concat ", " ps));
  let b = increase_indent ctx in
  List.iter (fun (sf : struct_field) ->
    let f = mangle sf.sf_name.name in
    emit_line b (Printf.sprintf "this.%s = %s;" f f)) fields;
  emit_line ctx "}";
  emit ctx ""

(* Emit `export class Name { ... }`. [ctor] is the constructor fn (or a
   field-wise default is synthesised); [methods] are (recv_name, js_name,
   fn) receiver-first associated functions. *)
let gen_class ctx ~(name : string) ~(fields : struct_field list)
    ~(ctor : fn_decl option)
    ~(methods : (string * string * fn_decl) list) : unit =
  emit_line ctx (Printf.sprintf "export class %s {" name);
  let cls = increase_indent ctx in
  (match ctor with
   | Some fd -> gen_constructor cls fd
   | None    -> gen_default_constructor cls fields);
  List.iter (fun (recv_name, js_name, fd) ->
    gen_method cls ~recv_name ~js_name fd) methods;
  emit_line ctx "}";
  emit ctx "\n"

let gen_type_decl ctx (td : type_decl) : unit =
  match td.td_body with
  | TyEnum variants ->
      let exp = if visibility_is_public td.td_vis then "export " else "" in
      List.iter (fun (vd : variant_decl) ->
        let name = mangle vd.vd_name.name in
        let arity = List.length vd.vd_fields in
        if arity = 0 then
          emit_line ctx
            (Printf.sprintf "%sconst %s = { tag: \"%s\" };" exp name
               vd.vd_name.name)
        else if arity = 1 then
          emit_line ctx
            (Printf.sprintf "%sconst %s = (value) => ({ tag: \"%s\", value });"
               exp name vd.vd_name.name)
        else
          let ps = List.init arity (fun i -> "v" ^ string_of_int i) in
          emit_line ctx
            (Printf.sprintf
               "%sconst %s = (%s) => ({ tag: \"%s\", values: [%s] });"
               exp name (String.concat ", " ps) vd.vd_name.name
               (String.concat ", " ps))
      ) variants;
      emit ctx "\n"
  | TyStruct _ | TyAlias _ | TyExtern ->
      (* Struct shape surfaces via {!gen_class} when an impl block exists;
         a bare struct/alias/extern type carries no runtime value. *)
      emit_line ctx (Printf.sprintf "// type %s" td.td_name.name)

let generate (program : program) (symbols : Symbol.t) : string =
  let ctx = create_ctx symbols in
  (* Register extern names so calls lower via the builtin table, and
     user-defined top-level names so they shadow host intrinsics. *)
  List.iter (function
    | TopExternFn { ef_name; _ } ->
        Hashtbl.replace ctx.externs ef_name.name ()
    | TopFn fd when fd.fd_body = FnExtern ->
        Hashtbl.replace ctx.externs fd.fd_name.name ()
    | TopFn fd ->
        Hashtbl.replace ctx.local_fns fd.fd_name.name ();
        if fd_is_async fd then
          Hashtbl.replace ctx.async_fns fd.fd_name.name ();
        (* Record [Int]-returning fns so calls to them count as integer
           operands for the truncating-division lowering (#478). *)
        (match fd.fd_ret_ty with
         | Some t when type_head_is_int t ->
             Hashtbl.replace ctx.int_fns fd.fd_name.name ()
         | _ -> ())
    | TopConst { tc_name; _ } ->
        Hashtbl.replace ctx.local_fns tc_name.name ()
    | TopImpl ib ->
        List.iter (function
          | ImplFn fd ->
              Hashtbl.replace ctx.local_fns fd.fd_name.name ();
              (match fd.fd_ret_ty with
               | Some t when type_head_is_int t ->
                   Hashtbl.replace ctx.int_fns fd.fd_name.name ()
               | _ -> ())
          | ImplType _ -> ()) ib.ib_items
    | _ -> ()) program.prog_decls;

  emit_line ctx "// Generated by AffineScript compiler (Deno-ESM target, issue #122)";
  emit_line ctx "// SPDX-License-Identifier: MPL-2.0";
  emit ctx prelude;

  (* Collect structs. AffineScript's grammar accepts neither inherent
     [impl Type {}] nor a [self] expression (SELF_KW has no expression
     production — even stdlib/traits.affine fails to parse), so an
     "instance method" is necessarily a free function taking the struct
     as its first parameter. We synthesise `export class` from a struct
     plus its receiver-first / struct-returning free functions. *)
  let structs = Hashtbl.create 16 in
  List.iter (function
    | TopType ({ td_body = TyStruct fields; _ } as td) ->
        Hashtbl.replace structs td.td_name.name fields
    | _ -> ()) program.prog_decls;

  (* methods_of : struct -> (recv_name, js_name, fd) list (decl order)
     ctors_of   : struct -> fd list (struct-returning candidates)
     consumed   : fn names emitted inside a class, not as free functions *)
  let methods_of = Hashtbl.create 16 in
  let ctors_of   = Hashtbl.create 16 in
  let consumed   = Hashtbl.create 32 in
  let push tbl k v =
    Hashtbl.replace tbl k (v :: (try Hashtbl.find tbl k with Not_found -> [])) in
  List.iter (function
    | TopFn fd when fd.fd_body <> FnExtern ->
        (match receiver_struct ~known:structs fd with
         | Some (s, rn) ->
             let js = method_js_name ~struct_name:s fd.fd_name.name in
             push methods_of s (rn, js, fd);
             Hashtbl.replace ctx.assoc fd.fd_name.name js;
             Hashtbl.replace consumed fd.fd_name.name ()
         | None ->
             (match returns_struct ~known:structs fd with
              | Some s ->
                  push ctors_of s fd;
                  Hashtbl.replace consumed fd.fd_name.name ()
              | None -> ()))
    | _ -> ()) program.prog_decls;
  let methods_for s =
    List.rev (try Hashtbl.find methods_of s with Not_found -> []) in
  let ctor_for s =
    match (try Hashtbl.find ctors_of s with Not_found -> []) with
    | [] -> None
    | cs ->
        let cs = List.rev cs in
        (match List.find_opt
                 (fun fd -> ctor_name_hint fd.fd_name.name) cs with
         | Some fd -> Some fd
         | None -> Some (List.hd cs))
  in
  let is_class s =
    Hashtbl.mem methods_of s || Hashtbl.mem ctors_of s in

  let emitted_class = Hashtbl.create 16 in
  let emit_class_for s fields =
    if not (Hashtbl.mem emitted_class s) then begin
      gen_class ctx ~name:s ~fields ~ctor:(ctor_for s)
        ~methods:(methods_for s);
      Hashtbl.replace emitted_class s ()
    end
  in
  List.iter (fun top ->
    match top with
    | TopFn fd when fd.fd_body <> FnExtern ->
        if not (Hashtbl.mem consumed fd.fd_name.name) then
          gen_function ctx fd
    | TopFn _ -> ()  (* extern-as-fn: lowered at call sites *)
    | TopExternFn _ | TopExternType _ -> ()  (* lowered at call sites *)
    | TopType td ->
        (match td.td_body with
         | TyStruct fields when is_class td.td_name.name ->
             emit_class_for td.td_name.name fields
         | _ -> gen_type_decl ctx td)
    | TopImpl ib ->
        (* Inherent/trait impls don't parse in the current grammar; this
           is a defensive fallback only. *)
        List.iter (function
          | ImplFn fd -> gen_function ctx fd
          | ImplType _ -> ()) ib.ib_items
    | TopConst { tc_vis; tc_name; tc_value; _ } ->
        let exp = if visibility_is_public tc_vis then "export " else "" in
        emit_line ctx
          (Printf.sprintf "%sconst %s = %s;" exp (mangle tc_name.name)
             (gen_expr ctx tc_value))
    | TopEffect _ -> emit_line ctx "// effect declaration (erased)"
    | TopTrait _  -> emit_line ctx "// trait declaration (erased)"
  ) program.prog_decls;

  (* If a `main` exists, invoke it (await — it may be async-shaped). *)
  let has_main = List.exists (function
    | TopFn fd -> fd.fd_name.name = "main" | _ -> false)
    program.prog_decls in
  if has_main then emit_line ctx "await main();";
  Buffer.contents ctx.output

let codegen_deno (program : program) (symbols : Symbol.t)
  : (string, string) result =
  try Ok (generate program symbols)
  with
  | Failure msg -> Error ("Deno-ESM codegen error: " ^ msg)
  | e           -> Error ("Deno-ESM codegen error: " ^ Printexc.to_string e)
