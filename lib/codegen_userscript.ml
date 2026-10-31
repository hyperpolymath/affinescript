(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Userscript code generator (`--target userscript`, alias `--iife`).

    A userscript is a *single* non-module file that an arbitrary page's
    userscript manager (Tampermonkey / Greasemonkey / Violentmonkey) injects
    into a page. That shape forbids everything an ES module emitter relies
    on: no [import]/[export], no dynamic [import()], no [eval], no
    [new Function], and no top-level [await]. This backend therefore emits

    {v
      (function () { "use strict"; <runtime>; <program>; })();
    v}

    — one IIFE, ES2020 syntax, no module system — plus an optional
    verbatim banner (the [==UserScript==] metadata block) prepended by the
    caller.

    Unlike {!Codegen_deno} (the Bun-ESM host profile, which lowers an
    unknown [extern fn] to an optimistic same-named host call), this target
    is a closed capability surface: only the extern names listed in
    {!extern_lowerings} are lowered, and every other host operation is a
    build error that names the module and the construct
    ({!check_reachable}). That is deliberate: a userscript runs inside
    somebody else's page, so "silently call whatever global happens to be
    named [tea_query]" is exactly the failure mode we must not have.

    Reachability is computed over the module graph the resolver already
    loaded ({!Module_loader}), before import flattening erases provenance —
    that is why the diagnostic can name the module a construct came from.

    Non-goals: no source maps, no minification, no framework bindings, no
    change to any other target's output. *)

open Ast

(* ============================================================================
   Unsupported-construct reporting
   ============================================================================ *)

(** A construct the userscript target cannot honour, and where it lives. *)
type unsupported = {
  u_module : string;  (** module display name, e.g. [Bun] or the source file *)
  u_construct : string;  (** source-level construct, e.g. [extern fn `bun_run`] *)
}

let show_unsupported (u : unsupported) : string =
  Printf.sprintf "%s (in module %s)" u.u_construct u.u_module

(* ============================================================================
   Expression walker: constructs with no userscript lowering
   ============================================================================ *)

(** Report the first construct in [e] that this backend cannot lower.

    Effect handlers ({!ExprHandle}/{!ExprResume}) need a runtime dispatch
    mechanism and [unsafe] memory operations need a linear-memory host; both
    are rejected rather than approximated (the same fail-loudly stance
    {!Js_codegen} takes for handlers, but reported with module provenance
    by {!check_reachable}). *)
let opt_to_list = function Some x -> [ x ] | None -> []

let rec expr_unsupported (e : expr) : string option =
  let first (es : expr list) : string option = List.find_map expr_unsupported es in
  let blocks (bs : block list) : string option = List.find_map block_unsupported bs in
  let arm_list (arms : match_arm list) : expr list =
    List.concat_map
      (fun (a : match_arm) -> a.ma_body :: opt_to_list a.ma_guard) arms
  in
  match e with
  | ExprHandle _ -> Some "effect handler (`handle { ... }`)"
  | ExprResume _ -> Some "`resume` (only valid inside `handle`)"
  | ExprUnsafe _ -> Some "`unsafe` operation (no linear memory in a userscript)"
  | ExprLet { el_value; el_body; _ } -> first (el_value :: opt_to_list el_body)
  | ExprIf { ei_cond; ei_then; ei_else } ->
    first (ei_cond :: ei_then :: opt_to_list ei_else)
  | ExprMatch { em_scrutinee; em_arms } -> first (em_scrutinee :: arm_list em_arms)
  | ExprLambda { elam_body; _ } -> expr_unsupported elam_body
  | ExprApp (f, args) -> first (f :: args)
  | ExprField (r, _) | ExprTupleIndex (r, _) -> expr_unsupported r
  | ExprIndex (a, i) | ExprFloatIndex (a, i) -> first [ a; i ]
  | ExprTuple es | ExprArray es | ExprFloatArray es -> first es
  | ExprRecord { er_fields; er_spread } ->
    first (List.filter_map (fun (_, v) -> v) er_fields @ opt_to_list er_spread)
  | ExprCellRecord fields -> first (List.map (fun (_, e, _) -> e) fields)
  | ExprRowRestrict (e, _) -> expr_unsupported e
  | ExprBinary (a, _, b) | ExprStringConcat (a, b) | ExprStringRel (a, b, _)
  | ExprFloatBinary (a, _, b) | ExprStringEq (a, b, _) -> first [ a; b ]
  | ExprCellTuple cells -> first (List.map fst cells)
  | ExprCellTupleIndex (e, _, _) -> expr_unsupported e
  | ExprCellField _ -> Some "float-bearing record field access"
  | ExprUnary (_, e) -> expr_unsupported e
  | ExprBlock blk -> block_unsupported blk
  | ExprReturn (Some e) -> expr_unsupported e
  | ExprReturn None -> None
  | ExprTry { et_body; et_catch; et_finally } ->
    (match block_unsupported et_body with
     | Some m -> Some m
     | None ->
       (match first (arm_list (Option.value ~default:[] et_catch)) with
        | Some m -> Some m
        | None -> blocks (opt_to_list et_finally)))
  | ExprSpan (e, _) -> expr_unsupported e
  | ExprLit _ | ExprVar _ | ExprVariant _ | ExprBreak _ | ExprContinue _ -> None

and block_unsupported (blk : block) : string option =
  match List.find_map stmt_unsupported blk.blk_stmts with
  | Some m -> Some m
  | None -> (match blk.blk_expr with Some e -> expr_unsupported e | None -> None)

and stmt_unsupported (stmt : stmt) : string option =
  match stmt with
  | StmtLet { sl_value; _ } -> expr_unsupported sl_value
  | StmtExpr e -> expr_unsupported e
  | StmtAssign (a, _, b) ->
    (match expr_unsupported a with Some m -> Some m | None -> expr_unsupported b)
  | StmtWhile (c, body) ->
    (match expr_unsupported c with Some m -> Some m | None -> block_unsupported body)
  | StmtFor (_, iter, body) ->
    (match expr_unsupported iter with Some m -> Some m | None -> block_unsupported body)

(* ============================================================================
   Capability surface: the extern names this target can lower
   ============================================================================ *)

(** Every [extern fn] name this backend knows how to lower, grouped by the
    runtime helper it becomes. Anything not listed here is a build error, so
    adding a binding is a one-line, reviewable act. *)
let extern_lowerings : (string * (string list -> string)) list = [
  (* ── DOM: the host side of `Userscript`'s element constructors ───────── *)
  ("dom_body",            (fun _ -> "__as_domBody()"));
  ("dom_create_element",  (fun a -> Printf.sprintf "__as_domCreate(%s)" (List.nth a 0)));
  ("dom_create_text",     (fun a -> Printf.sprintf "__as_domText(%s)" (List.nth a 0)));
  ("dom_query",           (fun a -> Printf.sprintf "__as_domQuery(%s)" (List.nth a 0)));
  ("dom_append",          (fun a -> Printf.sprintf "(__as_domAppend(%s, %s), Unit)" (List.nth a 0) (List.nth a 1)));
  ("dom_remove",          (fun a -> Printf.sprintf "(__as_domRemove(%s, %s), Unit)" (List.nth a 0) (List.nth a 1)));
  ("dom_set_attribute",   (fun a -> Printf.sprintf "(__as_domAttr(%s, %s, %s), Unit)" (List.nth a 0) (List.nth a 1) (List.nth a 2)));
  ("dom_remove_attribute",(fun a -> Printf.sprintf "(__as_domRemoveAttr(%s, %s), Unit)" (List.nth a 0) (List.nth a 1)));
  ("dom_set_text",        (fun a -> Printf.sprintf "(__as_domTextSet(%s, %s), Unit)" (List.nth a 0) (List.nth a 1)));
  ("dom_add_class",       (fun a -> Printf.sprintf "(__as_domClassAdd(%s, %s), Unit)" (List.nth a 0) (List.nth a 1)));
  ("dom_remove_class",    (fun a -> Printf.sprintf "(__as_domClassRemove(%s, %s), Unit)" (List.nth a 0) (List.nth a 1)));
  ("dom_is_null",         (fun a -> Printf.sprintf "((%s) == null)" (List.nth a 0)));
  (* ── Event listeners (release = `us_off`) ────────────────────────────── *)
  ("us_on_window",        (fun a -> Printf.sprintf "__as_onWindow(%s, %s)" (List.nth a 0) (List.nth a 1)));
  ("us_on_element",       (fun a -> Printf.sprintf "__as_onElement(%s, %s, %s)" (List.nth a 0) (List.nth a 1) (List.nth a 2)));
  ("us_on_window_once",   (fun a -> Printf.sprintf "__as_onWindowOnce(%s, %s)" (List.nth a 0) (List.nth a 1)));
  ("us_on_element_once",  (fun a -> Printf.sprintf "__as_onElementOnce(%s, %s, %s)" (List.nth a 0) (List.nth a 1) (List.nth a 2)));
  ("us_off",              (fun a -> Printf.sprintf "(__as_off(%s), Unit)" (List.nth a 0)));
  (* ── Timers (release = `us_clear_timeout`) ──────────────────────────── *)
  ("us_set_timeout",      (fun a -> Printf.sprintf "__as_setTimeout(%s, %s)" (List.nth a 0) (List.nth a 1)));
  ("us_clear_timeout",    (fun a -> Printf.sprintf "(__as_clearTimeout(%s), Unit)" (List.nth a 0)));
  (* ── Capability probes ──────────────────────────────────────────────── *)
  ("us_gm_has",           (fun a -> Printf.sprintf "__as_gmHas(%s)" (List.nth a 0)));
  ("us_key_valid",        (fun a -> Printf.sprintf "__as_keyOk(%s)" (List.nth a 0)));
  ("us_local_available",  (fun _ -> "__as_localAvailable()"));
  (* ── Manager storage (GM_getValue / GM_setValue / GM_deleteValue) ───── *)
  ("us_gm_get",           (fun a -> Printf.sprintf "__as_gmGet(%s)" (List.nth a 0)));
  ("us_gm_set",           (fun a -> Printf.sprintf "__as_gmSet(%s, %s)" (List.nth a 0) (List.nth a 1)));
  ("us_gm_delete",        (fun a -> Printf.sprintf "__as_gmDelete(%s)" (List.nth a 0)));
  (* ── localStorage fallback ──────────────────────────────────────────── *)
  ("us_local_get",        (fun a -> Printf.sprintf "__as_localGet(%s)" (List.nth a 0)));
  ("us_local_set",        (fun a -> Printf.sprintf "__as_localSet(%s, %s)" (List.nth a 0) (List.nth a 1)));
  ("us_local_delete",     (fun a -> Printf.sprintf "__as_localDelete(%s)" (List.nth a 0)));
  (* ── GM_registerMenuCommand ─────────────────────────────────────────── *)
  ("us_gm_register_menu", (fun a -> Printf.sprintf "__as_gmMenu(%s, %s)" (List.nth a 0) (List.nth a 1)));
  ("us_gm_unregister_menu",(fun a -> Printf.sprintf "__as_gmUnmenu(%s)" (List.nth a 0)));
  (* ── GM_addStyle ────────────────────────────────────────────────────── *)
  ("us_gm_add_style",     (fun a -> Printf.sprintf "__as_gmStyle(%s)" (List.nth a 0)));
  ("us_gm_remove_style",  (fun a -> Printf.sprintf "__as_gmUnstyle(%s)" (List.nth a 0)));
  (* ── GM_notification ────────────────────────────────────────────────── *)
  ("us_gm_notify",        (fun a -> Printf.sprintf "__as_gmNotify(%s, %s)" (List.nth a 0) (List.nth a 1)));
  ("us_gm_close_notification",(fun a -> Printf.sprintf "__as_gmCloseNotify(%s)" (List.nth a 0)));
]

let extern_table : (string, string list -> string) Hashtbl.t =
  let h = Hashtbl.create 64 in
  List.iter (fun (n, f) -> Hashtbl.replace h n f) extern_lowerings;
  h

let is_known_extern (name : string) : bool = Hashtbl.mem extern_table name

(** Declarations this backend refuses, as (module-local) construct strings.
    A [FnExtern] body is [extern fn] in either spelling ([TopExternFn] or a
    bodiless [TopFn]). *)
let decl_unsupported (d : top_level) : string option =
  let extern_name = function
    | Some n when is_known_extern n -> None
    | Some n -> Some (Printf.sprintf "`extern fn %s` (no userscript binding)" n)
    | None -> None
  in
  match d with
  | TopExternFn { ef_name; _ } -> extern_name (Some ef_name.name)
  | TopFn fd when fd.fd_body = FnExtern -> extern_name (Some fd.fd_name.name)
  | TopFn fd ->
    (match fd.fd_body with
     | FnExpr e -> expr_unsupported e
     | FnBlock blk -> block_unsupported blk
     | FnExtern -> None)
  | TopConst { tc_value; _ } -> expr_unsupported tc_value
  | TopImpl ib ->
    List.find_map (function
        | ImplFn fd ->
          (match fd.fd_body with
           | FnExpr e -> expr_unsupported e
           | FnBlock blk -> block_unsupported blk
           | FnExtern -> None)
        | ImplType _ -> None) ib.ib_items
  | TopType _ | TopEffect _ | TopTrait _ | TopExternType _ -> None

let program_unsupported ~(module_name : string) (prog : program) : unsupported list =
  List.filter_map (fun d ->
      match decl_unsupported d with
      | Some c -> Some { u_module = module_name; u_construct = c }
      | None -> None) prog.prog_decls

(* ============================================================================
   Reachability
   ============================================================================ *)

let import_path_names (imp : import_decl) : string list =
  let path = match imp with
    | ImportSimple (p, _) | ImportList (p, _) | ImportGlob p -> p
  in
  List.map (fun (id : ident) -> id.name) path

(** Walk the module graph from [root] through everything the resolver loaded,
    and report every unsupported construct with the module it came from.

    The walk happens *before* [Module_loader.flatten_imports] (which inlines
    bodies and discards provenance), so a diagnostic can say "in module Bun"
    rather than "somewhere in the flattened program". Modules the resolver
    never loaded are reported as unresolved imports so a missing dependency
    cannot silently produce a smaller script. *)
let check_reachable ~(loader : Module_loader.t) ~(root_name : string)
    (root : program) : unsupported list =
  let seen = Hashtbl.create 16 in
  let errors = ref [] in
  let report (origin : string) (prog : program) =
    errors := !errors @ program_unsupported ~module_name:origin prog
  in
  let rec visit (origin : string) (prog : program) =
    if not (Hashtbl.mem seen origin) then begin
      Hashtbl.replace seen origin ();
      report origin prog;
      List.iter (fun imp ->
          let path = import_path_names imp in
          let key = String.concat "." path in
          match Hashtbl.find_opt loader.Module_loader.loaded path with
          | Some lm -> visit key lm.Module_loader.mod_program
          | None -> ()) prog.prog_imports
    end
  in
  visit root_name root;
  !errors

(* ============================================================================
   Emitted-artifact guard
   ============================================================================ *)

(** The module-shaped constructs a userscript must never contain. Checked on
    the finished text as a belt-and-braces invariant: the emitter structurally
    cannot produce them, and a future edit that reintroduces one fails the
    build instead of shipping a file the manager will refuse to run. *)
let forbidden_artifact_lines : (string * (string -> bool)) list = [
  ("import declaration", fun line ->
     let l = String.trim line in
     String.length l >= 7 && String.sub l 0 7 = "import ");
  ("export declaration", fun line ->
     let l = String.trim line in
     String.length l >= 7 && String.sub l 0 7 = "export ");
  ("top-level await", fun line ->
     let l = String.trim line in
     String.length l >= 6 && String.sub l 0 6 = "await ");
  ("dynamic import()", fun line ->
     let l = String.trim line in
     String.length l >= 8 && String.sub l 0 8 = "import(" );
  ("eval()", fun line ->
     let l = String.trim line in
     String.length l >= 5 && String.sub l 0 5 = "eval(");
  ("new Function()", fun line ->
     let l = String.trim line in
     let needle = "new Function" in
     let nl = String.length needle in
     String.length l >= nl && String.sub l 0 nl = needle);
  ("CommonJS require()", fun line ->
     let l = String.trim line in
     let needle = "require(" in
     let nl = String.length needle in
     String.length l >= nl && String.sub l 0 nl = needle);
]

(** Scan [text] line by line for the forbidden constructs above. Only
    *line-leading* forms are matched, which is precise here because the
    emitter puts every statement on its own line and never starts a line
    with a string literal. *)
let find_forbidden (text : string) : string option =
  let lines = String.split_on_char '\n' text in
  List.find_map (fun line ->
      List.find_map (fun (name, pred) -> if pred line then Some name else None)
        forbidden_artifact_lines) lines

let verify_artifact (text : string) : (unit, string) result =
  match find_forbidden text with
  | None -> Ok ()
  | Some construct ->
    Error (Printf.sprintf
             "userscript target: emitted artifact contains a %s, which \
              Tampermonkey/Greasemonkey/Violentmonkey will not run \
              (a userscript is a single non-module file)" construct)

(* ============================================================================
   Runtime prelude
   ============================================================================ *)

(** Always-emitted core: the Option/Result constructors the other JS-family
    backends supply from their preamble, plus the result-shape builders the
    host bindings return. Kept small; everything else is emitted only when a
    reachable [extern fn] needs it. *)
let core_prelude = {|// ---- AffineScript userscript runtime ----------------------------------
const Some = (value) => ({ tag: "Some", value });
const None = { tag: "None" };
const Ok   = (value) => ({ tag: "Ok",  value });
const Err  = (error) => ({ tag: "Err", error });
const Unit = null;
const __as_concat = (a, b) =>
  (typeof a === "string" || typeof b === "string") ? String(a) + String(b) : a.concat(b);
const print   = (s) => { console.log(String(s)); };
const println = (s) => { console.log(String(s)); };
// Every host access below is feature-detected: under `@grant none` the
// manager injects no GM_* globals at all, and guessing would throw.
const __as_has = (name) => {
  try { return typeof globalThis[name] === "function"; } catch (_) { return false; }
};
const __as_msg = (e) => (e && e.message ? String(e.message) : String(e));
// `show` (a typechecker builtin, like `len`) prints strings as-is and
// everything else the way the other JS backends do.
const __as_show = (v) => (typeof v === "string" ? v : JSON.stringify(v));
const __as_readFound   = (v) => ({ found: true, value: v == null ? "" : String(v), error: "" });
const __as_readMissing = () => ({ found: false, value: "", error: "" });
const __as_readError   = (e) => ({ found: false, value: "", error: __as_msg(e) });
const __as_writeOk     = () => ({ ok: true, error: "" });
const __as_writeError  = (e) => ({ ok: false, error: __as_msg(e) });
const __as_handleOk    = (id) => ({ ok: true, id: id, error: "" });
const __as_handleError = (e) => ({ ok: false, id: 0, error: __as_msg(e) });
|}

(** Prelude sections keyed by the extern names that need them. A section is
    emitted only when the program (or a module it inlines) calls one of its
    externs, so an unrelated script does not pay for the GM surface. *)
let prelude_sections : (string list * string) list = [
  ([ "dom_body"; "dom_create_element"; "dom_create_text"; "dom_query";
     "dom_append"; "dom_remove"; "dom_set_attribute"; "dom_remove_attribute";
     "dom_set_text"; "dom_add_class"; "dom_remove_class"; "dom_is_null" ],
   {|// ---- DOM host (returns null instead of throwing when there is no document)
const __as_doc = () => { try { return globalThis.document || null; } catch (_) { return null; } };
const __as_domBody = () => { const d = __as_doc(); return d ? d.body : null; };
const __as_domCreate = (tag) => { const d = __as_doc(); return d ? d.createElement(String(tag)) : null; };
const __as_domText = (s) => { const d = __as_doc(); return d ? d.createTextNode(String(s)) : null; };
const __as_domQuery = (sel) => {
  const d = __as_doc();
  if (!d) return null;
  try { return d.querySelector(String(sel)); } catch (_) { return null; }
};
const __as_domAppend = (parent, child) => {
  if (parent && child && typeof parent.appendChild === "function") parent.appendChild(child);
  return 0;
};
const __as_domRemove = (parent, child) => {
  try { if (parent && child && typeof parent.removeChild === "function") parent.removeChild(child); }
  catch (_) { /* already detached */ }
  return 0;
};
const __as_domAttr = (el, name, value) => {
  if (el && typeof el.setAttribute === "function") el.setAttribute(String(name), String(value));
  return 0;
};
const __as_domRemoveAttr = (el, name) => {
  if (el && typeof el.removeAttribute === "function") el.removeAttribute(String(name));
  return 0;
};
const __as_domTextSet = (node, text) => { if (node) node.textContent = String(text); return 0; };
const __as_domClassAdd = (el, cls) => { if (el && el.classList) el.classList.add(String(cls)); return 0; };
const __as_domClassRemove = (el, cls) => {
  if (el && el.classList) el.classList.remove(String(cls));
  return 0;
};
|});
  ([ "us_on_window"; "us_on_element"; "us_on_window_once"; "us_on_element_once"; "us_off" ],
   {|// ---- Event listeners: one registry, released by handle (`us_off`)
const __as_listeners = new Map();
let __as_listenSeq = 0;
const __as_onTarget = (target, event, handler) => {
  if (!target || typeof target.addEventListener !== "function") return 0;
  const name = String(event);
  const wrapped = () => {
    try { handler(); } catch (e) { console.error("[affinescript] listener failed", e); }
  };
  target.addEventListener(name, wrapped, false);
  const id = ++__as_listenSeq;
  __as_listeners.set(id, { target: target, event: name, handler: wrapped });
  return id;
};
const __as_onWindow = (event, handler) => __as_onTarget(globalThis, event, handler);
const __as_onElement = (el, event, handler) => __as_onTarget(el, event, handler);
// A *once* listener releases itself before the handler runs, so a script can
// react to a one-shot page event without owning a handle for it.
const __as_onTargetOnce = (target, event, handler) => {
  if (!target || typeof target.addEventListener !== "function") return 0;
  const name = String(event);
  let id = 0;
  const wrapped = () => {
    if (id) __as_off(id);
    try { handler(); } catch (e) { console.error("[affinescript] listener failed", e); }
  };
  target.addEventListener(name, wrapped, false);
  id = ++__as_listenSeq;
  __as_listeners.set(id, { target: target, event: name, handler: wrapped });
  return id;
};
const __as_onWindowOnce = (event, handler) => __as_onTargetOnce(globalThis, event, handler);
const __as_onElementOnce = (el, event, handler) => __as_onTargetOnce(el, event, handler);
const __as_off = (id) => {
  const key = Number(id);
  const entry = __as_listeners.get(key);
  if (!entry) return 0;
  __as_listeners.delete(key);
  try { entry.target.removeEventListener(entry.event, entry.handler, false); }
  catch (_) { /* target vanished with its document */ }
  return 1;
};
|});
  ([ "us_set_timeout"; "us_clear_timeout" ],
   {|// ---- Timers
const __as_setTimeout = (handler, ms) => {
  if (typeof globalThis.setTimeout !== "function") return 0;
  return Number(globalThis.setTimeout(() => {
    try { handler(); } catch (e) { console.error("[affinescript] timer failed", e); }
  }, Number(ms)));
};
const __as_clearTimeout = (id) => {
  if (typeof globalThis.clearTimeout === "function") globalThis.clearTimeout(Number(id));
  return 0;
};
|});
  ([ "us_local_available"; "us_local_get"; "us_local_set"; "us_local_delete" ],
   {|// ---- localStorage fallback (the `@grant none` path). Access can throw on
// an opaque origin, so every call is wrapped and reports a typed error.
const __as_localStore = () => { try { return globalThis.localStorage || null; } catch (_) { return null; } };
const __as_localAvailable = () => {
  const store = __as_localStore();
  if (!store) return false;
  try {
    const probe = "__affinescript_userscript_probe__";
    store.setItem(probe, "1");
    store.removeItem(probe);
    return true;
  } catch (_) { return false; }
};
const __as_localGet = (key) => {
  const store = __as_localStore();
  if (!store) return __as_readError("localStorage is not available");
  try {
    const value = store.getItem(String(key));
    return value === null ? __as_readMissing() : __as_readFound(value);
  } catch (e) { return __as_readError(e); }
};
const __as_localSet = (key, value) => {
  const store = __as_localStore();
  if (!store) return __as_writeError("localStorage is not available");
  try { store.setItem(String(key), String(value)); return __as_writeOk(); }
  catch (e) { return __as_writeError(e); }
};
const __as_localDelete = (key) => {
  const store = __as_localStore();
  if (!store) return __as_writeError("localStorage is not available");
  try { store.removeItem(String(key)); return __as_writeOk(); }
  catch (e) { return __as_writeError(e); }
};
|});
  ([ "us_key_valid" ],
   {|// Setting keys are validated here so every storage backend agrees on what a
// key is: non-empty, <= 128 chars, no control characters.
const __as_keyOk = (key) => {
  const k = String(key);
  if (k.length === 0 || k.length > 128) return false;
  for (let i = 0; i < k.length; i += 1) {
    const c = k.charCodeAt(i);
    if (c < 0x20 || c === 0x7f) return false;
  }
  return true;
};
|});
  ([ "us_gm_has"; "us_gm_get"; "us_gm_set"; "us_gm_delete" ],
   {|// ---- GM_getValue / GM_setValue / GM_deleteValue
const __as_gmHas = (suffix) => __as_has("GM_" + String(suffix));
const __as_gmGet = (key) => {
  if (!__as_has("GM_getValue")) return __as_readError("GM_getValue is not available");
  try {
    const value = globalThis.GM_getValue(String(key), undefined);
    if (value === undefined || value === null) return __as_readMissing();
    return __as_readFound(typeof value === "string" ? value : JSON.stringify(value));
  } catch (e) { return __as_readError(e); }
};
const __as_gmSet = (key, value) => {
  if (!__as_has("GM_setValue")) return __as_writeError("GM_setValue is not available");
  try { globalThis.GM_setValue(String(key), String(value)); return __as_writeOk(); }
  catch (e) { return __as_writeError(e); }
};
const __as_gmDelete = (key) => {
  if (!__as_has("GM_deleteValue")) return __as_writeError("GM_deleteValue is not available");
  try { globalThis.GM_deleteValue(String(key)); return __as_writeOk(); }
  catch (e) { return __as_writeError(e); }
};
|});
  ([ "us_gm_register_menu"; "us_gm_unregister_menu" ],
   {|// ---- GM_registerMenuCommand (released by handle via GM_unregisterMenuCommand)
const __as_menu = new Map();
let __as_menuSeq = 0;
const __as_gmMenu = (label, handler) => {
  if (!__as_has("GM_registerMenuCommand")) {
    return __as_handleError("GM_registerMenuCommand is not available");
  }
  try {
    const id = ++__as_menuSeq;
    const wrapped = () => {
      try { handler(); } catch (e) { console.error("[affinescript] menu command failed", e); }
    };
    const native = globalThis.GM_registerMenuCommand(String(label), wrapped);
    __as_menu.set(id, { native: native === undefined ? id : native, wrapped: wrapped });
    return __as_handleOk(id);
  } catch (e) { return __as_handleError(e); }
};
const __as_gmUnmenu = (id) => {
  const key = Number(id);
  const entry = __as_menu.get(key);
  if (!entry) return __as_writeError("unknown menu-command handle " + key);
  __as_menu.delete(key);
  if (!__as_has("GM_unregisterMenuCommand")) {
    return __as_writeError("GM_unregisterMenuCommand is not available");
  }
  try { globalThis.GM_unregisterMenuCommand(entry.native); return __as_writeOk(); }
  catch (e) { return __as_writeError(e); }
};
|});
  ([ "us_gm_add_style"; "us_gm_remove_style" ],
   {|// ---- GM_addStyle (released by handle: the injected <style> is removed)
const __as_styles = new Map();
let __as_styleSeq = 0;
const __as_gmStyle = (css) => {
  if (!__as_has("GM_addStyle")) return __as_handleError("GM_addStyle is not available");
  try {
    const el = globalThis.GM_addStyle(String(css));
    const id = ++__as_styleSeq;
    __as_styles.set(id, el);
    return __as_handleOk(id);
  } catch (e) { return __as_handleError(e); }
};
const __as_gmUnstyle = (id) => {
  const key = Number(id);
  if (!__as_styles.has(key)) return __as_writeError("unknown style handle " + key);
  const el = __as_styles.get(key);
  __as_styles.delete(key);
  try {
    if (__as_has("GM_removeStyle")) globalThis.GM_removeStyle(el);
    else if (el && el.parentNode) el.parentNode.removeChild(el);
    return __as_writeOk();
  } catch (e) { return __as_writeError(e); }
};
|});
  ([ "us_gm_notify"; "us_gm_close_notification" ],
   {|// ---- GM_notification (released by handle where the manager allows it)
const __as_notifications = new Map();
let __as_notifySeq = 0;
const __as_gmNotify = (text, title) => {
  if (!__as_has("GM_notification")) return __as_handleError("GM_notification is not available");
  try {
    const control = globalThis.GM_notification(String(text), String(title));
    const id = ++__as_notifySeq;
    __as_notifications.set(id, control);
    return __as_handleOk(id);
  } catch (e) { return __as_handleError(e); }
};
const __as_gmCloseNotify = (id) => {
  const key = Number(id);
  if (!__as_notifications.has(key)) return __as_writeError("unknown notification handle " + key);
  const control = __as_notifications.get(key);
  __as_notifications.delete(key);
  try {
    if (control && typeof control.close === "function") { control.close(); return __as_writeOk(); }
    if (__as_has("GM_closeNotification")) { globalThis.GM_closeNotification(key); return __as_writeOk(); }
    return __as_writeError("the manager exposes no notification-close capability");
  } catch (e) { return __as_writeError(e); }
};
|});
]

(** Every identifier the program mentions (over-approximation: local
    binders included). Used for two things — deciding which runtime
    sections to emit, and computing the top-level call graph for
    entry-reachability pruning. Over-approximating keeps too much, never
    too little, which is the safe direction for both. *)
let program_names (prog : program) : (string, unit) Hashtbl.t =
  let names = Hashtbl.create 256 in
  let add n = Hashtbl.replace names n () in
  let rec walk (e : expr) =
    match e with
    | ExprVar id -> add id.name
    | ExprVariant (_, ctor) -> add ctor.name
    | ExprApp (f, args) -> walk f; List.iter walk args
    | ExprLet { el_value; el_body; _ } ->
      walk el_value;
      (match el_body with Some b -> walk b | None -> ())
    | ExprIf { ei_cond; ei_then; ei_else } ->
      walk ei_cond; walk ei_then;
      (match ei_else with Some e -> walk e | None -> ())
    | ExprMatch { em_scrutinee; em_arms } ->
      walk em_scrutinee;
      List.iter (fun (a : match_arm) ->
          walk a.ma_body;
          match a.ma_guard with Some g -> walk g | None -> ()) em_arms
    | ExprLambda { elam_body; _ } -> walk elam_body
    | ExprField (r, _) | ExprTupleIndex (r, _) | ExprRowRestrict (r, _) -> walk r
    | ExprIndex (a, b) | ExprStringConcat (a, b) | ExprFloatIndex (a, b)
    | ExprStringEq (a, b, _) | ExprStringRel (a, b, _) -> walk a; walk b
    | ExprTuple es | ExprArray es | ExprFloatArray es -> List.iter walk es
    | ExprRecord { er_fields; er_spread } ->
      List.iter (fun (_, v) -> match v with Some e -> walk e | None -> ()) er_fields;
      (match er_spread with Some e -> walk e | None -> ())
    | ExprBinary (a, _, b) | ExprFloatBinary (a, _, b) -> walk a; walk b
    | ExprCellTuple cells -> List.iter (fun (e, _) -> walk e) cells
    | ExprCellTupleIndex (e, _, _) | ExprCellField (e, _, _) -> walk e
    | ExprCellRecord fields -> List.iter (fun (_, e, _) -> walk e) fields
    | ExprUnary (_, e) | ExprReturn (Some e) | ExprSpan (e, _) -> walk e
    | ExprBlock blk -> walk_block blk
    | ExprTry { et_body; et_catch; et_finally } ->
      walk_block et_body;
      List.iter (fun (a : match_arm) -> walk a.ma_body)
        (Option.value ~default:[] et_catch);
      (match et_finally with Some b -> walk_block b | None -> ())
    | ExprHandle { eh_body; eh_handlers } ->
      walk eh_body;
      List.iter (function
          | HandlerReturn (_, e) -> walk e
          | HandlerOp (_, _, e) -> walk e) eh_handlers
    | ExprResume e -> (match e with Some e -> walk e | None -> ())
    | ExprUnsafe ops ->
      List.iter (function
          | UnsafeRead e | UnsafeForget e -> walk e
          | UnsafeWrite (a, b) | UnsafeOffset (a, b) -> walk a; walk b
          | UnsafeTransmute (_, _, e) -> walk e) ops
    | ExprLit _ | ExprReturn None | ExprBreak _ | ExprContinue _ -> ()
  and walk_block (blk : block) =
    List.iter walk_stmt blk.blk_stmts;
    (match blk.blk_expr with Some e -> walk e | None -> ())
  and walk_stmt (s : stmt) =
    match s with
    | StmtLet { sl_value; _ } -> walk sl_value
    | StmtExpr e -> walk e
    | StmtAssign (a, _, b) -> walk a; walk b
    | StmtWhile (c, b) -> walk c; walk_block b
    | StmtFor (_, it, b) -> walk it; walk_block b
  in
  let walk_decl (d : top_level) =
    match d with
    | TopFn fd ->
      (match fd.fd_body with
       | FnExpr e -> walk e
       | FnBlock b -> walk_block b
       | FnExtern -> ())
    | TopConst { tc_value; _ } -> walk tc_value
    | TopImpl ib ->
      List.iter (function
          | ImplFn fd ->
            (match fd.fd_body with
             | FnExpr e -> walk e
             | FnBlock b -> walk_block b
             | FnExtern -> ())
          | ImplType _ -> ()) ib.ib_items
    | _ -> ()
  in
  List.iter walk_decl prog.prog_decls;
  names

(** Entry-reachability pruning.

    A userscript has exactly one entry point, [main]; a program without one is
    emitted whole. Everything a manager-injected page script carries is paid
    for on every page load, and `use prelude::*` alone would otherwise inline
    the whole prelude (issue: glob imports are inlined by
    [Module_loader.flatten_imports]). So top-level [fn]s that [main] cannot
    reach are dropped. Top-level [const]s and type declarations are always
    kept: a const initialiser can have host effects, and enum constructors are
    referenced from pattern tests.

    This is reachability, not minification: bindings are neither renamed nor
    reformatted, and the output stays a faithful one-to-one rendering of the
    surviving declarations. *)
let prune_to_entry (prog : program) : program =
  let has_main =
    List.exists (function
        | TopFn fd -> fd.fd_name.name = "main"
        | _ -> false) prog.prog_decls
  in
  if not has_main then prog
  else begin
    (* name -> body, for every top-level function *)
    let bodies = Hashtbl.create 64 in
    List.iter (function
        | TopFn fd when fd.fd_body <> FnExtern ->
          Hashtbl.replace bodies fd.fd_name.name fd
        | _ -> ()) prog.prog_decls;
    let keep = Hashtbl.create 64 in
    let rec visit name =
      if Hashtbl.mem bodies name && not (Hashtbl.mem keep name) then begin
        Hashtbl.replace keep name ();
        let fd = Hashtbl.find bodies name in
        let refs = program_names { prog with prog_decls = [ TopFn fd ] } in
        Hashtbl.iter (fun r () -> if r <> name then visit r) refs
      end
    in
    visit "main";
    { prog with
      prog_decls =
        List.filter (function
            | TopFn fd when fd.fd_body <> FnExtern ->
              Hashtbl.mem keep fd.fd_name.name
            | _ -> true) prog.prog_decls }
  end

(** Extern names the program mentions. *)
let used_externs (prog : program) : (string, unit) Hashtbl.t =
  let all = program_names prog in
  let used = Hashtbl.create 16 in
  Hashtbl.iter (fun name () ->
      if is_known_extern name then Hashtbl.replace used name ()) all;
  used

(** Runtime prelude for a program: the core plus every section whose
    externs it calls, in a fixed order. *)
let prelude_for (prog : program) : string =
  let used = used_externs prog in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf core_prelude;
  List.iter (fun (names, js) ->
      if List.exists (fun n -> Hashtbl.mem used n) names then
        Buffer.add_string buf js) prelude_sections;
  Buffer.contents buf

(* ============================================================================
   Code generation context
   ============================================================================ *)

type ctx = {
  output : Buffer.t;                (** top-level output *)
  symbols : Symbol.t;
  idc : Int_div_classifier.state;
}

let create_ctx symbols = { output = Buffer.create 4096; symbols; idc = Int_div_classifier.create () }

let emit ctx str = Buffer.add_string ctx.output str
let emit_line ctx str = emit ctx (str ^ "\n")

(* ============================================================================
   Name mangling
   ============================================================================ *)

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

(** UTF-8 -> JS string literal with [\uXXXX] escapes: identical to
    {!Js_codegen.js_string_lit} (octal-escape avoidance, #460). *)
let js_string_lit (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  Buffer.add_char buf '"';
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let b0 = Char.code s.[!i] in
    if b0 < 0x80 then begin
      (match Char.chr b0 with
       | '\\' -> Buffer.add_string buf "\\\\"
       | '"'  -> Buffer.add_string buf "\\\""
       | '\n' -> Buffer.add_string buf "\\n"
       | '\r' -> Buffer.add_string buf "\\r"
       | '\t' -> Buffer.add_string buf "\\t"
       | c when b0 >= 0x20 && b0 <= 0x7E -> Buffer.add_char buf c
       | _ -> Buffer.add_string buf (Printf.sprintf "\\x%02X" b0));
      incr i
    end else begin
      let cp, len =
        if b0 < 0xC0 then (b0, 1)
        else if b0 < 0xE0 && !i + 1 < n then
          let b1 = Char.code s.[!i + 1] in
          (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F), 2)
        else if b0 < 0xF0 && !i + 2 < n then
          let b1 = Char.code s.[!i + 1] in
          let b2 = Char.code s.[!i + 2] in
          (((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F), 3)
        else if !i + 3 < n then
          let b1 = Char.code s.[!i + 1] in
          let b2 = Char.code s.[!i + 2] in
          let b3 = Char.code s.[!i + 3] in
          (((b0 land 0x07) lsl 18) lor ((b1 land 0x3F) lsl 12)
           lor ((b2 land 0x3F) lsl 6) lor (b3 land 0x3F), 4)
        else (b0, 1)
      in
      if cp <= 0xFFFF then Buffer.add_string buf (Printf.sprintf "\\u%04X" cp)
      else Buffer.add_string buf (Printf.sprintf "\\u{%X}" cp);
      i := !i + len
    end
  done;
  Buffer.add_char buf '"';
  Buffer.contents buf

let preamble_ctors = [ "Some"; "None"; "Ok"; "Err" ]

(* ============================================================================
   Patterns
   ============================================================================ *)

let rec gen_pattern ctx (pat : pattern) : string =
  match pat with
  | PatWildcard _ -> "_"
  | PatVar name -> mangle name.name
  | PatLit _ -> "_"
  | PatTuple pats -> "[" ^ String.concat ", " (List.map (gen_pattern ctx) pats) ^ "]"
  | PatRecord (fields, _) ->
    let strs = List.map (fun (n, sub) ->
        match sub with
        | None -> mangle n.name
        | Some sub -> mangle n.name ^ ": " ^ gen_pattern ctx sub) fields
    in
    "{ " ^ String.concat ", " strs ^ " }"
  | PatAs (id, _) -> mangle id.name
  | PatCon (id, _) -> mangle id.name
  | PatOr (p, _) -> gen_pattern ctx p

and gen_literal (lit : literal) : string =
  match lit with
  | LitInt (n, _) -> string_of_int n
  | LitFloat (f, _) ->
    let s = string_of_float f in
    if String.length s > 0 && s.[String.length s - 1] = '.' then s ^ "0" else s
  | LitBool (true, _) -> "true"
  | LitBool (false, _) -> "false"
  | LitString (s, _) -> js_string_lit s
  | LitChar (c, _) -> js_string_lit (String.make 1 c)
  | LitUnit _ -> "Unit"

(* ============================================================================
   Expressions
   ============================================================================ *)

(** Field carrying a one-payload constructor's value. The prelude's [Err]
    stores its payload under [error] (`const Err = (error) => ({tag, error})`),
    every other single-payload constructor — [Ok], [Some] and user enums —
    under [value]. Getting this wrong does not fail loudly: it hands the arm
    binder [undefined] and the match then dies on the next `.tag` read. *)
let payload_field (id : ident) : string =
  match id.name with
  | "Err" -> "error"
  | _ -> "value"

let rec gen_expr ctx (expr : expr) : string =
  match expr with
  | ExprLit lit -> gen_literal lit
  | ExprVar name -> mangle name.name
  | ExprApp (func, args) ->
    let arg_strs = List.map (gen_expr ctx) args in
    (match func with
     | ExprVar id when id.name = "len" && List.length arg_strs = 1 ->
       (* `len` is a typechecker builtin rather than a declared extern
          (lib/typecheck.ml binds it as a scheme), so it reaches this backend
          with no declaration of its own to lower. `.length` is right for both
          strings and arrays, which is what the other JS backends do. A program
          that declares its own `len` shadows this, as it does everywhere
          else. *)
       "(" ^ List.hd arg_strs ^ ").length"
     | ExprVar id when id.name = "show" && List.length arg_strs = 1 ->
       (* `show` is the other typechecker builtin with no extern declaration
          to lower through. The JS backends spell it `__as_show`, which the
          runtime defines below; keep the same shape so a value prints the
          same way under every JS target. *)
       "__as_show(" ^ List.hd arg_strs ^ ")"
     | ExprVar id when is_known_extern id.name ->
       (match Hashtbl.find_opt extern_table id.name with
        | Some lower -> lower arg_strs
        | None ->
          failwith (Printf.sprintf
                      "userscript target: no lowering for extern `%s`" id.name))
     | _ -> gen_expr ctx func ^ "(" ^ String.concat ", " arg_strs ^ ")")
  | ExprBinary (e1, OpDiv, e2)
    when Int_div_classifier.expr_is_int ctx.idc e1
      && Int_div_classifier.expr_is_int ctx.idc e2 ->
    "Math.trunc((" ^ gen_expr ctx e1 ^ ") / (" ^ gen_expr ctx e2 ^ "))"
  | ExprBinary (e1, op, e2) ->
    let op_str = match op with
      | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%"
      | OpEq -> "===" | OpNe -> "!==" | OpLt -> "<" | OpLe -> "<="
      | OpGt -> ">" | OpGe -> ">=" | OpAnd -> "&&" | OpOr -> "||"
      | OpBitAnd -> "&" | OpBitOr -> "|" | OpBitXor -> "^"
      | OpShl -> "<<" | OpShr -> ">>"
      | OpConcat -> ""
    in
    if op = OpConcat then
      (* `++` is overloaded: String concatenation and array concatenation.
         JS `+` would stringify arrays, so dispatch on the runtime type. *)
      "__as_concat(" ^ gen_expr ctx e1 ^ ", " ^ gen_expr ctx e2 ^ ")"
    else
      "(" ^ gen_expr ctx e1 ^ " " ^ op_str ^ " " ^ gen_expr ctx e2 ^ ")"
  | ExprStringConcat (a, b) | ExprFloatBinary (a, OpAdd, b) ->
    "(" ^ gen_expr ctx a ^ " + " ^ gen_expr ctx b ^ ")"
  | ExprStringEq (a, b, negated) ->
    "(" ^ gen_expr ctx a ^ (if negated then " !== " else " === ") ^ gen_expr ctx b ^ ")"
  | ExprStringRel (a, b, op) | ExprFloatBinary (a, op, b) ->
    let op_str = match op with
      | OpLt -> "<" | OpLe -> "<=" | OpGt -> ">" | OpGe -> ">="
      | OpEq -> "===" | OpNe -> "!==" | OpAdd -> "+" | OpSub -> "-"
      | OpMul -> "*" | OpDiv -> "/" | OpMod -> "%" | OpAnd -> "&&"
      | OpOr -> "||" | OpConcat -> "+" | OpBitAnd -> "&" | OpBitOr -> "|"
      | OpBitXor -> "^" | OpShl -> "<<" | OpShr -> ">>"
    in
    "(" ^ gen_expr ctx a ^ " " ^ op_str ^ " " ^ gen_expr ctx b ^ ")"
  | ExprUnary (op, e) ->
    (match op with
     | OpNeg -> "(-" ^ gen_expr ctx e ^ ")"
     | OpNot -> "(!" ^ gen_expr ctx e ^ ")"
     | OpBitNot -> "(~" ^ gen_expr ctx e ^ ")"
     | OpRef | OpMutRef -> gen_expr ctx e  (* references are erased at runtime *)
     | OpDeref -> gen_expr ctx e)
  | ExprIf { ei_cond; ei_then; ei_else } ->
    let else_str = match ei_else with
      | Some e -> gen_expr ctx e
      | None -> "Unit"
    in
    "(" ^ gen_expr ctx ei_cond ^ " ? " ^ gen_expr ctx ei_then ^ " : " ^ else_str ^ ")"
  | ExprLet { el_pat; el_value; el_body; el_mut; _ } ->
    let kw = if el_mut then "let" else "const" in
    let bind = kw ^ " " ^ gen_pattern ctx el_pat ^ " = " ^ gen_expr ctx el_value ^ "; " in
    (match el_body with
     | Some body -> "((() => { " ^ bind ^ "return " ^ gen_expr ctx body ^ "; })())"
     | None -> "((() => { " ^ bind ^ "return Unit; })())")
  | ExprTuple exprs | ExprArray exprs | ExprFloatArray exprs ->
    "[" ^ String.concat ", " (List.map (gen_expr ctx) exprs) ^ "]"
  | ExprCellTuple cells ->
    "[" ^ String.concat ", " (List.map (fun (e, _) -> gen_expr ctx e) cells) ^ "]"
  | ExprIndex (arr, idx) | ExprFloatIndex (arr, idx) ->
    gen_expr ctx arr ^ "[" ^ gen_expr ctx idx ^ "]"
  | ExprTupleIndex (e, n) | ExprCellTupleIndex (e, n, _) ->
    gen_expr ctx e ^ "[" ^ string_of_int n ^ "]"
  | ExprRecord { er_fields; er_spread } ->
    let field_strs = List.map (fun (name, e_opt) ->
        let val_str = match e_opt with
          | Some e -> gen_expr ctx e
          | None -> mangle name.name
        in
        mangle name.name ^ ": " ^ val_str) er_fields
    in
    let spread_str = match er_spread with
      | Some e -> "...(" ^ gen_expr ctx e ^ "), "
      | None -> ""
    in
    "({ " ^ spread_str ^ String.concat ", " field_strs ^ " })"
  | ExprCellRecord fields ->
    let field_strs = List.map (fun (name, e, _) -> mangle name.name ^ ": " ^ gen_expr ctx e) fields in
    "({ " ^ String.concat ", " field_strs ^ " })"
  | ExprField (record, field) -> gen_expr ctx record ^ "." ^ mangle field.name
  | ExprCellField _ ->
    failwith "userscript target: float-cell record field has no lowering"
  | ExprMatch { em_scrutinee; em_arms } -> gen_match ctx em_scrutinee em_arms
  | ExprBlock block -> gen_block_expr ctx block
  | ExprReturn (Some e) -> "(() => { return " ^ gen_expr ctx e ^ "; })()"
  | ExprReturn None -> "(() => { return Unit; })()"
  | ExprBreak _ ->
    failwith "userscript target: `break` in expression position has no lowering"
  | ExprContinue _ ->
    failwith "userscript target: `continue` in expression position has no lowering"
  | ExprLambda { elam_params; elam_body; _ } ->
    let params = List.map (fun (p : param) -> mangle p.p_name.name) elam_params in
    "((" ^ String.concat ", " params ^ ") => " ^ gen_expr ctx elam_body ^ ")"
  | ExprTry { et_body; et_catch; et_finally } -> gen_try ctx et_body et_catch et_finally
  | ExprVariant (_, ctor) -> mangle ctor.name
  | ExprSpan (inner, _) -> gen_expr ctx inner
  | ExprRowRestrict (e, _) -> gen_expr ctx e
  | ExprHandle _ ->
    failwith "userscript target: effect handler (`handle`) has no lowering"
  | ExprResume _ ->
    failwith "userscript target: `resume` has no lowering"
  | ExprUnsafe _ ->
    failwith "userscript target: `unsafe` operation has no lowering"

and gen_match ctx scrutinee arms =
  let scrut = "__scrut" in
  let rec gen_arms = function
    | [] -> "throw new Error(\"non-exhaustive match\");"
    | arm :: rest ->
      let cond = gen_pattern_test scrut arm.ma_pat in
      let bindings = gen_pattern_bindings scrut arm.ma_pat in
      let guard = match arm.ma_guard with
        | Some g -> " && (" ^ gen_expr ctx g ^ ")"
        | None -> ""
      in
      let body = gen_expr ctx arm.ma_body in
      "if (" ^ cond ^ guard ^ ") { " ^ bindings ^ "return " ^ body ^ "; } else "
      ^ gen_arms rest
  in
  "((" ^ scrut ^ ") => { " ^ gen_arms arms ^ " })(" ^ gen_expr ctx scrutinee ^ ")"

and gen_pattern_test scrut (pat : pattern) : string =
  match pat with
  | PatWildcard _ | PatVar _ -> "true"
  | PatLit lit -> scrut ^ " === " ^ gen_literal lit
  | PatCon (id, args) ->
    let tag_test = scrut ^ ".tag === " ^ js_string_lit id.name in
    let sub_tests = match args with
      | [] -> []
      | [ single ] -> [ gen_pattern_test (scrut ^ "." ^ payload_field id) single ]
      | many ->
        List.mapi (fun i p ->
            gen_pattern_test (scrut ^ ".values[" ^ string_of_int i ^ "]") p) many
    in
    String.concat " && "
      (tag_test :: List.filter (fun s -> s <> "true") sub_tests)
  | PatTuple pats ->
    let conds = List.mapi (fun i p ->
        gen_pattern_test (scrut ^ "[" ^ string_of_int i ^ "]") p) pats
    in
    String.concat " && " (("Array.isArray(" ^ scrut ^ ")") :: conds)
  | PatRecord (fields, _) ->
    let conds = List.map (fun (n, sub) ->
        match sub with
        | None -> "true"
        | Some sub -> gen_pattern_test (scrut ^ "." ^ mangle n.name) sub) fields
    in
    String.concat " && " conds
  | PatAs (_, p) -> gen_pattern_test scrut p
  | PatOr (p1, p2) ->
    "((" ^ gen_pattern_test scrut p1 ^ ") || (" ^ gen_pattern_test scrut p2 ^ "))"

and gen_pattern_bindings scrut (pat : pattern) : string =
  let buf = Buffer.create 64 in
  let rec walk path = function
    | PatWildcard _ | PatLit _ -> ()
    | PatVar id -> Buffer.add_string buf ("const " ^ mangle id.name ^ " = " ^ path ^ "; ")
    | PatTuple pats ->
      List.iteri (fun i p -> walk (path ^ "[" ^ string_of_int i ^ "]") p) pats
    | PatRecord (fields, _) ->
      List.iter (fun (n, sub) ->
          let sub_path = path ^ "." ^ mangle n.name in
          match sub with
          | None -> Buffer.add_string buf ("const " ^ mangle n.name ^ " = " ^ sub_path ^ "; ")
          | Some sub -> walk sub_path sub) fields
    | PatCon (id, args) ->
      (match args with
       | [] -> ()
       | [ single ] -> walk (path ^ "." ^ payload_field id) single
       | many ->
         List.iteri (fun i p -> walk (path ^ ".values[" ^ string_of_int i ^ "]") p) many)
    | PatAs (id, sub) ->
      Buffer.add_string buf ("const " ^ mangle id.name ^ " = " ^ path ^ "; ");
      walk path sub
    | PatOr (p, _) -> walk path p
  in
  walk scrut pat;
  Buffer.contents buf

and gen_block_expr ctx (block : block) : string =
  let body = Buffer.create 64 in
  List.iter (fun s ->
      Buffer.add_string body (gen_stmt ctx s);
      Buffer.add_char body ' ') block.blk_stmts;
  let result = match block.blk_expr with
    | Some e -> "return " ^ gen_expr ctx e ^ ";"
    | None -> "return Unit;"
  in
  "(() => { " ^ Buffer.contents body ^ result ^ " })()"

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
  "(() => { try { return " ^ body_str ^ "; } " ^ catch_str ^ finally_str ^ " })()"

(* ---- statement position: `return`/`if`/`match` are real JS statements ---- *)

and unspan = function
  | ExprSpan (e, _) -> unspan e
  | e -> e

and gen_stmt_expr ctx (e : expr) : string =
  match unspan e with
  | ExprReturn (Some e) -> "return " ^ gen_expr ctx e ^ ";"
  | ExprReturn None -> "return;"
  | ExprBreak _ -> "break;"
  | ExprContinue _ -> "continue;"
  | ExprIf { ei_cond; ei_then; ei_else } ->
    let else_str = match ei_else with
      | Some e -> " else { " ^ gen_branch ctx e ^ " }"
      | None -> ""
    in
    "if (" ^ gen_expr ctx ei_cond ^ ") { " ^ gen_branch ctx ei_then ^ " }" ^ else_str
  | ExprBlock blk -> gen_stmt_seq ctx blk
  | ExprMatch { em_scrutinee; em_arms } -> gen_match_stmt ctx em_scrutinee em_arms
  | ExprTry { et_body; et_catch; et_finally } ->
    gen_try_stmt ctx et_body et_catch et_finally
  | other -> gen_expr ctx other ^ ";"

and gen_branch ctx (e : expr) : string =
  match unspan e with
  | ExprBlock blk -> gen_stmt_seq ctx blk
  | other -> gen_stmt_expr ctx other

and gen_stmt_seq ctx (blk : block) : string =
  let b = Buffer.create 64 in
  List.iter (fun s ->
      Buffer.add_string b (gen_stmt ctx s);
      Buffer.add_char b ' ') blk.blk_stmts;
  (match blk.blk_expr with
   | Some e -> Buffer.add_string b (gen_stmt_expr ctx e)
   | None -> ());
  Buffer.contents b

and gen_match_stmt ctx scrut arms =
  let sv = "__scrut" in
  let rec arms_js = function
    | [] -> "throw new Error(\"non-exhaustive match\");"
    | arm :: rest ->
      let cond = gen_pattern_test sv arm.ma_pat in
      let binds = gen_pattern_bindings sv arm.ma_pat in
      let guard = match arm.ma_guard with
        | Some g -> " && (" ^ gen_expr ctx g ^ ")"
        | None -> ""
      in
      "if (" ^ cond ^ guard ^ ") { " ^ binds ^ gen_branch ctx arm.ma_body
      ^ " } else " ^ arms_js rest
  in
  "{ const " ^ sv ^ " = " ^ gen_expr ctx scrut ^ "; " ^ arms_js arms ^ " }"

and gen_try_stmt ctx body catch finally =
  let b = gen_stmt_seq ctx body in
  let c = match catch with
    | None | Some [] -> "catch (__e) { throw __e; }"
    | Some (arm :: _) ->
      let bind = match arm.ma_pat with
        | PatVar id -> "const " ^ mangle id.name ^ " = __e; "
        | _ -> ""
      in
      "catch (__e) { " ^ bind ^ gen_branch ctx arm.ma_body ^ " }"
  in
  let f = match finally with
    | None -> ""
    | Some blk -> " finally { " ^ gen_stmt_seq ctx blk ^ " }"
  in
  "try { " ^ b ^ " } " ^ c ^ f

and gen_stmt ctx (stmt : stmt) : string =
  match stmt with
  | StmtLet { sl_pat = PatWildcard _; sl_value; _ } -> gen_expr ctx sl_value ^ ";"
  | StmtLet { sl_pat; sl_value; sl_mut; _ } ->
    let kw = if sl_mut then "let" else "const" in
    let js = kw ^ " " ^ gen_pattern ctx sl_pat ^ " = " ^ gen_expr ctx sl_value ^ ";" in
    (match Int_div_classifier.pat_var_name sl_pat with
     | Some n ->
       Int_div_classifier.track_int_binding ctx.idc n sl_value;
       Int_div_classifier.track_int_array_binding ctx.idc n sl_value
     | None -> ());
    js
  | StmtExpr e -> gen_stmt_expr ctx e
  | StmtAssign (lhs, op, rhs) ->
    let div_int = op = AssignDiv
                  && Int_div_classifier.expr_is_int ctx.idc lhs
                  && Int_div_classifier.expr_is_int ctx.idc rhs in
    if div_int then begin
      let t = gen_expr ctx lhs in
      t ^ " = Math.trunc(" ^ t ^ " / (" ^ gen_expr ctx rhs ^ "));"
    end else begin
      let op_str = match op with
        | AssignEq -> "=" | AssignAdd -> "+=" | AssignSub -> "-="
        | AssignMul -> "*=" | AssignDiv -> "/="
      in
      gen_expr ctx lhs ^ " " ^ op_str ^ " " ^ gen_expr ctx rhs ^ ";"
    end
  | StmtWhile (cond, body) ->
    let cond_js = gen_expr ctx cond in
    let body_js = gen_stmt_seq ctx body in
    "while (" ^ cond_js ^ ") { " ^ body_js ^ " }"
  | StmtFor (pat, iter, body) ->
    let iter_js = gen_expr ctx iter in
    let body_js = gen_stmt_seq ctx body in
    "for (const " ^ gen_pattern ctx pat ^ " of " ^ iter_js ^ ") { " ^ body_js ^ " }"

(* ============================================================================
   Top-level declarations
   ============================================================================ *)

let gen_function ctx (fd : fn_decl) : unit =
  let name = mangle fd.fd_name.name in
  let params = List.map (fun (p : param) -> mangle p.p_name.name) fd.fd_params in
  let is_async = match fd.fd_eff with
    | Some (EffVar id) | Some (EffCon (id, _)) -> id.name = "Async"
    | Some (EffUnion _) | None -> false
  in
  let header =
    Printf.sprintf "%sfunction %s(%s) {"
      (if is_async then "async " else "") name (String.concat ", " params)
  in
  emit_line ctx header;
  Int_div_classifier.enter_fn_scope ctx.idc fd.fd_params;
  (match fd.fd_body with
   | FnExpr body ->
     emit_line ctx ("  return " ^ gen_expr ctx body ^ ";")
   | FnBlock block ->
     List.iter (fun s -> emit_line ctx ("  " ^ gen_stmt ctx s)) block.blk_stmts;
     (match block.blk_expr with
      | Some e -> emit_line ctx ("  return " ^ gen_expr ctx e ^ ";")
      | None -> ())
   | FnExtern ->
     emit_line ctx "  throw new Error(\"userscript target: extern function without a binding\");");
  emit_line ctx "}"

let gen_type_decl ctx (td : type_decl) : unit =
  match td.td_body with
  | TyEnum variants ->
    List.iter (fun (vd : variant_decl) ->
        let name = mangle vd.vd_name.name in
        if List.mem vd.vd_name.name preamble_ctors then
          (* Some/None/Ok/Err come from the runtime prelude. *)
          ()
        else
          let arity = List.length vd.vd_fields in
          if arity = 0 then
            emit_line ctx (Printf.sprintf "const %s = { tag: %s };"
                             name (js_string_lit vd.vd_name.name))
          else if arity = 1 then
            emit_line ctx
              (Printf.sprintf "const %s = (value) => ({ tag: %s, value });"
                 name (js_string_lit vd.vd_name.name))
          else begin
            let ps = List.init arity (fun i -> "v" ^ string_of_int i) in
            emit_line ctx
              (Printf.sprintf "const %s = (%s) => ({ tag: %s, values: [%s] });"
                 name (String.concat ", " ps)
                 (js_string_lit vd.vd_name.name) (String.concat ", " ps))
          end) variants
  | TyStruct _ | TyAlias _ | TyExtern ->
    emit_line ctx (Printf.sprintf "// type %s (erased at runtime)" td.td_name.name)

let gen_top_level ctx (top : top_level) : unit =
  match top with
  | TopFn fd when fd.fd_body = FnExtern ->
    (* Lowered at call sites; nothing to emit. *)
    emit_line ctx (Printf.sprintf "// extern fn %s (lowered at call sites)" fd.fd_name.name)
  | TopFn fd -> gen_function ctx fd
  | TopType td -> gen_type_decl ctx td
  | TopConst { tc_name; tc_mut; tc_value; _ } ->
    emit_line ctx
      (Printf.sprintf "%s %s = %s;"
         (if tc_mut then "let" else "const") (mangle tc_name.name)
         (gen_expr ctx tc_value))
  | TopExternFn { ef_name; _ } ->
    emit_line ctx (Printf.sprintf "// extern fn %s (lowered at call sites)" ef_name.name)
  | TopExternType { et_name } ->
    emit_line ctx (Printf.sprintf "// extern type %s (opaque)" et_name.name)
  | TopEffect _ -> emit_line ctx "// effect declaration (erased)"
  | TopTrait _ -> emit_line ctx "// trait declaration (erased)"
  | TopImpl ib ->
    List.iter (function
        | ImplFn fd -> gen_function ctx fd
        | ImplType _ -> ()) ib.ib_items

(* ============================================================================
   Entry point
   ============================================================================ *)

let main_invocation = {|// A userscript's entry point runs once, in the page, after the IIFE body.
// `main` may be async (Async effect row): the returned promise is observed
// here rather than awaited, because top-level `await` is not allowed in a
// non-module script.
try {
  const __as_result = main();
  if (__as_result && typeof __as_result.then === "function") {
    __as_result.catch((e) => console.error("[affinescript] main failed", e));
  }
} catch (e) {
  console.error("[affinescript] main failed", e);
}
|}

(** Emit the single-file userscript for [program].

    @param banner  verbatim [==UserScript==] metadata block, already
                   validated by the CLI ({!Bin.main} checks it is a
                   complete metadata header).
    @raise Failure when a construct slips past {!check_reachable} (a bug:
                   the CLI runs the reachability check first and reports it
                   with module provenance). *)
let generate ?(banner = "") (program : program) (symbols : Symbol.t) : string =
  let program = prune_to_entry program in
  let ctx = create_ctx symbols in
  (* Register every Int-returning fn before any body is emitted so a forward
     or self-recursive call counts as an integer operand (#478). *)
  List.iter (function
      | TopFn fd ->
        (match fd.fd_ret_ty with
         | Some t when Int_div_classifier.type_head_is_int t ->
           Int_div_classifier.register_int_fn ctx.idc fd.fd_name.name
         | _ -> ())
      | TopImpl ib ->
        List.iter (function
            | ImplFn fd ->
              (match fd.fd_ret_ty with
               | Some t when Int_div_classifier.type_head_is_int t ->
                 Int_div_classifier.register_int_fn ctx.idc fd.fd_name.name
               | _ -> ())
            | ImplType _ -> ()) ib.ib_items
      | _ -> ()) program.prog_decls;

  if banner <> "" then begin
    emit ctx banner;
    if banner.[String.length banner - 1] <> '\n' then emit ctx "\n"
  end;
  emit_line ctx "// Generated by AffineScript compiler (userscript target).";
  emit_line ctx "// SPDX-License-Identifier: MPL-2.0";
  emit_line ctx "// One ES2020 IIFE: no import/export, no dynamic import, no eval,";
  emit_line ctx "// no top-level await. Do not hand-edit; regenerate from source.";
  emit_line ctx "(function () {";
  emit_line ctx "  \"use strict\";";
  emit ctx (prelude_for program);
  (* Enum constructor bindings precede uses: a top-level `const` may name a
     variant declared later in the file, which would otherwise be a temporal
     dead zone read. *)
  List.iter (function
      | TopType ({ td_body = TyEnum _; _ } as td) -> gen_type_decl ctx td
      | _ -> ()) program.prog_decls;
  List.iter (fun top ->
      match top with
      | TopType { td_body = TyEnum _; _ } -> ()
      | other -> gen_top_level ctx other) program.prog_decls;
  let has_main = List.exists (function
      | TopFn fd -> fd.fd_name.name = "main"
      | _ -> false) program.prog_decls in
  if has_main then emit ctx main_invocation;
  emit_line ctx "})();";
  Buffer.contents ctx.output

let codegen_userscript ?banner (program : program) (symbols : Symbol.t)
  : (string, string) result =
  try
    let text = generate ?banner program symbols in
    match verify_artifact text with
    | Ok () -> Ok text
    | Error msg -> Error msg
  with
  | Failure msg -> Error msg
  | e -> Error ("userscript codegen error: " ^ Printexc.to_string e)
