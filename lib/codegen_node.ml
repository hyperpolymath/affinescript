(* SPDX-License-Identifier: MPL-2.0 *)
(* SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell *)

(** Node-CJS Emit Mode (issue #35, Phase 1).

    Wraps a compiled [Wasm.wasm_module] in a CommonJS shim that VS Code's
    extension host (and any other Node CJS consumer) can [require()].  The
    shim:

    - Embeds the Wasm bytes inline as base64 so the output is a single
      self-contained file.
    - Lazily instantiates the Wasm module on first [activate] / [deactivate]
      call.
    - Maintains a JS-side handle table for opaque host objects
      ([ExtensionContext], [Terminal], [LanguageClient], …) that bound Wasm
      code references via integer ids.
    - Re-exports the Wasm module's [activate] and [deactivate] (when present)
      as CJS [exports.activate] / [exports.deactivate].

    Phase 1 explicitly does NOT include any [vscode] API bindings — that's
    Phase 2 (a separate stdlib/Vscode.affine module that wires concrete
    imports into the table this shim leaves empty by default).  An extension
    author can still pass an extra import map at compile time via the
    [~extra_imports_js] parameter for early experimentation.

    Out of scope: WASI runtime improvements, browser-host VS Code (web worker
    extension host has different constraints), changes to typed-wasm. *)

open Wasm
open Wasm_encode

(** Encode a sequence of bytes as a base64 string (RFC 4648, no line breaks).
    Implemented inline so codegen has zero added dependencies. *)
let base64_encode (bytes : bytes) : string =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let buf = Buffer.create ((Bytes.length bytes * 4 / 3) + 4) in
  let len = Bytes.length bytes in
  let i = ref 0 in
  while !i + 2 < len do
    let b0 = Char.code (Bytes.get bytes !i) in
    let b1 = Char.code (Bytes.get bytes (!i + 1)) in
    let b2 = Char.code (Bytes.get bytes (!i + 2)) in
    Buffer.add_char buf alphabet.[b0 lsr 2];
    Buffer.add_char buf alphabet.[((b0 land 0x03) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf alphabet.[((b1 land 0x0F) lsl 2) lor (b2 lsr 6)];
    Buffer.add_char buf alphabet.[b2 land 0x3F];
    i := !i + 3
  done;
  let rem = len - !i in
  if rem = 1 then begin
    let b0 = Char.code (Bytes.get bytes !i) in
    Buffer.add_char buf alphabet.[b0 lsr 2];
    Buffer.add_char buf alphabet.[(b0 land 0x03) lsl 4];
    Buffer.add_char buf '=';
    Buffer.add_char buf '='
  end else if rem = 2 then begin
    let b0 = Char.code (Bytes.get bytes !i) in
    let b1 = Char.code (Bytes.get bytes (!i + 1)) in
    Buffer.add_char buf alphabet.[b0 lsr 2];
    Buffer.add_char buf alphabet.[((b0 land 0x03) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf alphabet.[(b1 land 0x0F) lsl 2];
    Buffer.add_char buf '='
  end;
  Buffer.contents buf

(** Encode a Wasm module to a byte buffer in memory (no file I/O).
    Mirrors [Wasm_encode.write_module_to_file] but returns the bytes. *)
let encode_module_to_bytes (m : wasm_module) : bytes =
  let buf = Buffer.create 4096 in
  Buffer.add_string buf "\x00asm";
  Buffer.add_string buf "\x01\x00\x00\x00";
  add_section buf 1 (fun b -> add_vec b m.types add_func_type);
  add_section buf 2 (fun b -> add_vec b m.imports add_import);
  add_section buf 3 (fun b -> add_vec b m.funcs (fun b f -> add_u32_leb b f.f_type));
  add_section buf 4 (fun b ->
    add_vec b m.tables (fun b t -> add_u8 b 0x70; add_limits b t.tab_type)
  );
  add_section buf 5 (fun b ->
    add_vec b m.mems (fun b mem -> add_limits b mem.mem_type)
  );
  add_section buf 6 (fun b -> add_vec b m.globals add_global);
  add_section buf 7 (fun b -> add_vec b m.exports add_export);
  (match m.start with
   | None -> ()
   | Some idx -> add_section buf 8 (fun b -> add_u32_leb b idx));
  add_section buf 9 (fun b -> add_vec b m.elems add_elem);
  add_section buf 10 (fun b -> add_vec b m.funcs add_code);
  add_section buf 11 (fun b -> add_vec b m.datas add_data);
  List.iter (fun (name, payload) ->
    add_section buf 0 (fun b ->
      add_string b name;
      add_bytes b payload
    )
  ) m.custom_sections;
  Bytes.of_string (Buffer.contents buf)

(** Escape a string for embedding inside a JS double-quoted literal. Only
    the characters that would break out of (or corrupt) the literal are
    escaped — sufficient for require() specifiers and module paths. *)
let js_string_escape (s : string) : string =
  let buf = Buffer.create (String.length s + 8) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string buf "\\\\"
    | '"'  -> Buffer.add_string buf "\\\""
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | c    -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Default npm specifier for the vscode-API adapter (issue #105). Callers
    can override it via [~vscode_extension_adapter]. *)
let default_vscode_adapter = "@hyperpolymath/affine-vscode"

(** Build the [--vscode-extension] wiring block (issue #105).

    Returns the JS that installs [exports.extraImports] so the generated
    [.cjs] is directly loadable as a VS Code extension's [main] — no
    hand-written [index.cjs], no vendored adapter. [adapter] is the
    require() specifier for the adapter; when [no_lc] is set the extension
    ships no language client, so the [vscode-languageclient/node] require
    is skipped and [null] is passed in its place. *)
let vscode_extension_wiring ~(adapter : string) ~(no_lc : bool) : string =
  let lc_arg =
    if no_lc then "null"
    else {|require("vscode-languageclient/node")|}
  in
  Printf.sprintf {|
// Inserted by --vscode-extension (issue #105): auto-generated glue so this
// file is directly loadable as a VS Code extension's `main`. Replaces the
// previously hand-written index.cjs + vendored adapter boilerplate.
const _makeVscodeBindings = require("%s");
exports.extraImports = function() {
  return _makeVscodeBindings(
    require("vscode"),
    %s,
    exports,
  );
};
|} (js_string_escape adapter) lc_arg

(** Wrap [m] in a Node-CJS shim. The shim is a single self-contained
    JavaScript string suitable for writing to a [.cjs] file.

    When [~vscode_extension:true] (issue #105), the shim additionally
    installs [exports.extraImports] inline so the output is directly
    loadable as a VS Code extension's [main]. [~vscode_extension_adapter]
    overrides the adapter require() specifier (default
    {!default_vscode_adapter}); [~vscode_extension_no_lc] omits the
    [vscode-languageclient/node] dependency for extensions that ship no
    language client. *)
let emit_node_cjs
    ?(extra_imports_js : string option)
    ?(vscode_extension : bool = false)
    ?(vscode_extension_adapter : string option)
    ?(vscode_extension_no_lc : bool = false)
    (m : wasm_module) : string =
  let wasm_bytes = encode_module_to_bytes m in
  let b64 = base64_encode wasm_bytes in
  let extra =
    match extra_imports_js with
    | Some js -> js
    | None -> "{}"
  in
  let vscode_block =
    if vscode_extension then
      let adapter =
        match vscode_extension_adapter with
        | Some a -> a
        | None -> default_vscode_adapter
      in
      vscode_extension_wiring ~adapter ~no_lc:vscode_extension_no_lc
    else ""
  in
  Printf.sprintf {|// Generated by AffineScript. Do not edit.
// Node-CJS shim wrapping the compiled .wasm module so VS Code's extension
// host (or any CJS consumer) can require() it directly.
//
// Issue #35 Phase 1 — no vscode API bindings yet (that's Phase 2 via
// stdlib/Vscode.affine). The Wasm module's `activate` and `deactivate`
// exports become exports.activate / exports.deactivate.

"use strict";

const _wasmBase64 = "%s";
const _wasmBytes = Buffer.from(_wasmBase64, "base64");

// Per-process opaque-handle table for host objects (ExtensionContext,
// Terminal, LanguageClient, ...). Wasm refers to host objects by integer id.
const _handles = new Map();
let _nextHandle = 1;
function _registerHandle(obj) {
  const h = _nextHandle++;
  _handles.set(h, obj);
  return h;
}
function _getHandle(h) { return _handles.get(h); }
function _freeHandle(h) { _handles.delete(h); }

// WASI-style minimal imports — affinescript codegen wires in fd_write on
// every module so we satisfy that even if no IO is exercised.
function _writeFdString(fd, ptr, len, memory) {
  const bytes = new Uint8Array(memory.buffer, ptr, len);
  const text = new TextDecoder("utf-8").decode(bytes);
  if (fd === 1) process.stdout.write(text);
  else if (fd === 2) process.stderr.write(text);
}

let _instance = null;
let _memory = null;

function _buildImports() {
  const wasi_snapshot_preview1 = {
    fd_write: (fd, iovs_ptr, iovs_len, nwritten_ptr) => {
      // Minimal fd_write: walk the iovec array and concat to fd.
      const view = new DataView(_memory.buffer);
      let total = 0;
      for (let i = 0; i < iovs_len; i++) {
        const ptr = view.getUint32(iovs_ptr + i * 8, true);
        const len = view.getUint32(iovs_ptr + i * 8 + 4, true);
        _writeFdString(fd, ptr, len, _memory);
        total += len;
      }
      view.setUint32(nwritten_ptr, total, true);
      return 0;
    },
  };
  // Phase 2 hook: a caller may install an `extraImports` factory on the
  // exports object returning a `{ ModuleName: { exportName: fn, ... } }`
  // map of concrete host bindings (this is what the --vscode-extension
  // wiring installs). Default is empty so the shim works standalone.
  const extras = (typeof exports.extraImports === "function")
    ? exports.extraImports()
    : %s;
  return { wasi_snapshot_preview1, ...extras };
}

async function _init() {
  if (_instance) return _instance;
  const { instance } = await WebAssembly.instantiate(_wasmBytes, _buildImports());
  _instance = instance;
  _memory = instance.exports.memory;
  // Phase 2 hook: the active instance + memory must be reachable from
  // bindings adapters that need to read string args / call back into the
  // wasm table. Surface them on the exports object as soon as init finishes.
  exports._instance = instance;
  exports._memory = instance.exports.memory;
  return _instance;
}

exports.activate = async function activate(context) {
  const inst = await _init();
  if (typeof inst.exports.activate === "function") {
    const ctxHandle = _registerHandle(context);
    return inst.exports.activate(ctxHandle);
  }
};

exports.deactivate = async function deactivate() {
  if (!_instance) return;
  if (typeof _instance.exports.deactivate === "function") {
    return _instance.exports.deactivate();
  }
};

// Surfaced for Phase 2 binding modules to register concrete vscode-API
// implementations before the Wasm is instantiated.
exports._registerHandle = _registerHandle;
exports._getHandle = _getHandle;
exports._freeHandle = _freeHandle;
%s|} b64 extra vscode_block
