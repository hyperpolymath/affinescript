// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-vscode: JS-side adapter for stdlib/Vscode.affine + stdlib/VscodeLanguageClient.affine.
//
// Issue #35 Phase 2 deliverable. Plug this into your Node-CJS extension's
// `_extraImports()` so each `extern fn` declared in the bindings resolves
// to the right vscode API call.
//
// Usage from a hand-written .cjs (until Phase 3 automates the wiring):
//
//   const vscodeBindings = require("@hyperpolymath/affine-vscode")(
//     require("vscode"),
//     require("vscode-languageclient/node"),
//     instance,                         // WebAssembly.Instance, set after instantiate
//   );
//   // Pass vscodeBindings into the Wasm imports map under "env".
//
// The adapter maintains a per-process JS-side handle table keyed by Int
// so opaque handles passed across the FFI boundary survive round-trips.

"use strict";

module.exports = function makeVscodeBindings(vscode, lcModule, hostShim) {
  // `hostShim` is the .cjs module produced by `affinescript compile -o ...`.
  // We share its handle table (so ExtensionContext registered at activate
  // time is visible here) and read its `_instance` lazily so this adapter
  // can be constructed BEFORE `WebAssembly.instantiate` runs — the calls
  // back into adapter functions happen later, once `_instance` is live.
  const reg = (obj) => hostShim._registerHandle(obj);
  const get = (h) => hostShim._getHandle(h);
  const getInstance = () => hostShim._instance;

  // ── String marshalling ─────────────────────────────────────────────
  // AffineScript's WASM 1.0 codegen stores string literals at the offset
  // returned by the call-site; the layout is [u32 length][utf-8 bytes].
  // Read that shape out of the module's exported memory.
  function readString(ptr) {
    const inst = getInstance();
    if (!inst || !inst.exports.memory) return "";
    const dv = new DataView(inst.exports.memory.buffer);
    const len = dv.getUint32(ptr, true);
    const bytes = new Uint8Array(inst.exports.memory.buffer, ptr + 4, len);
    return new TextDecoder("utf-8").decode(bytes);
  }

  // ── Wasm-table callbacks → JS callable ─────────────────────────────
  // Wasm function-pointer args (e.g. command handlers) come in as table
  // indices. Wrap each in a JS thunk that re-enters the Wasm module.
  function wrapHandler(idx) {
    return () => {
      const inst = getInstance();
      const tbl = inst && inst.exports && inst.exports.__indirect_function_table;
      if (!tbl) return;
      const fn = tbl.get(idx);
      if (typeof fn === "function") fn();
    };
  }

  // Returned shape is namespaced by the AffineScript module that declared
  // each extern: cross-module imports in the wasm reference module="Vscode"
  // and module="VscodeLanguageClient" (the dotted module path), so the
  // import map's top-level keys must match.
  const Vscode = {
    // ── vscode.commands ──────────────────────────────────────────────
    registerCommand: (namePtr, handlerIdx) => {
      const name = readString(namePtr);
      const handler = wrapHandler(handlerIdx);
      const disposable = vscode.commands.registerCommand(name, handler);
      return reg(disposable);
    },

    // ── vscode.workspace ─────────────────────────────────────────────
    getConfiguration: (sectionPtr) =>
      reg(vscode.workspace.getConfiguration(readString(sectionPtr))),

    workspaceConfigGetString: (cfgHandle, keyPtr, defPtr) => {
      const cfg = get(cfgHandle);
      const result = cfg.get(readString(keyPtr), readString(defPtr));
      // Returning a string-pointer would require allocating in the Wasm
      // module's memory. Until that helper exists, return a sentinel
      // handle that the caller treats as "look me up via getResultString".
      // For now: register the JS string and return its handle.
      return reg(String(result));
    },

    createFileSystemWatcher: (globPtr) =>
      reg(vscode.workspace.createFileSystemWatcher(readString(globPtr))),

    // ── vscode.window ────────────────────────────────────────────────
    activeTextEditor: () => {
      const ed = vscode.window.activeTextEditor;
      return ed ? reg(ed) : 0;
    },
    showErrorMessage:       (msgPtr) => reg(vscode.window.showErrorMessage(readString(msgPtr))),
    showWarningMessage:     (msgPtr) => reg(vscode.window.showWarningMessage(readString(msgPtr))),
    showInformationMessage: (msgPtr) => reg(vscode.window.showInformationMessage(readString(msgPtr))),

    createTerminal: (namePtr) =>
      reg(vscode.window.createTerminal(readString(namePtr))),
    terminalShow: (tHandle) => { const t = get(tHandle); if (t) t.show(); return 0; },
    terminalSendText: (tHandle, textPtr) => {
      const t = get(tHandle); if (t) t.sendText(readString(textPtr)); return 0;
    },

    // ── ExtensionContext ────────────────────────────────────────────
    pushSubscription: (ctxHandle, dHandle) => {
      const ctx = get(ctxHandle);
      const d = get(dHandle);
      if (ctx && d) ctx.subscriptions.push(d);
      return 0;
    },

    // ── Editor document helpers ───────────────────────────────────────
    editorActiveFilePath: () => {
      const ed = vscode.window.activeTextEditor;
      return ed ? reg(ed.document.uri.fsPath) : reg("");
    },
    editorActiveLanguageId: () => {
      const ed = vscode.window.activeTextEditor;
      return ed ? reg(ed.document.languageId) : reg("");
    },

    // ── Boolean config ───────────────────────────────────────────────
    workspaceConfigGetBool: (cfgHandle, keyPtr, defVal) => {
      const cfg = get(cfgHandle);
      if (!cfg) return defVal;
      return cfg.get(readString(keyPtr), defVal !== 0) ? 1 : 0;
    },

    // ── Host process / IO ────────────────────────────────────────────
    consoleLog: (msgPtr) => { console.log(readString(msgPtr)); return 0; },
    execSync: (cmdPtr) => {
      try {
        require("child_process").execSync(readString(cmdPtr), { stdio: "ignore" });
        return 0;
      } catch (e) {
        return e.status ?? 1;
      }
    },

    // ── String helpers ────────────────────────────────────────────────
    stringConcat: (aPtr, bPtr) => reg(readString(aPtr) + readString(bPtr)),
    stringEndsWith: (sPtr, suffixPtr) =>
      readString(sPtr).endsWith(readString(suffixPtr)) ? 1 : 0,
    stringReplaceSuffix: (sPtr, suffixPtr, replacementPtr) => {
      const s = readString(sPtr);
      const suffix = readString(suffixPtr);
      const replacement = readString(replacementPtr);
      const out = s.endsWith(suffix) ? s.slice(0, -suffix.length) + replacement : s;
      return reg(out);
    },
  };

  const VscodeLanguageClient = {
    // ── vscode-languageclient/node ──────────────────────────────────
    newLanguageClient: (idPtr, namePtr, cmdPtr, argsNlPtr, transportKind) => {
      const id = readString(idPtr);
      const name = readString(namePtr);
      const command = readString(cmdPtr);
      const args = readString(argsNlPtr).split("\n").filter(s => s.length > 0);
      const transport = transportKind === 1 ? lcModule.TransportKind.ipc : lcModule.TransportKind.stdio;
      const serverOptions = { run: { command, args, transport }, debug: { command, args, transport } };
      const clientOptions = { documentSelector: [{ scheme: "file" }] };
      return reg(new lcModule.LanguageClient(id, name, serverOptions, clientOptions));
    },
    languageClientStart: (cHandle) => {
      const c = get(cHandle);
      if (c) c.start();
      return 0;
    },
    languageClientStop: (cHandle) => {
      const c = get(cHandle);
      if (c) c.stop();
      return 0;
    },
  };

  return { Vscode, VscodeLanguageClient };
};
