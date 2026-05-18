// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// affine-vscode: JS-side adapter for stdlib/Vscode.affine + stdlib/VscodeLanguageClient.affine.
//
// Issue #35 Phase 2 deliverable. Resolves each `extern fn` declared in the
// bindings to the right vscode API call.
//
// Preferred wiring (issue #105): compile with `--vscode-extension` and the
// generated .cjs installs `exports.extraImports` calling this adapter
// automatically — no hand-written entry point.
//
// Manual wiring (fallback), from a hand-written .cjs:
//
//   const shim = require("./extension.cjs");
//   shim.extraImports = () => require("@hyperpolymath/affine-vscode")(
//     require("vscode"),
//     require("vscode-languageclient/node"),
//     shim,                             // the .cjs shim module (hostShim)
//   );
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
  // Settled host-Thenable values, keyed by Thenable handle (issue #205).
  const __thenableResults = new Map();

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

  // ── Wasm closure callbacks → JS callable ───────────────────────────
  // Post-#199 (function-value callback ABI) a handler arrives as a
  // *closure pointer*, not a bare table index: an 8-byte heap pair
  // [i32 function_id @ +0][i32 env_ptr @ +4] (codegen.ml). To invoke,
  // read the pair from exported memory, look the compiled lambda up in
  // __indirect_function_table by function_id, and call it with env_ptr
  // as the first argument (the closure calling convention), zero-filling
  // any further declared params (e.g. the `Unit` handler arg).
  function wrapHandler(closurePtr) {
    return () => {
      const inst = getInstance();
      if (!inst || !inst.exports || !inst.exports.memory) return;
      const tbl = inst.exports.__indirect_function_table;
      if (!tbl) return;
      const dv = new DataView(inst.exports.memory.buffer);
      const fnId = dv.getInt32(closurePtr, true);
      const envPtr = dv.getInt32(closurePtr + 4, true);
      const fn = tbl.get(fnId);
      if (typeof fn !== "function") return;
      const args = [envPtr];
      while (args.length < fn.length) args.push(0);
      return fn(...args);
    };
  }

  // Returned shape is namespaced by the AffineScript module that declared
  // each extern: cross-module imports in the wasm reference module="Vscode"
  // and module="VscodeLanguageClient" (the dotted module path), so the
  // import map's top-level keys must match.
  const Vscode = {
    // ── vscode.commands ──────────────────────────────────────────────
    registerCommand: (namePtr, handlerPtr) => {
      const name = readString(namePtr);
      const handler = wrapHandler(handlerPtr);
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
    stringIsEmpty: (sPtr) => readString(sPtr).length === 0 ? 1 : 0,

    // ── Workspace ───────────────────────────────────────────────────
    workspaceFolderFirstPath: () => {
      const folders = vscode.workspace.workspaceFolders;
      const first = folders && folders[0];
      return reg(first ? first.uri.fsPath : "");
    },
    workspaceRootUri: () => {
      const folders = vscode.workspace.workspaceFolders;
      const first = folders && folders[0];
      return first ? reg(first.uri) : 0;
    },

    // ── URI / file-system / text documents ─────────────────────────
    uriFromPath: (pathPtr) => reg(vscode.Uri.file(readString(pathPtr))),
    uriJoinPath: (baseHandle, segPtr) => {
      const base = get(baseHandle);
      if (!base) return 0;
      return reg(vscode.Uri.joinPath(base, readString(segPtr)));
    },
    uriPath: (uHandle) => {
      const u = get(uHandle);
      return reg(u ? u.fsPath : "");
    },
    fsWriteFile: (uHandle, contentPtr) => {
      const u = get(uHandle);
      if (!u) return 1;
      try {
        // Fire-and-forget the Thenable. The host serialises FS ops so
        // a subsequent openTextDocument on the same URI sees the file.
        vscode.workspace.fs.writeFile(u, Buffer.from(readString(contentPtr)));
        return 0;
      } catch (e) {
        return 1;
      }
    },
    openTextDocument: (uHandle) => {
      const u = get(uHandle);
      if (!u) return 0;
      // openTextDocument returns a Thenable<TextDocument>. The synchronous
      // FFI returns a handle to the Thenable itself; showTextDocument is
      // also Thenable-returning and chains via vscode's internal queue,
      // so this works in practice for the open-then-show pattern.
      return reg(vscode.workspace.openTextDocument(u));
    },
    showTextDocument: (dHandle) => {
      const d = get(dHandle);
      if (!d) return 1;
      // If `d` is itself a Thenable<TextDocument>, vscode unwraps it.
      Promise.resolve(d).then((doc) => vscode.window.showTextDocument(doc));
      return 0;
    },

    // ── Status bar ─────────────────────────────────────────────────
    createStatusBarItem: (alignment, priority) => {
      const align = alignment === 1
        ? vscode.StatusBarAlignment.Right
        : vscode.StatusBarAlignment.Left;
      return reg(vscode.window.createStatusBarItem(align, priority));
    },
    statusBarItemSetText: (sHandle, tPtr) => {
      const s = get(sHandle);
      if (s) s.text = readString(tPtr);
      return 0;
    },
    statusBarItemSetTooltip: (sHandle, tPtr) => {
      const s = get(sHandle);
      if (s) s.tooltip = readString(tPtr);
      return 0;
    },
    statusBarItemSetCommand: (sHandle, cPtr) => {
      const s = get(sHandle);
      if (s) s.command = readString(cPtr);
      return 0;
    },
    statusBarItemSetBackgroundColorTheme: (sHandle, cPtr) => {
      const s = get(sHandle);
      if (!s) return 0;
      const name = readString(cPtr);
      s.backgroundColor = name.length === 0 ? undefined : new vscode.ThemeColor(name);
      return 0;
    },
    statusBarItemShow: (sHandle) => { const s = get(sHandle); if (s) s.show(); return 0; },
    statusBarItemHide: (sHandle) => { const s = get(sHandle); if (s) s.hide(); return 0; },
    statusBarItemAsDisposable: (sHandle) => sHandle, // same JS object is a Disposable

    // ── Diagnostics ────────────────────────────────────────────────
    createDiagnosticCollection: (namePtr) =>
      reg(vscode.languages.createDiagnosticCollection(readString(namePtr))),
    diagnosticCollectionClear: (cHandle) => {
      const c = get(cHandle);
      if (c) c.clear();
      return 0;
    },
    diagnosticCollectionSetForUri: (cHandle, uHandle, jsonPtr) => {
      const c = get(cHandle);
      const u = get(uHandle);
      if (!c || !u) return 1;
      let arr;
      try { arr = JSON.parse(readString(jsonPtr)); }
      catch (e) { return 2; }
      if (!Array.isArray(arr)) return 3;
      const diagnostics = arr.map((d) => {
        const range = new vscode.Range(
          d.startLine | 0, d.startCol | 0,
          d.endLine | 0, d.endCol | 0
        );
        const severity = [
          vscode.DiagnosticSeverity.Error,
          vscode.DiagnosticSeverity.Warning,
          vscode.DiagnosticSeverity.Information,
          vscode.DiagnosticSeverity.Hint,
        ][Math.max(0, Math.min(3, d.severity | 0))];
        return new vscode.Diagnostic(range, String(d.message ?? ""), severity);
      });
      c.set(u, diagnostics);
      return 0;
    },
    diagnosticCollectionAsDisposable: (cHandle) => cHandle,

    // ── Webview ────────────────────────────────────────────────────
    createWebviewPanel: (vtPtr, titlePtr, vc) => {
      const viewColumn =
        vc === 2 ? vscode.ViewColumn.Two :
        vc === 3 ? vscode.ViewColumn.Three :
        vscode.ViewColumn.One;
      return reg(vscode.window.createWebviewPanel(
        readString(vtPtr), readString(titlePtr), viewColumn, {}
      ));
    },
    webviewPanelSetHtml: (pHandle, htmlPtr) => {
      const p = get(pHandle);
      if (p) p.webview.html = readString(htmlPtr);
      return 0;
    },
    webviewPanelAsDisposable: (pHandle) => pHandle,

    // ── Clipboard ──────────────────────────────────────────────────
    clipboardWriteText: (tPtr) => {
      try {
        vscode.env.clipboard.writeText(readString(tPtr));
        return 0;
      } catch (e) {
        return 1;
      }
    },

    // ── Events ─────────────────────────────────────────────────────
    onDidSaveTextDocument: (handlerPtr) => {
      const thunk = wrapHandler(handlerPtr);
      // The vscode event ships a TextDocument; we deliberately drop it at
      // the FFI boundary (see Vscode.affine docstring). Handlers that
      // need the saved file path can call editorActiveFilePath().
      return reg(vscode.workspace.onDidSaveTextDocument(() => thunk()));
    },

    // ── Path helpers ───────────────────────────────────────────────
    pathBasename: (pPtr) => reg(require("path").basename(readString(pPtr))),
    pathJoin: (aPtr, bPtr) =>
      reg(require("path").join(readString(aPtr), readString(bPtr))),
    processPlatform: () => reg(process.platform),

    // ── ExtensionContext helpers ───────────────────────────────────
    extensionAbsolutePath: (ctxHandle, relPtr) => {
      const ctx = get(ctxHandle);
      return reg(ctx ? ctx.asAbsolutePath(readString(relPtr)) : "");
    },

    // ── Thenable resolution (issue #205) ───────────────────────────
    // The wasm guest cannot await; these let it observe a settled host
    // Thenable. thenableThen registers the guest closure (reusing the
    // #199 closure-pointer marshalling via wrapHandler) and stores the
    // settled value keyed by the Thenable handle; thenableResultJson
    // returns it JSON-encoded (same reg(string) return convention as
    // every other `-> String` extern).
    thenableThen: (tHandle, onSettlePtr) => {
      const thenable = get(tHandle);
      const cb = wrapHandler(onSettlePtr);
      if (!thenable || typeof thenable.then !== "function") {
        return reg({ dispose() {} });
      }
      Promise.resolve(thenable).then(
        (val) => { __thenableResults.set(tHandle, val); try { cb(); } catch (_e) {} },
        (err) => {
          __thenableResults.set(tHandle, { __error: String(err) });
          try { cb(); } catch (_e) {}
        }
      );
      return reg({ dispose() {} });
    },
    thenableResultJson: (tHandle) => {
      if (!__thenableResults.has(tHandle)) return reg("");
      try { return reg(JSON.stringify(__thenableResults.get(tHandle))); }
      catch (_e) { return reg(""); }
    },

    // `httpPostJson(url, body_json)` — out-of-process JSON POST for BoJ
    // cartridge calls (e.g. boj-server :7700 reposystem_run_audit). Like
    // languageClientSendRequest, we register the response Thenable in the
    // handle table and let the guest observe it via thenableThen /
    // thenableResultJson. Resolves with the parsed JSON body so
    // thenableResultJson re-serialises it consistently; a non-JSON or
    // failed response settles as { __error } (same shape thenableThen
    // uses for rejections), so the guest can branch to its fallback.
    httpPostJson: (urlPtr, bodyPtr) => {
      const url = readString(urlPtr);
      const body = readString(bodyPtr);
      const doFetch = (typeof fetch === "function")
        ? fetch(url, {
            method: "POST",
            headers: { "content-type": "application/json" },
            body,
          }).then((r) => r.json())
        : Promise.reject(new Error("fetch unavailable"));
      return reg(doFetch.catch((err) => ({ __error: String(err) })));
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
    // `LanguageClient.sendRequest(method, params)` (issue #103). `params`
    // arrives as a JSON string (the binding's synchronous extern shape);
    // we parse it, invoke the LSP request, and register the returned
    // Thenable in the handle table. The consumer awaits it on the
    // source-to-source path (the wasm path additionally needs the
    // thenable-resolution primitives — tracked in #199). An empty or
    // malformed params string is treated as no params.
    languageClientSendRequest: (cHandle, methodPtr, paramsJsonPtr) => {
      const c = get(cHandle);
      if (!c) return 0;
      const method = readString(methodPtr);
      const raw = readString(paramsJsonPtr);
      let params;
      if (raw && raw.length > 0) {
        try { params = JSON.parse(raw); } catch (_e) { params = undefined; }
      }
      const thenable = params === undefined
        ? c.sendRequest(method)
        : c.sendRequest(method, params);
      return reg(thenable);
    },
  };

  return { Vscode, VscodeLanguageClient };
};
