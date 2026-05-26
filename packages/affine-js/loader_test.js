// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affine-js/loader: host-agnostic loader bridge tests (INT-02, issue #179).
//
// Run:  deno test --allow-read --allow-write packages/affine-js/loader_test.js
//
// These cover the SAT-02 fix (no more `url.pathname` path mangling), host
// detection, byte-reader parity, the multi-namespace import-object builder,
// and the `typedwasm.ownership` custom-section parser (whose binary format
// must stay byte-identical to Codegen.build_ownership_section /
// Tw_verify.parse_ownership_section_payload in the compiler).

import { assertEquals, assertThrows } from "jsr:@std/assert@1";
import {
  buildImportObject,
  detectHost,
  parseOwnershipSection,
  readBytes,
  resolveUrl,
} from "./loader.js";

Deno.test("detectHost identifies Deno by feature", () => {
  assertEquals(detectHost(), "deno");
});

Deno.test("resolveUrl: URL passthrough", () => {
  const u = new URL("https://example.test/a.wasm");
  assertEquals(resolveUrl(u), u);
});

Deno.test("resolveUrl: absolute URL string", () => {
  assertEquals(
    resolveUrl("https://example.test/a.wasm").href,
    "https://example.test/a.wasm",
  );
});

Deno.test("resolveUrl: POSIX absolute path -> file URL", () => {
  assertEquals(resolveUrl("/srv/app/x.wasm").href, "file:///srv/app/x.wasm");
});

Deno.test("resolveUrl: Windows absolute path -> file URL (the SAT-02 case)", () => {
  // The old code did `new URL(path).pathname` then `Deno.readFile`, which
  // dropped the drive letter and percent-mangled the path. This is the
  // regression guard for that.
  assertEquals(
    resolveUrl("C:\\dir\\sub\\x.wasm").href,
    "file:///C:/dir/sub/x.wasm",
  );
});

Deno.test("resolveUrl: relative needs a base", () => {
  assertThrows(() => resolveUrl("./x.wasm"), Error, "needs a base");
  assertEquals(
    resolveUrl("./x.wasm", "file:///srv/app/mod.js").href,
    "file:///srv/app/x.wasm",
  );
});

Deno.test("readBytes: Uint8Array passthrough is identity", async () => {
  const src = new Uint8Array([1, 2, 3]);
  assertEquals(await readBytes(src), src);
});

Deno.test("readBytes: ArrayBuffer -> Uint8Array", async () => {
  const ab = new Uint8Array([4, 5, 6]).buffer;
  assertEquals(await readBytes(ab), new Uint8Array([4, 5, 6]));
});

Deno.test("readBytes: file URL and relative spec parity", async () => {
  const dir = await Deno.makeTempDir();
  const path = `${dir}/blob.bin`;
  const payload = new Uint8Array([0xde, 0xad, 0xbe, 0xef]);
  await Deno.writeFile(path, payload);
  try {
    const viaFileUrl = await readBytes(`file://${path}`);
    assertEquals(viaFileUrl, payload);
    const viaRelative = await readBytes("./blob.bin", {
      base: `file://${dir}/anchor.js`,
    });
    assertEquals(viaRelative, payload);
  } finally {
    await Deno.remove(dir, { recursive: true });
  }
});

Deno.test("buildImportObject: legacy env merge is preserved", () => {
  const rt = { affine_io_println: () => {} };
  const extra = () => 1;
  const obj = buildImportObject(rt, { imports: { custom: extra } });
  assertEquals(obj.env.affine_io_println, rt.affine_io_println);
  assertEquals(obj.env.custom, extra);
});

Deno.test("buildImportObject: cross-module namespaces (INT-01)", () => {
  const fn = () => 7;
  const obj = buildImportObject({}, { modules: { Mod: { helper: fn } } });
  assertEquals(obj.Mod.helper, fn);
  assertEquals(typeof obj.env, "object");
});

Deno.test("buildImportObject: modules.env merges, does not clobber", () => {
  const rt = { runtime_fn: () => {} };
  const guest = () => {};
  const obj = buildImportObject(rt, { modules: { env: { guest } } });
  assertEquals(obj.env.runtime_fn, rt.runtime_fn);
  assertEquals(obj.env.guest, guest);
});

// ── ownership custom-section parser ───────────────────────────────────────────

/** Build a minimal valid WASM module carrying one custom section. */
function wasmWithCustomSection(name, payload) {
  const enc = new TextEncoder();
  const nameBytes = enc.encode(name);
  // section content = uleb(nameLen) + name + payload
  const content = [
    ...uleb(nameBytes.length),
    ...nameBytes,
    ...payload,
  ];
  return new Uint8Array([
    0x00, 0x61, 0x73, 0x6d, // \0asm
    0x01, 0x00, 0x00, 0x00, // version 1
    0x00, // custom section id
    ...uleb(content.length),
    ...content,
  ]);
}

function uleb(n) {
  const out = [];
  do {
    let b = n & 0x7f;
    n >>>= 7;
    if (n !== 0) b |= 0x80;
    out.push(b);
  } while (n !== 0);
  return out;
}

function u32le(n) {
  return [n & 0xff, (n >>> 8) & 0xff, (n >>> 16) & 0xff, (n >>> 24) & 0xff];
}

Deno.test("parseOwnershipSection: round-trips the compiler's binary format", () => {
  // One entry: func_idx=2, params=[Unrestricted,Linear,SharedBorrow,ExclBorrow],
  // ret=Linear.
  const payload = [
    ...u32le(1), // count
    ...u32le(2), // func_idx
    4, // n_params
    0,
    1,
    2,
    3,
    1, // ret_kind
  ];
  const bytes = wasmWithCustomSection("typedwasm.ownership", payload);
  const mod = new WebAssembly.Module(bytes);
  assertEquals(parseOwnershipSection(mod), [
    {
      funcIdx: 2,
      paramKinds: ["unrestricted", "linear", "sharedBorrow", "exclBorrow"],
      retKind: "linear",
    },
  ]);
});

Deno.test("parseOwnershipSection: absent section -> []", () => {
  const bytes = wasmWithCustomSection("something.else", [0]);
  const mod = new WebAssembly.Module(bytes);
  assertEquals(parseOwnershipSection(mod), []);
});
