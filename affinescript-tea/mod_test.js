// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
// affinescript-tea runtime tests (INT-07, issue #182).
//
// Drives the canonical TEA bridge module (emitted by the compiler:
// `affinescript tea-bridge -o tea.wasm`, see lib/tea_bridge.ml) through
// the generic runtime — proving the run loop, layout-driven model
// decode, the Linear-msg ownership annotation, and the re-entrancy guard.
//
// Run: deno test --allow-read --allow-write --allow-run affinescript-tea/mod_test.js

import { assertEquals, assertThrows } from "jsr:@std/assert@1";
import { parseTeaLayout, TeaApp } from "./mod.js";

const COMPILER = new URL(
  "../_build/default/bin/main.exe",
  import.meta.url,
).pathname;

async function bridgeWasm() {
  const out = await Deno.makeTempFile({ suffix: ".wasm" });
  const cmd = new Deno.Command(COMPILER, { args: ["tea-bridge", "-o", out] });
  const { success, stderr } = await cmd.output();
  if (!success) {
    throw new Error(
      "tea-bridge generation failed: " + new TextDecoder().decode(stderr),
    );
  }
  const bytes = await Deno.readFile(out);
  await Deno.remove(out);
  return bytes;
}

Deno.test("parseTeaLayout decodes the model layout", async () => {
  const mod = await WebAssembly.compile(await bridgeWasm());
  const layout = parseTeaLayout(mod);
  assertEquals(layout.version, 1);
  assertEquals(layout.baseAddr, 64);
  assertEquals(layout.fields, [
    { name: "screen_w", offset: 0, type: "i32" },
    { name: "screen_h", offset: 4, type: "i32" },
    { name: "bgm_playing", offset: 8, type: "i32" },
    { name: "selected", offset: 12, type: "i32" },
  ]);
});

Deno.test("ownership marks update's msg Linear", async () => {
  const app = await TeaApp.load(await bridgeWasm());
  // fn 1 = affinescript_update(msg) — msg consumed exactly once / cycle.
  const update = app.ownership.find((e) => e.funcIdx === 1);
  assertEquals(update.paramKinds, ["linear"]);
});

Deno.test("init yields the default model", async () => {
  const app = await TeaApp.load(await bridgeWasm());
  assertEquals(app.init(), {
    screen_w: 1280,
    screen_h: 720,
    bgm_playing: 0,
    selected: 0,
  });
});

Deno.test("dispatch runs one TEA cycle (selected := msg + 1)", async () => {
  const app = await TeaApp.load(await bridgeWasm());
  app.init();
  assertEquals(app.dispatch(0).selected, 1); // NewGame
  assertEquals(app.dispatch(3).selected, 4); // Credits
});

Deno.test("setScreen updates the model", async () => {
  const app = await TeaApp.load(await bridgeWasm());
  app.init();
  const m = app.setScreen(800, 600);
  assertEquals([m.screen_w, m.screen_h], [800, 600]);
});

Deno.test("run loop: view fires after init and each message", async () => {
  const app = await TeaApp.load(await bridgeWasm());
  const seen = [];
  const final = await app.run({
    messages: [0, 1, 2, 3],
    view: (m) => seen.push(m.selected),
  });
  // init (selected 0) then msg+1 for each of 0,1,2,3.
  assertEquals(seen, [0, 1, 2, 3, 4]);
  assertEquals(final.selected, 4);
});

Deno.test("dispatch rejects a non-integer msg", async () => {
  const app = await TeaApp.load(await bridgeWasm());
  app.init();
  assertThrows(() => app.dispatch(1.5), TypeError, "i32");
});

// Minimal hand-built TEA-conformant module whose `affinescript_update`
// synchronously calls an imported `env.reenter` — the exact shape of a
// future effectful module that could re-enter the runtime. Used to
// genuinely exercise the Linear-msg re-entrancy guard.
function reentryWasm() {
  const s = (str) => {
    const b = [...new TextEncoder().encode(str)];
    return [b.length, ...b];
  };
  const sec = (id, payload) => [id, payload.length, ...payload];
  const bytes = [
    0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
    // Type: t0 ()->() , t1 (i32)->()
    ...sec(1, [0x02, 0x60, 0x00, 0x00, 0x60, 0x01, 0x7f, 0x00]),
    // Import: env.reenter : t0
    ...sec(2, [0x01, ...s("env"), ...s("reenter"), 0x00, 0x00]),
    // Function: f1=init:t0, f2=update:t1  (f0 is the import)
    ...sec(3, [0x02, 0x00, 0x01]),
    // Memory: 1 page
    ...sec(5, [0x01, 0x00, 0x01]),
    // Export: init->f1, update->f2, memory->m0
    ...sec(7, [
      0x03,
      ...s("affinescript_init"), 0x00, 0x01,
      ...s("affinescript_update"), 0x00, 0x02,
      ...s("memory"), 0x02, 0x00,
    ]),
    // Code: init = {end}; update = {call 0; end}
    ...sec(10, [
      0x02,
      0x02, 0x00, 0x0b,
      0x04, 0x00, 0x10, 0x00, 0x0b,
    ]),
    // Custom affinescript.tea_layout: version1, base0, 0 fields
    ...sec(0, [...s("affinescript.tea_layout"), 0x01, 0x00, 0x00]),
  ];
  return new Uint8Array(bytes);
}

Deno.test("re-entrant dispatch is rejected (Linear-msg invariant)", async () => {
  let app;
  // The module's update() calls env.reenter, which re-enters dispatch
  // while the first cycle is still in flight — must throw.
  app = await TeaApp.load(reentryWasm(), {
    imports: {
      reenter: () => {
        app.dispatch(1);
      },
    },
  });
  app.init();
  assertThrows(
    () => app.dispatch(0),
    Error,
    "re-entrant dispatch",
  );
});

Deno.test("a normal dispatch after another is NOT locked out", async () => {
  const app = await TeaApp.load(await bridgeWasm());
  app.init();
  app.dispatch(0);
  // guard must have cleared in `finally` — no false-positive lockout.
  assertEquals(app.dispatch(2).selected, 3);
});
