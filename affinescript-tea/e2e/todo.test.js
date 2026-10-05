// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// Browser tests for the AffineScript TEA runtime, driven through the Todo
// reference app (../examples/todo/Todo.affine) in headless Chromium.
//
// Run: cd affinescript-tea/e2e && bun install --frozen-lockfile && bun test
// Needs the compiler built (`dune build` at the repo root) and Playwright's
// Chromium (`bunx playwright install chromium` if not cached).

import { afterAll, beforeAll, expect, test } from "bun:test";
import { mkdtempSync, copyFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { chromium } from "playwright";

const repo = new URL("../../", import.meta.url).pathname;
const teaSrc = join(repo, "affinescript-tea/src");
const site = mkdtempSync(join(tmpdir(), "tea-e2e-"));

let server;
let browser;
let page;
const consoleErrors = [];

/** Compile the Todo example to Bun-ESM into the served directory. */
function compileTodo() {
  const out = Bun.spawnSync({
    cmd: [join(repo, "_build/default/bin/main.exe"), "compile", "--bun-esm",
      join(repo, "affinescript-tea/examples/todo/Todo.affine"), "-o", join(site, "todo.bun.js")],
    cwd: join(repo, "affinescript-tea/examples/todo"),
    env: { ...process.env, AFFINESCRIPT_STDLIB: join(repo, "stdlib"), AFFINESCRIPT_PATH: teaSrc },
  });
  if (out.exitCode !== 0) {
    throw new Error(`compile failed:\n${out.stdout}\n${out.stderr}`);
  }
}

beforeAll(async () => {
  compileTodo();
  copyFileSync(join(teaSrc, "tea_host.js"), join(site, "tea_host.js"));
  writeFileSync(
    join(site, "index.html"),
    `<!doctype html><meta charset="utf-8"><title>todo</title><div id="root"></div>
<script type="module">import "./tea_host.js"; import "./todo.bun.js";</script>`,
  );
  server = Bun.serve({
    port: 0,
    fetch(req) {
      const path = new URL(req.url).pathname;
      const file = Bun.file(join(site, path === "/" ? "index.html" : path.slice(1)));
      return new Response(file);
    },
  });
  browser = await chromium.launch();
  page = await browser.newPage();
  page.on("console", (m) => {
    if (m.type() === "error") consoleErrors.push(m.text());
  });
  page.on("pageerror", (e) => consoleErrors.push(String(e)));
  await page.goto(`http://localhost:${server.port}/`);
  await page.waitForSelector("#app");
});

afterAll(async () => {
  await browser?.close();
  server?.stop(true);
});

/**
 * Poll `read` until `check(value)` passes (or 5 s elapse), then assert it.
 * Rendering is batched to the next animation frame, so DOM reads after an
 * interaction must wait for it.
 */
async function eventually(read, check) {
  const deadline = Date.now() + 5000;
  let value = await read();
  while (Date.now() < deadline) {
    try {
      check(value);
      return;
    } catch {
      await new Promise((r) => setTimeout(r, 20));
      value = await read();
    }
  }
  check(value);
}

/** Text of every item title, in DOM order. */
const titles = () => page.locator("#items .title").evaluateAll((els) => els.map((e) => e.textContent));

/** Add an item by typing it and pressing Enter. */
async function add(title) {
  await page.fill("#draft", title);
  await page.press("#draft", "Enter");
  await eventually(titles, (v) => expect(v).toContain(title));
}

test("mounts the initial view and runs the init command", async () => {
  expect(await page.textContent("h1")).toBe("Todo");
  expect(await page.textContent("#count")).toBe("0 left");
  await eventually(() => page.textContent("#notice"), (v) => expect(v).toBe("ready"));
});

test("controlled input + Enter adds an item and clears the draft", async () => {
  await add("milk");
  expect(await page.inputValue("#draft")).toBe("");
  expect(await page.textContent("#count")).toBe("1 left");
  await eventually(() => page.textContent("#notice"), (v) => expect(v).toBe("added milk"));
});

test("the add button and an empty draft", async () => {
  await page.click("#add");
  expect(await titles()).toEqual(["milk"]);
  await page.fill("#draft", "eggs");
  await page.click("#add");
  await eventually(titles, (v) => expect(v).toEqual(["milk", "eggs"]));
});

test("keyed reorder moves the existing DOM node instead of recreating it", async () => {
  await add("bread");
  await page.locator('#items li[data-id="3"]').evaluate((li) => {
    li.__marker = "kept";
  });
  await page.click('#items li[data-id="3"] .top');
  await eventually(titles, (v) => expect(v).toEqual(["bread", "milk", "eggs"]));
  const marker = await page.locator("#items li:first-child").evaluate((li) => li.__marker ?? null);
  expect(marker).toBe("kept");
});

test("toggling updates the checkbox, class and count", async () => {
  await page.click('#items li[data-id="1"] input[type=checkbox]');
  await eventually(() => page.textContent("#count"), (v) => expect(v).toBe("2 left"));
  expect(await page.getAttribute('#items li[data-id="1"]', "class")).toBe("item done");
  expect(await page.isChecked('#items li[data-id="1"] input[type=checkbox]')).toBe(true);
});

test("removing an item removes exactly its node", async () => {
  await page.click('#items li[data-id="2"] .remove');
  await eventually(titles, (v) => expect(v).toEqual(["bread", "milk"]));
  expect(await page.locator("#items li").count()).toBe(2);
});

test("a subscription starts and stops with the model", async () => {
  await page.click("#tick");
  await eventually(async () => Number(await page.textContent("#ticks")), (v) => expect(v).toBeGreaterThan(3));
  await page.click("#tick");
  await page.waitForTimeout(60);
  const stopped = Number(await page.textContent("#ticks"));
  await page.waitForTimeout(150);
  expect(Number(await page.textContent("#ticks"))).toBe(stopped);
});

test("exactly one app instance is mounted", async () => {
  expect(await page.locator("#app").count()).toBe(1);
  expect(await page.locator("h1").count()).toBe(1);
});

test("keyed diffing: random edits keep order and identity, with minimal moves", async () => {
  const result = await page.evaluate(async () => {
    const m = await import("./todo.bun.js");
    let seed = 7;
    const rand = () => {
      seed = (seed * 1103515245 + 12345) & 0x7fffffff;
      return seed / 0x7fffffff;
    };
    const vnode = (keys) => m.node("ul", [], keys.map((k) => m.node("li", [m.key(k)], [m.text(k)])));
    const host = document.createElement("div");
    document.body.appendChild(host);
    let keys = Array.from({ length: 30 }, (_, i) => `k${i}`);
    let next = 30;
    let old = vnode(keys);
    host.appendChild(m.create(old, () => {}));
    const ul = host.firstChild;
    for (const li of ul.children) li.__id = li.textContent;
    let moves = 0;
    const observer = new MutationObserver((records) => {
      for (const r of records) moves += r.addedNodes.length;
    });
    observer.observe(ul, { childList: true });
    for (let round = 0; round < 200; round++) {
      const before = new Map(Array.from(ul.children, (li) => [li.textContent, li]));
      let fresh = keys.filter(() => rand() > 0.1);
      for (let i = 0; i < 3; i++) fresh.splice(Math.floor(rand() * (fresh.length + 1)), 0, `k${next++}`);
      for (let i = 0; i < 4; i++) {
        const a = Math.floor(rand() * fresh.length);
        const b = Math.floor(rand() * fresh.length);
        [fresh[a], fresh[b]] = [fresh[b], fresh[a]];
      }
      const neu = vnode(fresh);
      m.patch(host, ul, old, neu, () => {});
      const got = Array.from(ul.children, (li) => li.textContent);
      if (JSON.stringify(got) !== JSON.stringify(fresh)) return { ok: false, round, got, fresh };
      for (const li of ul.children) {
        const prev = before.get(li.textContent);
        if (prev && prev !== li) return { ok: false, round, reason: `recreated ${li.textContent}` };
      }
      keys = fresh;
      old = neu;
    }
    observer.disconnect();
    // Moving the last item to the front needs exactly one DOM move.
    const rotated = [keys[keys.length - 1], ...keys.slice(0, -1)];
    let single = 0;
    const o2 = new MutationObserver((rs) => {
      for (const r of rs) single += r.addedNodes.length;
    });
    o2.observe(ul, { childList: true });
    m.patch(host, ul, old, vnode(rotated), () => {});
    await new Promise((r) => setTimeout(r, 0));
    o2.disconnect();
    host.remove();
    return { ok: true, moves, single };
  });
  expect(result).toMatchObject({ ok: true });
  expect(result.single).toBe(1);
});

test("no runtime errors were reported", () => {
  expect(consoleErrors).toEqual([]);
});
