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
<script type="module">import "./tea_host.js"; import { main } from "./todo.bun.js"; main();</script>`,
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
const titles = () => page.$$eval("#items .title", (els) => els.map((e) => e.textContent));

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
  await page.$eval('#items li[data-id="3"]', (li) => {
    li.__marker = "kept";
  });
  await page.click('#items li[data-id="3"] .top');
  await eventually(titles, (v) => expect(v).toEqual(["bread", "milk", "eggs"]));
  const marker = await page.$eval("#items li:first-child", (li) => li.__marker ?? null);
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
  expect(await page.$$eval("#items li", (els) => els.length)).toBe(2);
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

test("no runtime errors were reported", () => {
  expect(consoleErrors).toEqual([]);
});
