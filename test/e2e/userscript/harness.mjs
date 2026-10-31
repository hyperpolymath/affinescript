// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell
//
// Userscript target — runtime harness.
//
// The compiled artefact is a single non-module ES2020 IIFE, so it is exercised
// the way a manager would run it: as a classic <script> in a page, with the
// GM_* surface that the `@grant` list implies.
//
// Two engines, same assertions:
//   * playwright (headless Chromium) — the CI path, pinned by
//     affinescript-tea/e2e; a real browser, real event dispatch.
//   * jsdom — the no-browser path for machines that cannot download Chromium.
//     It is a DOM *implementation*, not a browser: it is used to keep the
//     fixture runnable and honest locally, and run.sh says out loud which
//     engine ran.
//
// Usage: node harness.mjs <compiled.user.js> [--engine=auto|playwright|jsdom]

import { readFileSync, statSync } from "node:fs";
import { createRequire } from "node:module";
import { delimiter, resolve } from "node:path";

const require = createRequire(import.meta.url);

/**
 * Engines are optional and are never vendored into this fixture: `playwright`
 * comes from the pinned affinescript-tea/e2e package (one Chromium download
 * for the whole repo), `jsdom` from wherever the caller points us. Extra
 * module roots are searched in order after the normal resolution path.
 */
const EXTRA_MODULE_ROOTS = (process.env.AFFINESCRIPT_E2E_MODULES ?? "")
  .split(delimiter)
  .map((p) => p.trim())
  .filter((p) => p.length > 0);

function tryRequire(name) {
  try {
    return require(name);
  } catch {
    /* fall through to the extra roots */
  }
  for (const root of EXTRA_MODULE_ROOTS) {
    try {
      return require(resolve(root, name));
    } catch {
      /* keep looking */
    }
  }
  return null;
}

const SCRIPT_PATH = resolve(process.argv[2] ?? "probe.user.js");
const ENGINE = (process.argv.find((a) => a.startsWith("--engine=")) ?? "--engine=auto").split("=")[1];
/** Static artefact checks only: needs no engine, so CI can always run them. */
const STATIC_ONLY = process.argv.includes("--static-only");

// ── Budgets and expectations ────────────────────────────────────────────────

/** One file, one capsule: a userscript is pasted into a page, not bundled. */
const SIZE_BUDGET_BYTES = 24 * 1024;

/** The custom event the fixture tears itself down on. */
const TEARDOWN_EVENT = "affinescript-userscript:teardown";

/** id of the div the fixture appends. */
const PROBE_ID = "affinescript-userscript-probe";

const EXPECTED_FIXTURE_LOG = [
  "probe:div-appended",
  "probe:setting-written",
  `probe:setting=42`,
];

// ── Comment-stripping, so `no eval` in the generated header cannot trip the
//    forbidden-token scan (the artefact talks about import/export in prose).
function stripComments(source) {
  let out = "";
  let i = 0;
  const n = source.length;
  while (i < n) {
    const c = source[i];
    const next = source[i + 1];
    if (c === "/" && next === "/") {
      while (i < n && source[i] !== "\n") i += 1;
    } else if (c === "/" && next === "*") {
      i += 2;
      while (i < n && !(source[i] === "*" && source[i + 1] === "/")) i += 1;
      i += 2;
    } else if (c === '"' || c === "'" || c === "`") {
      const quote = c;
      out += c;
      i += 1;
      while (i < n) {
        if (source[i] === "\\") {
          out += source[i] + (source[i + 1] ?? "");
          i += 2;
          continue;
        }
        out += source[i];
        if (source[i] === quote) {
          i += 1;
          break;
        }
        i += 1;
      }
    } else {
      out += c;
      i += 1;
    }
  }
  return out;
}

// ── Page-side environment ───────────────────────────────────────────────────
//
// Runs *inside* the page (or against a jsdom window): installs the GM_*
// surface for the requested grant, spies on window listener add/remove, and
// keeps the observations on window.__asE2E so both engines read the same
// shape.

function installPageEnv(win, grants) {
  const state = { logs: [], calls: [], store: Object.create(null) };
  const listeners = { live: new Map(), adds: [], removes: [] };

  state.logs = [];
  win.console.log = (...args) => state.logs.push(args.map(String).join(" "));
  win.console.error = (...args) => state.logs.push("console.error: " + args.map(String).join(" "));

  // Listener spy: wrap the window's own add/remove so the fixture's balance
  // claim ("no listener survives teardown") is checkable.
  const addRef = win.addEventListener.bind(win);
  const removeRef = win.removeEventListener.bind(win);
  win.addEventListener = function (type, fn, opts) {
    listeners.adds.push(String(type));
    listeners.live.set(fn, String(type));
    return addRef(type, fn, opts);
  };
  win.removeEventListener = function (type, fn, opts) {
    listeners.removes.push(String(type));
    listeners.live.delete(fn);
    return removeRef(type, fn, opts);
  };

  if (grants === "manager") {
    win.GM_getValue = (key, fallback) =>
      Object.prototype.hasOwnProperty.call(state.store, key) ? state.store[key] : fallback;
    win.GM_setValue = (key, value) => {
      state.store[key] = value;
      state.calls.push("set:" + key);
    };
    win.GM_deleteValue = (key) => {
      delete state.store[key];
      state.calls.push("delete:" + key);
    };
    win.GM_registerMenuCommand = (label, fn) => {
      state.calls.push("menu:" + label);
      state.menuHandler = fn;
      return "native-menu-" + listeners.adds.length;
    };
    win.GM_unregisterMenuCommand = (id) => state.calls.push("unmenu:" + id);
    win.GM_addStyle = (css) => {
      const style = win.document.createElement("style");
      style.textContent = String(css);
      win.document.head.appendChild(style);
      state.calls.push("style");
      return style;
    };
    win.GM_notification = (text) => {
      state.calls.push("notify:" + text);
      return { close: () => state.calls.push("close-notification") };
    };
  }

  // A page that refuses storage access entirely (opaque origin / private mode).
  if (grants === "hostile") {
    Object.defineProperty(win, "localStorage", {
      configurable: true,
      get() {
        throw new Error("access denied by the page");
      }
    });
  }

  win.__asE2E = {
    report() {
      return {
        logs: state.logs.slice(),
        calls: state.calls.slice(),
        store: Object.assign({}, state.store),
        listeners: {
          adds: listeners.adds.slice(),
          removes: listeners.removes.slice(),
          live: listeners.live.size
        },
        probe: (() => {
          const el = win.document.getElementById(PROBE_ID);
          return el ? { text: el.textContent, tag: el.tagName } : null;
        })(),
        styles: win.document.querySelectorAll("style").length
      };
    },
    teardown() {
      win.dispatchEvent(new win.Event(TEARDOWN_EVENT));
    },
    clickMenu() {
      if (typeof state.menuHandler === "function") state.menuHandler();
    }
  };

  return true;
}

// ── Engines ─────────────────────────────────────────────────────────────────

function loadEngine(name, cache = ENGINE_CACHE) {
  if (name === "jsdom") {
    const mod = tryRequire("jsdom");
    if (mod && mod.JSDOM) {
      cache.set("jsdom", mod);
      return "jsdom";
    }
    return null;
  }
  if (name === "playwright") {
    const mod = tryRequire("playwright");
    if (mod && mod.chromium) {
      cache.set("playwright", mod);
      return "playwright";
    }
    return null;
  }
  // auto: prefer a real browser, fall back to jsdom.
  return loadEngine("playwright", cache) ?? loadEngine("jsdom", cache);
}

async function runInJsdom(jsdomMod, source, grants) {
  const dom = new jsdomMod.JSDOM("<!doctype html><html><head></head><body><p>host page</p></body></html>", {
    url: "https://affinescript.test/probe",
    runScripts: "outside-only",
    pretendToBeVisual: true
  });
  const win = dom.window;
  installPageEnv(win, grants);
  win.eval(source);
  return {
    report: async () => win.__asE2E.report(),
    teardown: async () => win.__asE2E.teardown(),
    clickMenu: async () => win.__asE2E.clickMenu(),
    close: async () => win.close()
  };
}

async function runInPlaywright(playwrightMod, source, grants) {
  const browser = await playwrightMod.chromium.launch();
  const context = await browser.newContext();
  const page = await context.newPage();
  await page.route("https://affinescript.test/**", (route) =>
    route.fulfill({
      status: 200,
      contentType: "text/html",
      body: "<!doctype html><html><head></head><body><p>host page</p></body></html>"
    })
  );
  await page.goto("https://affinescript.test/probe");
  await page.evaluate(installPageEnv, grants);
  await page.addScriptTag({ content: source });
  return {
    report: () => page.evaluate(() => window.__asE2E.report()),
    teardown: () => page.evaluate(() => window.__asE2E.teardown()),
    clickMenu: () => page.evaluate(() => window.__asE2E.clickMenu()),
    close: async () => {
      await context.close();
      await browser.close();
    }
  };
}

// ── Assertions ──────────────────────────────────────────────────────────────

const failures = [];
const ENGINE_CACHE = new Map();

function check(name, condition, detail) {
  if (condition) {
    console.log(`  ok   ${name}`);
  } else {
    failures.push(name);
    console.log(`  FAIL ${name}${detail === undefined ? "" : `\n         ${detail}`}`);
  }
}

function staticChecks(source) {
  console.log("static checks");
  const code = stripComments(source);
  const bytes = statSync(SCRIPT_PATH).size;

  check(`one file, ${bytes} bytes <= ${SIZE_BUDGET_BYTES}` , bytes <= SIZE_BUDGET_BYTES,
    `artefact is ${bytes} bytes, budget ${SIZE_BUDGET_BYTES}`);
  check("exactly one IIFE capsule", (code.match(/\(function \(\) \{/g) ?? []).length === 1,
    `found ${(code.match(/\(function \(\) \{/g) ?? []).length}`);
  check("capsule is the whole file", /\(function \(\) \{/.test(code) &&
    code.trimEnd().endsWith("})();"), "artefact does not end with `})();`");
  check("no import declarations", !/^\s*import[\s{"']/m.test(code), "found `import`");
  check("no export declarations", !/^\s*export[\s{"']/m.test(code), "found `export`");
  check("no dynamic import", !/\bimport\s*\(/.test(code), "found `import(`");
  check("no eval", !/\beval\s*\(/.test(code), "found `eval(`");
  check("no Function constructor", !/\bnew\s+Function\s*\(/.test(code), "found `new Function(`");
  check("no top-level await", !/^\s*await\b/m.test(code), "found a top-level `await`");
  check("no require()", !/\brequire\s*\(/.test(code), "found `require(`");
  check("capsule is strict mode", /"use strict";/.test(code), "missing `\"use strict\";`");
}

async function scenario(engine, mod, label, grants, source, expectations) {
  console.log(`scenario: ${label}`);
  const page = engine === "jsdom"
    ? await runInJsdom(mod, source, grants)
    : await runInPlaywright(mod, source, grants);

  try {
    const afterRun = await page.report();
    for (const line of expectations.logs) {
      check(`${label}: logs ${line}`, afterRun.logs.includes(line),
        `logs were ${JSON.stringify(afterRun.logs)}`);
    }
    check(`${label}: div appended to the host page`,
      afterRun.probe !== null && afterRun.probe.tag === "DIV",
      `probe element: ${JSON.stringify(afterRun.probe)}`);
    check(`${label}: div carries the fixture text`,
      afterRun.probe !== null && afterRun.probe.text.length > 0,
      `text: ${JSON.stringify(afterRun.probe && afterRun.probe.text)}`);
    check(`${label}: host page content untouched`, afterRun.logs.every((l) => !l.startsWith("console.error")),
      `console.error lines: ${JSON.stringify(afterRun.logs.filter((l) => l.startsWith("console.error")))}`);

    for (const [name, predicate] of Object.entries(expectations.extra ?? {})) {
      check(`${label}: ${name}`, predicate(afterRun), JSON.stringify({ logs: afterRun.logs, calls: afterRun.calls, store: afterRun.store }));
    }

    // Teardown: the once-listener releases itself, the div leaves the DOM.
    await page.teardown();
    const afterTeardown = await page.report();
    check(`${label}: div removed on teardown`, afterTeardown.probe === null,
      `probe element: ${JSON.stringify(afterTeardown.probe)}`);
    check(`${label}: teardown ran`, afterTeardown.logs.includes("probe:div-removed"),
      `logs were ${JSON.stringify(afterTeardown.logs)}`);
    const addCount = afterTeardown.listeners.adds.filter((t) => t === TEARDOWN_EVENT).length;
    const removeCount = afterTeardown.listeners.removes.filter((t) => t === TEARDOWN_EVENT).length;
    check(`${label}: listener spy saw the registration`, addCount === 1, `adds: ${addCount}`);
    check(`${label}: listener spy saw the runtime release it`, removeCount === 1, `removes: ${removeCount}`);
    check(`${label}: no listener survives teardown`, afterTeardown.listeners.live === 0,
      `live listeners: ${afterTeardown.listeners.live}`);

    if (expectations.afterTeardown) {
      for (const [name, predicate] of Object.entries(expectations.afterTeardown)) {
        check(`${label}: ${name}`, predicate(afterTeardown), JSON.stringify({ logs: afterTeardown.logs, calls: afterTeardown.calls }));
      }
    }
  } finally {
    await page.close();
  }
}

// ── Main ────────────────────────────────────────────────────────────────────

if (STATIC_ONLY) {
  staticChecks(readFileSync(SCRIPT_PATH, "utf8"));
  if (failures.length > 0) {
    console.error(`\n${failures.length} static check(s) failed`);
    process.exit(1);
  }
  console.log("\nall static userscript checks passed");
  process.exit(0);
}

const engine = loadEngine(ENGINE);
if (!engine && ENGINE !== "auto") {
  console.error(`harness: requested engine \`${ENGINE}\` is not installed`);
  process.exit(2);
}
if (!engine) {
  console.log("SKIP: no userscript runtime engine available (need `playwright` or `jsdom`)");
  process.exit(3);
}

const source = readFileSync(SCRIPT_PATH, "utf8");
console.log(`userscript harness: ${SCRIPT_PATH} via ${engine}`);

staticChecks(source);

const mod = ENGINE_CACHE.get(engine);

// 1. `@grant none`: no GM_* globals at all. Storage must fall back to
//    localStorage and the manager-only capabilities must be typed errors.
await scenario(engine, mod, "@grant none", "none", source, {
  logs: [...EXPECTED_FIXTURE_LOG, "probe:storage=localStorage"],
  extra: {
    "menu is a typed ManagerMissing error": (r) =>
      r.logs.some((l) => l.startsWith("probe:menu-unavailable=userscript: capability not granted: GM_registerMenuCommand")),
    "style is a typed ManagerMissing error": (r) =>
      r.logs.some((l) => l.startsWith("probe:style-unavailable=userscript: capability not granted: GM_addStyle")),
    "setting round-trips through localStorage": (r) =>
      r.logs.includes("probe:setting=42")
  },
  afterTeardown: {
    "no menu handle was released (there was none)": (r) => r.logs.includes("probe:menu-released")
  }
});

// 2. `@grant GM_*`: the manager surface wins over localStorage.
await scenario(engine, mod, "@grant GM_*", "manager", source, {
  logs: [...EXPECTED_FIXTURE_LOG, "probe:storage=manager", "probe:menu=registered"],
  extra: {
    "setting went to the manager store": (r) => r.store["affinescript-userscript-probe"] === "42",
    "menu command registered with the manager": (r) =>
      r.calls.some((c) => c.startsWith("menu:AffineScript probe")),
    "style injected then released": (r) =>
      r.logs.includes("probe:style=added-and-released") && r.styles === 0
  },
  afterTeardown: {
    "menu command unregistered exactly once": (r) =>
      r.calls.filter((c) => c.startsWith("unmenu:")).length === 1
  }
});

// 3. Hostile page: no manager and a localStorage that throws on access. The
//    script must report a typed capability error, not throw.
await scenario(engine, mod, "hostile page", "hostile", source, {
  logs: ["probe:div-appended"],
  extra: {
    "storage capability is reported as none": (r) => r.logs.includes("probe:storage=none"),
    "setting write is a typed error": (r) =>
      r.logs.some((l) => l.startsWith("probe:setting-write-failed=userscript: capability not granted: GM_setValue")),
    "setting read is a typed error": (r) =>
      r.logs.some((l) => l.startsWith("probe:setting-read-failed=userscript: capability not granted: GM_getValue")),
    "nothing threw out of the capsule": (r) =>
      r.logs.every((l) => !l.startsWith("console.error"))
  }
});

if (failures.length > 0) {
  console.error(`\n${failures.length} check(s) failed (engine: ${engine})`);
  for (const f of failures) console.error(`  - ${f}`);
  process.exit(1);
}
console.log(`\nall userscript fixture checks passed (engine: ${engine})`);
