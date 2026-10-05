// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// tea_host.js — browser host primitives for Tea.affine.
//
// Host carve-out: this file is the *only* JavaScript in the TEA runtime. It
// implements the `extern fn`s declared in Tea.affine as plain globals (the
// Bun-ESM backend lowers an unknown extern to a same-named global call).
// Every decision — what to render, diffing, keyed moves, message ordering,
// subscription sets — is made in AffineScript; these functions only perform
// the primitive operation they are named for. Load this module before the
// compiled application module.

const g = globalThis;

/** Install `fns` as globals, refusing to silently shadow an existing one. */
function install(fns) {
  for (const [name, fn] of Object.entries(fns)) {
    if (name in g && g[name] !== fn) {
      throw new Error(`tea_host: global ${name} is already defined`);
    }
    g[name] = fn;
  }
}

// ── Mutable cells ──────────────────────────────────────────────────────────

install({
  /** A new mutable cell holding `v`. */
  tea_cell: (v) => ({ v }),
  /** The value in cell `c`. */
  tea_get: (c) => c.v,
  /** Replace the value in cell `c`. */
  tea_put: (c, v) => {
    c.v = v;
  },
});

// ── DOM ────────────────────────────────────────────────────────────────────

/** Per-node current handler per event type; one real listener per pair. */
const HANDLERS = Symbol("tea.handlers");

/** Properties that are booleans in the DOM (set from "true"/"false"). */
const BOOL_PROPS = new Set(["checked", "disabled", "selected", "readOnly", "hidden", "open"]);

install({
  /** The element matching `selector`; throws if there is none. */
  tea_query: (selector) => {
    const el = document.querySelector(selector);
    if (!el) throw new Error(`tea: no element matches ${selector}`);
    return el;
  },
  /** A new element; `ns` is "" for HTML or a namespace URI (SVG). */
  tea_create: (tag, ns) => (ns ? document.createElementNS(ns, tag) : document.createElement(tag)),
  /** A new text node. */
  tea_create_text: (s) => document.createTextNode(s),
  /** Replace a text node's content. */
  tea_set_text: (n, s) => {
    n.data = s;
  },
  /** Set an attribute (skipped when unchanged). */
  tea_set_attr: (n, k, v) => {
    if (n.getAttribute(k) !== v) n.setAttribute(k, v);
  },
  /** Remove an attribute. */
  tea_remove_attr: (n, k) => n.removeAttribute(k),
  /**
   * Set a DOM property. Boolean properties take "true"/"false"; `value` is
   * only written when it differs, so typing does not reset the caret.
   */
  tea_set_prop: (n, k, v) => {
    if (BOOL_PROPS.has(k)) {
      const b = v === "true";
      if (n[k] !== b) n[k] = b;
    } else if (n[k] !== v) {
      n[k] = v;
    }
  },
  /** Set one inline style declaration (custom properties included). */
  tea_set_style: (n, k, v) => {
    if (n.style.getPropertyValue(k) !== v) n.style.setProperty(k, v);
  },
  /** Remove one inline style declaration. */
  tea_remove_style: (n, k) => n.style.removeProperty(k),
  /** A snapshot array of a node's children. */
  tea_children: (parent) => Array.from(parent.childNodes),
  /** The child at position `i`. */
  tea_child_at: (parent, i) => parent.childNodes[i],
  /** Append `child` to `parent`. */
  tea_append: (parent, child) => {
    parent.appendChild(child);
  },
  /** Insert `child` at position `index` (appending past the end). */
  tea_insert_at: (parent, child, index) => {
    const at = parent.childNodes[index];
    if (at) at.before(child);
    else parent.append(child);
  },
  /** Insert (or move) `child` immediately before `before`. */
  tea_insert_before: (parent, child, before) => {
    if (child.nextSibling !== before || child.parentNode !== parent) before.before(child);
  },
  /** Move an existing child to position `index` unless it is already there. */
  tea_move_to: (parent, child, index) => {
    const at = parent.childNodes[index];
    if (at === child) return;
    if (at) at.before(child);
    else parent.append(child);
  },
  /** Remove `child` from `parent`. */
  tea_remove: (_parent, child) => {
    child.remove();
  },
  /** Replace `old` with `fresh` in `parent`. */
  tea_replace: (_parent, fresh, old) => {
    old.replaceWith(fresh);
  },
  /** Remove every child of `parent`. */
  tea_clear: (parent) => parent.replaceChildren(),
  /**
   * Point `n`'s handler for `event` at `h`. The DOM listener is added once
   * and forwards to the current handler, so re-rendering only swaps a slot.
   */
  tea_set_handler: (n, event, h) => {
    const slots = n[HANDLERS] ?? (n[HANDLERS] = new Map());
    if (!slots.has(event)) {
      n.addEventListener(event, (e) => {
        const current = slots.get(event);
        if (current) current(e);
      });
    }
    slots.set(event, h);
  },
  /** Stop handling `event` on `n`. */
  tea_clear_handler: (n, event) => {
    n[HANDLERS]?.set(event, null);
  },
});

// ── Scheduling and subscriptions ───────────────────────────────────────────

/** Live subscriptions by key: { kind, arg, h, stop }. */
const SUBS = new Map();

/** Start a host source for a subscription; returns its stop function. */
function start(sub) {
  switch (sub.kind) {
    case "window": {
      const listener = (e) => sub.h(e);
      window.addEventListener(sub.arg, listener);
      return () => window.removeEventListener(sub.arg, listener);
    }
    case "frame": {
      let id = requestAnimationFrame(function tick(t) {
        sub.h({ timeStamp: t });
        id = requestAnimationFrame(tick);
      });
      return () => cancelAnimationFrame(id);
    }
    case "every": {
      const id = setInterval(() => sub.h({ timeStamp: performance.now() }), Number(sub.arg));
      return () => clearInterval(id);
    }
    default:
      throw new Error(`tea: unknown subscription kind ${sub.kind}`);
  }
}

install({
  /** Run `f` before the next paint. */
  tea_next_frame: (f) => {
    requestAnimationFrame(() => f());
  },
  /** Run `f` after the current task (a microtask). */
  tea_defer: (f) => {
    queueMicrotask(() => f());
  },
  /** Ensure subscription `key` is live with handler `h`. */
  tea_sub_set: (key, kind, arg, h) => {
    const live = SUBS.get(key);
    if (live && live.kind === kind && live.arg === arg) {
      live.h = h;
      return;
    }
    if (live) live.stop();
    const sub = { kind, arg, h, stop: () => {} };
    sub.stop = start(sub);
    SUBS.set(key, sub);
  },
  /** End subscription `key` if it is live. */
  tea_sub_clear: (key) => {
    SUBS.get(key)?.stop();
    SUBS.delete(key);
  },
  /** Viewport width in CSS pixels. */
  tea_viewport_width: () => window.innerWidth,
  /** Viewport height in CSS pixels. */
  tea_viewport_height: () => window.innerHeight,
  /** Report a runtime problem (never thrown into the app). */
  tea_report: (context, message) => console.error(`[tea] ${context}: ${message}`),
});

/** Pixels per wheel `deltaMode` unit: pixel, line, page. */
const WHEEL_UNIT = [1, 16, 800];

// ── Events ─────────────────────────────────────────────────────────────────

install({
  /** `event.target.value` ("" when absent). */
  ev_value: (e) => e.target?.value ?? "",
  /** `event.target.checked`. */
  ev_checked: (e) => Boolean(e.target?.checked),
  /** `event.key` ("" when absent). */
  ev_key: (e) => e.key ?? "",
  /** Pointer x in viewport coordinates. */
  ev_client_x: (e) => e.clientX ?? 0,
  /** Pointer y in viewport coordinates. */
  ev_client_y: (e) => e.clientY ?? 0,
  /** Wheel delta in pixels (line/page deltas normalised). */
  ev_delta_y: (e) => (e.deltaY ?? 0) * WHEEL_UNIT[e.deltaMode ?? 0],
  /** Mouse button. */
  ev_button: (e) => e.button ?? 0,
  /** Modifier keys. */
  ev_ctrl: (e) => Boolean(e.ctrlKey),
  ev_shift: (e) => Boolean(e.shiftKey),
  ev_meta: (e) => Boolean(e.metaKey),
  /** Event (or frame/interval) timestamp in ms. */
  ev_time: (e) => e.timeStamp ?? performance.now(),
  /** Whether the event originated on the element the handler is bound to. */
  ev_target_is_self: (e) => e.target === e.currentTarget,
  /** Pointer x relative to the handler's element (its bounding box). */
  ev_local_x: (e) => (e.clientX ?? 0) - (e.currentTarget?.getBoundingClientRect?.().left ?? 0),
  /** Pointer y relative to the handler's element (its bounding box). */
  ev_local_y: (e) => (e.clientY ?? 0) - (e.currentTarget?.getBoundingClientRect?.().top ?? 0),
  /** Whether the target is a text input, textarea, select or contenteditable. */
  ev_target_editable: (e) => {
    const t = e.target;
    if (!t?.tagName) return false;
    const tag = t.tagName.toLowerCase();
    return tag === "input" || tag === "textarea" || tag === "select" || Boolean(t.isContentEditable);
  },
  /** `preventDefault()`. */
  ev_prevent: (e) => e.preventDefault?.(),
  /** `stopPropagation()`. */
  ev_stop: (e) => e.stopPropagation?.(),
});

// ── Linear-time list helpers ───────────────────────────────────────────────

install({
  /** `xs.map(f)` without exposing the index argument to `f`. */
  tea_map: (xs, f) => xs.map((x) => f(x)),
  /** `xs.filter(keep)`. */
  tea_filter: (xs, keep) => xs.filter((x) => keep(x)),
  /** Concatenate a list of lists. */
  tea_concat: (xss) => xss.flat(1),
  /** `xs` without its first `from` elements. */
  tea_slice: (xs, from) => xs.slice(from),
  /** `[0, 1, ..., n - 1]`. */
  tea_range: (n) => Array.from({ length: n }, (_, i) => i),
  /** A fresh array of `n` copies of `v`. */
  tea_filled: (n, v) => new Array(n).fill(v),
  /** A key → first-index map. */
  tea_key_index: (keys) => {
    const m = new Map();
    keys.forEach((k, i) => {
      if (!m.has(k)) m.set(k, i);
    });
    return m;
  },
  /** Index of `k` in a key index, or -1. */
  tea_key_lookup: (ix, k) => ix.get(k) ?? -1,
  /** Set `xs[i] = v` and return `xs` (only used on runtime-private arrays). */
  tea_set_at: (xs, i, v) => {
    xs[i] = v;
    return xs;
  },
});
