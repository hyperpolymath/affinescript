// SPDX-License-Identifier: MPL-2.0
// SPDX-FileCopyrightText: 2026 hyperpolymath
//
// router_host.js — browser primitives for Router.affine (host carve-out: the
// History/Location API calls the AffineScript module declares as externs).
// Load after tea_host.js and before the compiled app.

/** Split a route string ("/a/b?q=1#h") into Router.affine's `Url` record. */
function toUrl(route) {
  const hashAt = route.indexOf("#");
  const beforeHash = hashAt < 0 ? route : route.slice(0, hashAt);
  const hash = hashAt < 0 ? "" : route.slice(hashAt + 1);
  const queryAt = beforeHash.indexOf("?");
  const path = queryAt < 0 ? beforeHash : beforeHash.slice(0, queryAt);
  const query = queryAt < 0 ? "" : beforeHash.slice(queryAt + 1);
  return { path: path.startsWith("/") ? path : `/${path}`, query, hash };
}

/** The history-API URL for `href` in the given mode. */
const target = (hashMode, href) => (hashMode ? `${location.pathname}${location.search}#${href}` : href);

const fns = {
  /** The current location; in hash mode, the route after `#`. */
  router_location: (hashMode) =>
    hashMode ? toUrl(location.hash.slice(1) || "/") : toUrl(`${location.pathname}${location.search}${location.hash}`),
  /** Add a history entry (no event fires; Router.navigate dispatches). */
  router_push: (hashMode, href) => history.pushState(null, "", target(hashMode, href)),
  /** Replace the current history entry. */
  router_replace: (hashMode, href) => history.replaceState(null, "", target(hashMode, href)),
  /** Go back one entry (fires popstate). */
  router_back: () => history.back(),
  /** decodeURIComponent, tolerant of malformed input. */
  router_decode: (s) => {
    try {
      return decodeURIComponent(s);
    } catch {
      return s;
    }
  },
  /** encodeURIComponent. */
  router_encode: (s) => encodeURIComponent(s),
};

for (const [name, fn] of Object.entries(fns)) {
  if (name in globalThis) throw new Error(`router_host: global ${name} is already defined`);
  globalThis[name] = fn;
}
