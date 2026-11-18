// SPDX-License-Identifier: MPL-2.0
import * as subject from "./url_module.bun.js";

const expect = (label, got, want) => {
  if (got !== want) throw new Error(`${label}: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`);
};
expect("relative against base", subject.resolve("../x?q#f", "https://a.example/b/c/d"), "https://a.example/b/x?q#f");
expect("no base, absolute", subject.resolve("HTTPS://A.Example:443/p", ""), "https://a.example/p");
expect("no base, relative fails", subject.resolve("/p", ""), "<none>");
expect("invalid base fails", subject.resolve("p", "not a url"), "<none>");
expect("WHATWG backslash", subject.resolve("\\x", "https://a.example/b/"), "https://a.example/x");
expect("parts", subject.path_of("http://h.example:8080/a/%7e?x=1#y"), "http:|h.example:8080|/a/%7e|?x=1|#y");
expect("parse rejects relative", subject.path_of("/a"), "<none>");
const all = (u) => { const p = subject.parts(u); return p.tag === "Some" ? JSON.stringify(p.value) : "none"; };
expect("every field, with credentials and a port", all("https://u:p@h.example:8443/a/b?q=1#f"),
  JSON.stringify({ href: "https://u:p@h.example:8443/a/b?q=1#f", protocol: "https:", username: "u", password: "p", host: "h.example:8443", hostname: "h.example", port: "8443", pathname: "/a/b", search: "?q=1", hash: "#f" }));
expect("every field, nothing optional", all("http://h.example"),
  JSON.stringify({ href: "http://h.example/", protocol: "http:", username: "", password: "", host: "h.example", hostname: "h.example", port: "", pathname: "/", search: "", hash: "" }));
expect("host URL despite a variant named URL", subject.kind_of("https://e.example/").tag, "URL");
console.log("url_module: ok");
