// SPDX-License-Identifier: MPL-2.0
// e2e host for dom_drive.wasm — Int-handle DOM, mutation log, assertions.
import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';

let inst = null;
const readString = (ptr) => {
  const dv = new DataView(inst.exports.memory.buffer);
  const len = dv.getUint32(ptr, true);
  return new TextDecoder('utf-8').decode(
    new Uint8Array(inst.exports.memory.buffer, ptr + 4, len));
};

// handle 1 = #root
const nodes = new Map([[1, { tag: '#root', attrs: new Map(), children: [], text: null }]]);
let next = 2;
const log = [];
const mk = (n) => { const h = next++; nodes.set(h, n); return h; };

const env = {
  dom_query_selector: (p) => { const s = readString(p); log.push(`query(${s})`); return s === '#root' ? 1 : 0; },
  dom_create_element: (p) => { const t = readString(p); const h = mk({ tag: t, attrs: new Map(), children: [], text: null }); log.push(`createElement(${t})=#${h}`); return h; },
  dom_create_text_node: (p) => { const s = readString(p); const h = mk({ tag: '#text', attrs: new Map(), children: [], text: s }); log.push(`createText("${s}")=#${h}`); return h; },
  dom_append_child: (p, c) => { nodes.get(p).children.push(c); log.push(`append(#${p},#${c})`); },
  dom_replace_child: (p, o, n) => { const cs = nodes.get(p).children; const i = cs.indexOf(o); assert.ok(i >= 0, `replace: #${o} not under #${p}`); cs[i] = n; log.push(`replace(#${p},#${o}->#${n})`); },
  dom_remove_child: (p, c) => { const cs = nodes.get(p).children; const i = cs.indexOf(c); assert.ok(i >= 0, `remove: #${c} not under #${p}`); cs.splice(i, 1); log.push(`remove(#${p},#${c})`); },
  dom_child_at: (p, i) => { const c = nodes.get(p).children[i] ?? 0; log.push(`childAt(#${p},${i})=#${c}`); return c; },
  dom_set_attribute: (el, n, v) => { const name = readString(n), val = readString(v); nodes.get(el).attrs.set(name, val); log.push(`setAttr(#${el},${name}=${val})`); },
  dom_remove_attribute: (el, n) => { const name = readString(n); nodes.get(el).attrs.delete(name); log.push(`removeAttr(#${el},${name})`); },
  dom_set_text: (el, p) => { const s = readString(p); nodes.get(el).text = s; log.push(`setText(#${el},"${s}")`); },
  dom_str_eq: (a, b) => (readString(a) === readString(b) ? 1 : 0),
};

const dump = (h, d = 0) => {
  const n = nodes.get(h);
  const attrs = [...n.attrs].map(([k, v]) => ` ${k}="${v}"`).join('');
  const line = n.tag === '#text' ? `"${n.text}"` : `<${n.tag}${attrs}> #${h}`;
  return ['  '.repeat(d) + line, ...n.children.flatMap((c) => dump(c, d + 1))].join('\n');
};

const bytes = await readFile(process.argv[2]);
const { instance } = await WebAssembly.instantiate(bytes, {
  env, wasi_snapshot_preview1: { fd_write: () => 0 },
});
inst = instance;

const ret = inst.exports.main();
console.log('main() =', ret);
console.log('--- mutation log ---'); console.log(log.join('\n'));
console.log('--- final DOM under #root ---'); console.log(dump(1));

// assertions: reconcile patched in place
const root = nodes.get(1);
assert.equal(root.children.length, 1, 'root has exactly one child');
const div = nodes.get(root.children[0]);
assert.equal(div.tag, 'div');
assert.equal(div.attrs.get('id'), 'app');
assert.equal(div.attrs.get('title'), 't2', 'title added by patch_attrs');
assert.ok(!div.attrs.has('class'), 'class removed by patch_attrs');
assert.equal(div.children.length, 1, 'span child removed by while-loop reconcile');
const t = nodes.get(div.children[0]);
assert.equal(t.tag, '#text');
assert.equal(t.text, 'world', 'text updated in place');
assert.equal(ret, root.children[0], 'reconcile returned the in-place div handle');
console.log('ALL ASSERTIONS PASS — reconciler ran end-to-end (loops executed)');
