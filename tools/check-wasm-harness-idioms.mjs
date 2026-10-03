// SPDX-License-Identifier: MPL-2.0
// Guard: the two WebAssembly.instantiate overloads are not interchangeable.
//
//   WebAssembly.instantiate(BufferSource, imports) -> { module, instance }
//   WebAssembly.instantiate(Module,       imports) -> Instance
//
// So `.instance` (or `const { instance } = ...`) is correct ONLY for the
// BufferSource form. Applied to the Module form it is `undefined`, which is
// how tests/codegen/test_dom_pilot_*.mjs and test_wasi_fs_combo.mjs crashed
// with `TypeError: Cannot read properties of undefined (reading 'exports')`.
//
// Usage: node tools/check-wasm-harness-idioms.mjs [root]   (root defaults to .)
import { readFileSync, readdirSync, statSync } from 'node:fs';
import { join, relative, sep } from 'node:path';

const ROOT = process.argv[2] ?? '.';
const MODULE_DECL =
  /(?:const|let|var)\s+([A-Za-z_$][\w$]*)\s*=\s*new\s+WebAssembly\.Module\s*\(/g;

function* walkMjs(dir) {
  let entries;
  try {
    entries = readdirSync(dir, { withFileTypes: true });
  } catch {
    return;
  }
  for (const e of entries) {
    const p = join(dir, e.name);
    if (e.isDirectory()) yield* walkMjs(p);
    else if (e.isFile() && e.name.endsWith('.mjs')) yield p;
  }
}

function matchingParen(src, openIdx) {
  let depth = 0;
  for (let i = openIdx; i < src.length; i++) {
    const c = src[i];
    if (c === '(') depth++;
    else if (c === ')') {
      depth--;
      if (depth === 0) return i;
    }
  }
  return -1;
}

const lineOf = (src, idx) => src.slice(0, idx).split('\n').length;

const problems = [];
const scanned = { files: 0, withModule: 0 };

for (const file of walkMjs(join(ROOT, 'tests'))) {
  scanned.files++;
  const src = readFileSync(file, 'utf8');
  const moduleVars = new Set();
  for (const m of src.matchAll(MODULE_DECL)) moduleVars.add(m[1]);
  if (moduleVars.size === 0) continue;
  scanned.withModule++;
  const rel = relative(ROOT, file).split(sep).join('/');

  for (const v of moduleVars) {
    const callRe = new RegExp(
      `WebAssembly\\.instantiate\\(\\s*${v.replace(/\$/g, '\\$')}\\b`,
      'g',
    );
    for (const m of src.matchAll(callRe)) {
      const openIdx = src.indexOf('(', m.index + 'WebAssembly.instantiate'.length);
      const closeIdx = matchingParen(src, openIdx);
      if (closeIdx < 0) continue;
      const after = src.slice(closeIdx + 1, closeIdx + 40);
      const line = lineOf(src, m.index);
      if (/^\s*\)?\s*\.\s*instance\b/.test(after)) {
        problems.push(
          `${rel}:${line}: WebAssembly.instantiate(${v}, …) is the Module overload ` +
            `(resolves to an Instance), so a trailing \`.instance\` is undefined`,
        );
        continue;
      }
      // Reverse case: destructuring `{ instance }` out of the Module overload.
      const stmtStart = Math.max(
        src.lastIndexOf(';', m.index),
        src.lastIndexOf('\n', m.index),
        0,
      );
      const head = src.slice(stmtStart, m.index);
      if (/\{\s*instance\s*(?::\s*[\w$]+\s*)?\}\s*=\s*(?:await\s*)?$/.test(head)) {
        problems.push(
          `${rel}:${line}: \`const { instance } = await WebAssembly.instantiate(${v}, …)\` ` +
            `destructures the BufferSource result shape, not the Module one`,
        );
      }
    }
  }
}

if (problems.length > 0) {
  console.error('WASM harness instantiation-idiom gate: FAILED\n');
  for (const p of problems) console.error('  ' + p);
  console.error(
    `\n${problems.length} problem(s) in ${scanned.withModule} module-creating harness(es).` +
      '\nFix: with a WebAssembly.Module argument, use the returned value directly;' +
      '\n     with a BufferSource argument, destructure `{ instance }`.' +
      '\nSee tools/check-wasm-harness-idioms.mjs for the full explanation.',
  );
  process.exit(1);
}

console.log(
  `WASM harness instantiation-idiom gate: OK ` +
    `(${scanned.files} harnesses scanned, ${scanned.withModule} create a WebAssembly.Module)`,
);
