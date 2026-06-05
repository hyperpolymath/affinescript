// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
// SPDX-FileCopyrightText: 2025-2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// evangelist.mjs -- the nextgen-languages-evangelist orchestrator.
//
// Walks a target tree and runs the component tools to produce one
// "next-gen readiness report":
//   * every .res  -> affine-migratability (migratable now? which wall?)
//   * every .affine -> affine-assail      (weak points: clamps, decoders)
//   * (optional manifest) per .affine     -> echo-boundary (proof) + affine-parity (parity)
//
// Trial-here principle: this composes the *verified* component tools; run it
// on a small target and read the report. No registries (github-only firewall):
// it shells out to `deno` running the sibling component .mjs files only.
//
// Usage:
//   deno run --allow-read --allow-run evangelist.mjs <target-dir> [manifest.json]
//
// manifest.json (optional) lets the proof/parity steps run (they need a table
// / oracle that can't be inferred from source):
//   {
//     "boundary": [{ "name": "SecurityRank", "table": {"Open":0,"Weak":1,"Medium":2,"Strong":3} }],
//     "parity":   [{ "name": "SecurityRank", "config": "../affine-parity/securityrank.config.mjs" }]
//   }

const HERE = new URL(".", import.meta.url).pathname;
const COMP = {
  migratability: HERE + "../affine-migratability/migratability.mjs",
  assail:        HERE + "../affine-assail/assail.mjs",
  boundary:      HERE + "../echo-boundary/boundary.mjs",
  parity:        HERE + "../affine-parity/parity.mjs",
};
const SKIP_DIRS = new Set(["node_modules", ".git", "lib", "_build", ".deno"]);

async function run(cmd, args) {
  try {
    const p = new Deno.Command(cmd, { args, stdout: "piped", stderr: "piped" });
    const { code, stdout, stderr } = await p.output();
    return { code, out: new TextDecoder().decode(stdout), err: new TextDecoder().decode(stderr) };
  } catch (e) {
    return { code: 127, out: "", err: String(e) };
  }
}

// Tolerant: pull the last line that parses as a JSON object, else null.
function lastJson(s) {
  const lines = (s || "").trim().split("\n");
  for (let i = lines.length - 1; i >= 0; i--) {
    const t = lines[i].trim();
    if (t.startsWith("{")) { try { return JSON.parse(t); } catch { /* keep looking */ } }
  }
  return null;
}

async function* walk(dir) {
  let entries;
  try { entries = Deno.readDir(dir); } catch { return; }
  for await (const e of entries) {
    const p = `${dir}/${e.name}`;
    if (e.isDirectory) { if (!SKIP_DIRS.has(e.name)) yield* walk(p); }
    else yield p;
  }
}

const target = Deno.args[0];
if (!target) { console.error("usage: deno run --allow-read --allow-run evangelist.mjs <target-dir> [manifest.json]"); Deno.exit(2); }
const manifest = Deno.args[1] ? JSON.parse(await Deno.readTextFile(Deno.args[1])) : {};

const resFiles = [], affFiles = [];
for await (const f of walk(target)) {
  if (f.endsWith(".res")) resFiles.push(f);
  else if (f.endsWith(".affine")) affFiles.push(f);
}

const report = [];
report.push(`# Next-gen readiness report`);
report.push(``);
report.push(`Target: \`${target}\`  |  ${resFiles.length} .res, ${affFiles.length} .affine`);
report.push(``);

// 1. Migratability triage of every ReScript kernel.
report.push(`## ① ReScript migratability (.res)`);
report.push(``);
report.push(`| file | verdict | walls |`);
report.push(`|---|---|---|`);
for (const f of resFiles) {
  const r = await run("deno", ["run", "--allow-read", "--allow-run", "--allow-write", COMP.migratability, f]);
  const j = lastJson(r.out);
  const tok = (r.out.match(/STRING-GATED|EFFECT-GATED|MIGRATABLE NOW/) || [])[0];
  const verdict = j?.verdict ?? tok ?? (r.code === 0 ? "ran" : `err(${r.code})`);
  const walls = (j?.walls?.length ? j.walls : (j?.notes ?? [])).join("; ");
  report.push(`| \`${f.replace(target, ".")}\` | ${verdict} | ${walls} |`);
}
report.push(``);

// 2. Weak-point scan of every AffineScript core.
report.push(`## ② AffineScript weak points (.affine)`);
report.push(``);
report.push(`| file | findings |`);
report.push(`|---|---|`);
for (const f of affFiles) {
  const r = await run("deno", ["run", "--allow-read", "--allow-run", "--allow-write", COMP.assail, f]);
  const j = lastJson(r.out);
  const n = j?.findings?.length ?? (r.code === 0 ? 0 : "?");
  const kinds = j?.findings ? [...new Set(j.findings.map((x) => x.rule))].join(", ") : "";
  report.push(`| \`${f.replace(target, ".")}\` | ${n}${kinds ? " — " + kinds : ""} |`);
}
report.push(``);

// 3. Boundary proofs (manifest-driven; these emit machine-checked Agda).
if (manifest.boundary?.length) {
  report.push(`## ③ Boundary faithfulness proofs (echo-boundary)`);
  report.push(``);
  report.push(`| encoding | verdict |`);
  report.push(`|---|---|`);
  for (const b of manifest.boundary) {
    const r = await run("deno", ["run", "--allow-read", "--allow-run", "--allow-write", COMP.boundary, JSON.stringify({ table: b.table, clamp: b.clamp })]);
    const j = lastJson(r.out);
    const tok = (r.out.match(/CONTROLLED LOSS|LOSSLESS/) || [])[0];
    const verdict = j?.verdict ?? tok ?? (r.code === 0 ? "checked" : `err(${r.code})`);
    report.push(`| ${b.name} | ${verdict} |`);
  }
  report.push(``);
}

// 4. Differential parity (manifest-driven configs).
if (manifest.parity?.length) {
  report.push(`## ④ Differential parity (affine-parity)`);
  report.push(``);
  report.push(`| kernel | result |`);
  report.push(`|---|---|`);
  for (const p of manifest.parity) {
    const r = await run("deno", ["run", "--allow-read", "--allow-run", "--allow-write", COMP.parity, p.config]);
    const all = [...r.out.matchAll(/(\d+\/\d+)\s*pass/g)];
    const m = all.length ? `${all[all.length - 1][1]} pass` : (r.code === 0 ? "pass" : `err(${r.code})`);
    report.push(`| ${p.name} | ${m} |`);
  }
  report.push(``);
}

report.push(`---`);
report.push(`_Generated by the nextgen-languages-evangelist orchestrator._`);
console.log(report.join("\n"));
