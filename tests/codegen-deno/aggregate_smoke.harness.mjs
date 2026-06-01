// SPDX-License-Identifier: MPL-2.0
// db-theory #3 — Node ESM harness for the Aggregate codegen smoke.
//
// Extends the #1a/#1b/#1c/#2 mock with aggregation surface:
// aggCount / aggSum / aggMinInt / aggMaxInt / aggAvg / groupBy /
// groupCount. The SQL parser handles a tiny subset sufficient for
// the smoke (COUNT/SUM/MIN/MAX/AVG single-row + GROUP BY with
// COUNT/SUM in the SELECT list).

import assert from "node:assert/strict";

let nextDbHandle = 1;
const dbs = new Map();

const parseRowLiteral = (s) => {
  const out = [];
  let buf = "";
  let inStr = false;
  for (const ch of s) {
    if (ch === "'") { inStr = !inStr; buf += ch; }
    else if (ch === "," && !inStr) { out.push(buf.trim()); buf = ""; }
    else buf += ch;
  }
  if (buf.trim()) out.push(buf.trim());
  return out.map((t) => (
    t.startsWith("'") && t.endsWith("'") ? t.slice(1, -1) :
    /^-?\d+$/.test(t) ? Number(t) :
    t
  ));
};

globalThis.__as_sqlite = {
  open(path) {
    const h = nextDbHandle++;
    dbs.set(h, { path, tables: new Map(), schema: new Map() });
    return h;
  },
  close(h) { dbs.delete(h); },

  execute(h, sql) {
    const db = dbs.get(h);
    if (!db) throw new Error("invalid db handle " + h);
    const create = sql.match(/CREATE TABLE (\w+)\s*\(([^)]+)\)/i);
    if (create) {
      const cols = create[2].split(",").map((c) => {
        const parts = c.trim().split(/\s+/);
        return { name: parts[0], type: parts[1] || "" };
      });
      db.tables.set(create[1], []);
      db.schema.set(create[1], cols);
      return;
    }
    const insert = sql.match(/INSERT INTO (\w+)\s+VALUES\s+(.+)/i);
    if (insert) {
      const tableName = insert[1];
      const valuesPart = insert[2].replace(/;$/, "").trim();
      const tupleRe = /\(([^)]+)\)/g;
      let m;
      while ((m = tupleRe.exec(valuesPart))) {
        const cols = parseRowLiteral(m[1]);
        const tbl = db.tables.get(tableName);
        if (!tbl) throw new Error("no such table " + tableName);
        tbl.push(cols);
      }
    }
  },

  // ── Aggregation (db-theory #3) ────────────────────────────────────

  aggCount(h, sql /* , _params */) {
    const db = dbs.get(h);
    if (!db) return 0;
    const m = sql.match(/FROM\s+(\w+)/i);
    if (!m) return 0;
    return (db.tables.get(m[1]) || []).length;
  },

  aggSum(h, sql) {
    const db = dbs.get(h);
    if (!db) return 0;
    const fromM = sql.match(/FROM\s+(\w+)/i);
    const colM  = sql.match(/SUM\(\s*(\w+)\s*\)/i);
    if (!fromM || !colM) return 0;
    const rows = db.tables.get(fromM[1]) || [];
    const cols = db.schema.get(fromM[1]) || [];
    const idx  = cols.findIndex((c) => c.name === colM[1]);
    if (idx === -1) return 0;
    return rows.reduce((acc, r) => acc + Number(r[idx]), 0);
  },

  aggMinInt(h, sql) {
    const db = dbs.get(h);
    if (!db) return 0;
    const fromM = sql.match(/FROM\s+(\w+)/i);
    const colM  = sql.match(/MIN\(\s*(\w+)\s*\)/i);
    if (!fromM || !colM) return 0;
    const rows = db.tables.get(fromM[1]) || [];
    const cols = db.schema.get(fromM[1]) || [];
    const idx  = cols.findIndex((c) => c.name === colM[1]);
    if (idx === -1) return 0;
    if (rows.length === 0) return 0;
    return rows.reduce((acc, r) => Math.min(acc, Number(r[idx])), Number(rows[0][idx]));
  },

  aggMaxInt(h, sql) {
    const db = dbs.get(h);
    if (!db) return 0;
    const fromM = sql.match(/FROM\s+(\w+)/i);
    const colM  = sql.match(/MAX\(\s*(\w+)\s*\)/i);
    if (!fromM || !colM) return 0;
    const rows = db.tables.get(fromM[1]) || [];
    const cols = db.schema.get(fromM[1]) || [];
    const idx  = cols.findIndex((c) => c.name === colM[1]);
    if (idx === -1) return 0;
    if (rows.length === 0) return 0;
    return rows.reduce((acc, r) => Math.max(acc, Number(r[idx])), Number(rows[0][idx]));
  },

  aggAvg(h, sql) {
    const db = dbs.get(h);
    if (!db) return 0;
    const fromM = sql.match(/FROM\s+(\w+)/i);
    const colM  = sql.match(/AVG\(\s*(\w+)\s*\)/i);
    if (!fromM || !colM) return 0;
    const rows = db.tables.get(fromM[1]) || [];
    const cols = db.schema.get(fromM[1]) || [];
    const idx  = cols.findIndex((c) => c.name === colM[1]);
    if (idx === -1 || rows.length === 0) return 0;
    const sum = rows.reduce((acc, r) => acc + Number(r[idx]), 0);
    return sum / rows.length;
  },

  groupBy(h, sql /* , _params */) {
    const db = dbs.get(h);
    if (!db) return "[]";
    const fromM = sql.match(/FROM\s+(\w+)/i);
    const grpM  = sql.match(/GROUP\s+BY\s+(\w+)/i);
    if (!fromM || !grpM) return "[]";
    const rows = db.tables.get(fromM[1]) || [];
    const cols = db.schema.get(fromM[1]) || [];
    const keyIdx = cols.findIndex((c) => c.name === grpM[1]);
    if (keyIdx === -1) return "[]";

    // Bucket rows by key.
    const buckets = new Map();
    for (const r of rows) {
      const k = r[keyIdx];
      if (!buckets.has(k)) buckets.set(k, []);
      buckets.get(k).push(r);
    }

    // SELECT list parse: pull the comma-separated expressions out of
    // `SELECT ... FROM`. Each expr is either a column-name or an
    // aggregate function call.
    const selM = sql.match(/SELECT\s+(.+?)\s+FROM/i);
    const selectExprs = selM[1].split(",").map((e) => e.trim());

    const evalExpr = (expr, bucket) => {
      // Aggregate? e.g. COUNT(*) | SUM(col)
      const fn = expr.match(/^(COUNT|SUM|MIN|MAX|AVG)\(\s*([\w*]+)\s*\)$/i);
      if (fn) {
        const op = fn[1].toUpperCase();
        if (op === "COUNT") return bucket.length;
        const colName = fn[2];
        const ci = cols.findIndex((c) => c.name === colName);
        if (ci === -1) return 0;
        const vals = bucket.map((r) => Number(r[ci]));
        if (op === "SUM") return vals.reduce((a, b) => a + b, 0);
        if (op === "MIN") return Math.min(...vals);
        if (op === "MAX") return Math.max(...vals);
        if (op === "AVG") return vals.reduce((a, b) => a + b, 0) / vals.length;
      }
      // Plain column: pick from any bucket row (group key is uniform within bucket).
      const ci = cols.findIndex((c) => c.name === expr);
      if (ci === -1) return null;
      return bucket[0][ci];
    };

    const out = [...buckets.entries()].map(([_k, bucket]) =>
      selectExprs.map((e) => evalExpr(e, bucket))
    );
    return JSON.stringify(out);
  },

  groupCount(h, table, keyCol) {
    const db = dbs.get(h);
    if (!db) return "[]";
    const rows = db.tables.get(table) || [];
    const cols = db.schema.get(table) || [];
    const idx = cols.findIndex((c) => c.name === keyCol);
    if (idx === -1) return "[]";
    const buckets = new Map();
    for (const r of rows) {
      const k = r[idx];
      buckets.set(k, (buckets.get(k) ?? 0) + 1);
    }
    return JSON.stringify([...buckets.entries()].map(([k, n]) => [k, n]));
  },

  // Compat stubs for stack-sibling harnesses.
  query() { return []; }, queryOne() { return null; }, queryInt() { return 0; },
  prepare() { throw new Error("prepare not used here"); },
  bindInt() {}, bindText() {}, bindNull() {},
  step() { return false }, columnCount() { return 0 },
  columnInt() { return 0 }, columnText() { return "" },
  reset() {}, finalize() {},
  schemaTables() { return "[]"; }, schemaColumns() { return "[]"; },
  tableExists() { return false; }, importCsv() { return 0; },
  exportCsv() { return 0; }, lastError() { return ""; },
  txBegin() { return 0; }, txCommit() {}, txRollback() {},
  txSavepoint() {}, txRelease() {}, txRollbackTo() {},
  txDb() { return 0; }, txIsLive() { return false; },
};

const mod = await import("./aggregate_smoke.deno.js");

// ── COUNT ───────────────────────────────────────────────────────────
assert.equal(mod.smoke_count(":memory:"), 5, "smoke_count: 5 rows inserted");

// ── SUM ─────────────────────────────────────────────────────────────
assert.equal(mod.smoke_sum(":memory:"), 15, "smoke_sum: 1+2+3+4+5 = 15");

// ── MIN / MAX ──────────────────────────────────────────────────────
assert.equal(mod.smoke_min_int(":memory:"), 1, "smoke_min_int: min(7,3,9,1,5) = 1");
assert.equal(mod.smoke_max_int(":memory:"), 9, "smoke_max_int: max(7,3,9,1,5) = 9");

// ── AVG ────────────────────────────────────────────────────────────
assert.equal(mod.smoke_avg(":memory:"), 20.0, "smoke_avg: avg(10,20,30) = 20.0");

// ── GROUP BY ────────────────────────────────────────────────────────
const groupJson = mod.smoke_group_by(":memory:");
const groups = JSON.parse(groupJson);
assert.equal(groups.length, 2, "smoke_group_by: 2 dept buckets");
// Sort to make assertion order-independent.
groups.sort((a, b) => String(a[0]).localeCompare(String(b[0])));
assert.deepEqual(groups[0], ["eng", 2, 300], "eng bucket: 2 rows / sum salary = 300");
assert.deepEqual(groups[1], ["sales", 2, 125], "sales bucket: 2 rows / sum salary = 125");

// ── GROUP COUNT ─────────────────────────────────────────────────────
const gcJson = mod.smoke_group_count(":memory:");
const gc = JSON.parse(gcJson);
gc.sort((a, b) => String(a[0]).localeCompare(String(b[0])));
assert.deepEqual(gc, [["a", 3], ["b", 2], ["c", 1]], "smoke_group_count: a×3, b×2, c×1");

console.log("aggregate_smoke OK");
