// SPDX-License-Identifier: MPL-2.0
// db-theory #1a — Node ESM harness for the Sqlite codegen smoke.
//
// Installs a minimal in-memory mock at `globalThis.__as_sqlite` BEFORE
// importing the compiled module, then asserts the three smoke
// functions return what the SQL-subset semantics dictate.
//
// The mock is intentionally narrow — just enough SQL parsing to drive
// the 6-extern codegen path. Production deployments swap it for a
// real adapter (Deno: `jsr:@db/sqlite`; Node: `better-sqlite3`)
// satisfying the same `__as_sqlite` contract documented in
// `lib/codegen_deno.ml :: __as_dbOpen`.

import assert from "node:assert/strict";

let nextHandle = 1;
const dbs = new Map();

const parseRowLiteral = (s) => {
  // Split on commas at top-level (no nested parens, no embedded commas
  // inside quoted strings beyond a single value).  Sufficient for the
  // smoke's CREATE / INSERT / SELECT subset.
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
    /^-?\d*\.\d+$/.test(t) ? Number(t) :
    t
  ));
};

globalThis.__as_sqlite = {
  open(path) {
    const h = nextHandle++;
    dbs.set(h, { path, tables: new Map(), schema: new Map() });
    return h;
  },
  close(h) { dbs.delete(h); },
  execute(h, sql) {
    const db = dbs.get(h);
    if (!db) throw new Error("invalid handle " + h);

    const create = sql.match(/CREATE TABLE (\w+)\s*\(([^)]+)\)/i);
    if (create) {
      const cols = create[2].split(",").map((c) => c.trim().split(/\s+/)[0]);
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
      return;
    }

    // Unknown DDL — accept silently for smoke purposes.
  },

  // Project a row (array) into a row object keyed by column name.
  _project(db, tableName, row) {
    const cols = db.schema.get(tableName) || [];
    const obj = {};
    for (let i = 0; i < cols.length; i++) obj[cols[i]] = row[i];
    return obj;
  },

  query(h, sql, params) {
    const db = dbs.get(h);
    const tbl = sql.match(/FROM\s+(\w+)/i);
    if (!tbl) return [];
    const rows = db.tables.get(tbl[1]) || [];

    // WHERE id = ? — single-column equality filter on a positional bind.
    const where = sql.match(/WHERE\s+(\w+)\s*=\s*\?/i);
    let filtered = rows;
    if (where) {
      const col = where[1];
      const cols = db.schema.get(tbl[1]) || [];
      const idx = cols.indexOf(col);
      const v = params[0];
      filtered = rows.filter((r) => r[idx] === v);
    }

    const ordered = /ORDER\s+BY/i.test(sql)
      ? [...filtered].sort((a, b) => (a[0] > b[0] ? 1 : a[0] < b[0] ? -1 : 0))
      : filtered;

    return ordered.map((r) => this._project(db, tbl[1], r));
  },

  queryOne(h, sql, params) {
    const all = this.query(h, sql, params);
    return all.length > 0 ? all[0] : null;
  },

  queryInt(h, sql, params) {
    if (/COUNT\(\*\)/i.test(sql)) {
      const tbl = sql.match(/FROM\s+(\w+)/i);
      const db = dbs.get(h);
      return (db.tables.get(tbl[1]) || []).length;
    }
    const all = this.query(h, sql, params);
    if (all.length === 0) return 0;
    const first = all[0];
    const k = Object.keys(first)[0];
    return Number(first[k]);
  },
};

const mod = await import("./sqlite_smoke.deno.js");

assert.equal(
  mod.smoke_full_lifecycle(":memory:"),
  3,
  "smoke_full_lifecycle: CREATE + 3-row INSERT + COUNT(*) returns 3",
);

const oneStr = mod.smoke_query_one(":memory:");
const one = JSON.parse(oneStr);
assert.equal(one.id, 1, "smoke_query_one: param-bound WHERE id=1 returns row with id=1");
assert.equal(one.name, "first", "smoke_query_one: row['name'] field round-trips");

const allStr = mod.smoke_query_all(":memory:");
const all = JSON.parse(allStr);
assert.equal(all.length, 3, "smoke_query_all: ORDER BY id returns 3 rows");
assert.equal(all[0].id, 1, "smoke_query_all: row[0].id = 1");
assert.equal(all[2].id, 3, "smoke_query_all: row[2].id = 3");

console.log("sqlite_smoke OK");
