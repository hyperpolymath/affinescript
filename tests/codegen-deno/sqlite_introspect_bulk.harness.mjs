// SPDX-License-Identifier: MPL-2.0
// db-theory #1c — Node ESM harness for the Sqlite schema-introspection +
// bulk-I/O + error-inspection codegen smoke.
//
// Extends the #1a/#1b mock adapter with `schemaTables`, `schemaColumns`,
// `tableExists`, `importCsv`, `exportCsv`, `lastError`. The csv methods
// here use a virtual filesystem map keyed by path so the smoke runs
// hermetically (no real disk I/O); production adapters back the same
// methods with `Deno.readTextFileSync` / `fs.writeFileSync`.

import assert from "node:assert/strict";

let nextDbHandle = 1;
const dbs = new Map();
const vfs = new Map();   // path -> string contents
const errors = new Map(); // db handle -> last error message

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
    errors.set(h, "");
    return h;
  },
  close(h) { dbs.delete(h); errors.delete(h); },

  execute(h, sql) {
    const db = dbs.get(h);
    if (!db) throw new Error("invalid db handle " + h);

    // Fault-injection convention for the smoke: a literal SQL string
    // starting with `RAISE` records a last-error and returns without
    // throwing, so the smoke can validate the read-back path.
    if (/^\s*RAISE\s+'(.+)'\s*$/i.test(sql)) {
      const m = sql.match(/^\s*RAISE\s+'(.+)'\s*$/i);
      errors.set(h, m[1]);
      return;
    }

    const create = sql.match(/CREATE TABLE (\w+)\s*\(([^)]+)\)/i);
    if (create) {
      const cols = create[2].split(",").map((c) => {
        const parts = c.trim().split(/\s+/);
        return { name: parts[0], type: parts[1] || "" };
      });
      db.tables.set(create[1], []);
      db.schema.set(create[1], cols);
      errors.set(h, "");
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
        if (!tbl) { errors.set(h, "no such table " + tableName); throw new Error(errors.get(h)); }
        tbl.push(cols);
      }
      errors.set(h, "");
    }
  },

  // ── Schema introspection ──────────────────────────────────────────

  schemaTables(h) {
    const db = dbs.get(h);
    if (!db) return "[]";
    const names = [...db.tables.keys()].filter((n) => !n.startsWith("sqlite_"));
    return JSON.stringify(names);
  },

  schemaColumns(h, table) {
    const db = dbs.get(h);
    if (!db) return "[]";
    const cols = db.schema.get(table);
    if (!cols) return "[]";
    return JSON.stringify(cols.map((c, i) => ({
      name: c.name,
      type: c.type,
      notnull: false,
      pk: i === 0 && /id/i.test(c.name),
    })));
  },

  tableExists(h, table) {
    const db = dbs.get(h);
    if (!db) return false;
    return db.tables.has(table) && !table.startsWith("sqlite_");
  },

  // ── Bulk I/O ──────────────────────────────────────────────────────

  importCsv(h, table, csvPath, hasHeader) {
    const db = dbs.get(h);
    if (!db) { errors.set(h, "invalid handle"); return 0; }
    const text = vfs.get(csvPath);
    if (text == null) { errors.set(h, "no such csv " + csvPath); return 0; }
    const tbl = db.tables.get(table);
    if (!tbl) { errors.set(h, "no such table " + table); return 0; }
    const cols = db.schema.get(table) || [];
    const lines = text.split(/\r?\n/).filter((l) => l.length > 0);
    const data = hasHeader ? lines.slice(1) : lines;
    let inserted = 0;
    for (const line of data) {
      const fields = line.split(",").map((f) => {
        const t = f.trim();
        return /^-?\d+$/.test(t) ? Number(t) : t;
      });
      // Pad/truncate to schema width.
      while (fields.length < cols.length) fields.push(null);
      tbl.push(fields.slice(0, cols.length));
      inserted++;
    }
    errors.set(h, "");
    return inserted;
  },

  exportCsv(h, sql, params, csvPath) {
    const db = dbs.get(h);
    if (!db) { errors.set(h, "invalid handle"); return 0; }
    const tblMatch = sql.match(/FROM\s+(\w+)/i);
    if (!tblMatch) { errors.set(h, "no FROM in sql"); return 0; }
    const table = tblMatch[1];
    const rows = db.tables.get(table) || [];
    const schema = db.schema.get(table) || [];
    const header = schema.map((c) => c.name).join(",");
    const body = rows.map((r) => r.map((v) => v == null ? "" : String(v)).join(",")).join("\n");
    vfs.set(csvPath, header + "\n" + body + "\n");
    errors.set(h, "");
    return rows.length;
  },

  lastError(h) { return errors.get(h) ?? ""; },

  // Convenience surface methods present from the #1a/#1b mock — not
  // exercised in this smoke but kept for future stack-on harnesses.
  query() { return []; },
  queryOne() { return null; },
  queryInt() { return 0; },
  prepare() { throw new Error("prepare not used in this smoke"); },
  bindInt() {}, bindText() {}, bindNull() {},
  step() { return false; },
  columnCount() { return 0; },
  columnInt() { return 0; },
  columnText() { return ""; },
  reset() {}, finalize() {},
};

const mod = await import("./sqlite_introspect_bulk.deno.js");

// ── schema_tables ────────────────────────────────────────────────────
const tablesJson = mod.smoke_schema_tables(":memory:");
const tables = JSON.parse(tablesJson);
assert.deepEqual(
  tables.sort(),
  ["posts", "users"],
  "smoke_schema_tables: returns the two created tables (sqlite_* excluded)",
);

// ── schema_columns ──────────────────────────────────────────────────
const colsJson = mod.smoke_schema_columns(":memory:");
const cols = JSON.parse(colsJson);
assert.equal(cols.length, 3, "smoke_schema_columns: 3 columns parsed");
assert.equal(cols[0].name, "id", "first column is id");
assert.equal(cols[0].pk, true, "id column flagged as PK");
assert.equal(cols[1].name, "name", "second column is name");
assert.equal(cols[2].name, "age", "third column is age");

// ── table_exists ────────────────────────────────────────────────────
assert.equal(mod.smoke_table_exists_true(":memory:"), true, "present table: exists=true");
assert.equal(mod.smoke_table_exists_false(":memory:"), false, "absent table: exists=false");

// ── import_csv ──────────────────────────────────────────────────────
vfs.set("/tmp/in.csv", "a,b\n1,alpha\n2,beta\n3,gamma\n");
const inserted = mod.smoke_import_csv(":memory:", "rows", "/tmp/in.csv");
assert.equal(inserted, 3, "smoke_import_csv: 3 data rows imported (header skipped)");

// ── export_csv ──────────────────────────────────────────────────────
const exported = mod.smoke_export_csv(":memory:", "SELECT a, b FROM rows", "[]", "/tmp/out.csv");
assert.equal(exported, 3, "smoke_export_csv: 3 rows exported");
const out = vfs.get("/tmp/out.csv");
assert.ok(out.startsWith("a,b\n"), "exported CSV starts with column header");
assert.ok(out.includes("1,one"), "exported CSV contains first row");
assert.ok(out.includes("3,three"), "exported CSV contains third row");

// ── last_error ──────────────────────────────────────────────────────
assert.equal(mod.smoke_last_error_empty(":memory:"), "", "smoke_last_error_empty: returns ''");
assert.equal(
  mod.smoke_last_error_set(":memory:"),
  "simulated failure",
  "smoke_last_error_set: fault-injected message round-trips",
);

console.log("sqlite_introspect_bulk OK");
