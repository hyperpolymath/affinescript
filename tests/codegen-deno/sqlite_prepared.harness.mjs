// SPDX-License-Identifier: MPL-2.0
// db-theory #1b — Node ESM harness for the Sqlite prepared-statement
// codegen smoke.
//
// Reuses the #1a `__as_sqlite` mock shape (minimal in-memory SQL
// engine for the smoke subset) and extends it with prepared-statement
// methods. Each `Stmt` is an opaque integer handle that the mock
// resolves via an internal map; bind / step / column / reset /
// finalize operate on those handles directly.
//
// Production deployments use a real `__as_sqlite` adapter wrapping
// `jsr:@db/sqlite` or `better-sqlite3`; each library already provides
// near-1:1 wrappers for `prepare`, `bindInt`, `bindText`, `bindNull`,
// `step`, `columnCount`, `columnInt`, `columnText`, `reset`,
// `finalize` (with trivial name adaptations).

import assert from "node:assert/strict";

let nextDbHandle = 1;
let nextStmtHandle = 1;
const dbs = new Map();
const stmts = new Map();

// Same lightweight literal parser as sqlite_smoke.harness.mjs (kept
// inline to keep the harness self-contained — runtime tooling does
// not share helpers across harnesses by design).
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
    /^-?\d*\.\d+$/.test(t) ? Number(t) :
    t
  ));
};

// Substitute `?` placeholders left-to-right with the bound values
// (sqlite3 bind-index is 1-based).
const substituteBinds = (sql, binds) => {
  let i = 1;
  return sql.replace(/\?/g, () => {
    const v = binds[i++];
    if (v === null || v === undefined) return "NULL";
    if (typeof v === "number") return String(v);
    return "'" + String(v).replace(/'/g, "''") + "'";
  });
};

// Project a row array into a named-column row object using the table schema.
const projectRow = (db, table, row) => {
  const cols = db.schema.get(table) || [];
  const obj = {};
  for (let i = 0; i < cols.length; i++) obj[cols[i]] = row[i];
  return obj;
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
      const cols = create[2].split(",").map((c) => c.trim().split(/\s+/)[0]);
      db.tables.set(create[1], []);
      db.schema.set(create[1], cols);
      return;
    }

    const insert = sql.match(/INSERT INTO (\w+)(?:\s*\([^)]*\))?\s+VALUES\s+(.+)/i);
    if (insert) {
      const tableName = insert[1];
      const valuesPart = insert[2].replace(/;$/, "").trim();
      const tupleRe = /\(([^)]+)\)/g;
      let m;
      while ((m = tupleRe.exec(valuesPart))) {
        const cols = parseRowLiteral(m[1]);
        const tbl = db.tables.get(tableName);
        if (!tbl) throw new Error("no such table " + tableName);
        // Pad to schema width with NULL so single-column inserts work.
        const schemaCols = db.schema.get(tableName) || [];
        while (cols.length < schemaCols.length) cols.push(null);
        tbl.push(cols);
      }
    }
  },

  query(h, sql, params) {
    const db = dbs.get(h);
    const tblMatch = sql.match(/FROM\s+(\w+)/i);
    if (!tblMatch) return [];
    const table = tblMatch[1];
    const rows = db.tables.get(table) || [];

    const where = sql.match(/WHERE\s+(\w+)\s*=\s*\?/i);
    let filtered = rows;
    if (where) {
      const col = where[1];
      const cols = db.schema.get(table) || [];
      const idx = cols.indexOf(col);
      const v = params[0];
      filtered = rows.filter((r) => r[idx] === v);
    }

    const ordered = /ORDER\s+BY/i.test(sql)
      ? [...filtered].sort((a, b) => (a[0] > b[0] ? 1 : a[0] < b[0] ? -1 : 0))
      : filtered;

    return ordered.map((r) => projectRow(db, table, r));
  },

  queryOne(h, sql, params) {
    const all = this.query(h, sql, params);
    return all.length > 0 ? all[0] : null;
  },

  queryInt(h, sql, params) {
    if (/COUNT\(\*\)/i.test(sql)) {
      const tbl = sql.match(/FROM\s+(\w+)/i);
      return (dbs.get(h).tables.get(tbl[1]) || []).length;
    }
    // SUM-of-expression like `SELECT a + b FROM t LIMIT 1`: substitute
    // column names with row values from the first matching row.
    const db = dbs.get(h);
    const tbl = sql.match(/FROM\s+(\w+)/i);
    if (!tbl) return 0;
    const rows = db.tables.get(tbl[1]) || [];
    if (rows.length === 0) return 0;
    const cols = db.schema.get(tbl[1]) || [];
    const projMatch = sql.match(/SELECT\s+(.+?)\s+FROM/i);
    if (!projMatch) return 0;
    const expr = projMatch[1].trim();
    if (/^\w+$/.test(expr)) {
      const idx = cols.indexOf(expr);
      return Number(rows[0][idx] ?? 0);
    }
    // Tiny `a + b` evaluator: substitute column names with the first
    // row's numeric values, then eval. The smoke fixture's only
    // expression is `a + b`; this is intentionally narrow.
    let e = expr;
    for (let i = 0; i < cols.length; i++) {
      e = e.replace(new RegExp("\\b" + cols[i] + "\\b", "g"), String(rows[0][i] ?? 0));
    }
    // eslint-disable-next-line no-new-func
    return Number(new Function("return (" + e + ")")()) | 0;
  },

  // ── Prepared-statement surface (db-theory #1b) ───────────────────

  prepare(h, sql) {
    const sh = nextStmtHandle++;
    stmts.set(sh, {
      dbHandle: h,
      sql,
      binds: {},               // 1-indexed
      cursor: -1,              // -1 = not stepped yet
      resultRows: null,        // populated on first step for SELECT
      resultProjected: null,   // matching projected-row form
    });
    return sh;
  },
  bindInt(s, idx, v) { stmts.get(s).binds[idx] = Number(v) | 0; },
  bindText(s, idx, v) { stmts.get(s).binds[idx] = String(v); },
  bindNull(s, idx) { stmts.get(s).binds[idx] = null; },

  step(s) {
    const st = stmts.get(s);
    if (!st) throw new Error("invalid stmt handle " + s);
    const db = dbs.get(st.dbHandle);
    const sql = st.sql;

    // First step: execute the statement.
    if (st.cursor === -1) {
      if (/^\s*SELECT/i.test(sql)) {
        // Materialise rows; substitute binds into SQL so the existing
        // query machinery handles SELECT shape (`WHERE id = ?` etc.).
        const substituted = substituteBinds(sql, st.binds);
        const projected = this.query(st.dbHandle, substituted, []);
        st.resultProjected = projected;
        // Also keep the raw row form so column_int by index works
        // regardless of projection.
        const tblMatch = substituted.match(/FROM\s+(\w+)/i);
        const table = tblMatch ? tblMatch[1] : null;
        const cols = (table && db.schema.get(table)) || [];
        st.resultRows = projected.map((p) => cols.map((c) => p[c]));
        st.cursor = 0;
      } else {
        // INSERT / UPDATE / DELETE / DDL: substitute binds then execute.
        const substituted = substituteBinds(sql, st.binds);
        this.execute(st.dbHandle, substituted);
        st.cursor = 1; // already past the "row available" state
        return false; // SQLITE_DONE immediately
      }
    } else {
      st.cursor += 1;
    }

    if (st.resultRows && st.cursor < st.resultRows.length) return true;
    return false;
  },

  columnCount(s) {
    const st = stmts.get(s);
    if (!st.resultRows || st.cursor < 0 || st.cursor >= st.resultRows.length) return 0;
    return st.resultRows[st.cursor].length;
  },

  columnInt(s, idx) {
    const st = stmts.get(s);
    if (!st.resultRows || st.cursor < 0 || st.cursor >= st.resultRows.length) return null;
    const v = st.resultRows[st.cursor][idx];
    return v == null ? null : (Number(v) | 0);
  },

  columnText(s, idx) {
    const st = stmts.get(s);
    if (!st.resultRows || st.cursor < 0 || st.cursor >= st.resultRows.length) return null;
    const v = st.resultRows[st.cursor][idx];
    return v == null ? null : String(v);
  },

  reset(s) {
    const st = stmts.get(s);
    st.cursor = -1;
    st.resultRows = null;
    st.resultProjected = null;
    // Per sqlite3 semantics, `sqlite3_reset` does NOT clear binds —
    // they persist for the next step. Tests rely on this (and on the
    // option to re-bind any subset before stepping again).
  },

  finalize(s) { stmts.delete(s); },
};

const mod = await import("./sqlite_prepared.deno.js");

assert.equal(
  mod.smoke_prepare_bind_int_step_finalize(":memory:"),
  42,
  "smoke_prepare_bind_int_step_finalize: a=7, b=35 -> a+b = 42",
);

assert.equal(
  mod.smoke_step_iteration(":memory:"),
  15,
  "smoke_step_iteration: 1+2+3+4+5 = 15",
);

assert.equal(
  mod.smoke_text_bind_and_column(":memory:"),
  "affinescript",
  "smoke_text_bind_and_column: bound text round-trips through column_text",
);

assert.equal(
  mod.smoke_null_bind(":memory:"),
  0,
  "smoke_null_bind: bound NULL coerces to 0 via column_int",
);

assert.equal(
  mod.smoke_reset_and_reuse(":memory:"),
  2,
  "smoke_reset_and_reuse: prepare once + bind/step + reset + bind/step = 2 rows",
);

assert.equal(
  mod.smoke_column_count_basic(":memory:"),
  3,
  "smoke_column_count_basic: SELECT a, b, c FROM t -> column_count = 3",
);

console.log("sqlite_prepared OK");
