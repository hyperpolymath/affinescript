// SPDX-License-Identifier: MPL-2.0
// db-theory #2 — Node ESM harness for the Transaction codegen smoke.
//
// Extends the #1a/#1b/#1c mock with the full transaction surface:
// txBegin / txCommit / txRollback / txSavepoint / txRelease /
// txRollbackTo / txDb / txIsLive.
//
// The mock implements WRITE-SET ISOLATION at the JS level by deep-
// cloning the affected table on `txBegin`, accumulating mutations
// in the cloned shadow, and either promoting on commit or
// discarding on rollback. This is enough to witness the
// rollback-discards-writes invariant from the AffineScript side —
// the formal proof of this invariant is the pending obligation
// tracked at `docs/proof-obligations/db-theory-2-transaction-safety.md`.

import assert from "node:assert/strict";

let nextDbHandle = 1;
let nextTxHandle = 1000;
const dbs = new Map();
const txs = new Map();   // tx handle -> { db, savepoints: [{name, snapshot}], live: bool, snapshot }

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

const snapshotTables = (db) => {
  const snap = new Map();
  for (const [k, v] of db.tables.entries()) snap.set(k, v.map((row) => [...row]));
  return snap;
};

const restoreTables = (db, snap) => {
  db.tables = new Map();
  for (const [k, v] of snap.entries()) db.tables.set(k, v.map((row) => [...row]));
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

  query(h, sql /* , params */) {
    const db = dbs.get(h);
    if (!db) return [];
    const m = sql.match(/FROM\s+(\w+)/i);
    if (!m) return [];
    return db.tables.get(m[1]) || [];
  },

  queryOne(h, sql, params) { return this.query(h, sql, params)[0] ?? null; },

  queryInt(h, sql) {
    const db = dbs.get(h);
    if (!db) return 0;
    // COUNT(*) FROM <table>
    const count = sql.match(/SELECT\s+COUNT\(\*\)\s+FROM\s+(\w+)/i);
    if (count) return (db.tables.get(count[1]) || []).length;
    // SELECT <expr> FROM <table> [WHERE ...] LIMIT 1 — return the first
    // row's first-column-relevant arithmetic. The smoke uses only `n`
    // and `a + b`, so handle both shapes.
    const fromM = sql.match(/FROM\s+(\w+)/i);
    if (!fromM) return 0;
    const rows = db.tables.get(fromM[1]) || [];
    if (rows.length === 0) return 0;
    if (/SELECT\s+a\s*\+\s*b/i.test(sql)) return Number(rows[0][0]) + Number(rows[0][1]);
    return Number(rows[0][0]);
  },

  // ── Transactions (db-theory #2) ───────────────────────────────────

  txBegin(h) {
    const db = dbs.get(h);
    if (!db) throw new Error("invalid db handle " + h);
    const t = nextTxHandle++;
    txs.set(t, { db, savepoints: [], live: true, snapshot: snapshotTables(db) });
    return t;
  },

  txCommit(t) {
    const tx = txs.get(t);
    if (!tx || !tx.live) throw new Error("invalid or already-consumed tx handle " + t);
    // Commit: drop the snapshot, leave current tables in place.
    tx.live = false;
    txs.delete(t);
  },

  txRollback(t) {
    const tx = txs.get(t);
    if (!tx || !tx.live) throw new Error("invalid or already-consumed tx handle " + t);
    // Rollback: restore the snapshot — discards every write since txBegin.
    restoreTables(tx.db, tx.snapshot);
    tx.live = false;
    txs.delete(t);
  },

  txSavepoint(t, name) {
    const tx = txs.get(t);
    if (!tx || !tx.live) throw new Error("invalid or already-consumed tx handle " + t);
    tx.savepoints.push({ name, snapshot: snapshotTables(tx.db) });
  },

  txRelease(t, name) {
    const tx = txs.get(t);
    if (!tx || !tx.live) throw new Error("invalid or already-consumed tx handle " + t);
    // Release: forget the savepoint (LIFO match) but keep writes.
    const idx = [...tx.savepoints].reverse().findIndex((sp) => sp.name === name);
    if (idx === -1) throw new Error("no such savepoint " + name);
    tx.savepoints.splice(tx.savepoints.length - 1 - idx, 1);
  },

  txRollbackTo(t, name) {
    const tx = txs.get(t);
    if (!tx || !tx.live) throw new Error("invalid or already-consumed tx handle " + t);
    // Rollback to: restore from named savepoint; outer tx remains live.
    const idx = [...tx.savepoints].reverse().findIndex((sp) => sp.name === name);
    if (idx === -1) throw new Error("no such savepoint " + name);
    const realIdx = tx.savepoints.length - 1 - idx;
    restoreTables(tx.db, tx.savepoints[realIdx].snapshot);
    // SQLite semantics: rollback_to keeps the savepoint itself.
  },

  txDb(t) {
    const tx = txs.get(t);
    if (!tx) throw new Error("invalid tx handle " + t);
    // Aliasing: return the same db handle the user already has.
    // We don't carry the original handle ID, so retrieve it by
    // matching the db object reference.
    for (const [k, v] of dbs.entries()) if (v === tx.db) return k;
    throw new Error("tx_db: db not found for tx " + t);
  },

  txIsLive(t) { const tx = txs.get(t); return !!(tx && tx.live); },

  // Convenience surface methods retained for compat with other harnesses.
  prepare() { throw new Error("prepare not used in this smoke"); },
  bindInt() {}, bindText() {}, bindNull() {},
  step() { return false }, columnCount() { return 0 },
  columnInt() { return 0 }, columnText() { return "" },
  reset() {}, finalize() {},
  schemaTables() { return "[]"; }, schemaColumns() { return "[]"; },
  tableExists() { return false; }, importCsv() { return 0; },
  exportCsv() { return 0; }, lastError() { return ""; },
};

const mod = await import("./transaction_smoke.deno.js");

// ── commit_persists ────────────────────────────────────────────────
assert.equal(
  mod.smoke_commit_persists(":memory:"), 42,
  "smoke_commit_persists: inserted-then-committed row visible afterward",
);

// ── rollback_discards (the key safety witness) ─────────────────────
assert.equal(
  mod.smoke_rollback_discards(":memory:"), 1,
  "smoke_rollback_discards: only pre-tx row remains after rollback (writes during tx discarded)",
);

// ── savepoint_release ──────────────────────────────────────────────
assert.equal(
  mod.smoke_savepoint_release(":memory:"), 3,
  "smoke_savepoint_release: release keeps savepoint writes within outer tx",
);

// ── savepoint_rollback_to ──────────────────────────────────────────
assert.equal(
  mod.smoke_savepoint_rollback_to(":memory:"), 1,
  "smoke_savepoint_rollback_to: rollback_to discards writes since savepoint; outer tx commits remaining",
);

// ── tx_db aliasing ─────────────────────────────────────────────────
assert.equal(
  mod.smoke_tx_db_aliasing(":memory:"), 7,
  "smoke_tx_db_aliasing: tx_db returns Db handle participating in open tx",
);

// ── is_live lifecycle ──────────────────────────────────────────────
assert.equal(
  mod.smoke_is_live_lifecycle(":memory:"), 1,
  "smoke_is_live_lifecycle: tx_is_live returns 1 while tx open",
);

console.log("transaction_smoke OK");
