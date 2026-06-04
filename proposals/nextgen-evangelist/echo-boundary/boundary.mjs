// SPDX-License-Identifier: MPL-2.0
// hypatia: allow cicd_rules/javascript_detected -- Deno trial component for nextgen-evangelist; production target is Rust/AffineScript (see proposals/nextgen-evangelist/README.adoc)
// SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
//
// echo-boundary: turn the @boundary encoding-faithfulness obligation into
// a machine-checked check. Given an encoding table (host name -> integer
// code), decide whether the boundary is LOSSLESS (the codes are pairwise
// distinct, so the integer faithfully names the host value) or CONTROLLED
// LOSS (two distinct names collide on one code, e.g. a clamp/sentinel),
// then GENERATE a self-contained Agda module that PROVES that verdict by
// reusing the existing EchoEncodingFaithfulness framework, and TYPECHECK
// it (agda exit 0 = the verdict is certified).
//
// This is a Deno tool. Run as plain JavaScript (TypeScript is banned
// estate-wide). No network access is used.
//
//   deno run --allow-read --allow-write --allow-run --allow-env \
//     boundary.mjs '{"Open":0,"Weak":1,"Medium":2,"Strong":3}'
//
//   echo '{"table":{"Open":0,"Weak":1,"Medium":2,"Strong":3},"clamp":{"OOB":0}}' \
//     | deno run --allow-read --allow-write --allow-run --allow-env boundary.mjs
//
// Flags:
//   --name <Mod>     name of the generated Agda module (default auto)
//   --mirror <dir>   path to the echo mirror repo
//                    (default /tmp/mirror-boundary)
//   --out <file>     also write the generated .agda to this path
//   --no-check       generate only; do not invoke agda
//   --help

// ---------------------------------------------------------------------------
// Argument / input handling
// ---------------------------------------------------------------------------

function parseArgs(argv) {
  const opts = {
    name: null,
    mirror: "/tmp/mirror-boundary",
    out: null,
    check: true,
    json: null,
  };
  const positional = [];
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === "--help" || a === "-h") {
      opts.help = true;
    } else if (a === "--no-check") {
      opts.check = false;
    } else if (a === "--name") {
      opts.name = argv[++i];
    } else if (a === "--mirror") {
      opts.mirror = argv[++i];
    } else if (a === "--out") {
      opts.out = argv[++i];
    } else {
      positional.push(a);
    }
  }
  if (positional.length > 0) opts.json = positional.join(" ");
  return opts;
}

async function readStdin() {
  const chunks = [];
  const buf = new Uint8Array(4096);
  while (true) {
    const n = await Deno.stdin.read(buf);
    if (n === null) break;
    chunks.push(buf.slice(0, n));
  }
  const total = chunks.reduce((s, c) => s + c.length, 0);
  const out = new Uint8Array(total);
  let off = 0;
  for (const c of chunks) {
    out.set(c, off);
    off += c.length;
  }
  return new TextDecoder().decode(out).trim();
}

const HELP = `echo-boundary — machine-check the @boundary encoding-faithfulness obligation

USAGE
  boundary.mjs '<json>'                 table on argv
  echo '<json>' | boundary.mjs          table on stdin

INPUT
  Bare table:    {"Open":0,"Weak":1,"Medium":2,"Strong":3}
  With clamp:    {"table":{...},"clamp":{"OOB":0}}
                 (clamp entries are merged into the table; an OOB name that
                  shares a code with a valid name is the canonical clamp bug)

VERDICT
  LOSSLESS        codes pairwise distinct (encoder injective)
  CONTROLLED LOSS two distinct names share a code (collision / clamp)

For each verdict a self-contained Agda module is generated against the
EchoEncodingFaithfulness framework and typechecked (agda exit 0 = certified).

FLAGS
  --name <Mod>    generated Agda module name (default auto)
  --mirror <dir>  echo mirror repo (default /tmp/mirror-boundary)
  --out <file>    also write the generated .agda here
  --no-check      generate only, skip agda
  --help
`;

// ---------------------------------------------------------------------------
// Table normalisation
// ---------------------------------------------------------------------------

// Accept either a bare {name: code} object or {table:{...}, clamp:{...}}.
// Returns an ordered array of {name, code, oob} entries. `oob` marks entries
// that arrived via the clamp spec (purely informational for the report; the
// faithfulness logic treats every entry uniformly).
function normalizeTable(raw) {
  let table;
  let clamp = {};
  if (raw && typeof raw === "object" && !Array.isArray(raw) &&
      Object.prototype.hasOwnProperty.call(raw, "table")) {
    table = raw.table;
    clamp = raw.clamp || {};
  } else {
    table = raw;
  }
  if (!table || typeof table !== "object" || Array.isArray(table)) {
    throw new Error("table must be a JSON object of {name: integer-code}");
  }
  const entries = [];
  const seenNames = new Set();
  for (const [name, code] of Object.entries(table)) {
    checkEntry(name, code, seenNames);
    entries.push({ name, code, oob: false });
  }
  for (const [name, code] of Object.entries(clamp)) {
    checkEntry(name, code, seenNames);
    entries.push({ name, code, oob: true });
  }
  if (entries.length === 0) throw new Error("table is empty");
  return entries;
}

function checkEntry(name, code, seenNames) {
  if (seenNames.has(name)) {
    // JSON objects can't have duplicate keys after parse, but a clamp name
    // could shadow a table name; that's a genuine spec error.
    throw new Error(`duplicate name in table: ${JSON.stringify(name)}`);
  }
  seenNames.add(name);
  if (typeof code !== "number" || !Number.isInteger(code)) {
    throw new Error(`code for ${JSON.stringify(name)} must be an integer`);
  }
  if (code < 0) {
    throw new Error(
      `code for ${JSON.stringify(name)} is negative (${code}); ` +
      `codes are encoded as Agda ℕ literals, so must be ≥ 0`,
    );
  }
}

// ---------------------------------------------------------------------------
// Verdict
// ---------------------------------------------------------------------------

// Lossless iff codes pairwise distinct. Otherwise find a witnessing
// collision: the first two distinct names sharing one code.
function decide(entries) {
  const byCode = new Map();
  for (const e of entries) {
    if (!byCode.has(e.code)) byCode.set(e.code, []);
    byCode.get(e.code).push(e);
  }
  for (const [code, group] of byCode) {
    if (group.length >= 2) {
      return {
        lossless: false,
        collision: { code, a: group[0], b: group[1], group },
      };
    }
  }
  return { lossless: true, collision: null };
}

// ---------------------------------------------------------------------------
// Agda identifier safety
// ---------------------------------------------------------------------------

// Map each entry to a guaranteed-safe Agda constructor name. We use indexed
// `cN` constructors rather than the host names directly, which sidesteps every
// hazard at once: Agda keywords (`open`, `data`, `where`, ...), names with
// spaces / punctuation / non-ASCII, and names that collide after sanitisation.
// The original host name is preserved in a comment on each constructor.
function assignCtors(entries) {
  return entries.map((e, i) => ({ ...e, ctor: `c${i}` }));
}

function moduleNameFor(entries, verdict, explicit) {
  if (explicit) return explicit;
  return verdict.lossless ? "BoundaryLossless" : "BoundaryControlledLoss";
}

// Escape a host name for safe inclusion inside an Agda line comment.
function commentSafe(s) {
  return String(s).replace(/[\r\n]/g, " ");
}

// ---------------------------------------------------------------------------
// Agda code generation
// ---------------------------------------------------------------------------

const GEN_HEADER = (modName, srcJson) =>
  `{-# OPTIONS --safe --without-K #-}
-- SPDX-License-Identifier: MPL-2.0
-- SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
--
-- GENERATED by echo-boundary (boundary.mjs). Do not edit by hand.
--
-- This module certifies an @boundary encoding-faithfulness verdict for a
-- concrete host-value -> integer-code table, by reusing the
-- EchoEncodingFaithfulness framework. It follows the SHAPE of that module's
-- worked Rank / rankCode / rankCode-injective instance.
--
-- Source table (host name -> code):
${srcJson.split("\n").map((l) => "--   " + l).join("\n")}

module ${modName} where

open import Echo                   using (Echo)
open import EchoImageFactorization using (Injective)
open import EchoEncodingFaithfulness using (Encoding; module EncodingTheorems)

open import Data.Nat.Base         using (ℕ)
open import Data.Product.Base     using (Σ; _,_; _×_; proj₁)
open import Relation.Binary.PropositionalEquality
                                  using (_≡_; _≢_; refl)
open import Relation.Nullary      using (¬_)
`;

function genEnum(modName, entries) {
  const ctorList = entries.map((e) => e.ctor).join(" ");
  const lines = [];
  lines.push("-- The host-side enumerated value (one constructor per table name).");
  for (const e of entries) {
    lines.push(`--   ${e.ctor} = ${commentSafe(e.name)}` +
      (e.oob ? "   (out-of-band / clamp entry)" : ""));
  }
  lines.push(`data Host : Set where`);
  lines.push(`  ${ctorList} : Host`);
  return lines.join("\n");
}

function genCode(entries) {
  const lines = [];
  lines.push("-- The integer code the pure kernel computes on.");
  lines.push("code : Host → ℕ");
  // Align the arrows for readability.
  const w = Math.max(...entries.map((e) => e.ctor.length));
  for (const e of entries) {
    lines.push(`code ${e.ctor.padEnd(w)} = ${e.code}`);
  }
  return lines.join("\n");
}

// Lossless: diagonal-only injectivity (exactly like rankCode-injective).
// This is sound BECAUSE every `code` clause maps to a literal ℕ numeral, so
// Agda's coverage checker discharges every off-diagonal pair as an absurd
// numeral equality (e.g. 0 ≡ 1) without us listing it. See the note in
// EchoEncodingFaithfulness.agda lines ~369-371. We emit only the diagonal.
function genLosslessProof(modName, entries) {
  const w = Math.max(...entries.map((e) => e.ctor.length));
  const lines = [];
  lines.push("-- Injectivity by exhaustive case analysis. The codes are literal");
  lines.push("-- numerals, so listing the diagonal refl clauses is enough: Agda's");
  lines.push("-- coverage checker discharges the off-diagonal pairs as absurd");
  lines.push("-- numeral equalities (mirrors rankCode-injective).");
  lines.push("code-injective : Injective code");
  for (const e of entries) {
    const pat = `{${e.ctor}}`.padEnd(w + 2);
    lines.push(`code-injective ${pat} ${pat} _ = refl`);
  }
  lines.push("");
  lines.push("encoding : Encoding");
  lines.push("encoding = record");
  lines.push("  { Source = Host");
  lines.push("  ; Code   = ℕ");
  lines.push("  ; enc    = code");
  lines.push("  }");
  lines.push("");
  lines.push("-- Re-export the lossless-half headline, instantiated and proved:");
  lines.push("-- every integer code determines its host value uniquely.");
  lines.push("module Theorems = EncodingTheorems encoding");
  lines.push("");
  lines.push("boundary-lossless :");
  lines.push("  (i : ℕ) (e₁ e₂ : Echo code i) → proj₁ e₁ ≡ proj₁ e₂");
  lines.push("boundary-lossless = Theorems.encoding-lossless code-injective");
  return lines.join("\n");
}

// Controlled loss: exhibit the two colliding constructors and invoke
// encoding-collision⇒no-section (exactly like clamp-sentinel-no-section).
function genLossyProof(modName, entries, collision) {
  const a = collision.a;
  const b = collision.b;
  const lines = [];
  lines.push("encoding : Encoding");
  lines.push("encoding = record");
  lines.push("  { Source = Host");
  lines.push("  ; Code   = ℕ");
  lines.push("  ; enc    = code");
  lines.push("  }");
  lines.push("");
  lines.push("module Theorems = EncodingTheorems encoding");
  lines.push("");
  lines.push(`-- The collision, as data: two distinct host values collapse onto the`);
  lines.push(`-- same code ${collision.code}.`);
  lines.push(`--   ${a.ctor} = ${commentSafe(a.name)}` +
    (a.oob ? "  (out-of-band / clamp)" : ""));
  lines.push(`--   ${b.ctor} = ${commentSafe(b.name)}` +
    (b.oob ? "  (out-of-band / clamp)" : ""));
  lines.push(`-- Distinctness is constructor disjointness; the shared code is refl.`);
  lines.push(`${a.ctor}≢${b.ctor} : ${a.ctor} ≢ ${b.ctor}`);
  lines.push(`${a.ctor}≢${b.ctor} ()`);
  lines.push("");
  lines.push(`collision : ${a.ctor} ≢ ${b.ctor} × code ${a.ctor} ≡ code ${b.ctor}`);
  lines.push(`collision = ${a.ctor}≢${b.ctor} , refl`);
  lines.push("");
  lines.push("-- THE VERDICT, certified: the boundary exhibits controlled loss.");
  lines.push("-- The colliding fibre admits NO section -- there is no pure");
  lines.push("-- decode : ℕ → Host recovering the host value from its code,");
  lines.push("-- because two distinct host values share one code. The lost");
  lines.push(`-- distinction is named and localised at code ${collision.code}.`);
  lines.push("boundary-controlled-loss :");
  lines.push("  ¬ Σ (ℕ → Host) (λ decode → ∀ x → decode (code x) ≡ x)");
  lines.push("boundary-controlled-loss =");
  lines.push("  Theorems.encoding-collision⇒no-section");
  lines.push(`    ${a.ctor} ${b.ctor}`);
  lines.push(`    ${a.ctor}≢${b.ctor}`);
  lines.push("    refl");
  return lines.join("\n");
}

function generateAgda(modName, entries, verdict, srcJson) {
  const parts = [
    GEN_HEADER(modName, srcJson),
    genEnum(modName, entries),
    genCode(entries),
    verdict.lossless
      ? genLosslessProof(modName, entries)
      : genLossyProof(modName, entries, verdict.collision),
    "",
  ];
  return parts.join("\n\n");
}

// ---------------------------------------------------------------------------
// Typecheck driver
// ---------------------------------------------------------------------------

async function typecheck(mirror, modName, source) {
  const agdaDir = `${mirror}/proofs/agda`;
  const modPath = `${agdaDir}/${modName}.agda`;
  await Deno.writeTextFile(modPath, source);
  const cmd = new Deno.Command("agda", {
    args: ["-i", "proofs/agda", `proofs/agda/${modName}.agda`],
    cwd: mirror,
    env: { LC_ALL: "C.UTF-8" },
    stdout: "piped",
    stderr: "piped",
  });
  const { code, stdout, stderr } = await cmd.output();
  const out = new TextDecoder().decode(stdout) +
    new TextDecoder().decode(stderr);
  return { exit: code, output: out.trim(), modPath };
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const opts = parseArgs(Deno.args);
  if (opts.help) {
    console.log(HELP);
    return 0;
  }
  let jsonText = opts.json;
  if (!jsonText) jsonText = await readStdin();
  if (!jsonText) {
    console.error("error: no input table (pass JSON on argv or stdin; --help)");
    return 2;
  }

  let raw;
  try {
    raw = JSON.parse(jsonText);
  } catch (e) {
    console.error(`error: input is not valid JSON: ${e.message}`);
    return 2;
  }

  let entries;
  try {
    entries = normalizeTable(raw);
  } catch (e) {
    console.error(`error: ${e.message}`);
    return 2;
  }

  entries = assignCtors(entries);
  const verdict = decide(entries);
  const modName = moduleNameFor(entries, verdict, opts.name);

  // Pretty-print the normalised table back for the generated-module banner.
  const tableForBanner = {};
  for (const e of entries) tableForBanner[e.name] = e.code;
  const srcJson = JSON.stringify(tableForBanner, null, 2);

  const source = generateAgda(modName, entries, verdict, srcJson);

  // Report the verdict to the human.
  const label = verdict.lossless ? "LOSSLESS" : "CONTROLLED LOSS";
  console.log(`verdict: ${label}`);
  console.log(`  names:  ${entries.length}`);
  console.log(`  codes:  ${[...new Set(entries.map((e) => e.code))].sort((a, b) => a - b).join(", ")}`);
  if (!verdict.lossless) {
    const c = verdict.collision;
    console.log(`  collision at code ${c.code}: ${JSON.stringify(c.a.name)} and ${JSON.stringify(c.b.name)}`);
  }
  console.log(`  module: ${modName}`);

  if (opts.out) {
    await Deno.writeTextFile(opts.out, source);
    console.log(`  wrote:  ${opts.out}`);
  }

  if (!opts.check) {
    if (!opts.out) {
      // Still surface the generated source on stdout when not checking.
      console.log("\n----- generated Agda -----");
      console.log(source);
    }
    return 0;
  }

  // Typecheck.
  let tc;
  try {
    tc = await typecheck(opts.mirror, modName, source);
  } catch (e) {
    console.error(`error: could not run agda: ${e.message}`);
    return 3;
  }
  console.log(`  agda:   exit ${tc.exit} (wrote ${tc.modPath})`);
  if (tc.exit === 0) {
    console.log(verdict.lossless
      ? "  PROOF HOLDS: encoder injective -> boundary is lossless (the integer IS the host value)."
      : "  PROOF HOLDS: code collision -> no section -> controlled loss is certified.");
  } else {
    console.log("  PROOF FAILED to typecheck; agda output:");
    console.log(tc.output);
  }
  return tc.exit === 0 ? 0 : 1;
}

if (import.meta.main) {
  Deno.exit(await main());
}
