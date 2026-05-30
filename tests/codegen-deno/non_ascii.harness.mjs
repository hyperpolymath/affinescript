// SPDX-License-Identifier: MPL-2.0
// issue #460 — round-trip non-ASCII string literals through the
// Deno-ESM backend under strict-mode ESM. The `import` itself is the
// strictest test: if the emitted `.deno.js` contains octal escapes,
// the module fails to parse and the import throws SyntaxError before
// any assertion can run.
import assert from "node:assert/strict";
import {
  emoji_cross,
  emoji_check,
  cjk_hello,
  latin_accent,
  non_bmp_sob,
  mixed,
  ascii_only,
  quotes_and_backslash,
} from "./non_ascii.deno.js";

assert.equal(emoji_cross(), "❌", "BMP emoji ❌ round-trips");
assert.equal(emoji_check(), "✓", "BMP check mark ✓ round-trips");
assert.equal(cjk_hello(), "你好", "CJK 'nihao' round-trips");
assert.equal(latin_accent(), "café résumé", "Latin accented round-trips");
assert.equal(non_bmp_sob(), "\u{1F62D}", "non-BMP code point round-trips");
assert.equal(mixed(), "[OK] café 你好 ❌", "mixed ASCII+non-ASCII round-trips");
assert.equal(ascii_only(), "plain ASCII", "ASCII-only unchanged");
assert.equal(quotes_and_backslash(), "\"escaped\" and \\back", "quote+backslash escapes preserved");

console.log("non_ascii.harness.mjs OK");
