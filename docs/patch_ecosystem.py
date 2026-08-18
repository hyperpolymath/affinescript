import sys

with open('ECOSYSTEM.adoc', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace block 1
old1 = '''|ffinescript-dom-loader |scope-deferred |INT-02 substrate (#179)
shipped + closed 2026-05-31 as packages/affine-js/loader.js — already
host-agnostic (Deno/Node/browser parity). Whether the satellite repo
still earns its keep (vs. folding into ffine-js) is the open question
in #489; INT-08 reconciler runtime (#183) is verified end-to-end 2026-07-07
(ffinescript-dom/e2e/run.sh; #255 fixed via #257) — revisit when it
dictates any DOM-specific loader surface.

|ffinescript-cadre'''

new1 = '''|ffinescript-cadre'''

content = content.replace(old1, new1)

# Replace block 2
old2 = '''|INT-02 |Host-agnostic loader bridge (ffinescript-dom-loader) |#179
**CLOSED 2026-05-31** |loader in packages/affine-js (SAT-02 fixed;
Deno/Node/browser parity, multi-namespace import object, ownership-section
accessor). *PROVEN + regression-locked:* 14 unit tests via pinned
deno task test (was flag-fragile — no run task) + 	ests/modules/loader-bridge/
drives the *real* loader API over genuine compiler-emitted cross-module wasm
(eadBytes+uildImportObject link CrossCallee.consume(42)=42;
parseOwnershipSection reads a real Linear-param entry) — closes
INT-01 ↔ INT-02. S1; **unblocked INT-05/08/11**. The ffinescript-dom-loader
satellite shell is downstream — scope question deferred to #489.'''

new2 = '''|INT-02 |Host-agnostic loader bridge |#179
**CLOSED 2026-05-31** |loader in packages/affine-js (SAT-02 fixed;
Deno/Node/browser parity, multi-namespace import object, ownership-section
accessor). *PROVEN + regression-locked:* 14 unit tests via pinned
deno task test (was flag-fragile — no run task) + 	ests/modules/loader-bridge/
drives the *real* loader API over genuine compiler-emitted cross-module wasm
(eadBytes+uildImportObject link CrossCallee.consume(42)=42;
parseOwnershipSection reads a real Linear-param entry) — closes
INT-01 ↔ INT-02. S1; **unblocked INT-05/08/11**. The ffinescript-dom-loader
satellite repo concept was dropped (#489 closed via Option A: folded into ffine-js).'''

content = content.replace(old2, new2)

# Replace block 3
old3 = '''|INT-11 |Browser host parity (DOM loader + reconciler end-to-end) |
ledger-only |planned (INT-02 dep cleared 2026-05-31 via #179; INT-08
runtime verified end-to-end 2026-07-07 under Node — browser-host parity
is the remaining leg). Satellite-repo question = #489.'''

new3 = '''|INT-11 |Browser host parity (ffine-js loader + ffinescript-dom reconciler end-to-end) |
ledger-only |planned (INT-02 dep cleared 2026-05-31 via #179; INT-08
runtime verified end-to-end 2026-07-07 under Node — browser-host parity
is the remaining leg). Satellite-repo question resolved (#489 closed via Option A: folded into ffine-js).'''

content = content.replace(old3, new3)

with open('ECOSYSTEM.adoc', 'w', encoding='utf-8') as f:
    f.write(content)

print(\
