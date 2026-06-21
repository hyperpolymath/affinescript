// SPDX-License-Identifier: MIT OR MPL-2.0
// In-editor smoke harness driver for #139.
//
// Downloads a pinned VS Code, launches it with this extension folder loaded
// via --extensionDevelopmentPath, and runs the Mocha suite at suite/index.js
// inside the extension host. Plain JavaScript (not TypeScript): the VS Code
// test runner is Node-native and unavoidable; an exemption is recorded in
// CLAUDE.md under "Runtime Exemptions".

"use strict";

const path = require("path");
const { runTests } = require("@vscode/test-electron");

async function main() {
  const extensionDevelopmentPath = path.resolve(__dirname, "..");
  const extensionTestsPath = path.resolve(__dirname, "suite", "index.js");

  try {
    await runTests({
      extensionDevelopmentPath,
      extensionTestsPath,
      launchArgs: ["--disable-extensions"],
    });
  } catch (err) {
    console.error("Smoke run failed:", err);
    process.exit(1);
  }
}

main();
