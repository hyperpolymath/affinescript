// SPDX-License-Identifier: MPL-2.0
// Copyright (c) Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Mocha entry point that the extension host invokes via
// --extensionTestsPath. Discovers *.test.js files in this directory.

"use strict";

const path = require("path");
const Mocha = require("mocha");
const { glob } = require("glob");

async function run() {
  const mocha = new Mocha({ ui: "tdd", color: false, timeout: 60000 });
  const testsRoot = path.resolve(__dirname);
  const files = await glob("**/*.test.js", { cwd: testsRoot });
  for (const f of files) mocha.addFile(path.resolve(testsRoot, f));

  return new Promise((resolve, reject) => {
    try {
      mocha.run((failures) => {
        if (failures > 0) reject(new Error(`${failures} test(s) failed.`));
        else resolve();
      });
    } catch (err) {
      reject(err);
    }
  });
}

exports.run = run;
