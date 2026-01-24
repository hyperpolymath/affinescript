// Simple WASM test runner for Node.js
const fs = require('fs');
const path = require('path');

const wasmFile = process.argv[2] || 'out.wasm';
const functionName = process.argv[3] || 'main';

const wasmBuffer = fs.readFileSync(wasmFile);

WebAssembly.instantiate(wasmBuffer).then(result => {
  const exports = result.instance.exports;

  if (typeof exports[functionName] === 'function') {
    const returnValue = exports[functionName]();
    console.log(`${functionName}() returned: ${returnValue}`);
    process.exit(0);  // Success regardless of return value
  } else {
    console.error(`Function ${functionName} not found in exports`);
    process.exit(1);
  }
}).catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
