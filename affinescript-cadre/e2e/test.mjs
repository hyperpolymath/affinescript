// SPDX-License-Identifier: MPL-2.0
// e2e test for the CadreRouter JS wrapper.

import assert from 'node:assert/strict';
import { CadreRouter } from '../src/CadreRouter.js';

async function main() {
  const wasmPath = process.argv[2] || './router.wasm';
  const router = await CadreRouter.create(wasmPath, { base: import.meta.url });

  // Initial State assertions
  assert.equal(router.screenW, 1280, 'Initial screen width should be 1280');
  assert.equal(router.screenH, 720, 'Initial screen height should be 720');
  assert.equal(router.stackLen, 0, 'Initial stack should be empty');
  assert.equal(router.stackTop, -1, 'Initial stack top should be -1');
  assert.equal(router.popupTag, -1, 'Initial popup tag should be -1');

  // Push a screen
  router.push(4); // 4 = Game
  assert.equal(router.stackLen, 1, 'Stack length should be 1 after push');
  assert.equal(router.stackTop, 4, 'Stack top should be 4 (Game)');

  // Push another screen
  router.push(1); // 1 = CharacterSelect
  assert.equal(router.stackLen, 2, 'Stack length should be 2 after push');
  assert.equal(router.stackTop, 1, 'Stack top should be 1 (CharacterSelect)');

  // Resize
  router.resize(1920, 1080);
  assert.equal(router.screenW, 1920, 'Width should be updated to 1920');
  assert.equal(router.screenH, 1080, 'Height should be updated to 1080');

  // Pop
  router.pop();
  assert.equal(router.stackLen, 1, 'Stack length should be 1 after pop');
  assert.equal(router.stackTop, 4, 'Stack top should be 4 (Game) after pop');

  // Popup
  router.presentPopup(2); // 2 = Hacking
  assert.equal(router.popupTag, 2, 'Popup tag should be 2 (Hacking)');
  
  router.dismissPopup();
  assert.equal(router.popupTag, -1, 'Popup tag should be -1 after dismiss');

  console.log('ALL ASSERTIONS PASS — CadreRouter ran end-to-end');
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
