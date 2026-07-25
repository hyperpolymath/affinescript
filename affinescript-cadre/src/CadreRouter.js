// SPDX-License-Identifier: MPL-2.0
// Cadre Router runtime wrapping the affine-js loader and tea_router wasm.

import { readBytes, buildImportObject } from '../../packages/affine-js/loader.js';

export class CadreRouter {
  constructor(instance) {
    this.exports = instance.exports;
  }

  /**
   * Initialize a new CadreRouter instance from a WASM module.
   * @param {string | URL} wasmSource 
   * @param {{ base?: string | URL }} options 
   */
  static async create(wasmSource, options = {}) {
    const bytes = await readBytes(wasmSource, options);
    // The router does not have imports, but we use buildImportObject for host parity
    const importObject = buildImportObject({}, options);
    const { instance } = await WebAssembly.instantiate(bytes, importObject);
    const router = new CadreRouter(instance);
    router.exports.affinescript_router_init();
    return router;
  }

  // --- Actions ---

  push(screenTag) {
    this.exports.affinescript_router_push(screenTag);
  }

  pop() {
    this.exports.affinescript_router_pop();
  }

  presentPopup(popupTag) {
    this.exports.affinescript_router_present_popup(popupTag);
  }

  dismissPopup() {
    this.exports.affinescript_router_dismiss_popup();
  }

  resize(w, h) {
    this.exports.affinescript_router_resize(w, h);
  }

  // --- Getters ---

  get screenW() {
    return this.exports.affinescript_router_get_screen_w();
  }

  get screenH() {
    return this.exports.affinescript_router_get_screen_h();
  }

  get stackLen() {
    return this.exports.affinescript_router_get_stack_len();
  }

  get stackTop() {
    return this.exports.affinescript_router_get_stack_top();
  }

  get popupTag() {
    return this.exports.affinescript_router_get_popup_tag();
  }
}
