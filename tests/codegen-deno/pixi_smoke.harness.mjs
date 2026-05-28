// SPDX-License-Identifier: MPL-2.0
// bindings #1 — Node ESM harness for the PixiJS binding.
//
// Installs a globalThis.__as_pixi mock covering Application,
// Container, Sprite, Graphics, Texture; drives the smoke functions;
// asserts every side-effect was observed on the mock objects.

import assert from "node:assert/strict";

const initCalls = [];
const textureUrls = [];
const operationsLog = [];

class MockContainer {
  constructor() {
    this.x = 0; this.y = 0; this.visible = true;
    this.children = [];
  }
  addChild(c) { this.children.push(c); operationsLog.push("addChild"); }
  removeChild(c) {
    this.children = this.children.filter((ch) => ch !== c);
    operationsLog.push("removeChild");
  }
  destroy() { operationsLog.push("destroy"); }
}

class MockSprite extends MockContainer {
  constructor(texture) { super(); this.texture = texture; }
}

class MockGraphics extends MockContainer {
  constructor() { super(); this.paths = []; this.fills = []; }
  rect(x, y, w, h) { this.paths.push({ kind: "rect", x, y, w, h }); }
  fill(opts) { this.fills.push(opts); }
  clear() { this.paths = []; this.fills = []; }
}

class MockText extends MockContainer {
  constructor(opts) { super(); this.text = opts.text ?? ""; this.style = opts.style; }
}

class MockApplication {
  constructor() {
    this.stage = new MockContainer();
    this.canvas = { tagName: "CANVAS" };
    this.ticker = { add() {}, start() {}, stop() {} };
  }
  async init(options) { initCalls.push(options); }
  destroy() { operationsLog.push("appDestroy"); }
}

globalThis.__as_pixi = {
  Application: MockApplication,
  Container: MockContainer,
  Sprite: MockSprite,
  Graphics: MockGraphics,
  Text: MockText,
  Texture: {
    from(url) { textureUrls.push(url); return { __mockTexture: true, url }; },
  },
};

const { smokeInit, smokeSpriteFlow, smokeGraphicsFlow } = await import("./pixi_smoke.deno.js");

// Async init returns an Application after `await app.init(options)`
const app = await smokeInit({ width: 800, height: 600, backgroundColor: 0x1099bb });
assert.equal(initCalls.length, 1, "app.init called once");
assert.deepEqual(initCalls[0], { width: 800, height: 600, backgroundColor: 0x1099bb }, "init options reach host");

// Sprite flow: load texture → make sprite → upcast → addChild → position + visibility
assert.equal(smokeSpriteFlow(app, "/assets/player.png"), 0, "smokeSpriteFlow returns 0");
assert.deepEqual(textureUrls, ["/assets/player.png"], "Texture.from received the url");
assert.equal(app.stage.children.length, 1, "1 child added after sprite flow");
const sprite = app.stage.children[0];
assert.equal(sprite.x, 100, "sprite.x set");
assert.equal(sprite.y, 200, "sprite.y set");
assert.equal(sprite.visible, true, "sprite.visible set");

// Graphics flow: new → rect → fill → upcast → addChild
assert.equal(smokeGraphicsFlow(app, 0xff0000), 0, "smokeGraphicsFlow returns 0");
assert.equal(app.stage.children.length, 2, "graphics added as second child");
const graphics = app.stage.children[1];
assert.deepEqual(graphics.paths, [{ kind: "rect", x: 0, y: 0, w: 50, h: 50 }], "rect path recorded");
assert.deepEqual(graphics.fills, [{ color: 0xff0000 }], "fill recorded with color");

console.log("pixi_smoke.harness.mjs OK");
