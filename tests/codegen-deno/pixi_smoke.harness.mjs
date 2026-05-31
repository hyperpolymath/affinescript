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

class MockPoint {
  constructor() { this.x = 0; this.y = 0; }
  set(x, y) { this.x = x; this.y = y; }
}

class MockContainer {
  constructor() {
    this.x = 0; this.y = 0;
    this.scale = new MockPoint();
    this.pivot = new MockPoint();
    this.rotation = 0;
    this.alpha = 1;
    this.zIndex = 0;
    this.sortableChildren = false;
    this.eventMode = "auto";
    this.cursor = "";
    this.visible = true;
    this.children = [];
    this.handlers = new Map();
  }
  addChild(c) { this.children.push(c); operationsLog.push("addChild"); }
  removeChild(c) {
    this.children = this.children.filter((ch) => ch !== c);
    operationsLog.push("removeChild");
  }
  on(event, handler) {
    if (!this.handlers.has(event)) this.handlers.set(event, []);
    this.handlers.get(event).push(handler);
  }
  off(event, handler) {
    const list = this.handlers.get(event);
    if (!list) return;
    this.handlers.set(event, list.filter((h) => h !== handler));
  }
  destroy() { operationsLog.push("destroy"); }
}

class MockSprite extends MockContainer {
  constructor(texture) {
    super();
    this.texture = texture;
    this.anchor = new MockPoint();
  }
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

const { smokeInit, smokeSpriteFlow, smokeGraphicsFlow, smokeAccessorsFlow } = await import("./pixi_smoke.deno.js");

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

// Accessors flow: every Container 8.x setter + Sprite anchor + on/off
const onDown = (e) => { onDown.lastEvent = e; };
const onMove = (e) => { onMove.lastEvent = e; };
assert.equal(smokeAccessorsFlow(app, "/assets/btn.png", onDown, onMove), 0, "smokeAccessorsFlow returns 0");
assert.equal(textureUrls.length, 2, "second Texture.from recorded");
assert.equal(app.stage.children.length, 3, "accessor sprite added as third child");
const aSprite = app.stage.children[2];
assert.equal(aSprite.anchor.x, 0.5, "sprite.anchor.x"); assert.equal(aSprite.anchor.y, 0.5, "sprite.anchor.y");
assert.equal(aSprite.scale.x, 2.0, "container.scale.x"); assert.equal(aSprite.scale.y, 3.0, "container.scale.y");
assert.equal(aSprite.pivot.x, 10.0, "container.pivot.x"); assert.equal(aSprite.pivot.y, 20.0, "container.pivot.y");
assert.equal(aSprite.rotation, 1.5708, "container.rotation");
assert.equal(aSprite.alpha, 0.75, "container.alpha");
assert.equal(aSprite.zIndex, 7, "container.zIndex");
assert.equal(app.stage.sortableChildren, true, "stage.sortableChildren set");
assert.equal(aSprite.eventMode, "static", "container.eventMode");
assert.equal(aSprite.cursor, "pointer", "container.cursor");
assert.equal(aSprite.handlers.get("pointerdown").length, 1, "pointerdown handler registered");
assert.equal(aSprite.handlers.get("pointermove").length, 0, "pointermove handler off after off()");
assert.equal(aSprite.handlers.get("pointerdown")[0], onDown, "pointerdown handler identity preserved");

console.log("pixi_smoke.harness.mjs OK");
